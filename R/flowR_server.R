#' @import shiny
#' @import flowWorkspace
#' @import flowCore
#' @import ncdfFlow
#' @import gridExtra
#' @import DT
#' @import plotly
#' @import heatmaply
#' @import CytoML
#' @import Rgraphviz
#' @import scales
#' @import tools
#' @import utils
#' @import sp
#' @import viridis
#' @export
flowR_server <- function(session, input, output, user_module_name = NULL) {
  
  `%then%` <- shiny:::`%OR%`
  
  rval <- reactiveValues()

  gate <- reactiveValues(x = NULL, y = NULL)
  
  # Import module : import flowSet, gates from fcs files and workspace
  rval <- callModule(import, "import_module")
  
  # flow-set module
  rval <- callModule(flowsets, "flowsets_module", rval)
  
  # Metadata module
  rval <- callModule(metadata, "metadata_module", rval)

  # Transform module
  rval <- callModule(transform, "transform_module", rval)
  
  # Compensation module
  rval <- callModule(compensation, "compensation_module", rval)
  
  # Gating module
  rval <- callModule(gating, "gating_module", rval)
  
  # Subsample module
  rval <- callModule(subsample, "subsample_module", rval)
  
  # Dimensionality reduction module
  rval <- callModule(dimRed, "dim_reduction_module", rval)
  
  # Clustering module
  rval <- callModule(cluster, "cluster_module", rval)
  
  # Plot module
  rval <- callModule(plotting, "plotting_module", rval)
  # plot_display <- callModule(display, "plot_module", rval, 
  #                            module_server_name = "plotGatingSet", 
  #                            simple_plot = FALSE, auto_update = FALSE)
  
  # stats module
  rval <- callModule(stats, "stats_module", rval)
  # plot_statistics <- callModule(display, "statistics_module", rval, 
  #                               module_server_name = "plotStat")
  
  # save module
  #callModule(saveWorkspace, "save_module", rval)
    
  
  ##########################################################################################################
  # Add user-defined module
  module_server_function <- function(...){
    do.call(user_module_name, list(...) )
  }
  
  if(!is.null(user_module_name)){
   rval <- callModule(module_server_function, "user_module", rval)
  }
 
  ##########################################################################################################
  # General controls
  
  observeEvent(input$apply_trans, {
    rval$apply_trans <- input$apply_trans
  })
  
  observeEvent(input$apply_comp, {
    rval$apply_comp <- input$apply_comp
  })
  
  ##########################################################################################################
  # observe and reactive functions
  
  observeEvent( c(names(rval$flow_set_list), rval$flow_set_selected), {
    updateSelectInput(session, "flow_set", choices = names(rval$flow_set_list), selected = rval$flow_set_selected)
  })
  
  
  observeEvent(input$flow_set, {
    
    validate(
      need(input$flow_set %in% names(rval$flow_set_list), "No flow set available")
    )
    
    rval$flow_set_selected <- input$flow_set
    rval$flow_set <- rval$flow_set_list[[input$flow_set]]$flow_set
    
    print(input$flow_set)
    print(rval$gates_flowCore)
    print(rval$flow_set_list[[input$flow_set]]$gates)
    
    if(length(rval$gates_flowCore) == 0){
      rval$gates_flowCore <- rval$flow_set_list[[input$flow_set]]$gates
      print("gates updated")
      print(rval$gates_flowCore)
    }
    if(is.null(rval$df_spill)){
      rval$df_spill <- rval$flow_set_list[[input$flow_set]]$spill
    }
    if(is.null(rval$transformation)){
      rval$transformation <- rval$flow_set_list[[input$flow_set]]$transformation
    }
    if(is.null(rval$trans_parameters)){
      rval$trans_parameters <- rval$flow_set_list[[input$flow_set]]$trans_parameters
    }
    if(is.null(rval$pdata)){
      rval$pdata <- rval$flow_set_list[[input$flow_set]]$metadata
    }
    #if(is.null(rval$parameters)){
     rval$parameters <- NULL #rval$flow_set_list[[input$flow_set]]$parameters
    #}
    
    rval$gating_set <- GatingSet(rval$flow_set)
    rval$gating_set <- add_gates_flowCore(rval$gating_set, rval$gates_flowCore)

    
    fs <- rval$flow_set
    rval$Ncells_tot <- sum( sapply(1:length(fs), function(x){dim(fs[[x]]@exprs)[1]}) )
    
    params <- parameters(fs[[1]])$name
    
    min_val <- as.data.frame(fsApply(fs, each_col, min, na.rm = TRUE))
    min_val_all <- apply(min_val, 2, min)
    max_val <- as.data.frame(fsApply(fs, each_col, max,  na.rm = TRUE))
    max_val_all <- apply(max_val, 2, max)
    
    rval$data_range <- lapply(params, function(x){
      c(min_val_all[[x]] , max_val_all[[x]])
    })
    names(rval$data_range) <- params
    
  })
  
  # observe({
  #   rval$flow_set_list[[input$flow_set]]$gates <- rval$gates_flowCore
  #   rval$flow_set_list[[input$flow_set]]$spill <- rval$df_spill
  #   rval$flow_set_list[[input$flow_set]]$transformation <- rval$transformation
  #   rval$flow_set_list[[input$flow_set]]$trans_parameters <- rval$trans_parameters
  #   rval$flow_set_list[[input$flow_set]]$metadata <- rval$pdata
  # })
  
  ##########################################################################################################
  # Output value boxes
  
  output$progressBox <- renderValueBox({
    valueBox(
      length(rval$flow_set), "samples",icon = icon("list"),
      color = "purple"
    )
  })
  
  output$progressBox2 <- renderValueBox({
    ngates <- 0
    if(!is.null(rval$gating_set)){
      ngates <- length(setdiff(getNodes(rval$gating_set), "root"))
    }
    
    valueBox(
      ngates, "gates", icon = icon("list"),
      color = "yellow"
    )
  })
  
  output$progressBox3 <- renderValueBox({
    ncells <- 0
    if(!is.null(rval$Ncells_tot)){
      ncells <- rval$Ncells_tot
    }
    valueBox(
      format(ncells, digits = 2), "cells", icon = icon("list"),
      color = "green"
    )
  })
  
  output$progressBox4 <- renderValueBox({
    nparams <- 0
    if(!is.null(rval$flow_set)){
      nparams <- length(rval$flow_set@colnames)
    }
    
    valueBox(
      nparams, "parameters",icon = icon("list"),
      color = "red"
    )
  })
  
  output$flow_set_name <- renderText({
    if(nchar(rval$flow_set_selected)>0){
      paste("Flow-set : ", rval$flow_set_selected)
    }else{
      NULL
    }
  })

}