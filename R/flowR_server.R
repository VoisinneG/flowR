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
  
  rval <- reactiveValues(gates_flowCore = list())

  gate <- reactiveValues(x = NULL, y = NULL)
  
  # Import module : import flowSet, gates from fcs files and workspace
  rval <- callModule(import, "import_module")
  
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
  
  # Display module
  plot_display <- callModule(display, "plot_module", rval, module_server_name = "plotGatingSet", simple_plot = FALSE)
  
  # stat module
  plot_statistics <- callModule(display, "statistics_module", rval, module_server_name = "plotStat")
  
  # save module
  callModule(saveWorkspace, "save_module", rval)
    
  
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
  ##########################################################################################################
  # observe and reactive functions
  
  observeEvent(rval$flow_set_selected, {
    updateSelectInput(session, "flow_set", choices = rval$flow_set_names, selected = rval$flow_set_selected)
  })
  
  observe({
    validate(
      need(input$flow_set, "No flow set selected")
    )
    
    rval$flow_set <- switch(input$flow_set,
                            "imported" = rval$flow_set_imported,
                            "filter" = rval$flow_set_filter,
                            "sub-sample" = rval$flow_set_sample,
                            "dim-reduction" = rval$flow_set_dim_red,
                            "cluster" = rval$flow_set_cluster
    )
    
  })
  
  observe({
    validate(
      need(rval$flow_set, "No flow set available")
    )
    
    fs <- rval$flow_set
    rval$Ncells_tot <- sum( sapply(1:length(fs), function(x){dim(fs[[x]]@exprs)[1]}) )
    
  })
  
  
  observeEvent(input$flow_set, {
    
    validate(
      need(rval$flow_set, "No flow set available")
    )
    
    fs <- rval$flow_set
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
  
  observeEvent(rval$flow_set, {
    
    validate(
      need(rval$flow_set, "No flow set available")
    )
    
    rval$gating_set <- GatingSet(rval$flow_set)
    
    # add gates
    print(names(rval$gates_flowCore))
    print("add2")
    rval$gating_set <- add_gates_flowCore(rval$gating_set, rval$gates_flowCore)
    
    
  })
  
  ##########################################################################################################
  ##########################################################################################################
  # Output 
  
  
  ##########################################################################################################
  # value boxes
  
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

}