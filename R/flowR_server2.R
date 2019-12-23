#' flowR server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param module_names name of a shiny modules (server function) to integrate to the app
#' @import shiny
#' @importFrom flowCore fsApply each_col
#' @importFrom flowWorkspace gs_get_pop_paths
#' @importFrom shinydashboard renderValueBox
#' @export
flowR_server2 <- function(session, input, output, modules = NULL) {
  
  rval <- reactiveValues(update_gs = 0, # useful to force execution of 
                         #observe environment (for instance after updating a GatingSet with gs_pop_add()
                         gating_set = NULL,
                         flow_set_list = list(),
                         list_module_server_function = list(),
                         tab_elements = list(),
                         menu_elements = list(),
                         modules = NULL
                         )
  
  observe({
    #gs <- load_gs("./inst/ext/gs")
    #rval$gating_set <- gs
    data("GvHD", package = "flowCore")
    rval$gating_set <- GatingSet(GvHD)
  })
  
  
  observe({
    if(is.null(modules)){
      rval$modules <- c("import")
    }else{
      rval$modules <- modules
    }
  })
  
  output$body <- renderUI({
    tagList(
      textOutput("flow_set_name"),
      br(),
      fluidRow(
        valueBoxOutput("progressBox", width = 3),
        valueBoxOutput("progressBox2", width = 3),
        valueBoxOutput("progressBox3", width = 3),
        valueBoxOutput("progressBox4", width = 3),
      ),
      do.call(tabItems, rval$tab_elements)
    )
  })
  
  output$menu <- renderMenu({
    sidebarMenu(id = "menu",
                tagList(rval$menu_elements),
                menuItem("General controls",
                         tabName = "General_tab",
                         startExpanded = FALSE,
                         icon = icon("check-circle"),
                         checkboxInput("apply_comp", "apply compensation", TRUE),
                         checkboxInput("apply_trans", "apply transformation", TRUE),
                         selectInput("flow_set", "Select flow set", choices = NULL, selected = NULL),
                         br()
                )
    )
  })
  
  observeEvent(rval$modules, {
    
    rval$tab_elements <- list()
    rval$menu_elements <- list()
    
    for( mod_name in union(c("Modules", "import"), rval$modules) ){
      
      print("build_app")
      print(mod_name)
      
      mod_name_ui <- paste(mod_name, "UI", sep="")
        
      rval$list_module_server_function[[mod_name]] <- function(...){
        do.call(mod_name, list(...) )
      }
      
      rval <- callModule(rval$list_module_server_function[[mod_name]], 
                         id = paste(mod_name, "module", sep="_"),
                         rval = rval)
      
      rval$tab_elements[[mod_name]] <- tabItem(tabName = paste(mod_name, "tab", sep="_"),
                                               do.call(mod_name_ui, list(id = paste(mod_name, "module", sep="_") )))
      
      rval$menu_elements[[mod_name]] <- menuItem(mod_name,
                                                 tabName = paste(mod_name, "tab", sep="_"), 
                                                 startExpanded = FALSE,
                                                 icon = icon("check-circle"))
    }
    rval$tab_elements <- unname(rval$tab_elements)
    
  })
  
 
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
    validate(need(input$flow_set %in% names(rval$flow_set_list), "No flow set available"))
    print("INPUT")
    rval$flow_set_selected <- input$flow_set
  })
  
  observeEvent(rval$flow_set_selected, {
    
    validate(need(rval$flow_set_selected %in% names(rval$flow_set_list), "No flow set available"))
    print("OK flow set")
    print(rval$flow_set_selected)
    print( names(rval$flow_set_list) )
    print( names(rval$flow_set_list[[rval$flow_set_selected]]) )
    print( names(rval$flow_set_list[[rval$flow_set_selected]]$gates) )
    
    rval$flow_set <- rval$flow_set_list[[rval$flow_set_selected]]$flow_set
    
    # if(length(rval$gates_flowCore) == 0){
    #   rval$gates_flowCore <- rval$flow_set_list[[input$flow_set]]$gates
    #   print("gates updated")
    #   print(rval$gates_flowCore)
    # }
    
    
    rval$gates_flowCore <- rval$flow_set_list[[rval$flow_set_selected]]$gates
      
    print(rval$gates_flowCore)
    
    #if(is.null(rval$df_spill)){
      rval$df_spill <- rval$flow_set_list[[rval$flow_set_selected]]$spill
    #}
    
    #if(is.null(rval$transformation)){
      rval$transformation <- rval$flow_set_list[[rval$flow_set_selected]]$transformation
    #}
      
    #if(is.null(rval$trans_parameters)){
      rval$trans_parameters <- rval$flow_set_list[[rval$flow_set_selected]]$trans_parameters
    #}
    
    #if(is.null(rval$pdata)){
      rval$pdata <- rval$flow_set_list[[rval$flow_set_selected]]$metadata
    #}
    
    #if(is.null(rval$parameters)){
     rval$parameters <- NULL #rval$flow_set_list[[input$flow_set]]$parameters
    #}
    
    rval$gating_set <- GatingSet(rval$flow_set)
    rval$gating_set <- add_gates_flowCore(rval$gating_set, rval$gates_flowCore)
    
    print(colnames(rval$gating_set))
    
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
  
 
  ##################################################################################################
  # Updating spillover matrix for all related flow sets (in the same tree)
  ##################################################################################################
  
  observeEvent(c(rval$df_spill,
                 rval$gates_flowCore,
                 rval$transformation, 
                 rval$trans_parameters, 
                 rval$pdata) , {
    
    validate(need(rval$flow_set_list, "No flow-sets available"))
    validate(need(rval$flow_set_selected, "No flow-set selected"))
    
    # print("flow-sets")
    # print(rval$flow_set_selected)
    # print(get_all_descendants(rval$flow_set_list, rval$flow_set_selected))
    # print(get_all_ancestors(rval$flow_set_list, rval$flow_set_selected))
    
    items_to_update <- union(rval$flow_set_selected,
                             union(get_all_descendants(rval$flow_set_list, rval$flow_set_selected),
                                   get_all_ancestors(rval$flow_set_list, rval$flow_set_selected)))
    items_to_update <- intersect(items_to_update, names(rval$flow_set_list))
    
    print("updating")
    print(items_to_update)
    
    for(i in 1:length(items_to_update)){
      if(!is.null(rval$gates_flowCore)){
        if(length(rval$gates_flowCore)>0){
          print(rval$gates_flowCore)
          rval$flow_set_list[[items_to_update[i]]]$gates <- rval$gates_flowCore
        }
      }
      if(!is.null(rval$df_spill)){
        rval$flow_set_list[[items_to_update[i]]]$spill <- rval$df_spill
      }
      if(!is.null( rval$transformation)){
        rval$flow_set_list[[items_to_update[i]]]$transformation <- rval$transformation
      }
      if(!is.null(rval$trans_parameters)){
        rval$flow_set_list[[items_to_update[i]]]$trans_parameters <- rval$trans_parameters
      }
      if(!is.null(rval$pdata)){
        rval$flow_set_list[[items_to_update[i]]]$metadata <- rval$pdata
      }
      
    }
    
  })
  
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
    print(rval$update_gs)
    if(!is.null(rval$gating_set)){
      ngates <- length(setdiff(flowWorkspace::gs_get_pop_paths(rval$gating_set), "root"))
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
      nparams <- length(colnames(rval$flow_set))
    }
    
    valueBox(
      nparams, "parameters",icon = icon("list"),
      color = "red"
    )
  })
  
  output$flow_set_name <- renderText({
    if(!is.null(rval$flow_set_selected)){
      if(nchar(rval$flow_set_selected)>0){
        paste("Flow-set : ", rval$flow_set_selected)
      }
    }else{
      NULL
    }
  })

}