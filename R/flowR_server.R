#' flowR app main server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param modules Names of a shiny modules (server function) to integrate to the app.
#' The name of the ui function should be made of the name of the module's server function 
#' with the suffix 'UI'. In addition, the module should take the reactiveValues object 'rval' 
#' as input and return it as an output.
#' @import shiny
#' @importFrom flowCore fsApply each_col
#' @importFrom flowWorkspace gs_get_pop_paths
#' @importFrom shinydashboard renderValueBox
#' @export
flowR_server <- function(session, input, output, modules = NULL) {
  
  rval <- reactiveValues(update_gs = 0, # useful to force execution of 
                         #observe environment (for instance after updating a GatingSet with gs_pop_add()
                         gating_set = NULL,
                         gating_set_list = list(),
                         list_module_server_function = list(),
                         tab_elements = list(),
                         menu_elements = list(),
                         modules = NULL
                         )
  
  ##########################################################################################################
  # Build ui based on selected modules
  
  observe({
    default_modules <- c("Import", "Gating", "Plotting", "Subsample")
    if(is.null(modules)){
      rval$modules <- default_modules
    }else{
      rval$modules <- modules
    }
  })
  
  output$body <- renderUI({
    tagList(
      textOutput("gating_set_name"),
      br(),
      fluidRow(
        valueBoxOutput("progressBox", width = 3),
        valueBoxOutput("progressBox2", width = 3),
        valueBoxOutput("progressBox3", width = 3),
        valueBoxOutput("progressBox4", width = 3),
      ),
      do.call(tabItems, unname(rval$tab_elements))
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
                         selectInput("gating_set", "Select GatingSet", choices = NULL, selected = NULL),
                         br()
                )
    )
  })
  
  observeEvent(rval$modules, {
    
    modules <- union(rval$modules, "Modules")
    rval$menu_elements <- list()
    rval$tab_elements <- list()
    
    for( mod_name in modules ){

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
  
  observeEvent( c(names(rval$gating_set_list), rval$gating_set_selected), {
    updateSelectInput(session, "gating_set", choices = names(rval$gating_set_list), selected = rval$gating_set_selected)
  })
  
  observeEvent(input$gating_set, {
    rval$gating_set_selected <- input$gating_set
  })
  
  observeEvent(rval$gating_set_selected, {
    if(is.null(rval$gating_set_selected)){
      rval$gating_set <- NULL
    }
    else{
      rval$gating_set <- rval$gating_set_list[[rval$gating_set_selected]]$gating_set
      rval$trans_parameters <- rval$gating_set_list[[rval$gating_set_selected]]$trans_parameters
    }
    
    
    
    # fs <- rval$gating_set@data
    # rval$Ncells_tot <- sum( sapply(1:length(fs), function(x){dim(fs[[x]]@exprs)[1]}) )
    # 
    # params <- parameters(fs[[1]])$name
    # 
    # min_val <- as.data.frame(fsApply(fs, each_col, min, na.rm = TRUE))
    # min_val_all <- apply(min_val, 2, min)
    # max_val <- as.data.frame(fsApply(fs, each_col, max,  na.rm = TRUE))
    # max_val_all <- apply(max_val, 2, max)
    # 
    # rval$data_range <- lapply(params, function(x){
    #   c(min_val_all[[x]] , max_val_all[[x]])
    # })
    # names(rval$data_range) <- params
    
  })
  
 
  ##################################################################################################
  # Updating spillover matrix for all related flow sets (in the same tree)
  ##################################################################################################
  
  # observeEvent(c(rval$df_spill,
  #                rval$gates_flowCore,
  #                rval$transformation, 
  #                rval$trans_parameters, 
  #                rval$pdata) , {
  #   
  #   validate(need(rval$flow_set_list, "No flow-sets available"))
  #   validate(need(rval$flow_set_selected, "No flow-set selected"))
  #   
  #   # print("flow-sets")
  #   # print(rval$flow_set_selected)
  #   # print(get_all_descendants(rval$flow_set_list, rval$flow_set_selected))
  #   # print(get_all_ancestors(rval$flow_set_list, rval$flow_set_selected))
  #   
  #   items_to_update <- union(rval$flow_set_selected,
  #                            union(get_all_descendants(rval$flow_set_list, rval$flow_set_selected),
  #                                  get_all_ancestors(rval$flow_set_list, rval$flow_set_selected)))
  #   items_to_update <- intersect(items_to_update, names(rval$flow_set_list))
  #   
  #   print("updating")
  #   print(items_to_update)
  #   
  #   for(i in 1:length(items_to_update)){
  #     if(!is.null(rval$gates_flowCore)){
  #       if(length(rval$gates_flowCore)>0){
  #         print(rval$gates_flowCore)
  #         rval$flow_set_list[[items_to_update[i]]]$gates <- rval$gates_flowCore
  #       }
  #     }
  #     if(!is.null(rval$df_spill)){
  #       rval$flow_set_list[[items_to_update[i]]]$spill <- rval$df_spill
  #     }
  #     if(!is.null( rval$transformation)){
  #       rval$flow_set_list[[items_to_update[i]]]$transformation <- rval$transformation
  #     }
  #     if(!is.null(rval$trans_parameters)){
  #       rval$flow_set_list[[items_to_update[i]]]$trans_parameters <- rval$trans_parameters
  #     }
  #     if(!is.null(rval$pdata)){
  #       rval$flow_set_list[[items_to_update[i]]]$metadata <- rval$pdata
  #     }
  #     
  #   }
  #   
  # })
  
  ##########################################################################################################
  # Output value boxes
  
  output$progressBox <- renderValueBox({
    Nsamples <- 0
    print(rval$update_gs)
    if(!is.null(rval$gating_set)){
      Nsamples <- length(pData(rval$gating_set)$name)
    }
    valueBox(
      Nsamples, "samples",icon = icon("list"),
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
    print(rval$update_gs)
    if(!is.null(rval$gating_set)){
      fs <- rval$gating_set@data
      ncells <- sum( sapply(1:length(fs), function(x){dim(fs[[x]]@exprs)[1]}) )
    }
    valueBox(
      format(ncells, digits = 2), "cells", icon = icon("list"),
      color = "green"
    )
  })
  
  output$progressBox4 <- renderValueBox({
    nparams <- 0
    if(!is.null(rval$gating_set)){
      nparams <- length(colnames(rval$gating_set))
    }
    
    valueBox(
      nparams, "parameters",icon = icon("list"),
      color = "red"
    )
  })
  
  output$gating_set_name <- renderText({
    if(!is.null(rval$gating_set_selected)){
      if(nchar(rval$gating_set_selected)>0){
        paste("GatingSet : ", rval$gating_set_selected)
      }
    }else{
      NULL
    }
  })

}