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
                         #observe environments (for instance after updating a GatingSet with gs_pop_add() )
                         gating_set = NULL,
                         gating_set_list = list(),
                         tab_elements = list(),
                         menu_elements = list(),
                         modules = NULL
                         )
  
  ##########################################################################################################
  # Build ui based on selected modules
  
  observe({
    default_modules <- c("Import", "Transform", "Gating", "Plotting", "Subsample")
    if(is.null(modules)){
      rval$modules <- default_modules
    }else{
      rval$modules <- modules
    }
  })
  
  observe({
    rval$modules <- union(rval$modules, "Modules")
  })
  
  output$body <- renderUI({
    if(all(rval$modules %in% names(rval$tab_elements))){
      tagList(
        textOutput("gating_set_name"),
        br(),
        fluidRow(
          valueBoxOutput("progressBox", width = 3),
          valueBoxOutput("progressBox2", width = 3),
          valueBoxOutput("progressBox3", width = 3),
          valueBoxOutput("progressBox4", width = 3),
        ),
        do.call(tabItems, unname(rval$tab_elements[rval$modules]))
      )
    }else{
      tagList(list())
    }
    
  })
  
  output$menu <- renderMenu({
    if(all(rval$modules %in% names(rval$menu_elements))){
      sidebarMenu(id = "menu",
                  tagList(rval$menu_elements[rval$modules]),
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
    }else{
      
      NULL
    }
  })
  
  observeEvent(rval$modules, {
    
    modules_to_update <- union("Modules", setdiff(rval$modules, names(rval$menu_elements)))
    
    for( mod_name in modules_to_update ){

        mod_name_ui <- paste(mod_name, "UI", sep="")
        
        module_server_function <-function(...){do.call(mod_name, list(...) )}
        module_id <- paste(mod_name, "module", sep="_")
        module_tab_name <- paste(mod_name, "tab", sep="_")
        
        rval <- callModule(module_server_function, 
                           id = module_id,
                           rval = rval)
        
        rval$tab_elements[[mod_name]] <- tabItem(tabName = module_tab_name,
                                                 do.call(mod_name_ui, 
                                                         list(id = module_id)))
        
        rval$menu_elements[[mod_name]] <- menuItem(mod_name,
                                                   tabName = module_tab_name, 
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
    updateSelectInput(session, "gating_set", 
                      choices = names(rval$gating_set_list), 
                      selected = rval$gating_set_selected)
    rval$update_gs <- rval$update_gs + 1
  })
  
  observeEvent(input$gating_set, {
    rval$gating_set_selected <- input$gating_set
    rval$update_gs <- rval$update_gs + 1
  })
  
  observeEvent(rval$gating_set_selected, {
    if(is.null(rval$gating_set_selected)){
      rval$gating_set <- NULL
    }
    else{
      gs_name <- rval$gating_set_selected
      rval$gating_set <- rval$gating_set_list[[gs_name]]$gating_set
      rval$trans_parameters <- rval$gating_set_list[[gs_name]]$trans_parameters
    }
    rval$update_gs <- rval$update_gs + 1
  
  })
  
 
  ##################################################################################################
  # Updating GatingSets
  ##################################################################################################
  
  # Update transformation
  observeEvent(rval$gating_set, {
    validate(need(class(rval$gating_set) == "GatingSet", "No GatingSet available"))
    validate(need(rval$gating_set_list, "No list of GatingSets available"))
    validate(need(rval$gating_set_selected, "No GatingSet selected"))
    
    items_to_update <- union(rval$gating_set_selected,
                             union(get_all_descendants(rval$gating_set_list, rval$gating_set_selected),
                                   get_all_ancestors(rval$gating_set_list, rval$gating_set_selected)))
    items_to_update <- intersect(items_to_update, names(rval$gating_set_list))
    print("update GatingSet list")
    print(items_to_update)
    for(i in 1:length(items_to_update)){
      rval$gating_set_list[[items_to_update[i]]]$gating_set@transformation <- rval$gating_set@transformation
      rval$gating_set_list[[items_to_update[i]]]$trans_parameters <- rval$trans_parameters
    }
  })
  
  # observeEvent(rval$update_gs , {
  # 
  #   validate(need(rval$gating_set_list, "No flow-sets available"))
  #   validate(need(rval$gating_set_selected, "No flow-set selected"))
  # 
  #   # print("flow-sets")
  #   # print(rval$flow_set_selected)
  #   # print(get_all_descendants(rval$flow_set_list, rval$flow_set_selected))
  #   # print(get_all_ancestors(rval$flow_set_list, rval$flow_set_selected))
  # 
  #   items_to_update <- union(rval$gating_set_selected,
  #                            union(get_all_descendants(rval$gating_set_list, rval$gating_set_selected),
  #                                  get_all_ancestors(rval$gating_set_list, rval$gating_set_selected)))
  #   items_to_update <- intersect(items_to_update, names(rval$gating_set_list))
  # 
  #   print("updating")
  #   print(items_to_update)
  # 
  #   for(i in 1:length(items_to_update)){
  #     if(!is.null(rval$gates_flowCore)){
  #       if(length(rval$gates_flowCore)>0){
  #         rval$gating_set_list[[items_to_update[i]]]$gates <- rval$gates_flowCore
  #       }
  #     }
  #     
  #     if(!is.null(rval$gating_set@compensation)){
  #       rval$gating_set_list[[items_to_update[i]]]@transformation <- rval$gating_set@compensation
  #     }
  #     if(!is.null(rval$gating_set@transformation)){
  #       rval$gating_set_list[[items_to_update[i]]]@transformation <- rval$gating_set@transformation
  #     }
  #     if(!is.null(rval$trans_parameters)){
  #       rval$gating_set_list[[items_to_update[i]]]$trans_parameters <- rval$trans_parameters
  #     }
  #     if(!is.null(rval$pdata)){
  #       rval$gating_set_list[[items_to_update[i]]]$metadata <- rval$pdata
  #     }
  # 
  #   }
  # 
  # })
  
  ##########################################################################################################
  # Output value boxes
  
  output$progressBox <- renderValueBox({
    Nsamples <- 0
    rval$update_gs
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
    rval$update_gs
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
    rval$update_gs
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
    rval$update_gs
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