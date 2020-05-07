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
#' @importFrom pryr object_size
#' @export
flowR_server <- function(session, input, output, modules = NULL) {
  
  ### Definition of the main reactivevalues object #############################################
  
  rval <- reactiveValues(update_gs = 0, # useful to force execution of 
                         #observe environments (for instance after updating 
                         #a GatingSet with gs_pop_add() )
                         gating_set = NULL,
                         gating_set_list = list(),
                         gating_set_selected = NULL,
                         tab_elements = list(),
                         menu_elements = list(),
                         modules = NULL,
                         active_menu = NULL
                         )
  
  ### Build UI based on selected modules #######################################################
  
  observe({
    default_modules <- c("Import", "Metadata", "Transform", "Compensation", "Gating", "Subsample", 
                         "Clustering", "Dim_reduction", "Plotting", "GatingSets")
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
    #print('update body')
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
  
  # select first module loaded
  observe({
    validate(need(rval$modules, "No tab elements available"))
    tab_selected <- rval$modules[1]
    tab_selected <- paste(tab_selected, "tab", sep="_")
    updateTabItems(inputId = "sidebar_tabs", selected = tab_selected, session = session)
  })
  
  output$menu <- renderMenu({
    #print('update menu')
    if(all(rval$modules %in% names(rval$menu_elements))){
      sidebarMenu(id = "menu",
                  tagList(rval$menu_elements[rval$modules]),
                  tagList(rval$menu_elements[["General controls"]])
                  
      )
    }else{
      NULL
    }
  })
  
  observeEvent(rval$modules, {
    rval$menu_elements[["General controls"]] <- 
      menuItem("General controls",
               tabName = "General_tab",
               startExpanded = FALSE,
               icon = icon("check-circle"),
               checkboxInput("apply_comp", "apply compensation", TRUE),
               checkboxInput("apply_trans", "apply transformation", TRUE),
               selectInput("gating_set", "Select GatingSet", 
                           choices = names(rval$gating_set_list),
                           selected = rval$gating_set_selected),
               br()
      )
  })
  
  observeEvent(rval$modules, {
    
    modules_to_update <- union("Modules", setdiff(rval$modules, names(rval$menu_elements)))
    
    for( mod_name in modules_to_update ){

        mod_name_ui <- paste(mod_name, "UI", sep="")
        
        module_server_function <- function(...){do.call(mod_name, list(...) )}
        module_id <- paste(mod_name, "module", sep="_")
        module_tab_name <- paste(mod_name, "tab", sep="_")
        
        rval <- callModule(module_server_function, 
                           id = module_id,
                           rval = rval)
        
        rval$tab_elements[[mod_name]] <- tabItem(tabName = module_tab_name,
                                                 do.call(mod_name_ui, 
                                                         list(id = module_id)))
        
        rval$menu_elements[[mod_name]] <- menuItem(mod_name,
                                                   selected = TRUE,
                                                   tabName = module_tab_name, 
                                                   startExpanded = FALSE,
                                                   icon = icon("check-circle"))
    }
    rval$update_gs <- rval$update_gs + 1
  })
  
  ### General controls #########################################################################
  observeEvent(input$apply_trans, {
    rval$apply_trans <- input$apply_trans
    
  })
  
  observeEvent(input$apply_comp, {
    rval$apply_comp <- input$apply_comp
  })
  
  ### Update selected GatingSet ################################################################
  
  observeEvent( c(names(rval$gating_set_list), rval$gating_set_selected), {
    updateSelectInput(session, "gating_set",
                      choices = names(rval$gating_set_list),
                      selected = rval$gating_set_selected)
  })
  
  observeEvent(input$gating_set, {
        if(input$gating_set %in% names(rval$gating_set_list)){
          rval$gating_set_selected <- input$gating_set
        }
  })
  
  observeEvent(rval$gating_set_selected, {
    
    if(rval$gating_set_selected %in% names(rval$gating_set_list)){
      gs_name <- rval$gating_set_selected
      rval$gating_set <- rval$gating_set_list[[gs_name]]$gating_set
      rval$trans_parameters <- rval$gating_set_list[[gs_name]]$trans_parameters
      rval$update_gs <- rval$update_gs + 1
    }

  })

  ### Get parameters from GatingSet ################################################################
  
  choices <- reactive({
    rval$update_gs
    validate(need(class(rval$gating_set) == "GatingSet", "input is not a GatingSet"))
    get_parameters_gs(rval$gating_set)
  })
  
  ### Get the active menuItems #################################################################
  
  observe({
    rval$active_menu <- input$menu
  })
  
  ### Main Value boxes #########################################################################
  
  output$progressBox <- renderValueBox({
    
    Nsamples <- 0
    if(class(rval$gating_set) == "GatingSet"){
      Nsamples <- length(choices()$sample)
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