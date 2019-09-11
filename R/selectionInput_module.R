#' @title   selectionInput and selection
#' @description  A shiny Module that deals with metadata
#' @param id shiny id
#' @importFrom shinydashboard box tabBox
#' @import shiny
#' @import DT
selectionInput <- function(id, multiple_subset = TRUE) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    selectizeInput(ns("samples"), 
                   label = "samples",
                   choices = NULL,
                   selected = NULL,
                   multiple = TRUE),
    selectizeInput(ns("gate"), 
                   label = "subset",
                   choices = "root",
                   selected = "root",
                   multiple = multiple_subset),
    box(title = "Select using pattern", width = NULL, height = NULL, collapsible = TRUE, collapsed = TRUE,
      textInput(ns("pattern"), "Pattern"),
      checkboxInput(ns("match_as_is"), "use pattern as regular expression", TRUE),
      actionButton(ns("select_samples"), "Select samples"),
      if(multiple_subset){
        tagList(
          actionButton(ns("select_subsets"), "Select subsets"),
          checkboxInput(ns("use_whole_path"), "Search in entire subset path")
        )
      }
    )
                  
  )
  
}


#' selection server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @return a reactivevalues object with values "df_files", "flow_set_imported" and "gates_flowCore"
#' @import flowWorkspace
#' @import flowCore
#' @import shiny
#' @import DT
#' @export
#' @rdname selectionInput
selection <- function(input, output, session, rval, params = reactiveValues()) {
  
  `%then%` <- shiny:::`%OR%`
  
  observe({
    validate(need(rval$gates_flowCore, "no gating set"))
    updateSelectInput(session, "gate", choices = union("root", names(rval$gates_flowCore)), selected = "root")
  })
  
  observe({
    if("gate" %in% names(params)){
      updateSelectInput(session, "gate", choices = union("root", names(rval$gates_flowCore)), selected = params$gate)
    }
  })
  
  observe({
    updateSelectInput(session, "samples", choices = rval$pdata$name, selected = rval$pdata$name[1])
  })
  
  observeEvent(input$select_samples, {
    samples_selected <- NULL
    idx_selected <- try(grep(input$pattern, rval$pdata$name, fixed = input$match_as_is), silent = TRUE)

    if(class(idx_selected) == "try-error"){
      showModal(modalDialog(
        title = "Error",
        print(idx_selected),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    if(length(idx_selected)>0){
      samples_selected <- rval$pdata$name[idx_selected]
    }
    updateSelectInput(session, "samples", choices = rval$pdata$name, selected = samples_selected)
  })
  
  observeEvent(input$select_subsets, {
    subsets_selected <- NULL
    choices <- c("root", basename(names(rval$gates_flowCore)))
    if(input$use_whole_path){
      choices <- c("root", names(rval$gates_flowCore))
    }
    idx_selected <- try(grep(input$pattern, choices, fixed = input$match_as_is), silent = TRUE)
    
    if(class(idx_selected) == "try-error"){
      showModal(modalDialog(
        title = "Error",
        print(idx_selected),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    if(length(idx_selected)>0){
      subsets_selected <- c("root", names(rval$gates_flowCore))[idx_selected]
    }
    updateSelectInput(session, "gate", 
                      choices = union("root", names(rval$gates_flowCore)), 
                      selected = subsets_selected)
  })
  
  observe({
    if("samples" %in% names(params)){
      updateSelectInput(session, "samples", choices = rval$pdata$name, selected = params$samples)
    }
  })
  
  
  
  return(input)
  
}