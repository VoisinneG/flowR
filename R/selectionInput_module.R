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

    checkboxInput(ns("all_samples"), "Select all samples", FALSE),
    selectizeInput(ns("samples"), 
                   label = "samples",
                   choices = NULL,
                   selected = NULL,
                   multiple = TRUE),
    selectizeInput(ns("gate"), 
                   label = "subset",
                   choices = "root",
                   selected = "root",
                   multiple = multiple_subset)
    
                  
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
  
  observe({
    if(input$all_samples){
      updateSelectInput(session, "samples", choices = rval$pdata$name, selected = rval$pdata$name)
    }
  })
  
  return(input )
  
}