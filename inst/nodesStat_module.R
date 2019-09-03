
nodesStatUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(
    column(width = 6,
           box(
           verbatimTextOutput(ns("nodes_stat"))
                  
           )
    )
  )
  
}


#' cluster server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @return a reactivevalues object with values "df_files", "flow_set_imported" and "gates_flowCore"
#' @import flowWorkspace
#' @import flowCore
#' @import shiny
#' @import DT
#' @export
#' @rdname nodesStatUI
nodesStat <- function(input, output, session, rval) {
  
  `%then%` <- shiny:::`%OR%`
  
  output$nodes_stat <- renderText({
    validate(need(rval$gating_set, "no gating set"))
    print( getNodes(rval$gating_set) )
  })
  
  return( rval )
  
}