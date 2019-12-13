#' @title selection2Input and selection2
#' @description A shiny Module for selecting samples and subsets from a GatingSet
#' @param id shiny id
#' @param multiple_subset logical; allow selection of multiple subsets
#' @importFrom shinydashboard box
#' @import shiny
#' @examples 
#' \dontrun{
#' library(shiny)
#' if (interactive()){
#'   
#'   ui <- fluidPage(
#'     selection2Input("selection_module", multiple_subset = FALSE)
#'   )
#'   
#'   server <- function(input, output, session) {
#'     
#'     rval <- reactiveValues()
#'     params <- reactiveValues()
#'     
#'     observe({
#'       gs <- load_gs("./inst/ext/gs")
#'       rval$gating_set <- gs
#'       params$samples <- pData(gs)$name[2]
#'     })
#'     
#'     callModule(selection2, "selection_module", rval, params = params)
#'   }
#'   
#'   shinyApp(ui, server)
#'   
#' }
#' }
selection2Input <- function(id, multiple_subset = TRUE) {
  
  ns <- NS(id)
  
  tagList(
    selectizeInput(ns("samples"),
                   label = "samples",
                   choices = NULL,
                   selected = NULL,
                   multiple = TRUE),
    selectizeInput(ns("gate"),
                   label = "subset",
                   choices = NULL,
                   selected = NULL,
                   multiple = multiple_subset),
    box(title = "Select using pattern", width = NULL, height = NULL, collapsible = TRUE, collapsed = TRUE,
      textInput(ns("pattern"), "Pattern"),
      checkboxInput(ns("use_reg_expr"), "Use as regular expression", TRUE),
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


#' selection2 server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param rval reactivevalues object with the following elements :
#' \describe{
#'   \item{gating_set}{: a GatingSet object}
#'   \describe{
#'      \item{gate}{: a filter object (as build using polygonGate() for instance)}
#'      \item{parent}{: the name of the parent gate}
#'      }
#' }
#' @param params reactivevalues object used to initialize selected samples 
#' and subsets with elements (not mandatory) :
#' \describe{
#'   \item{samples}{: initially selected samples}
#'   \item{gate}{: initially selected subsets}
#'  }
#' @return a reactivevalues object with input values amongst which:
#' \describe{
#'   \item{samples}{: selected samples}
#'   \item{gate}{: selected subsets}
#' }
#' @import shiny
#' @rdname selection2Input
selection2 <- function(input, output, session, rval, params = reactiveValues()) {
  
  # Get available samples and subsets from rval$gating_set
  choices <- reactive({
    validate(need(class(rval$gating_set) == "GatingSet", "input is not a GatingSet"))
    return( 
      list(samples = pData(rval$gating_set)$name,
           subsets = gs_get_pop_paths(rval$gating_set)
          )
    )
  })
  
  # Default values
  observe({
    updateSelectInput(session, "gate", choices = choices()$subsets, selected = choices()$subsets[1])
  })
  
  observe({
    updateSelectInput(session, "samples", choices = choices()$samples, selected = choices()$samples[1])
  })
  
  # Initialization using params
  observeEvent(params$gate, {
    if("gate" %in% names(params)){
      if(!is.null(params$gate)){
        updateSelectInput(session, "gate", choices = choices()$subsets, selected = params$gate)
      }
    }
  })
  
  observeEvent(params$samples, {
    if("samples" %in% names(params)){
      if(!is.null(params$samples)){
        updateSelectInput(session, "samples", choices = choices()$samples, selected = params$samples)
      }
    }
  })
  
  
  
  # Sample selection using a pattern
  observeEvent(input$select_samples, {
    samples_selected <- NULL
    idx_selected <- try(grep(input$pattern, choices()$samples, fixed = !input$use_reg_expr), silent = TRUE)

    if(class(idx_selected) == "try-error"){
      showModal(modalDialog(
        title = "Error",
        print(idx_selected),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    if(length(idx_selected)>0){
      samples_selected <- choices()$samples[idx_selected]
    }
    updateSelectInput(session, 
                      "samples", 
                      choices = choices()$samples, 
                      selected = samples_selected)
  })
  
  # Subset selection using a pattern
  observeEvent(input$select_subsets, {
    subsets_selected <- NULL
    if(input$use_whole_path){
      idx_selected <- try(grep(input$pattern, choices()$subsets, fixed = !input$use_reg_expr), silent = TRUE)
    }else{
      idx_selected <- try(grep(input$pattern, basename(choices()$subsets), fixed = !input$use_reg_expr), silent = TRUE)
    }
    
    
    if(class(idx_selected) == "try-error"){
      showModal(modalDialog(
        title = "Error",
        print(idx_selected),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    if(length(idx_selected)>0){
      subsets_selected <-  choices()$subsets[idx_selected]
    }
    
    updateSelectInput(session, 
                      "gate", 
                      choices =  choices()$subsets, 
                      selected = subsets_selected)
  })
  
  
  return(input)
  
}


##################################################################################
# Tests
##################################################################################
# 
# library(shiny)
# if (interactive()){
# 
#   ui <- fluidPage(
#     selection2Input("selection_module", multiple_subset = FALSE)
#   )
# 
#   server <- function(input, output, session) {
# 
#     rval <- reactiveValues()
#     params <- reactiveValues()
# 
#     observe({
#       gs <- load_gs("./inst/ext/gs")
#       rval$gating_set <- gs
#       params$samples <- pData(gs)$name[2]
#     })
# 
#     callModule(selection2, "selection_module", rval, params = params)
#   }
# 
#   shinyApp(ui, server)
# 
# }