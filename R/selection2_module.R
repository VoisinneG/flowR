#' @title selection2Input and selection2
#' @description A shiny Module for selecting samples and subsets from a GatingSet
#' @param id shiny id
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
#'       params$sample <- pData(gs)$name[2]
#'     })
#'     
#'     callModule(selection2, "selection_module", rval, params = params)
#'   }
#'   
#'   shinyApp(ui, server)
#'   
#' }
#' }
selection2Input <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    selectizeInput(ns("sample"),
                   label = "sample",
                   choices = NULL,
                   selected = NULL,
                   multiple = TRUE),
    uiOutput(ns("subset_input")),
    box(title = "Select using a pattern", width = NULL, height = NULL, collapsible = TRUE, collapsed = TRUE,
        patternSelectionInput(ns("pattern_module"))
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
#'   \item{sample}{: initially selected samples}
#'   \item{subset}{: initially selected subsets}
#'  }
#'@param multiple_subset logical; allow selection of multiple subsets
#' @return a reactivevalues object with input values amongst which:
#' \describe{
#'   \item{sample}{: selected samples}
#'   \item{subset}{: selected subsets}
#' }
#' @import shiny
#' @rdname selection2Input
selection2 <- function(input, output, session, rval, params = reactiveValues(), multiple_subset = TRUE) {
  
  
  output$subset_input <- renderUI({
    ns <- session$ns
    selected <- choices$subset[1]
    if("subset" %in% names(params)){
      selected <- params$subset
    }
    tagList(
      selectizeInput(ns("subset"),
                   label = "subset",
                   choices = choices$subset,
                   selected = selected,
                   multiple = multiple_subset)
    )
  })
  
  # Get available samples and subsets from rval$gating_set
  
  choices <- reactiveValues()
  choices_pattern <- reactiveValues()
  
  observe({
    validate(need(class(rval$gating_set) == "GatingSet", "input is not a GatingSet"))
    choices$sample <- pData(rval$gating_set)$name
    choices$subset <- gs_get_pop_paths(rval$gating_set)
    if(multiple_subset){
      choices_pattern$sample <- choices$sample
      choices_pattern$subset <- choices$subset
    }else{
      choices_pattern$sample <- choices$sample
    }
  })
  
  # Default values
  # observe({
  #   updateSelectInput(session, "subset", choices = choices$subset, selected = choices$subset[1])
  # })
  
  observe({
    updateSelectInput(session, "sample", choices = choices$sample, selected = choices$sample[1])
  })
  
  # Initialization using params
  # observeEvent(params$subset, {
  #   if("subset" %in% names(params)){
  #     if(!is.null(params$subset)){
  #       print(params$subset)
  #       updateSelectInput(session, "subset", choices = choices$subset, selected = params$subset)
  #     }
  #   }
  # })
  
  observeEvent(params$sample, {
    if("sample" %in% names(params)){
      if(!is.null(params$sample)){
        updateSelectInput(session, "sample", choices = choices$sample, selected = params$sample)
      }
    }
  })
  
  res <- callModule(patternSelection, "pattern_module", choices = choices_pattern)
  
  observe({
    
    if(!is.null(res$variable)){
      print(res$variable)
      updateSelectizeInput(session, res$variable, choices = choices[[res$variable]], selected = res$values)
    }
  })
  
  return(input)
  
}


##################################################################################
# Tests
##################################################################################
# 
library(shiny)
library(shinydashboard)
library(flowWorkspace)

if (interactive()){

  ui <- dashboardPage(
    header = dashboardHeader(title = "selection2"),
    sidebar = dashboardSidebar(disable = TRUE),
    body = dashboardBody(
      fluidRow(
        column(4, box(width = NULL, selection2Input("selection_module")))
      )
    )
  )

  server <- function(input, output, session) {

    rval <- reactiveValues()
    params <- reactiveValues()

    observe({
      gs <- load_gs("./inst/ext/gs")
      rval$gating_set <- gs
      params$sample <- pData(gs)$name[2]
      params$subset <- gs_get_pop_paths(gs)[3]
    })

    callModule(selection2, "selection_module", rval, params = params, multiple_subset = TRUE)
  }

  shinyApp(ui, server)

}