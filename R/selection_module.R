#' Select samples and subsets from a GatingSet
#' @param id shiny id
#' @importFrom shinydashboard box
#' @import shiny
#' @export
#' @examples 
#' \dontrun{
#' library(shiny)
#' if (interactive()){
#'   
#'   ui <- fluidPage(
#'     selectionInput("selection_module", multiple_subset = FALSE)
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
#'     callModule(selection, "selection_module", rval, params = params)
#'   }
#'   
#'   shinyApp(ui, server)
#'   
#' }
#' }
selectionInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    selectizeInput(ns("sample"),
                   label = "sample",
                   choices = NULL,
                   selected = NULL,
                   multiple = TRUE),
    uiOutput(ns("subset_input")),
    box(title = "Select using a pattern", width = NULL, height = NULL, 
        collapsible = TRUE, collapsed = TRUE,
        patternSelectionInput(ns("pattern_module"))
    )
                  
  )
  
}


#' selection module server function
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
#' @export
#' @rdname selectionInput
selection <- function(input, output, session, 
                       rval, params = reactiveValues(), multiple_subset = TRUE) {
  
  choices <- reactiveValues()
  choices_pattern <- reactiveValues()
  
  
  output$subset_input <- renderUI({
    ns <- session$ns
    selected <- choices$subset[1]
    
    if("subset" %in% names(params)){
      if(!is.null(params$subset)){
        if(params$subset %in% choices$subset){
          selected <- params$subset
        }
      }
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
  observe({
    
    rval$update_gs
    
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
  observe({
    updateSelectInput(session, "sample", choices = choices$sample, selected = choices$sample[1])
  })
  
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
# library(shiny)
# library(shinydashboard)
# library(flowWorkspace)
# 
# if (interactive()){
# 
#   ui <- dashboardPage(
#     header = dashboardHeader(title = "selection2"),
#     sidebar = dashboardSidebar(disable = TRUE),
#     body = dashboardBody(
#       fluidRow(
#         actionButton("switch", "switch GatingSet"),
#         actionButton("add_gate", "add gate"),
#         column(4, box(width = NULL, selectionInput("selection_module")))
#       )
#     )
#   )
# 
#   server <- function(input, output, session) {
# 
#     rval <- reactiveValues()
#     params <- reactiveValues()
# 
#     observeEvent(input$switch, {
#       data("GvHD")
#       rval$gating_set <- GatingSet(GvHD)
#     })
#     
#     observeEvent(input$add_gate, {
#       filter1 <- rectangleGate(gate = data.frame('SSC-A' = c(1,2), check.names = FALSE), 
#                                filterId =  as.character(rval$update_gs))
#       flowWorkspace::gs_pop_add(rval$gating_set, filter1, parent= "root")
#       #rval$update_gs <- rval$update_gs + 1
#     })
#     
#     observe({
#       gs <- load_gs("./inst/ext/gs")
#       rval$gating_set <- gs
#       #rval$update_gs <- 0
#       params$sample <- pData(gs)$name[2]
#       params$subset <- gs_get_pop_paths(gs)[3]
#     })
# 
#     callModule(selection, "selection_module", rval, params = params, multiple_subset = TRUE)
#   }
# 
#   shinyApp(ui, server)
# 
# }