#' @title patternSelectionInput and patternSelection
#' @description A shiny Module for selecting variables based on pattern
#' @param id shiny id
#' @importFrom shinydashboard box
#' @import shiny
#' @export
#' @examples 
#' \dontrun{
#' library(shiny)
#' 
#'  if (interactive()){
#'   
#'   ui <- fluidPage(
#'     selectizeInput("selection",
#'                    label = "selected values",
#'                    choices = NULL, selected = NULL, multiple = TRUE),
#'     patternSelectionInput("pattern_module")
#'   )
#'   
#'   server <- function(input, output, session) {
#'     
#'     choices <- reactiveValues()
#'     
#'     observe({
#'       gs <- load_gs("./inst/ext/gs")
#'       choices$sample <- pData(gs)$name
#'       choices$subset <- gs_get_pop_paths(gs)
#'     })
#'     
#'     res <- callModule(patternSelection, "pattern_module", choices = choices)
#'     
#'     observe({
#'       if(!is.null(res$variable)){
#'         updateSelectizeInput(session, "selection", choices = choices[[res$variable]], selected = res$values)
#'       }
#'     })
#'     
#'   }
#'   
#'   shinyApp(ui, server)
#'   
#' }}
patternSelectionInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    selectizeInput(ns("var_name"),
                   multiple = FALSE,
                   label = "Variable",
                   choices = NULL,
                   selected = NULL),
    textInput(ns("pattern"), "Pattern"),
    checkboxInput(ns("use_reg_expr"), "Use as regular expression", TRUE),
    uiOutput(ns("options")),
    actionButton(ns("select_values"), "Select values")
  )
  
}


#' patternSelection server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param choices reactivevalues object containing vectors of possible choices for each selection variable 
#' @return a reactivevalues object with:
#' \describe{
#'   \item{values}{: selected values}
#'   \item{variable}{: selected variable}
#' }
#' @import shiny
#' @export
#' @rdname patternSelectionInput
patternSelection <- function(input, output, session, choices = reactiveValues()) {
  
  rval_mod <- reactiveValues()
  
  output$options <- renderUI({
    ns <- session$ns
    if(input$var_name == 'subset'){
      tagList(checkboxInput(ns("use_whole_path"), "Search in entire subset path", FALSE))
    }
  })
  
  observe({
    if(length(names(choices))>0){
      updateSelectInput(session, "var_name", choices = names(choices), selected = names(choices)[1])
    }
  })
  
  ######################################################################################
  # Select values of variable using a pattern
  
  observeEvent(input$select_values, {
    selected_values <- NULL

    values <- choices[[input$var_name]]
    if(input$var_name == 'subset'){
      if(!input$use_whole_path){
        values <- basename(values)
      }
    }
    
    
    idx_selected <- try(grep(input$pattern, values, fixed = !input$use_reg_expr), silent = TRUE)
    
    if(class(idx_selected) == "try-error"){
      showModal(modalDialog(
        title = "Error",
        print(idx_selected),
        easyClose = TRUE,
        footer = NULL
      ))
    }else{
      if(length(idx_selected)>0){
        selected_values <- choices[[input$var_name]][idx_selected]
      }
      rval_mod$values <- selected_values
      rval_mod$variable <- input$var_name
    }
    
    
    
    

  })
  
  return(rval_mod)
  
}


##################################################################################
# Tests
##################################################################################
# 
# library(shiny)
# library(shinydashboard)
# 
# if (interactive()){
# 
#   ui <- dashboardPage(
#     header = dashboardHeader(title = "patternSelection"),
#     sidebar = dashboardSidebar(disable = TRUE),
#     body = dashboardBody(
#       fluidRow(
#         column(4, 
#                box(width = NULL,
#                    selectizeInput("selection",
#                                   label = "selected values",
#                                   choices = NULL, selected = NULL, multiple = TRUE)),
#                box(width = NULL, 
#                    patternSelectionInput("pattern_module"))
#                )
#       )
#     )
#   )
# 
#   server <- function(input, output, session) {
# 
#     choices <- reactiveValues()
# 
#     observe({
#       gs <- load_gs("./inst/ext/gs")
#       choices$sample <- pData(gs)$name
#       choices$subset <- gs_get_pop_paths(gs)
#     })
# 
#     res <- callModule(patternSelection, "pattern_module", choices = choices)
# 
#     observe({
#       if(!is.null(res$variable)){
#         updateSelectizeInput(session, "selection", choices = choices[[res$variable]], selected = res$values)
#       }
#     })
# 
#   }
# 
#   shinyApp(ui, server)
# 
# }