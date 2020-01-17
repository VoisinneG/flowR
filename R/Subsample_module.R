#' Subsample a GatingSet
#' @param id shiny id
#' @importFrom shinydashboard tabBox valueBoxOutput
#' @import shiny
#' @export
#' @examples 
#' \dontrun{
#' library(shiny)
#' library(shinydashboard)
#' library(flowWorkspace)
#' library(flowCore)
#' 
#' if (interactive()){
#'   
#'   ui <- dashboardPage(
#'     dashboardHeader(title = "Subsample"),
#'     sidebar = dashboardSidebar(disable = TRUE),
#'     body = dashboardBody(
#'       fluidRow(
#'         column(12, box(width = NULL, SubsampleUI("module")))
#'       )
#'     )
#'   )
#'   
#'   server <- function(input, output, session) {
#'     
#'     rval <- reactiveValues()
#'     
#'     observe({
#'       utils::data("GvHD", package = "flowCore")
#'       rval$gating_set <- GatingSet(GvHD)
#'     })
#'     
#'     rval <- callModule(Subsample, "module", rval = rval)
#'     
#'   }
#'   
#'   shinyApp(ui, server)
#'   
#' }}
SubsampleUI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(width = 6,
      tabBox(title = "",
             width = NULL, height = NULL,
             tabPanel("Sample/Subset",
                      selectionInput(ns("selection_module"))
             ),
             tabPanel("Compute",
                      numericInput(ns("ncells_per_sample"), 
                                   "Number of cells / subset / sample", 1000),
                      textInput(ns("gs_name"), "GatingSet name", "sub-sample"),
                      actionButton(ns("compute_data"), "sample"),
                      br(),
                      br(),
                      "Summary",
                      br(),
                      verbatimTextOutput(ns("summary_sub_sample"))
             )
             
      ),
      fluidRow(
        valueBoxOutput(ns("progressBox"), width = 6),
        valueBoxOutput(ns("progressBox2"), width = 6)
      )
    )
  )
}


#' Subsample module server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param rval reactivevalues object with the following elements :
#' \describe{
#'   \item{gating_set}{: a GatingSet object}
#' }
#' @return The input reactivevalues object 'rval' with updated elements :
#' \describe{
#'   \item{gating_set_list}{list of GatingSet objects loaded}
#'   \item{gating_set}{selected GatingSet}
#'   \item{gating_set_selected}{Name of the selected GatingSet}
#' }
#' @importFrom flowWorkspace gs_get_pop_paths
#' @import shiny
#' @importFrom shinydashboard renderValueBox
#' @export
#' @rdname SubsampleUI
Subsample <- function(input, output, session, rval) {
  
  selected <- callModule(selection, "selection_module", rval)
  
  rval_mod <- reactiveValues( gs = NULL, df_sample = NULL)
  
  ### Get parameters from GatingSet ########################################################
  choices <- reactive({
    rval$update_gs
    validate(need(class(rval$gating_set) == "GatingSet", "input is not a GatingSet"))
    get_parameters_gs(rval$gating_set)
  })

  ### Observe functions for sub-sampling ###################################################
  
  observeEvent(input$compute_data, {

    # Create a Progress object
    progress <- shiny::Progress$new(min = 0, max = 100)
    on.exit(progress$close())
    progress$set(message = "Computing...", value = 0)
    updateProgress <- function(value = NULL, detail = NULL) {
      progress$set(value = value, detail = detail)
    }
    
    
    if( length(selected$sample) ==0 ){
      showModal(modalDialog(
        title = "No sample selected",
        paste("Please select samples before proceeding", sep=""),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    validate(need(length(selected$sample)>0, "No sample selected"))
    
   
    if( input$gs_name %in% names(rval$gating_set_list) | nchar(input$gs_name)==0 ){
      showModal(modalDialog(
        title = "Invalid GatingSet name",
        paste("Name is empty or already exists. Please choose another name", sep=""),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    validate(need(! input$gs_name %in% names(rval$gating_set_list), 
                  "Name already exists" ))

    
    if( length(selected$subset) == 0 ){
      showModal(modalDialog(
        title = "No subset selected",
        paste("Please select a subset before proceeding", sep=""),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    validate(need(selected$subset, "No subset selected"))
    
    spill <- choices()$compensation
    if(!is.null(rval$apply_comp)){
      if(!rval$apply_comp){
        spill <- NULL
      }
    }
    
    df_sample <- get_data_gs(gs = rval$gating_set,
                                  sample = selected$sample, 
                                  subset = selected$subset,
                                  spill = spill,
                                  Ncells = input$ncells_per_sample,
                                  return_comp_data = FALSE,
                                  updateProgress = updateProgress)
    rval_mod$df_sample <- df_sample
    
    if( length(df_sample) == 0 ){
      showModal(modalDialog(
        title = "No cells in selection",
        paste("Please modify selection", sep=""),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    validate(need(length(df_sample)>0, "No cells in selection"))
    
    fs <- build_flowset_from_df(df_sample, 
                                origin = rval$gating_set@data)
    
    rval_mod$gs <- GatingSet(fs)
    add_gates_flowCore(gs = rval_mod$gs, gates = choices()$gates)
    rval_mod$gs@compensation <- choices()$compensation
    rval_mod$gs@transformation <- choices()$transformation
    

    rval$gating_set_list[[input$gs_name]] <- list(gating_set = rval_mod$gs,
                                                  parent = rval$gating_set_selected)
    rval$gating_set_selected <- input$gs_name
    
  })
  
  ### Value boxes ##########################################################################
  
  output$progressBox <- renderValueBox({
    Nsamples <- 0
    if(!is.null(rval_mod$gs)){
      Nsamples <- length(pData(rval_mod$gs)$name)
    }
    
    valueBox(
      Nsamples, "samples",icon = icon("list"),
      color = "purple"
    )
  })
  
  output$progressBox2 <- renderValueBox({
    ncells <- 0
    if(!is.null(rval_mod$gs)){
      fs <- rval_mod$gs@data
      ncells <- sum( sapply(1:length(fs), function(x){dim(fs[[x]]@exprs)[1]}) )
    }
    
    valueBox(
      ncells, "cells", icon = icon("list"),
      color = "green"
    )
  })
  
  ### Summary ##############################################################################
  
  output$summary_sub_sample <- renderPrint({
    if(!is.null(rval_mod$df_sample)){
      print(summary(rval_mod$df_sample[, c("name", "subset")]))
    }else{
      "No sub-sampling performed yet"
    }
  })
  
  return( rval )
  
}

### Tests #################################################################################
# 
# library(shiny)
# library(shinydashboard)
# library(flowWorkspace)
# library(flowCore)
# 
# if (interactive()){
# 
#   ui <- dashboardPage(
#     dashboardHeader(title = "Subsample"),
#     sidebar = dashboardSidebar(disable = TRUE),
#     body = dashboardBody(
#       fluidRow(
#         column(6, box(width = NULL, SubsampleUI("module")))
#       )
#     )
#   )
# 
#   server <- function(input, output, session) {
# 
#     rval <- reactiveValues()
# 
#     observe({
#       utils::data("GvHD", package = "flowCore")
#       rval$gating_set <- GatingSet(GvHD)
#     })
# 
#     rval <- callModule(Subsample, "module", rval = rval)
# 
#   }
# 
#   shinyApp(ui, server)
# 
# }
