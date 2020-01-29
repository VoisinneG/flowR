#' Import spillover matrix
#' @param id shiny id
#' @import shiny
#' @importFrom shinydashboard box tabBox
#' @examples 
#' \dontrun{
#' library(shiny)
#' library(shinydashboard)
#' 
#' if (interactive()){
#'  
#'   ui <- dashboardPage(
#'     dashboardHeader(title = "ImportSpill"),
#'     sidebar = dashboardSidebar(disable = TRUE),
#'     body = dashboardBody(
#'       box(title="Import", width = NULL, height = NULL,
#'           ImportSpillUI("module")
#'           )
#'     )
#'   )
#' 
#'   server <- function(input, output, session) {
#'     rval <- reactiveValues()
#'     observe({
#'       utils::data("GvHD", package = "flowCore")
#'       rval$gating_set <- GatingSet(GvHD)
#'     })
#' 
#'     res <- callModule(ImportSpill, "module", rval = rval)
#'   }
#' 
#'   shinyApp(ui, server)
#' 
#' }
#' }
ImportSpillUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    fileInput(inputId = ns("file"),
              label = "Choose file",
              multiple = FALSE),
    uiOutput(ns("ui_import_spill")),
    uiOutput(ns("ui_preview_spill")),
    actionButton(ns("import_matrix"), "import matrix"),
    br(),
    br()
  )
  
}

#' ImportSpill server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param rval A reactive values object
#' @return The Importd matrix
#' @import shiny
#' @importFrom DT renderDT
#' @importFrom utils read.table
#' @importFrom  stats median
#' @rdname ImportSpillUI
ImportSpill <- function(input, output, session, rval) {

  rval_mod <- reactiveValues()
  
  ### Get parameters from GatingSet ################################################################
  choices <- reactive({
    rval$update_gs
    validate(need(class(rval$gating_set) == "GatingSet", "No GatingSet available"))
    get_parameters_gs(rval$gating_set)
  })
  
  ### Import compensation matrix ##################################################################
  
  observeEvent(c(input$file, input$sep_spill, input$spill_name), {
    
    rval_mod$imported_matrix_list <- list()
    
    if(tools::file_ext(input$file$datapath) %in% c("csv", "txt")){
      if(!is.null(input$sep_spill) & !is.null(input$spill_name)){
        sep <- switch(input$sep_spill,
                      "comma" = ",",
                      "semi-column" = ";",
                      "tab" = "\t",
                      "space" = " ")
        
        df <- utils::read.table(file = input$file$datapath,
                                sep = sep,
                                fill = TRUE,
                                quote = "\"",
                                header = TRUE,
                                check.names = FALSE)
        
        rval_mod$imported_matrix_list[[input$spill_name]] <- as.matrix(df)
      }
    }else if(tools::file_ext(input$file$datapath) %in% c("wsp")){
      rval_mod$imported_matrix_list <- get_spillover_matrices_from_ws(input$file$datapath)
    }else if(tools::file_ext(input$file$datapath) %in% c("xml")){
      rval_mod$imported_matrix_list <- get_spillover_matrices_from_ws_diva(input$file$datapath)
    }
  })
  
  
  # observeEvent(input$file, {
  #   
  # })
  
  # observe({
  #   ns <- session$ns
  #   rval_mod$import_items[["preview"]] <- box(title = "Preview",
  #                         width = NULL, collapsible = TRUE, collapsed = FALSE,
  #                         selectInput(ns("select_spill_matrix_imported"), "Select matrix", 
  #                                     choices = names(rval_mod$imported_matrix_list), 
  #                                     selected = names(rval_mod$imported_matrix_list)[1]),
  #                         div(style = 'overflow-x: scroll', DT::DTOutput(ns("spill_imported"))),
  #                         br()
  #   )
  # })
    
  output$ui_import_spill <- renderUI({
    ns <- session$ns
    x <- list()
    if(!is.null(input$file$datapath)){
      if(tools::file_ext(input$file$datapath) %in% c("csv", "txt")){
        
        x[["sep_spill"]] <- selectInput(ns("sep_spill"), "column separator", 
                                        choices = c("comma", "semi-column", "tab", "space"), 
                                        selected = "tab")
        
        x[["spill_name"]] <- textInput(ns("spill_name"), "Matrix name", "CompMat")
        
      }
    }
    tagList(x)
  })

  output$ui_preview_spill <- renderUI({
    ns <- session$ns
    tagList(box(title = "Preview",
        width = NULL, collapsible = TRUE, collapsed = FALSE,
        selectInput(ns("select_spill_matrix_imported"), "Select matrix", 
                    choices = names(rval_mod$imported_matrix_list), 
                    selected = names(rval_mod$imported_matrix_list)[1]),
        div(style = 'overflow-x: scroll', DT::DTOutput(ns("spill_imported"))),
        br()
    ))
  })
  # observe({
  #   validate(need(input$file$datapath, "Please select a file to import"))
  #   validate(need(input$sep_spill, "Please select a file to import"))
  # 
  #   sep <- switch(input$sep_spill,
  #                 "comma" = ",",
  #                 "semi-column" = ";",
  #                 "tab" = "\t",
  #                 "space" = " ")
  # 
  #   df <- utils::read.table(file = input$file$datapath,
  #                                        sep = sep,
  #                                        fill = TRUE,
  #                                        quote = "\"",
  #                                        header = TRUE,
  #                                        check.names = FALSE)
  #   
  #   rval_mod$spill <- as.matrix(df)
  # 
  # })

  output$spill_imported <- DT::renderDT({
    validate(need(rval_mod$imported_matrix_list, "No spillover data imported"))
    validate(need(input$select_spill_matrix_imported, "No selection"))
    DT::datatable(rval_mod$imported_matrix_list[[input$select_spill_matrix_imported]], 
                  rownames = FALSE)
  })

  observeEvent(input$import_matrix, {
    
    rval_mod$spill_list <- list()
    df <- rval_mod$spill
    if(dim(df)[1] == dim(df)[2]){
      row.names(df) <- colnames(df)
      rval_mod$spill_list[[input$spill_name]] <- df
    }else{
      showModal(modalDialog(
        title = "Error",
        paste("Incorrect matrix dimensions", sep=""),
        easyClose = TRUE,
        footer = NULL
      ))
    }

    
  })
  
  spill_matrix <- reactive({
    rval_mod$spill_list
  })
  
  return(spill_matrix)
  
}

### Tests ##############################################################################################
# library(shiny)
# library(shinydashboard)
# 
if (interactive()){

  ui <- dashboardPage(
    dashboardHeader(title = "ImportSpill"),
    sidebar = dashboardSidebar(disable = TRUE),
    body = dashboardBody(
      box(title="Import", width = NULL, height = NULL,
          ImportSpillUI("module")
          )
    )
  )

  server <- function(input, output, session) {
    rval <- reactiveValues()
    observe({
      utils::data("GvHD", package = "flowCore")
      rval$gating_set <- GatingSet(GvHD)
    })

    res <- callModule(ImportSpill, "module", rval = rval)
  }

  shinyApp(ui, server)

}