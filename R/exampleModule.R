#' @title exampleModuleUI and exampleModule
#' @description  A example shiny module to import a csv file
#' @param id shiny id
#' @examples
#' \dontrun{
#' library(shiny)
#' library(DT)
#' if (interactive()){
#' ui <- fluidPage(
#'   exampleModuleUI("fichier"),
#'   DTOutput("tableau")
#' )
#' server <- function(input, output, session) {
#'   data <- callModule(exampleModule,"fichier")
#'   output$tableau <- renderDT({data()})
#' }
#' shinyApp(ui, server)
#' }
#' }
#' 
exampleModuleUI <- function(id) {
  ns <- NS(id)
  out <-  tagList(
    fileInput(ns("file"), label = "csv file"),
    checkboxInput(ns("header"), TRUE),
    textInput(ns("quote"), label = "quote", value  = "\""),
    textInput(ns("sep"), label = "sep", value = "\t")
  )
}


#' exampleModule server function
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param stringsAsFactors logical; Convert strings to factors
#' @importFrom utils read.csv
#' @return A reactive function returning the imported file
#' @rdname exampleModuleUI
exampleModule <- function(input, output, session, stringsAsFactors=TRUE) {
  
  userFile <- reactive({
    validate(need(input$file, message = FALSE))
    input$file
  })
  
  observe({
    message(paste("File", userFile()$name, "uploaded")) 
  })
  
  dataframe <- reactive({
    read.csv(userFile()$datapath, 
             header = input$header,
             sep = input$sep,
             quote = input$quote,
             stringsAsFactors = stringsAsFactors)
  })
  
  output$log <- renderPrint({input$file})
  
  return(dataframe)
}


##################################################################################
# Run example
##################################################################################

 # library(shiny)
 # library(DT)
 # if (interactive()){
 # ui <- fluidPage(
 #   exampleModuleUI("module"),
 #   DTOutput("data_table")
 # )
 # server <- function(input, output, session) {
 #   data <- callModule(exampleModule,"module")
 #   output$data_table <- renderDT({data()})
 # }
 # shinyApp(ui, server)
 # }