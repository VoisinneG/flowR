#' @title   displayUI and display
#' @description  A shiny Module that deals with metadata
#' @param id shiny id
#' @importFrom shinydashboard box tabBox
#' @import shiny
#' @import DT
displayUI <- function(id, module_ui_name, ...){
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  
  
  module_ui_function <- function(...){
    do.call(module_ui_name, list(...) )
  }
  
  fluidRow(
    column(width = 4,
           box(width = NULL, height = NULL, title = "Parameters",
               module_ui_function(id = ns("plot_module"), ...)
           )
           
    ),
    column(width = 8,
           tabBox(title = "", 
                  width = NULL, height = NULL,
                  tabPanel("Plot",
                           uiOutput(ns("ui_plot"))
                           #plotOutput(ns("plot_display"))
                  ),
                  tabPanel("Display",
                           numericInput(ns("nrow_split"), label = "Number of rows", value = 1),
                           numericInput(ns("row_size"), label = "plot height (px)", value = 400),
                           numericInput(ns("col_size"), label = "plot width (px)", value = 400)
                           
                  ),
                  tabPanel("Save",
                           numericInput(ns("width_plot"), label = "width", value = 5),
                           numericInput(ns("height_plot"), label = "height", value = 5),
                           uiOutput(ns("ui_save"))
                           # fluidRow( 
                           #   column(12,
                           #    downloadButton(ns("download_plot"), "Save plot"),
                           #    uiOutput(ns("ui_save_data"))
                           #   )
                           # )
                  )
           )
    )
  )
  
}


#' display server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @return a reactivevalues object with values "df_files", "flow_set_imported" and "gates_flowCore"
#' @import flowWorkspace
#' @import flowCore
#' @import shiny
#' @import gridExtra
#' @import DT
#' @export
#' @rdname statisticsUI
display <- function(input, output, session, rval, module_server_name, ...) {
  
  `%then%` <- shiny:::`%OR%`
   
   
   rval_plot <- reactiveValues(nrow = 1, ncol = 1)
  
   module_server_function <- function(...){
     do.call(module_server_name, list(...) )
   }
   
   plist <- callModule(module_server_function, "plot_module", rval, ...)$plot
     
   plot_display <- reactive({

     if(class(plist()) == "list"){
       n <- length(plist())

       cat("length\n")
       print(n)

       rval_plot$nrow <- min(n, input$nrow_split)
       rval_plot$ncol <- ceiling(n/rval_plot$nrow)
       g <- gridExtra::marrangeGrob(plist(), nrow = rval_plot$nrow, ncol = rval_plot$ncol, top = "")

       g
     }else{
       plist()
     }
     #print(typeof(plist()))
     #plist()
     
   })

  output$plot_display  <- renderPlot({
   plot_display()
  })
  
  # plot_height <- reactive({
  #   if(class(plist()) == "list"){
  #     min(input$nrow_split, length(plist())) * input$row_size + 50
  #   }else{
  #     input$row_size + 50
  #   }
  #   #input$row_size
  # })

  output$ui_plot <- renderUI({
    ns <- session$ns
    div( style = 'overflow-x: scroll',
         plotOutput(ns("plot_display"), height = rval_plot$nrow*input$row_size, width = rval_plot$ncol*input$col_size)
    )
  })

  
  output$ui_save <- renderUI({
    ns <- session$ns
    x <- list()
    x[[1]] <- downloadButton(ns("download_plot"), "Save plot")
    if( class(plist()) != "list" & "data" %in% names(plist())){
      x[[2]] <- downloadButton(ns("download_data"), "Save data")
    }
    tagList(x)
  })
  
  output$download_data <- downloadHandler(
    filename = "data.txt",
    content = function(file) {
      write.table(x = plist()$data, file = file, sep = "\t", quote = FALSE, row.names = FALSE)
    }
  )
  
  output$download_plot <- downloadHandler(
    filename = "plot.pdf",
    content = function(file) {
      pdf(file, width = rval_plot$ncol*input$width_plot, height = rval_plot$nrow*input$height_plot)
      print(plot_display())
      dev.off()
    }
  )
  
  return( plot_display )
  
}