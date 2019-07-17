#' @title   displayUI and display
#' @description  A shiny Module that deals with metadata
#' @param id shiny id
#' @importFrom shinydashboard box tabBox
#' @import shiny
#' @import DT
displayUI <- function(id){
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(
    column(width = 6,
           plotUI(id = ns("plot_module"), simple_plot = FALSE)
    ),
    column(width = 6,
           tabBox(title = "", 
                  width = NULL, height = NULL,
                  tabPanel("Plot",
                           uiOutput(ns("ui_plot"))
                           #plotOutput(ns("plot_display"))
                  ),
                  tabPanel("Display",
                           numericInput(ns("nrow_split"), label = "Number of rows", value = 1),
                           numericInput(ns("row_size"), label = "row size (px)", value = 400)
                           
                  ),
                  tabPanel("Save",
                           numericInput(ns("width_plot"), label = "width", value = 5),
                           numericInput(ns("height_plot"), label = "height", value = 5),
                           downloadButton(ns("download_plot"), "Save plot")
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
#' @rdname displayUI
display <- function(input, output, session, rval) {
  
  `%then%` <- shiny:::`%OR%`
  
   plot_params <- reactiveValues()
  
   plist <- callModule(plot, "plot_module", rval, plot_params, simple_plot = FALSE)
     
   plot_display <- reactive({

     n <- length(plist())
     
     cat("length\n")
     print(n)
     
     nrow <- min(n, input$nrow_split)
     ncol <- ceiling(n/nrow)
     g <- gridExtra::marrangeGrob(plist(), nrow = nrow, ncol = ncol, top = "")

     g
     
   })

  
  output$plot_display  <- renderPlot({
   plot_display()
  })
  
  plot_height <- reactive({
    min(input$nrow_split, length(plist())) * input$row_size + 50
  })

  output$ui_plot <- renderUI({
    ns <- session$ns
    plotOutput(ns("plot_display"), height = plot_height())

  })

  output$download_plot <- downloadHandler(
    filename = "plot.pdf",
    content = function(file) {
      pdf(file, width = input$width_plot, height = input$height_plot)
      print(plot_display())
      dev.off()
    }
  )
  
  return( plot_display )
  
}