#' @title simpleDisplayUI and simpleDisplay
#' @description  A shiny Module to display and save plots
#' @param id shiny id
#' @param nrow number of rows in the layout
#' @param size Initial size of a single plot (in pixels)
#' @param save logical, add a box with a save button
#' @importFrom shinydashboard box
#' @import shiny
simpleDisplayUI <- function(id, nrow = 1, size = 400, save = TRUE){

  ns <- NS(id)

  tagList(
    fluidRow(
      column(width = 12,
             uiOutput(ns("ui_plot")),
             br(),
             br()
             )
    ),
    fluidRow(
      column(12,
        box(title = "Display", width = 6, collapsible = TRUE, collapsed = TRUE,
            numericInput(ns("nrow_split"), label = "Number of rows", value = nrow),
            numericInput(ns("row_size"), label = "plot height (px)", value = size),
            numericInput(ns("col_size"), label = "plot width (px)", value = size)
            
        ),
        if(save){
          tagList(
            box(title = "Save", width = 6, collapsible = TRUE, collapsed = TRUE,
                downloadButton(ns("download_plot"), "Save plot")
            )
          )
        }
        
      )
    )
  )
}


#' display server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param plot_list list of plots to display
#' @param params a reactivevalues object with an option to display plotly objects
#' @return A list containing the plot displayed and input parameters (these include events 
#' describing user interaction with the plot)
#' @import shiny
#' @importFrom gridExtra marrangeGrob
#' @importFrom grDevices pdf dev.off
#' @importFrom plotly plotlyOutput renderPlotly
#' @rdname simpleDisplayUI
simpleDisplay <- function(input, output, session, plot_list, params = reactiveValues(use_plotly = FALSE)) {
  
  rval_plot <- reactiveValues(nrow = 1, ncol = 1, facet_layout = NULL, ncol_facet = 1, nrow_facet =1)
  
  # plot_list <- reactive({
  #   plist()
  # })
  

  plot_display <- reactive({
    
    rval_plot$ncol_facet <- 1
    rval_plot$nrow_facet <- 1
    rval_plot$nrow <- 1
    rval_plot$ncol <- 1
    
     if(class(plot_list())[1] == "list"){
       
         n <- length(plot_list())
         
         if(n>0){
          if("ggplot" %in% class(plot_list()[[1]]) ){
            
            if(n > 0){
              p <- plot_list()[[1]]
              if("facet" %in% names(p)){
                facet_layout <- p$facet$compute_layout(p, p$facet$params)
                
                if(!is.null(facet_layout)){
                  rval_plot$ncol_facet <- max(facet_layout$COL)
                  rval_plot$nrow_facet <- max(facet_layout$ROW)
                }
              }
            }
            
            if(n > 1){
              rval_plot$nrow <- min(n, input$nrow_split)
              rval_plot$ncol <- ceiling(n/rval_plot$nrow)
              g <- gridExtra::marrangeGrob(plot_list(), nrow = rval_plot$nrow, ncol = rval_plot$ncol, top = "")
              g
            }else if(n == 1){
              plot_list()[[1]]
            }else{
              plot_list()
            }
            
          }else{
            plot_list()
          }
        }
         
     }else{
       p <- plot_list()
       if("facet" %in% names(p)){
         
         facet_layout <- p$facet$compute_layout(p, p$facet$params)
         
         if(!is.null(facet_layout)){
           rval_plot$ncol_facet <- max(facet_layout$COL)
           rval_plot$nrow_facet <- max(facet_layout$ROW)
         }
       }
       
       plot_list()
     }
     
   })
  
  output$plot_display  <- renderPlot({
    ns <- session$ns
    session$resetBrush(ns("plot_brush"))
    plot_display()
  })

  output$plot_display_ly  <- renderPlotly({
    plot_display()
  })

  output$ui_plot <- renderUI({
    ns <- session$ns
    
    if(params$use_plotly){
      div( style = 'overflow-x: scroll',
           plotlyOutput(ns("plot_display_ly"), 
                      height = rval_plot$nrow * rval_plot$nrow_facet * input$row_size, 
                      width = rval_plot$ncol * rval_plot$ncol_facet * input$col_size
           )
      )
    }else{
      div( style = 'overflow-x: scroll',
           plotOutput(ns("plot_display"), 
                      height = rval_plot$nrow * rval_plot$nrow_facet * input$row_size, 
                      width = rval_plot$ncol * rval_plot$ncol_facet * input$col_size,
                      brush = ns("plot_brush"),
                      click = ns("plot_click"),
                      dblclick = ns("plot_dblclick")
           )
      )
    }
    

  })

  
  output$download_plot <- downloadHandler(
    filename = "plot.pdf",
    content = function(file) {
      pdf(file, 
          width = rval_plot$ncol * rval_plot$ncol_facet * input$col_size * 5/400, 
          height = rval_plot$nrow * rval_plot$nrow_facet * input$row_size * 5/400)
      
      print(plot_display())

      dev.off()
    }
  )

  return( list( plot = plot_display, params = input ) )
}