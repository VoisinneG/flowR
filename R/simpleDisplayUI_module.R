#' @title   simpleDisplayUI and simpleDisplay
#' @description  A shiny Module that deals with metadata
#' @param id shiny id
#' @importFrom shinydashboard box tabBox
#' @import shiny
#' @import DT
simpleDisplayUI <- function(id, nrow = 1, size = 400, save = TRUE){
  # Create a namespace function using the provided id
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
                #numericInput(ns("width_plot"), label = "width", value = 5),
                #numericInput(ns("height_plot"), label = "height", value = 5),
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
#' @return a reactivevalues object with values "df_files", "flow_set_imported" and "gates_flowCore"
#' @import flowWorkspace
#' @import flowCore
#' @import shiny
#' @import gridExtra
#' @import DT
#' @import plotly
#' @export
#' @rdname simpleDisplayUI
simpleDisplay <- function(input, output, session, plist, gate = reactiveValues(), params = reactiveValues(use_plotly = FALSE)) {
  
  `%then%` <- shiny:::`%OR%`
  
  rval_plot <- reactiveValues(nrow = 1, ncol = 1, facet_layout = NULL, ncol_facet = 1, nrow_facet =1)
  
  plot_list <- reactive({
    plist()
  })
    
  plot_display <- reactive({
    
    rval_plot$ncol_facet <- 1
    rval_plot$nrow_facet <- 1
    rval_plot$nrow <- 1
    rval_plot$ncol <- 1
    
     if(class(plot_list()) == "list"){
       
         n <- length(plot_list())
         
         if(n>0){
          if("ggplot" %in% class(plot_list()[[1]]) ){
            
            if(n > 0){
              p <- plot_list()[[1]]
              if("facet" %in% names(p)){
                facet_layout <- p$facet$compute_layout(p, p$facet$params)
                #print(facet_layout)
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
         #print(facet_layout)
         if(!is.null(facet_layout)){
           rval_plot$ncol_facet <- max(facet_layout$COL)
           rval_plot$nrow_facet <- max(facet_layout$ROW)
         }
       }
       
       plot_list()
     }
     
   })

  output$plot_display  <- renderPlot({
   plot_display()
  })

  output$plot_display_ly  <- renderPlotly({
    plot_display()
  })

  output$ui_plot <- renderUI({
    ns <- session$ns
    
    #print(params$use_plotly)
    
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

  observeEvent(gate, {
    if( is.null(gate$x) & is.null(gate$y) ){
      session$resetBrush("plot_brush")
    }
    
   
  })
  
  output$download_plot <- downloadHandler(
    filename = "plot.pdf",
    content = function(file) {
      #pdf(file, width = rval_plot$ncol*input$width_plot, height = rval_plot$nrow*input$height_plot)
      pdf(file, 
          width = rval_plot$ncol * rval_plot$ncol_facet * input$col_size * 5/400, 
          height = rval_plot$nrow * rval_plot$nrow_facet * input$row_size * 5/400)
      
      print(plot_display())

      dev.off()
    }
  )
  
  return( list( plot = plot_display, params = input ) )
  
}