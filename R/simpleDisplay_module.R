#' @title simpleDisplayUI and simpleDisplay
#' @description  A shiny Module to display and save plots
#' @param id shiny id
#' @import shiny
#' @export
#' @examples 
#' \dontrun{
#' library(shiny)
#' library(shinydashboard)
#' library(gridExtra)
#' library(ggplot2)
#' library(plotly)
#' 
#' if (interactive()){
#' 
#'   ui <- dashboardPage(
#'     dashboardHeader(title = "simpleDisplay"),
#'     sidebar = dashboardSidebar(disable = TRUE),
#'     body = dashboardBody(
#'       fluidRow(
#'         column(12, box(width = NULL, simpleDisplayUI("simple_display_module")))
#'       )
#'     )
#'   )
#' 
#'   server <- function(input, output, session) {
#' 
#'     params <- reactiveValues(top = "Iris", use_plotly = FALSE)
#' 
#'     plot_list <- reactive({
#' 
#'       plist <- list()
#' 
#'       plist[[1]] <- ggplot(iris, aes(x=Sepal.Length, y = Sepal.Width, color = Species)) +
#'         geom_point(alpha = 0.5) +
#'         facet_wrap(~Species)
#' 
#'        plist[[2]] <- ggplot(iris, aes(x=Species, y = Sepal.Length, fill = Species)) +
#'          geom_col(alpha = 0.5)
#' 
#'       return(plist)
#' 
#'     })
#' 
#'     callModule(simpleDisplay, "simple_display_module", 
#'                plot_list = plot_list, 
#'                params = params,
#'                size = 500)
#' 
#'   }
#' 
#'   shinyApp(ui, server)
#' 
#' }
#'}
simpleDisplayUI <- function(id){

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
             uiOutput(ns("ui_options"))
             )

    )
    
  )
}


#' display server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param plot_list a reactivevalues object containing a plot or a list of plots
#' @param params reactivevalues object used to initialize plot parameters
#' with the following elements (not mandatory):
#' \describe{
#'   \item{use_plotly}{: use plotly library to render an interactive plot}
#'   \item{top}{: main title}
#'  }
#' @param nrow Initial number of rows in the layout
#' @param size Initial size of a single plot (in pixels)
#' @return a reactivevalues object with:
#' \describe{
#'   \item{plot}{: the plots displayed}
#'   \item{params}{: input parameters. These include events describing user interaction 
#'   with the plot such as:
#'     \describe{
#'       \item{plot_brush}{: plot brush events}
#'       \item{plot_click}{: plot click events}
#'       \item{plot_dblclick}{: plot double click events}
#'     }
#'   }
#' }
#' @importFrom gridExtra marrangeGrob
#' @importFrom grDevices pdf dev.off
#' @importFrom plotly plotlyOutput renderPlotly
#' @export
#' @rdname simpleDisplayUI
simpleDisplay <- function(input, output, session,
                          plot_list, 
                          params = reactiveValues(),
                          nrow = 1, 
                          size = 300) {
  
  rval_plot <- reactiveValues(nrow = 1,
                              ncol = 1, 
                              ncol_facet = 1, 
                              nrow_facet = 1,
                              use_plotly = FALSE,
                              top = "")
  
  observe({
    for(var in names(params)){
      rval_plot[[var]] <- params[[var]]
    }
    if("show_title" %in% names(input)){
      if(!input$show_title){
        rval_plot$top <- ""
      }
    }
    
  })
    
  
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
              rval_plot$use_plotly <- FALSE
              rval_plot$nrow <- min(n, input$nrow_split)
              rval_plot$ncol <- ceiling(n/rval_plot$nrow)
              
              g <- try(gridExtra::marrangeGrob(plot_list(), 
                                           nrow = rval_plot$nrow, 
                                           ncol = rval_plot$ncol, 
                                           top = rval_plot$top),
                       silent = TRUE)
                       
                       
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
  
  output$ui_options <- renderUI({
    ns <- session$ns
    
    if(!rval_plot$use_plotly){
      tagList(
        box(title = "Display", width = 6, collapsible = TRUE, collapsed = TRUE,
            numericInput(ns("nrow_split"), label = "Number of rows", value = nrow),
            numericInput(ns("row_size"), label = "plot height (px)", value = size),
            numericInput(ns("col_size"), label = "plot width (px)", value = size),
            checkboxInput(ns("show_title"), label = "show title", value = TRUE)
        ),
        box(title = "Save", width = 6, collapsible = TRUE, collapsed = TRUE,
            downloadButton(ns("download_plot"), "Save plot")
        )
      )
    }
  })
  
  output$ui_plot <- renderUI({
    
    validate(need(plot_display(), "No plot to display"))
    validate(need(input$row_size, "No input selected"))
    
    ns <- session$ns
    
    if(rval_plot$use_plotly){
           plotlyOutput(ns("plot_display_ly"), height = size)
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


##################################################################################
# Tests
##################################################################################
# library(shiny)
# library(shinydashboard)
# library(gridExtra)
# library(ggplot2)
# library(plotly)
# 
# if (interactive()){
# 
#   ui <- dashboardPage(
#     dashboardHeader(title = "simpleDisplay"),
#     sidebar = dashboardSidebar(disable = TRUE),
#     body = dashboardBody(
#       fluidRow(
#         column(12, box(width = NULL, simpleDisplayUI("simple_display_module")))
#       )
#     )
#   )
# 
#   server <- function(input, output, session) {
# 
#     params <- reactiveValues(top = "Iris", use_plotly = FALSE)
# 
#     plot_list <- reactive({
# 
#       plist <- list()
# 
#       plist[[1]] <- ggplot(iris, aes(x=Sepal.Length, y = Sepal.Width, color = Species)) +
#         geom_point(alpha = 0.5) +
#         facet_wrap(~Species)
# 
#        plist[[2]] <- ggplot(iris, aes(x=Species, y = Sepal.Length, fill = Species)) +
#          geom_col(alpha = 0.5)
# 
#       return(plist)
# 
#     })
# 
#     callModule(simpleDisplay, "simple_display_module", 
#                plot_list = plot_list, 
#                params = params,
#                size = 500)
# 
#   }
# 
#   shinyApp(ui, server)
# 
# }
