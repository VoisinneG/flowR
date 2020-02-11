#' A shiny Module to display and save plots
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
      #box(title = "Plot", width = 12, collapsible = TRUE, collapsed = FALSE,
          
          uiOutput(ns("ui_plot")),
          br(),
          br(),
          fluidRow(
            box(title = "Display options", width = 6, collapsible = TRUE, collapsed = TRUE,
                uiOutput(ns("ui_options"))
                ),
            box(title = "Save", width = 6, collapsible = TRUE, collapsed = TRUE,
                uiOutput(ns("ui_save"))
                )
          )
      #) 
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
                          size = 300,
                          max_height = 500) {
  
  rval_plot <- reactiveValues(nrow = 1,
                              ncol = 1,
                              ncol_facet = 1,
                              nrow_facet = 1,
                              use_plotly = FALSE,
                              top = "",
                              zoom = 100)
  
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
    

  ### Layout plots ##########################################################################
  
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
              print("ok g")
              print(rval_plot$top)
              print(length(plot_list()))
              if("try-error" %in% class(g)){
                showModal(modalDialog(
                  title = "Error",
                  print(g),
                  easyClose = TRUE,
                  footer = NULL
                ))
              }         
              print("ok g 2")        
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
  
  plot_to_render <- reactive({
    validate(need(plot_display(), "No plot to display"))

    if("graphNEL" %in% class(plot_display())){
      Rgraphviz::renderGraph(plot_display())
    }else{
      plot_display()
    }
  })
  
  output$plot_display  <- renderPlot({
    plot_to_render()
  })

  output$plot_display_ly  <- renderPlotly({
    plot_to_render()
  })
  
  ### Build UI for plot options #############################################################
  
  output$ui_options <- renderUI({
    ns <- session$ns
    display_items <- list()
    
    if(!rval_plot$use_plotly){
      if(! "graphNEL" %in% class(plot_display())){
        display_items[["nrow_split"]] <- numericInput(ns("nrow_split"),
                                                      label = "Number of rows", value = nrow)
      }
      display_items[["zoom"]] <- sliderInput(ns("zoom"), 
                                             label = "Zoom", min = 0, max = 800, step = 1, value = rval_plot$zoom)
      display_items[["row_size"]] <- numericInput(ns("row_size"), 
                                                  label = "plot height (px)", value = size)
      display_items[["col_size"]] <- numericInput(ns("col_size"), 
                                                  label = "plot width (px)", value = size)
      display_items[["max_height"]] <- numericInput(ns("max_height"), 
                                                    label = "max height (px)", value = max_height)
      if("top" %in% names(params) ){
        display_items[["show_title"]] <- checkboxInput(ns("show_title"), 
                                                       label = "show title", value = TRUE)
      }

    }
    return( tagList( display_items) )
  })
  
  output$ui_save <- renderUI({
    ns <- session$ns
    downloadButton(ns("download_plot"), "Save plot")
  })
  
  ### Display plot ##########################################################################
  
  ui_plot_elements <- reactive({
    validate(need(plot_display(), "No plot to display"))
    
    x <- list()
    ns <- session$ns
    
    if(is.null(input$row_size)){
      row_size <- size
    }else{
      row_size <- input$row_size
    }
    if(is.null(input$col_size)){
      col_size <- size
    }else{
      col_size <- input$col_size
    }
    if(is.null(input$zoom)){
      zoom_factor <- 100
    }else{
      zoom_factor <- input$zoom
    }
    
    if(rval_plot$use_plotly){
      x <- plotlyOutput(ns("plot_display_ly"), height = size)
    }else{
      height <- rval_plot$nrow * rval_plot$nrow_facet * row_size * zoom_factor/100
      width <- rval_plot$ncol * rval_plot$ncol_facet * col_size * zoom_factor/100
      width <- max(width, 150)
      height <- max(height, 150)
      x <- div(
        style = paste("overflow-y: scroll; overflow-x: scroll; height:", 
                      min(height, input$max_height) + 20, 'px',sep=""),
        plotOutput(ns("plot_display"),
                   height = height, 
                   width = width,
                   brush = ns("plot_brush"),
                   click = ns("plot_click"),
                   dblclick = ns("plot_dblclick")
        )
      )
    }
    return(x)
  })
  
  output$ui_plot <- renderUI({
    
    tagList(ui_plot_elements())
    

  })

  ### Save plot ############################################################################
  
  output$download_plot <- downloadHandler(
    filename = "plot.pdf",
    content = function(file) {
      if(is.null(input$row_size)){
        row_size <- size
      }else{
        row_size <- input$row_size
      }
      if(is.null(input$col_size)){
        col_size <- size
      }else{
        col_size <- input$col_size
      }
      if(is.null(input$zoom)){
        zoom_factor <- 100
      }else{
        zoom_factor <- input$zoom
      }
      height <- rval_plot$nrow * rval_plot$nrow_facet * row_size * zoom_factor/100
      width <- rval_plot$ncol * rval_plot$ncol_facet * col_size * zoom_factor/100
      width <- max(width, 150)
      height <- max(height, 150)
      
      pdf(file, width = width * 5/400, height = height * 5/400)
      if("graphNEL" %in% class(plot_to_render())){
        Rgraphviz::renderGraph(plot_to_render())
      }else{
        print(plot_to_render())
      }
      dev.off()
    }
  )

  return( list( plot = plot_display, params = input) )
}



### Tests ###################################################################################
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
#     params <- reactiveValues(use_plotly = FALSE)
# 
#     plot_list <- reactive({
# 
# 
#         gates <- get_gates_from_ws(
#              "~/2019-Exp-Tumor-042 (Lung Carcinoma)/Classical analysis 06012020.wsp")
#         p <- plot_tree(gates, fontsize = 40, rankdir = NULL, shape = "ellipse", fixedsize = TRUE)
#         p
# 
# 
#       # plist <- list()
#       # plist[[1]] <- ggplot(iris, aes(x=Sepal.Length, y = Sepal.Width, color = Species)) +
#       #   geom_point(alpha = 0.5)
#       #   #facet_wrap(~Species)
#       #
#       #  plist[[2]] <- ggplot(iris, aes(x=Species, y = Sepal.Length, fill = Species)) +
#       #    geom_col(alpha = 0.5)
#       #
#       # return(plist[1])
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
