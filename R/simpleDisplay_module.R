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
    uiOutput(ns("ui_plot")),
    br(),
    br(),
    fluidRow(
      uiOutput(ns("ui_options_all"))
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
                          save = TRUE,
                          multirow = FALSE) {
  
  rval_plot <- reactiveValues(nrow = 1,
                              nrow_display = 1,
                              ncol = 1,
                              ncol_facet = 1,
                              nrow_facet = 1,
                              use_plotly = FALSE,
                              title = "",
                              zoom = 100,
                              width = 300,
                              height = 300,
                              max_height = 1000,
                              init = TRUE,
                              show_title = TRUE)
  
  observeEvent(input$zoom, {
    rval_plot$zoom <- input$zoom
  })
  
  observeEvent(input$show_title, {
    rval_plot$show_title <- input$show_title
  })
  
  
  observeEvent(input$width, {
    rval_plot$width <- input$width
  })
  
  observeEvent(input$height, {
    rval_plot$height <- input$height
  })
  
  observeEvent(input$max_height, {
    rval_plot$max_height <- input$max_height
  })
  
  observeEvent(input$nrow, {
    rval_plot$nrow <- input$nrow
  })
  
  # observe({
  #   if(class(plot_list())[1] == "list"){
  #     n <- max(1, length(plot_list()))
  #     if(!is.null(input$nrow)){
  #       rval_plot$nrow <- min(n, input$nrow)
  #     }else{
  #       rval_plot$nrow <- min(n, rval_plot$nrow)
  #     }
  #   }else{
  #     rval_plot$nrow <- 1
  #   }
  # })

  observe({
      for(var in intersect(names(params), names(rval_plot))){
        rval_plot[[var]] <- params[[var]]
      }
  })
  
  observe({
    if(rval_plot$show_title){
      rval_plot$top <- rval_plot$title
    }else{
      rval_plot$top <- ""
    }
  })

  ### Layout plots ##########################################################################
  
  plot_display <- reactive({
    
      rval_plot$ncol_facet <- 1
      rval_plot$nrow_facet <- 1
      rval_plot$ncol <- 1
      rval_plot$nrow_display <- 1
      
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

              rval_plot$nrow_display <- min(n, rval_plot$nrow)
              rval_plot$ncol <- ceiling(n/rval_plot$nrow)

              g <- try(gridExtra::marrangeGrob(plot_list(), 
                                           nrow = rval_plot$nrow_display, 
                                           ncol = rval_plot$ncol, 
                                           top = rval_plot$top),
                       silent = TRUE)

              if("try-error" %in% class(g)){
                showModal(modalDialog(
                  title = "Error",
                  print(g),
                  easyClose = TRUE,
                  footer = NULL
                ))
              }
              return(g)
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
  
  # plot_to_render <- reactive({
  #   validate(need(plot_display(), "No plot to display"))
  # 
  #   if("graphNEL" %in% class(plot_display())){
  #     Rgraphviz::renderGraph(plot_display())
  #   }else{
  #     plot_display()
  #   }
  # })
  
  output$plot_render  <- renderPlot({
    validate(need(plot_display(), "No plot to display"))
    if("graphNEL" %in% class(plot_display())){
      Rgraphviz::renderGraph(plot_display())
    }else{
      plot_display()
    }
  })

  output$plot_render_ly  <- renderPlotly({
    plot_display()
  })
  
  ### Build UI for plot options #############################################################
  
  output$ui_options <- renderUI({
    ns <- session$ns
    display_items <- list()
    
    if(!rval_plot$use_plotly){
      if(multirow){
        display_items[["nrow"]] <- numericInput(ns("nrow"),
                                                      label = "Number of rows", value = rval_plot$nrow)
      }
      display_items[["zoom"]] <- sliderInput(ns("zoom"), 
                                             label = "Zoom", min = 0, max = 200, step = 1, value = rval_plot$zoom)
      display_items[["height"]] <- numericInput(ns("height"), 
                                                  label = "plot height (px)", value = rval_plot$height)
      display_items[["width"]] <- numericInput(ns("width"), 
                                                  label = "plot width (px)", value = rval_plot$width)
      display_items[["max_height"]] <- numericInput(ns("max_height"), 
                                                    label = "max height (px)", value = rval_plot$max_height)
      if("title" %in% names(params) ){
        display_items[["show_title"]] <- checkboxInput(ns("show_title"), 
                                                       label = "show title", value = rval_plot$show_title)
      }

    }
    return( tagList( display_items) )
  })
  
  output$ui_save <- renderUI({
    ns <- session$ns
    downloadButton(ns("download_plot"), "Save plot")
  })
  
  output$ui_options_all <- renderUI({
    ns <- session$ns
    x <- list()
    
    if(!rval_plot$use_plotly){
      x[[1]] <- box(title = "Display options", width = ifelse(save, 6, 12), 
                    collapsible = TRUE, collapsed = TRUE,
                    uiOutput(ns("ui_options"))
      )
    }
    if(save){
      x[[2]] <- box(title = "Save", width = 6, collapsible = TRUE, collapsed = TRUE,
                    uiOutput(ns("ui_save"))
      )
    }
    
    tagList(x)
  })
  
  ### Display plot ##########################################################################
  
  #output$ui_plot_elements <- reactive({
  output$ui_plot <- renderUI({

    x <- list()
    ns <- session$ns
    
    if(rval_plot$use_plotly){
      x[[1]] <- plotlyOutput(ns("plot_render_ly"), height = rval_plot$height)
    }else{
      
      width <- rval_plot$width * rval_plot$zoom/100
      height <- rval_plot$height * rval_plot$zoom/100
      width <- max(width, 150)
      height <- max(height, 150)
      height <- rval_plot$nrow_display * rval_plot$nrow_facet * height
      width <- rval_plot$ncol * rval_plot$ncol_facet * width

      x[[1]] <- #box(title = "Plot", width = NULL, collapsible = TRUE, collapsed = FALSE,
                div(
                  style = paste("overflow-y: scroll; overflow-x: scroll; height:", 
                                min(height, rval_plot$max_height) + 20, 'px',sep=""),
                  plotOutput(ns("plot_render"),
                             height = height,
                             width = width,
                             brush = ns("plot_brush"),
                             click = ns("plot_click"),
                             dblclick = ns("plot_dblclick")
                  )
                #)
      )
       
    }
    return(tagList(x))
  })
  
  # output$ui_plot <- renderUI({
  #   
  #   tagList(ui_plot_elements())
  #   
  # 
  # })

  ### Save plot ############################################################################
  
  output$download_plot <- downloadHandler(
    filename = "plot.pdf",
    content = function(file) {
      width <- rval_plot$width * rval_plot$zoom/100
      height <- rval_plot$height * rval_plot$zoom/100
      width <- max(width, 150)
      height <- max(height, 150)
      height <- rval_plot$nrow_display * rval_plot$nrow_facet * height
      width <- rval_plot$ncol * rval_plot$ncol_facet * width


      pdf(file, width = width * 5/400, height = height * 5/400)
      if("graphNEL" %in% class(plot_display())){
        Rgraphviz::renderGraph(plot_display())
      }else{
        print(plot_display())
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
#         column(6, box(width = NULL, simpleDisplayUI("simple_display_module"))),
#         column(6, box(width = NULL, plotOutput("plot")))
#       )
#     )
#   )
# 
#   server <- function(input, output, session) {
# 
#     params <- reactiveValues(use_plotly = FALSE, width = 500, height = 500, nrow = 2, title = "samples")
# 
#     plot_list <- reactive({
# 
#       load("../flowR_utils/demo-data/Rafa2Gui/analysis/cluster.rda")
#       fs <- build_flowset_from_df(df = res$cluster$data)
#       gs <- GatingSet(fs)
#       #add_gates_flowCore(gs, res$cluster$gates)
#       #plot_gh(gs)
#       
#         # gates <- get_gates_from_ws(
#         #      "../flowR_utils/demo-data/2019-Exp-Tumor-042 (Lung Carcinoma)/Classical analysis 06012020.wsp")
#         # p <- plot_tree(gates, fontsize = 40, rankdir = NULL, shape = "ellipse", fixedsize = TRUE)
#         # p
# 
# 
#       # plist <- list()
#       # plist[[1]] <- ggplot(iris, aes(x=Sepal.Length, y = Sepal.Width, color = Species)) +
#       #   geom_point(alpha = 0.5)+
#       #   facet_wrap(~Species)
#       # 
#       #  plist[[2]] <- ggplot(iris, aes(x=Species, y = Sepal.Length, fill = Species)) +
#       #    geom_col(alpha = 0.5)
#       # 
#       # return(plist)
# 
#       df <- get_data_gs(gs)
#       df_cluster <- get_cluster(df, yvar = names(df)[4:7], y_trans = logicle_trans() )
#       fSOM <- df_cluster$fSOM
#       graphics::plot.new()
#       PlotPies(fSOM, cellTypes=as.factor(df$name))
# 
#     })
# 
#     # output$plot <- renderPlot({10
#     #   #plot_list()
#     #   res$plot()
#     # })
#     
#     res <- callModule(simpleDisplay, "simple_display_module",
#                plot_list = plot_list,
#                params = params,
#                save = FALSE)
# 
#   }
# 
#   shinyApp(ui, server)
# 
# }
