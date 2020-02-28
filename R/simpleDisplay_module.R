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
                              use_plotly = FALSE,
                              title = "",
                              zoom = 100,
                              width = 300,
                              height = 300,
                              max_height = 1000,
                              min_size = 150,
                              init = TRUE,
                              show_title = TRUE)
  
  rval_mod <- reactiveValues(update_render = FALSE,
                             nrow_display = 1,
                             ncol = 1,
                             ncol_facet = 1,
                             nrow_facet = 1)
  
  rval_input <- reactiveValues()
  
  ### Set plot parameters using plot_params ##########################################################
  
  observeEvent(reactiveValuesToList(params), {
    for(var in intersect(names(params), names(rval_plot))){
      rval_plot[[var]] <- params[[var]]
    }
  })
  
  ### store plot parameters in rval_input #############################################################
  
  observeEvent(reactiveValuesToList(rval_plot), {
    for(var in names(rval_plot)){
      rval_input[[var]] <- rval_plot[[var]]
    }
  })
  
  observe({
    for(var in names(input)){
      rval_input[[var]] <- input[[var]]
    }
  })
  
  ### Layout plots #################################################################################
  
  plot_display <- reactive({
    
    
      rval_mod$ncol_facet <- 1
      rval_mod$nrow_facet <- 1
      rval_mod$ncol <- 1
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
                  rval_mod$ncol_facet <- max(facet_layout$COL)
                  rval_mod$nrow_facet <- max(facet_layout$ROW)
                }
              }
            }
            
            if(n > 1){
              
              rval_plot$use_plotly <- FALSE

              rval_mod$nrow_display <- min(n, rval_input$nrow)
              rval_mod$ncol <- ceiling(n/rval_input$nrow)
              
              top <- ""
              if(!is.null(rval_input$show_title)){
                if(rval_input$show_title){
                  top <- rval_plot$title
                }
              }

              g <- try(gridExtra::marrangeGrob(plot_list(), 
                                           nrow = rval_mod$nrow_display, 
                                           ncol = rval_mod$ncol, 
                                           top = top),
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
           rval_mod$ncol_facet <- max(facet_layout$COL)
           rval_mod$nrow_facet <- max(facet_layout$ROW)
         }
       }
       
       plot_list()
     }
     
   })
  
  ### Control plot rendering (when use_plotly == TRUE) #######################################
  
  observeEvent(rval_input$max_height, {
    rval_mod$update_render <- TRUE
  })
  
  observeEvent(c(rval_input$height), {
    rval_mod$update_render <- FALSE
  })
  
  ### Render plot ###########################################################################
  
  output$plot_render  <- renderPlot({
    validate(need(plot_display(), "No plot to display"))
    if("graphNEL" %in% class(plot_display())){
      Rgraphviz::renderGraph(plot_display())
    }else{
      plot_display()
    }
  })

  output$plot_render_ly  <- renderPlotly({
    rval_mod$update_render <- TRUE
    rval_input$height
    rval_input$width
    rval_input$zoom
    plot_display()
  })
  
  ### Build UI for plot options #############################################################
  
  output$ui_options <- renderUI({
    ns <- session$ns
    display_items <- list()
    
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
    
    return( tagList( display_items) )
  })
  
  output$ui_save <- renderUI({
    ns <- session$ns
    downloadButton(ns("download_plot"), "Save plot")
  })
  
  output$ui_options_all <- renderUI({
    ns <- session$ns
    x <- list()
    
    x[[1]] <- box(title = "Display options", width = ifelse(save, 6, 12), 
                  collapsible = TRUE, collapsed = TRUE,
        uiOutput(ns("ui_options"))
    )
    if(save){
      x[[2]] <- box(title = "Save", width = 6, collapsible = TRUE, collapsed = TRUE,
                    uiOutput(ns("ui_save"))
      )
    }
    
    tagList(x)
  })
  
  ### Build UI for plot display ##########################################################################

  
  output$ui_plot <- renderUI({

    x <- list()
    ns <- session$ns
    
    if(rval_plot$use_plotly){
      if(rval_mod$update_render){
        
        width <- rval_input$width * rval_input$zoom/100
        height <- rval_input$height * rval_input$zoom/100
        width <- max(width, min_size = rval_plot$min_size)
        height <- max(height, min_size = rval_plot$min_size)
        
        x[[1]] <- div(
          style = paste("overflow-y: scroll; overflow-x: scroll; height:", 
                        min(height, rval_input$max_height) + 20, 'px', sep=""),
          plotlyOutput(ns("plot_render_ly"), width = width, height = height)
        )
        
      }else{
        x[[1]] <- plotlyOutput(ns("plot_render_ly"))
      }
      
    }else{
      
      width <- rval_input$width * rval_input$zoom/100
      height <- rval_input$height * rval_input$zoom/100
      width <- max(width, min_size = rval_plot$min_size)
      height <- max(height, min_size = rval_plot$min_size)
      height <- rval_mod$nrow_display * rval_mod$nrow_facet * height
      width <- rval_mod$ncol * rval_mod$ncol_facet * width

      x[[1]] <- div(
        style = paste("overflow-y: scroll; overflow-x: scroll; height:", 
                      min(height, rval_input$max_height) + 20, 'px',sep=""),
        plotOutput(ns("plot_render"),
                   height = height,
                   width = width,
                   brush = ns("plot_brush"),
                   click = ns("plot_click"),
                   dblclick = ns("plot_dblclick")
        )
        
      )
       
    }
    return(tagList(x))
  })


  ### Save plot ############################################################################
  
  output$download_plot <- downloadHandler(
    filename = "plot.pdf",
    content = function(file) {
      width <- rval_input$width * rval_input$zoom/100
      height <- rval_input$height * rval_input$zoom/100
      width <- max(width, min_size = rval_plot$min_size)
      height <- max(height, min_size = rval_plot$min_size)
      height <- rval_mod$nrow_display * rval_mod$nrow_facet * height
      width <- rval_mod$ncol * rval_mod$ncol_facet * width


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
#     params <- reactiveValues(use_plotly = FALSE, 
#                              width = 300, 
#                              height = 300, 
#                              max_height = 500, 
#                              min_size = 200,
#                              nrow = 2,
#                              title = "samples")
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
#       print("plot")
#       PlotPies(fSOM, cellTypes=as.factor(df$name))
# 
#       #heatmaply(matrix(runif(100), 50, 2))
# 
#     })
# 
#     # output$plot <- renderPlot({
#     #   #plot_list()
#     #   res$plot()
#     # })
# 
#     res <- callModule(simpleDisplay, "simple_display_module",
#                plot_list = plot_list,
#                params = params,
#                save = FALSE,
#                multirow = FALSE)
# 
#   }
# 
#   shinyApp(ui, server)
# 
#}
