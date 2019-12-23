#' Build, display and save plots from a gating set
#' @param id shiny id
#' @importFrom shinydashboard box tabBox
#' @import shiny
#' @export
PlottingUI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(width = 4,
           box(width = NULL, height = NULL, title = "Parameters",
               plotGatingSetInput(id = ns("plot_module"))
           )
    ),
    column(width = 8,
           tabBox(title = "",
                  width = NULL, height = NULL,
                  tabPanel(title = "Plot",
                           simpleDisplayUI(ns("simple_display_module"))
                  )
           )
    )
    
  )
  
}


#' Plotting module server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param rval reactivevalues object with the following elements :
#' \describe{
#'   \item{gating_set}{: a GatingSet object}
#' }
#' @return The updated reactiveValues object \code{rval}
#' @import shiny
#' @export
#' @rdname PlottingUI
Plotting <- function(input, output, session, rval){

  plot_params <- reactiveValues()
  #rval_mod <- reactiveValues(init = TRUE)
  
  observe({
    
    validate(need(class(rval$gating_set) == "GatingSet", "No GatingSet avaialable"))

      plot_params$subset <- "root"
      plot_params$plot_type <- "hexagonal"
      plot_params$use_all_cells <- TRUE

  })
  
  res <- callModule(plotGatingSet, "plot_module", rval, plot_params,
                    simple_plot = FALSE, 
                    auto_update = FALSE
                    )
  
  callModule(simpleDisplay, "simple_display_module", res$plot)
  
  return(rval)
}


##################################################################################
# Tests
##################################################################################
# library(shiny)
# library(shinydashboard)
# library(flowWorkspace)
# library(flowCore)
# library(viridis)
# library(scales)
# library(ggplot2)
# library(ggrepel)
# library(plotly)
# library(ggridges)
# 
# if (interactive()){
# 
#   ui <- dashboardPage(
#     dashboardHeader(title = "plotting"),
#     sidebar = dashboardSidebar(disable = TRUE),
#     body = dashboardBody(
#       PlottingUI("module")
#     )
#   )
# 
#   server <- function(input, output, session) {
# 
#     rval <- reactiveValues()
#     plot_params <- reactiveValues()
# 
#     observe({
#       utils::data("GvHD", package = "flowCore")
#       rval$gating_set <- GatingSet(GvHD)
#       #gs <- load_gs("./inst/ext/gs")
#       #rval$gating_set <- gs
#     })
# 
#     res <- callModule(Plotting, "module", rval = rval)
# 
#   }
# 
#   shinyApp(ui, server)
# 
# }
