#' @title   plotting2UI and plotting2
#' @description  A shiny Module to build, display and save plots from a gating set
#' @param id shiny id
#' @importFrom shinydashboard box tabBox
#' @import shiny
plotting2UI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(width = 4,
           box(width = NULL, height = NULL, title = "Parameters",
               plotGatingSet2Input(id = ns("plot_module"))
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


#' ploting2 server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param rval reactivevalues object with the following elements :
#' \describe{
#'   \item{gating_set}{: a GatingSet object}
#' }
#' @return NULL
#' @import shiny
#' @rdname plottingUI
plotting2 <- function(input, output, session, rval) {

  plot_params <- reactiveValues()
  #rval_mod <- reactiveValues(init = TRUE)
  
  observe({
    
    validate(need(class(rval$gating_set) == "GatingSet", "No GatingSet avaialable"))

      plot_params$gate <- "root"
      plot_params$plot_type <- "hexagonal"
      plot_params$color_var <- NULL
      plot_params$group_var <- NULL
      plot_params$gate <- "root"
      plot_params$use_all_cells <- TRUE

  })
  
  res <- callModule(plotGatingSet2, "plot_module", rval, plot_params, 
                    simple_plot = FALSE, 
                    auto_update = FALSE)
  
  callModule(simpleDisplay, "simple_display_module", res$plot)
  
  return(NULL)
}


##################################################################################
# Tests
##################################################################################


library(shiny)
library(shinydashboard)
library(flowWorkspace)
library(flowCore)
library(viridis)
library(scales)
library(ggplot2)
library(ggrepel)
library(plotly)
library(ggridges)

if (interactive()){
  
  ui <- dashboardPage(
    dashboardHeader(title = "plotting2"),
    sidebar = dashboardSidebar(disable = TRUE),
    body = dashboardBody(
      plotting2UI("module")
    )
  )
  
  server <- function(input, output, session) {
    
    rval <- reactiveValues()
    plot_params <- reactiveValues()
    
    observe({
      gs <- load_gs("./inst/ext/gs")
      rval$gating_set <- gs
    })
    
    res <- callModule(plotting2, "module", rval = rval)
    
  }
  
  shinyApp(ui, server)
  
}
