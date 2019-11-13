#' @title   plottingUI and plotting
#' @description  A shiny Module to build, display and save plots from a gating set
#' @param id shiny id
#' @importFrom shinydashboard box tabBox
#' @import shiny
plottingUI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(width = 4,
           box(width = NULL, height = NULL, title = "Parameters",
               plotGatingSetInput(id = ns("plot_module"), simple_plot = FALSE, auto_update = FALSE)
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


#' ploting server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param rval A reactive values object
#' @return The updated reactiveValues object \code{rval}
#' @import shiny
#' @rdname plottingUI
plotting <- function(input, output, session, rval) {

  plot_params <- reactiveValues()
  rval_mod <- reactiveValues(init = TRUE)
  
  observe({
    
    validate(need(rval$plot_var, "No plotting parameters"))
    validate(need(rval$pdata, "No metadata available"))
    
    if(rval_mod$init){
      idx_x <- grep("FSC", rval$plot_var)
      if(length(idx_x)>0){
        xvar <- rval$plot_var[idx_x[1]]
      }else{
        xvar <- rval$plot_var[1]
      }
      
      idx_y <- grep("SSC", rval$plot_var)
      if(length(idx_y)>0){
        yvar <- rval$plot_var[idx_y[1]]
      }else{
        yvar <- rval$plot_var[2]
      }
      
      plot_params$samples <- rval$pdata$name[1]
      plot_params$gate <- "root"
      plot_params$xvar <- xvar
      plot_params$yvar <- yvar
      plot_params$plot_type <- "hexagonal"
      plot_params$color_var <- NULL
      plot_params$group_var <- NULL
      plot_params$gate <- "root"
      plot_params$use_all_cells <- TRUE
      rval_mod$init <- FALSE
      
    }

  })
  
  res <- callModule(plotGatingSet, "plot_module", rval, plot_params, 
                    simple_plot = FALSE, 
                    auto_update = FALSE)
  
  callModule(simpleDisplay, "simple_display_module", res$plot)
  
  return(rval)
}