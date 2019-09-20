#' @title   plottingUI and plotting
#' @description  A shiny Module that deals with metadata
#' @param id shiny id
#' @importFrom shinydashboard box tabBox
#' @import shiny
#' @import DT
plottingUI <- function(id) {
  # Create a namespace function using the provided id
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
#' @return a reactivevalues object with values "flow_set", "parameters" and "gates_flowCore"
#' @import flowWorkspace
#' @import flowCore
#' @import shiny
#' @import DT
#' @export
#' @rdname plottingUI
plotting <- function(input, output, session, rval) {
  
  `%then%` <- shiny:::`%OR%`

  plot_params <- reactiveValues()
  gate <- reactiveValues()
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
      plot_params$plot_type <- "dots"
      plot_params$color_var <- NULL
      plot_params$group_var <- NULL
      plot_params$gate <- "root"
      rval_mod$init <- FALSE
      
    }
    
  })
  
  res <- callModule(plotGatingSet, "plot_module", rval, plot_params, 
                    simple_plot = FALSE, 
                    auto_update = FALSE)
  res_display <- callModule(simpleDisplay, "simple_display_module", res$plot, gate = gate)
  
  
  
  observe({
    for(var in names(res$params)){
      plot_params[[var]] <- res$params[[var]]
    }
  })
  
  return(rval)
  
}