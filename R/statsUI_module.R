#' @title   statsUI and stats
#' @description  A shiny Module that deals with metadata
#' @param id shiny id
#' @importFrom shinydashboard box tabBox
#' @import shiny
#' @import DT
statsUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(
    column(width = 4,
           box(width = NULL, height = NULL, title = "Parameters",
               plotStatInput(id = ns("plotStat_module"))
           )
    ),
    column(width = 8,
           tabBox(title = "",
                  width = NULL, height = NULL,
                  tabPanel(title = "Plot",
                           simpleDisplayUI(ns("simple_display_module"))
                  ),
                  tabPanel(title = "Data",
                           br(),
                           downloadButton(ns("download_data")),
                           br(),
                           br(),
                           div(style = 'overflow-x: scroll', DT::dataTableOutput(ns("stats_data"))),
                           br()
                  )
           )
    )
    
  )
  
}


#' stats server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @return a reactivevalues object with values "flow_set", "parameters" and "gates_flowCore"
#' @import flowWorkspace
#' @import flowCore
#' @import shiny
#' @import DT
#' @export
#' @rdname statsUI
stats <- function(input, output, session, rval) {

  rval_mod <- reactiveValues(use_plotly = FALSE)
  
  res <- callModule(plotStat, "plotStat_module", rval)
  res_display <- callModule(simpleDisplay, "simple_display_module", res$plot, params =  rval_mod)

  observe({
    if("plot_type" %in% names(res$params)){
      rval_mod$use_plotly <- switch(res$params$plot_type,
                                    "heatmaply" = TRUE,
                                    FALSE)
    }

  })
  
  output$stats_data <- DT::renderDataTable({
    validate(need(res$data(), "No data available"))
    df <- res$data()
    #df[['value']] <- sprintf("%.2f", as.numeric(df[['value']]))
    DT::datatable(df, rownames = FALSE)
  })
  
  output$download_data <- downloadHandler(
    filename = "stats.txt",
    content = function(file) {
      write.table(res$data(), file = file, row.names = FALSE, quote = FALSE, sep = "\t")
    }
  )
  
  return(rval)
  
}