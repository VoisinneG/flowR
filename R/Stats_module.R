#' Compute and visualize sample/subset statistics from a GatingSet
#' @param id shiny id
#' @importFrom shinydashboard box tabBox
#' @import shiny
#' @importFrom  DT DTOutput
StatsUI <- function(id) {
  
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
                           div(style = 'overflow-x: scroll', DT::DTOutput(ns("stats_data"))),
                           br()
                  )
           )
    )
    
  )
  
}


#' Stats module server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param rval A reactive values object
#' @return The updated reactiveValues object \code{rval}
#' @import shiny
#' @importFrom DT renderDT datatable
#' @importFrom utils write.table
#' @rdname StatsUI
Stats <- function(input, output, session, rval) {

  rval_mod <- reactiveValues(use_plotly = FALSE)
  
  res <- callModule(plotStat, "plotStat_module", rval)
  callModule(simpleDisplay, "simple_display_module", plot_list = res$plot, params =  rval_mod)

  observe({
    if("plot_type" %in% names(res$params)){
      rval_mod$use_plotly <- switch(res$params$plot_type,
                                    "heatmaply" = TRUE,
                                    FALSE)
    }
  })
  
  output$stats_data <- DT::renderDT({
    validate(need(res$data(), "No data available"))
    df <- res$data()
    #df[['value']] <- sprintf("%.2f", as.numeric(df[['value']]))
    DT::datatable(df, rownames = FALSE)
  })
  
  output$download_data <- downloadHandler(
    filename = "stats.txt",
    content = function(file) {
      utils::write.table(res$data(), file = file, row.names = FALSE, quote = FALSE, sep = "\t")
    }
  )
  
  return(rval)
  
}