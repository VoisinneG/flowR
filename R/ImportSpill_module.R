#' Import spillover matrix
#' @param id shiny id
#' @import shiny
#' @importFrom shinydashboard box tabBox
ImportSpillUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    fileInput(inputId = ns("spill_file"), 
              label = "Choose file", 
              multiple = FALSE),
    uiOutput(ns("ui_import_spill")),
    
    box(title = "Preview",
        width = NULL, collapsible = TRUE,
        tabBox(
          tabPanel(title = "Table",
                   div(style = 'overflow-x: scroll', DT::DTOutput(ns("spill_imported")))
                   ),
          tabPanel(title  = "Heatmap",
                   plotlyOutput(ns("heatmap_spill"))
                   )
        )
    ),
    textInput(ns("spill_name"), "Matrix name", "CompMat"),
    actionButton(ns("inport_matrix"), "import matrix"),
    br(),
    br()
  )
  
}

#' ImportSpill server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param rval A reactive values object
#' @return The Importd matrix
#' @import shiny
#' @importFrom heatmaply heatmaply
#' @importFrom plotly renderPlotly event_data
#' @importFrom DT renderDT
#' @importFrom utils read.table
#' @importFrom  stats median
#' @rdname ImportSpillUI
ImportSpill <- function(input, output, session, rval) {

  rval_mod <- reactiveValues()
  
  ### Get parameters from GatingSet ################################################################
  choices <- reactive({
    rval$update_gs
    validate(need(class(rval$gating_set) == "GatingSet", "No GatingSet available"))
    get_parameters_gs(rval$gating_set)
  })
  
  ### Import compensation matrix ##################################################################

  output$ui_compute_spill <- renderUI({
    
    ns <- session$ns
    
    if(tools::file_ext(input$file$datapath) %in% c("csv", "txt")){
      tagList(
        selectInput(ns("sep_spill"), "column separator", 
                  choices = c("comma", "semi-column", "tab", "space"), 
                  selected = "tab")
      )
    }
    
  })

  observe({
    validate(
      need(input$spill_file$datapath, "Please select a file to import")
    )

    sep <- switch(input$sep_spill,
                  "comma" = ",",
                  "semi-column" = ";",
                  "tab" = "\t",
                  "space" = " ")

    rval$df_spill_imported <- utils::read.table(file = input$spill_file$datapath,
                                         sep = sep,
                                         fill = TRUE,
                                         quote = "\"",
                                         header = TRUE,
                                         check.names = FALSE)

  })

  output$spill_imported <- DT::renderDT({
    validate(
      need(rval$df_spill_imported, "No spillover data imported")
    )
    as.data.frame(rval$df_spill_imported)
  })

  observeEvent(input$set_spillover_matrix,{
    df <- as.data.frame(rval$df_spill_imported)
    row.names(df) <- colnames(df)
    df <- df[row.names(df) %in% colnames(rval$flow_set), colnames(df) %in% colnames(rval$flow_set)]
    rval$df_spill <- df

  })


  observe({

    validate(
      need(length(input$file_comp)>0, "Please select a file to load")
    )

    sep <- switch(input$sep_comp,
                  "comma" = ",",
                  "semi-column" = ";",
                  "tab" = "\t")

    rval$df_comp <- read.csv(input$file_comp$datapath, sep = sep, header = TRUE, quote = "\"", fill = TRUE)
    names(rval$df_comp)[1] <- "name"
  })

  observeEvent(input$import_comp, {
    idx_match_row <- match(rval$pdata$name, rval$df_comp[,1])
    idx_match_col <- match(rval$pdata$name, names(rval$df_comp))
    rval$df_spill <- rval$df_comp[idx_match_row, idx_match_col]
  })
  
  spill_matrix <- reactive({
    rval_mod$spill
  })
  
  output$heatmap_spill <- plotly::renderPlotly({
    
    validate(need(length(spill_matrix())>0, "No matrix computed"))
    df <- as.data.frame(100*spill_matrix()[[1]])
    df[df == 0] <- NA
    df_log <- log10(df)
    
    p <- heatmaply::heatmaply(df_log,
                              colors= viridis,
                              plot_method="plotly",
                              Rowv = NULL,
                              Colv = NULL,
                              column_text_angle = 90,
                              xlab = "detection channel",
                              ylab = "emitting fluorophore",
                              fontsize_row = 10,
                              fontsize_col = 10,
                              cellnote_size = 6,
                              hide_colorbar = TRUE,
                              main = "spillover (%)",
                              custom_hovertext = df,
                              margins = c(50, 50, 50, 0)
    )
    
    p
    
  })
  
  return(spill_matrix)
  
}

### Tests ##############################################################################################
# library(shiny)
# library(shinydashboard)
# library(plotly)
# if (interactive()){
# 
#   ui <- dashboardPage(
#     dashboardHeader(title = "ComputeSpill"),
#     sidebar = dashboardSidebar(disable = TRUE),
#     body = dashboardBody(
#       box(title="Compute", width = NULL, height = NULL,
#           ComputeSpillUI("module")
#           )
# 
#     )
#   )
# 
#   server <- function(input, output, session) {
#     rval <- reactiveValues()
#     observe({
#       #utils::data("GvHD", package = "flowCore")
#       #rval$gating_set <- GatingSet(GvHD)
#       load("../flowR_utils/demo-data/OC17BMGV/comp.rda")
#       gs <- GatingSet(res$comp$flow_set)
#       rval$gating_set <- gs
#     })
# 
#     res <- callModule(ComputeSpill, "module", rval = rval)
#   }
# 
#   shinyApp(ui, server)
# 
# }