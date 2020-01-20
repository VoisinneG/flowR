#' Compute spillover matrix
#' @param id shiny id
#' @import shiny
#' @importFrom shinydashboard box tabBox
ComputeSpillUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("ui_compute_spill")),
    actionButton(ns("add_spill_param"), "Add parameter"),
    br(),
    br(),
    selectizeInput(ns("spill_params"), "Spillover parameters", 
                   choices = NULL, selected = NULL, multiple = TRUE),
    textInput(ns("spill_name"), "Matrix name", "CompMat"),
    actionButton(ns("compute_spillover_matrix"), "Compute spillover matrix"),
    br(),
    br(),
    box(title = "Heatmap",
        width = NULL, collapsible = TRUE,
        plotlyOutput(ns("heatmap_spill"))
        )
  )
  
}

#' ComputeSpill server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param rval A reactive values object
#' @return The computed matrix
#' @import shiny
#' @importFrom heatmaply heatmaply
#' @importFrom plotly renderPlotly event_data
#' @importFrom DT renderDT
#' @importFrom utils read.table
#' @importFrom  stats median
#' @rdname ComputeSpillUI
ComputeSpill <- function(input, output, session, rval) {

  rval_mod <- reactiveValues()
  
  ### Get parameters from GatingSet ################################################################
  choices <- reactive({
    rval$update_gs
    validate(need(class(rval$gating_set) == "GatingSet", "No GatingSet available"))
    get_parameters_gs(rval$gating_set)
  })
  
  ### Compute compensation matrix ##################################################################

  output$ui_compute_spill <- renderUI({

    ns <- session$ns
    
    tagList(
      selectInput(ns("fluo"),
                  "fluorophore",
                  choices = choices()$plot_var,
                  selected = choices()$plot_var[1]),

      selectInput(ns("sample_pos"),
                  "sample pos",
                  choices = choices()$sample,
                  selected = choices()$sample[1]),

      selectInput(ns("gate_pos"),
                  "gate pos",
                  choices = choices()$subset,
                  selected = "root"),

      selectInput(ns("sample_neg"),
                  "sample neg",
                  choices = choices()$sample,
                  selected = choices()$sample[1]),

      selectInput(ns("gate_neg"),
                  "gate neg",
                  choices = choices()$subset,
                  selected = "root")
    )

  })

  observeEvent(input$add_spill_param, {

    spill_vars <- choices()$params$name
    
    df_pos <- get_data_gs(gs = rval$gating_set,
                          sample = input$sample_pos,
                          subset = input$gate_pos,
                          spill = NULL)
    
    df_pos <- df_pos[names(df_pos) %in% spill_vars]
    
    pos_values <- apply(df_pos, MARGIN = 2, FUN = stats::median,  na.rm = TRUE)
    pos_values <- pos_values[spill_vars]
    names(pos_values) <- spill_vars
    rval_mod$pos_values[[input$fluo]] <- pos_values

    print(rval_mod$pos_values)

    df_neg <- get_data_gs(gs = rval$gating_set,
                          sample = input$sample_neg,
                          subset = input$gate_neg,
                          spill = NULL)

    df_neg <- df_neg[names(df_neg) %in% spill_vars]
    neg_values <- apply(df_neg, MARGIN = 2, FUN = stats::median, na.rm = TRUE)
    neg_values <- neg_values[spill_vars]
    names(neg_values) <- spill_vars
    rval_mod$neg_values[[input$fluo]] <- neg_values

    print(rval_mod$neg_values)

  })

  observe({
    updateSelectInput(session, "spill_params",
                      choices = names(rval_mod$pos_values),
                      selected = names(rval_mod$pos_values))
  })

  observeEvent(input$compute_spillover_matrix, {
    
    validate(need(length(input$spill_params)>0, "Not enough parameters selected"))

    rval_mod$spill <- list()
    
    df_pos_tot <- data.frame(do.call(rbind, rval_mod$pos_values[input$spill_params]),
                             check.names = FALSE)
    row.names(df_pos_tot) <- input$spill_params
    df_pos_tot <- df_pos_tot[input$spill_params]
    print(df_pos_tot)
    
    df_neg_tot <- data.frame(do.call(rbind, rval_mod$neg_values[input$spill_params]),
                             check.names = FALSE)
    row.names(df_neg_tot) <- input$spill_params
    df_neg_tot <- df_neg_tot[input$spill_params]
    print(df_neg_tot)

    df_spill <- df_pos_tot
    for(var in input$spill_params){
      
      neg <- rval_mod$neg_values[[var]][[var]]
      pos <- rval_mod$pos_values[[var]][[var]]
      df_spill[var] <- (df_pos_tot[var] - neg)/(pos - neg)
      
    }
    
    print(df_spill)
    rval_mod$spill[[input$spill_name]] <- df_spill

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