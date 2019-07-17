#' @title   dimRedUI and dimRed
#' @description  A shiny Module that deals with metadata
#' @param id shiny id
#' @importFrom shinydashboard box tabBox
#' @import shiny
#' @import DT
dimRedUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(
    column(width = 6,
           tabBox(title = "",
                  width = NULL, height = NULL,
                  tabPanel("Variables",
                           div(style = 'overflow-x: scroll', DT::dataTableOutput(ns("variables_table")))
                  ),
                  tabPanel("Options",
                           selectInput(ns("y_trans"), 
                                       label = "Transform variables:", 
                                       choices = c("log10", "asinh", "identity", "default"), 
                                       selected = "default"),
                           selectInput(ns("dim_red_method"), label = "method", choices = c("tSNE" , "umap"), selected = "tSNE"),
                           uiOutput(ns("method_ui"))
                  ),
                  tabPanel("Compute",
                           numericInput(ns("ncells"), "Number of cells", 1000),
                           actionButton(ns("compute"), "Start"),
                           br(),
                           br(),
                           "Summary",
                           br(),
                           verbatimTextOutput(ns("summary"))
                  )
           ),
           fluidRow(
             valueBoxOutput(ns("progressBox"), width = 6),
             valueBoxOutput(ns("progressBox2"), width = 6)
           )
    )
  )
  
}


#' dim_reduction server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @return a reactivevalues object with values "df_files", "flow_set_imported" and "gates_flowCore"
#' @import flowWorkspace
#' @import flowCore
#' @import shiny
#' @import DT
#' @export
#' @rdname dim_reductionUI
dimRed <- function(input, output, session, rval) {
  
  `%then%` <- shiny:::`%OR%`
  
  ##########################################################################################################
  # Observe functions for t-SNE
  
  output$method_ui <- renderUI({
    ns <- session$ns
    x <- list()
    if(input$dim_red_method == 'tSNE'){
      x[[1]] <- numericInput(ns("perplexity"), "perplexity", 50)
    }
    tagList(x)
  })
  
  observeEvent(input$compute, {

    validate(
      need(rval$flow_set, "Empty flow set")
    )


    if( length(input$variables_table_rows_selected)==0){
      showModal(modalDialog(
        title = "No variable selected",
        paste("Please select variables before proceeding", sep=""),
        easyClose = TRUE,
        footer = NULL
      ))
    }

    validate(
      need(length(input$variables_table_rows_selected) >0, "No variables selected")
    )

    # Create a Progress object
    progress <- shiny::Progress$new(min = 0, max = 100)
    on.exit(progress$close())

    updateProgress <- function(value = NULL, detail = NULL) {
      progress$set(value = value, detail = detail)
    }

    transformation <- NULL
    if(rval$apply_trans){
      transformation <- rval$transformation
    }

    y_trans <- switch(input$y_trans,
                      "log10" = log10_trans(),
                      "asinh" = asinh_trans(),
                      "identity" = identity_trans(),
                      NULL)

    progress$set(message = "Getting data...", value = 0)

    df_raw <- get_data_gs(gs = rval$gating_set,
                          sample = pData(rval$gating_set)$name,
                          subset = "root",
                          spill = rval$spill,
                          Ncells = NULL,
                          return_comp_data = FALSE,
                          updateProgress = updateProgress)

    rval$df_dim_red <- get_data_gs(gs = rval$gating_set,
                                sample = pData(rval$gating_set)$name,
                                subset = "root",
                                spill = rval$spill,
                                Ncells = NULL,
                                return_comp_data = TRUE,
                                updateProgress = updateProgress)

    print(dim(rval$df_dim_red))

    progress$set(message = paste("Performing", input$dim_red_method, "..."), value = 0)

    res <- dim_reduction(df = rval$df_dim_red,
                         yvar = rval$parameters$name[input$variables_table_rows_selected],
                         Ncells = input$ncells,
                         y_trans = y_trans,
                         transformation = transformation,
                         method = input$dim_red_method,
                         perplexity = input$perplexity)
    rval$df_dim_red <- res$df

    df <- cbind( df_raw[res$keep, ], rval$df_dim_red[ , setdiff(names(rval$df_dim_red), names(df_raw))])

    rval$dim_red_var <- res$vars

    if(!is.null(rval$df_dim_red)){
      rval$flow_set_dim_red <- build_flowset_from_df(df = df, fs = rval$flow_set)
      rval$flow_set_names <- unique(c(rval$flow_set_names, "dim-reduction"))
      rval$flow_set_selected <- "dim-reduction"

      #updateSelectInput(session, "flow_set", choices = rval$flow_set_names, selected = "dim-reduction")
    }


  })
  
  output$variables_table <- DT::renderDataTable({
    
    validate(
      need(rval$parameters, "No data imported")
    )
    
    df <- rval$parameters
    df[["channel_name"]] <- df$name_long
    
    DT::datatable(
      df[, c("channel_name", "transform", "transform parameters")], 
      rownames = FALSE)
  })
  
  output$progressBox <- renderValueBox({
    valueBox(
      length(rval$flow_set_dim_red), "samples",icon = icon("list"),
      color = "purple"
    )
  })

  output$progressBox2 <- renderValueBox({
    ncells <- 0
    if(!is.null(rval$flow_set_dim_red)){
      fs <- rval$flow_set_dim_red
      ncells <- sum( sapply(1:length(fs), function(x){dim(fs[[x]]@exprs)[1]}) )
    }

    valueBox(
      ncells, "cells", icon = icon("list"),
      color = "green"
    )
  })
  
  output$summary <- renderPrint({
    validate(need(rval$df_dim_red, "no dim-reduction performed"))
    print(summary(rval$df_dim_red[, c("name", "subset")]))
    
    # if(!is.null(rval$df_dim_red)){
    #   print(summary(rval$df_dim_red[, c("name", "subset")]))
    # }else{
    #   "No t-SNE performed yet"
    # }
  })
  
  return( rval )
  
}