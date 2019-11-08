#' @title dimRedUI and dimRed
#' @description  A shiny Module that deals with dimensionality reduction
#' @param id shiny id
#' @import shiny
#' @importFrom shinydashboard tabBox valueBoxOutput
#' @importFrom DT DTOutput
dimRedUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(
    column(width = 6,
           tabBox(title = "",
                  width = NULL, height = NULL,
                  tabPanel("Sample/Subset",
                           selectionInput(ns("selection_module"), multiple_subset = TRUE)
                  ),
                  tabPanel("Variables",
                           checkboxInput(ns("select_all"), "Select all", value = FALSE),
                           br(),
                           div(style = 'overflow-x: scroll', DT::DTOutput(ns("variables_table")))
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
                           textInput(ns("fs_name"), "Flow-set name", "dim-reduction"),
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
#' @param rval A reactive values object
#' @return The updated reactiveValues object \code{rval}
#' @import shiny
#' @importFrom flowWorkspace gs_get_pop_paths
#' @importFrom shinydashboard renderValueBox
#' @importFrom DT renderDT datatable
#' @importFrom scales identity_trans log10_trans
#' @rdname dim_reductionUI
dimRed <- function(input, output, session, rval) {
  
  selected <- callModule(selection, "selection_module", rval)
  
  rval_mod <- reactiveValues( flow_set_dim_red = NULL )
  
  ##########################################################################################################
  # Observe functions for t-SNE
  
  output$method_ui <- renderUI({
    ns <- session$ns
    x <- list()
    if(input$dim_red_method == 'tSNE'){
      x[[1]] <- numericInput(ns("perplexity"), "perplexity", 50)
      x[[2]] <- numericInput(ns("dims"), "# dimensions", 2)
      x[[3]] <- checkboxInput(ns("check_duplicates"), "check duplicates", FALSE)
    }
    tagList(x)
  })
  
  observeEvent(input$compute, {

    validate(
      need(rval$flow_set, "Empty flow set")
    )

    if( length(selected$samples) ==0 ){
      showModal(modalDialog(
        title = "No sample selected",
        paste("Please select samples before proceeding", sep=""),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    validate(
      need(length(selected$samples)>0, "No sample selected")
    )
    
    
    if( nchar(selected$gate) == 0 ){
      showModal(modalDialog(
        title = "No subset selected",
        paste("Please select a subset before proceeding", sep=""),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    validate(
      need(selected$gate, "No subset selected")
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
    
    if( input$fs_name %in% names(rval$flow_set_list) ){
      showModal(modalDialog(
        title = "Name already exists",
        paste("Please choose another name", sep=""),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    validate(need(! input$fs_name %in% names(rval$flow_set_list), "Name already exists" ))
    
    
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
                      "log10" = scales::log10_trans(),
                      "asinh" = asinh_trans(),
                      "identity" = scales::identity_trans(),
                      NULL)

    progress$set(message = "Getting data...", value = 0)

    df_raw <- get_data_gs(gs = rval$gating_set,
                          sample = selected$samples,
                          subset = selected$gate,
                          spill = rval$spill,
                          Ncells = NULL,
                          return_comp_data = FALSE,
                          updateProgress = updateProgress)

    rval_mod$df_dim_red <- get_data_gs(gs = rval$gating_set,
                                   sample = selected$samples,
                                   subset = selected$gate,
                                   spill = rval$spill,
                                   Ncells = NULL,
                                   return_comp_data = TRUE,
                                   updateProgress = updateProgress)


    progress$set(message = paste("Performing", input$dim_red_method, "..."), value = 0)

    res <- try(dim_reduction(df = rval_mod$df_dim_red,
                         yvar = rval$parameters$name[input$variables_table_rows_selected],
                         Ncells = input$ncells,
                         y_trans = y_trans,
                         transformation = transformation,
                         method = input$dim_red_method,
                         perplexity = ifelse(is.null(input$perplexity), 50, input$perplexity),
                         dims = ifelse(is.null(input$dims), 2, input$dims),
                         check_duplicates = ifelse(is.null(input$check_duplicates), 2, input$check_duplicates)
                         ), silent = TRUE)
    if(class(res) == "try-error"){
      showModal(modalDialog(
        title = "Eroor",
        print(res),
        easyClose = TRUE,
        footer = NULL
      ))
    }else{
      rval_mod$df_dim_red <- res$df
      
      df <- cbind( df_raw[res$keep, ], rval_mod$df_dim_red[ , setdiff(names(rval_mod$df_dim_red), names(df_raw))])
      
      rval$dim_red_var <- res$vars
      
      if(!is.null(rval_mod$df_dim_red)){

        fs <- build_flowset_from_df(df = df, 
                                    origin = rval$flow_set_list[[rval$flow_set_selected]])
        
        rval_mod$flow_set_dim_red <- fs
        
        rval$flow_set_list[[input$fs_name]] <- list(flow_set = fs,
                                                    name = input$fs_name, 
                                                    parent = rval$flow_set_selected,
                                                    gates = rval$gates_flowCore[setdiff(flowWorkspace::gs_get_pop_paths(rval$gating_set), "root")],
                                                    spill = rval$df_spill,
                                                    transformation = rval$transformation,
                                                    trans_parameters = rval$trans_parameters)
        
        rval$flow_set_selected <- input$fs_name
    }
    
    }


  })
  
  output$variables_table <- DT::renderDT({
    
    validate(
      need(rval$parameters, "No data imported")
    )
    
    df <- rval$parameters
    df[["channel_name"]] <- df$name_long
    
    selected <- NULL
    if(input$select_all){
      selected <- 1:length(df$name)
    }
    
    
    DT::datatable(
      df[, c("channel_name", "transform", "transform parameters")], 
      rownames = FALSE, selection = list(target = 'row', selected = selected))
  })
  
  output$progressBox <- renderValueBox({
    valueBox(
      length(rval_mod$flow_set_dim_red), "samples",icon = icon("list"),
      color = "purple"
    )
  })

  output$progressBox2 <- renderValueBox({
    ncells <- 0
    if(!is.null(rval_mod$flow_set_dim_red)){
      fs <- rval_mod$flow_set_dim_red
      ncells <- sum( sapply(1:length(fs), function(x){dim(fs[[x]]@exprs)[1]}) )
    }

    valueBox(
      ncells, "cells", icon = icon("list"),
      color = "green"
    )
  })
  
  output$summary <- renderPrint({
    validate(need(rval_mod$df_dim_red, "no dim-reduction performed"))
    print(summary(rval_mod$df_dim_red[, c("name", "subset")]))
    
    # if(!is.null(rval$df_dim_red)){
    #   print(summary(rval$df_dim_red[, c("name", "subset")]))
    # }else{
    #   "No t-SNE performed yet"
    # }
  })
  
  return( rval )
  
}