#' @title   subsampleUI and subsample
#' @description  A shiny Module that deals with subsampling
#' @param id shiny id
#' @importFrom shinydashboard tabBox valueBoxOutput
#' @import shiny
subsampleUI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(width = 6,
           tabBox(title = "",
                  width = NULL, height = NULL,
                  tabPanel("Sample/Subset",
                    selectionInput(ns("selection_module"), multiple_subset = TRUE)
                  ),
                  tabPanel("Compute",
                           numericInput(ns("ncells_per_sample"), "Number of cells / subset / sample", 1000),
                           textInput(ns("fs_name"), "Flow-set name", "sub-sample"),
                           actionButton(ns("compute_data"), "sample"),
                           br(),
                           br(),
                           "Summary",
                           br(),
                           verbatimTextOutput(ns("summary_sub_sample"))
                  )
                  
           ),
           fluidRow(
             valueBoxOutput(ns("progressBox"), width = 6),
             valueBoxOutput(ns("progressBox2"), width = 6)
           )
        
    )
  )
  
}


#' subsample server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param rval A reactive values object
#' @return The updated reactiveValues object \code{rval}
#' @importFrom flowWorkspace gs_get_pop_paths
#' @import shiny
#' @importFrom shinydashboard renderValueBox
#' @rdname subsampleUI
subsample <- function(input, output, session, rval) {
  
  selected <- callModule(selection, "selection_module", rval)
  
  rval_mod <- reactiveValues( flow_set_subsample = NULL )
  
  ##########################################################################################################
  # Observe functions for sub-sampling
  
  observeEvent(input$compute_data, {

    # Create a Progress object
    progress <- shiny::Progress$new(min = 0, max = 100)
    on.exit(progress$close())
    progress$set(message = "Computing...", value = 0)
    updateProgress <- function(value = NULL, detail = NULL) {
      progress$set(value = value, detail = detail)
    }
    
    
    if( length(selected$samples) ==0 ){
      showModal(modalDialog(
        title = "No sample selected",
        paste("Please select samples before proceeding", sep=""),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    validate(need(length(selected$samples)>0, "No sample selected"))
    
   
    if( input$fs_name %in% names(rval$flow_set_list) ){
      showModal(modalDialog(
        title = "Name already exists",
        paste("Please choose another name", sep=""),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    validate(need(! input$fs_name %in% names(rval$flow_set_list), "Name already exists" ))

    
    if( nchar(selected$gate) == 0 ){
      showModal(modalDialog(
        title = "No subset selected",
        paste("Please select a subset before proceeding", sep=""),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    validate(need(selected$gate, "No subset selected"))
    
    #sample = rval$pdata$name[input$sub_sample_table_rows_selected]
    
    rval_mod$df_sample <- get_data_gs(gs = rval$gating_set,
                                  sample = selected$samples, 
                                  subset = selected$gate,
                                  spill = rval$spill,
                                  Ncells = input$ncells_per_sample,
                                  return_comp_data = FALSE,
                                  updateProgress = updateProgress)
    
    if( length(rval_mod$df_sample) == 0 ){
      showModal(modalDialog(
        title = "No cells in selection",
        paste("Please modify selection", sep=""),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    validate(need(length(rval_mod$df_sample)>0, "No cells in selection"))
    
    fs <- build_flowset_from_df(rval_mod$df_sample, 
                                origin = rval$flow_set_list[[rval$flow_set_selected]])
    
    rval_mod$flow_set_subsample <- fs
    
    rval$flow_set_list[[input$fs_name]] <- list(flow_set = fs,
                                                name = input$fs_name, 
                                                parent = rval$flow_set_selected,
                                                gates = rval$gates_flowCore[setdiff(flowWorkspace::gs_get_pop_paths(rval$gating_set), "root")],
                                                spill = rval$df_spill,
                                                transformation = rval$transformation,
                                                trans_parameters = rval$trans_parameters)
    
    rval$flow_set_selected <- input$fs_name
    
  })
  
  output$progressBox <- renderValueBox({
    valueBox(
      length(rval_mod$flow_set_subsample), "samples",icon = icon("list"),
      color = "purple"
    )
  })
  
  output$progressBox2 <- renderValueBox({
    ncells <- 0
    if(!is.null(rval_mod$flow_set_subsample)){
      fs <- rval_mod$flow_set_subsample
      ncells <- sum( sapply(1:length(fs), function(x){dim(fs[[x]]@exprs)[1]}) )
    }
    
    valueBox(
      ncells, "cells", icon = icon("list"),
      color = "green"
    )
  })
  
  output$summary_sub_sample <- renderPrint({
    if(!is.null(rval_mod$flow_set_subsample)){
      print(summary(rval_mod$df_sample[, c("name", "subset")]))
    }else{
      "No sub-sampling performed yet"
    }
  })
  
  return( rval )
  
}