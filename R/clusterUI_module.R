#' @title   clusterUI and cluster
#' @description  A shiny Module that deals with metadata
#' @param id shiny id
#' @importFrom shinydashboard box tabBox
#' @import shiny
#' @import DT
clusterUI <- function(id) {
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
                             div(style = 'overflow-x: scroll', DT::dataTableOutput(ns("clustering_variables_table")))
                    ),
                    tabPanel("Options",
                             selectInput(ns("y_trans_clustering"),
                                         label = "Transform variables:",
                                         choices = c("log10", "asinh", "identity", "default"),
                                         selected = "default"),
                             selectInput(ns("clustering_method"), label = "method", choices = c("ClusterX", "Rphenograph"), selected = "ClusterX"),
                             uiOutput(ns("method_ui"))
                    ),
                    tabPanel("Cluster",
                             textInput(ns("fs_name"), "Flow-set name", "cluster"),
                             actionButton(ns("start_clustering"), "Start"),
                             br(),
                             br(),
                             "Summary",
                             br(),
                             verbatimTextOutput(ns("summary_cluster"))
                    )
             )
      )
    )
  
}


#' cluster server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @return a reactivevalues object with values "df_files", "flow_set_imported" and "gates_flowCore"
#' @import flowWorkspace
#' @import flowCore
#' @import shiny
#' @import DT
#' @export
#' @rdname clusterUI
cluster <- function(input, output, session, rval) {
  
  `%then%` <- shiny:::`%OR%`
  
  selected <- callModule(selection, "selection_module", rval)
  
  rval_mod <- reactiveValues( flow_set_cluster = NULL )
  
  ##########################################################################################################
  # Observe functions for t-SNE
  
  output$method_ui <- renderUI({
    ns <- session$ns
    x <- list()
    if(input$clustering_method == 'ClusterX'){
      x[[1]] <- numericInput(ns("cluster_dc"), "dc", 5)
      x[[2]] <- numericInput(ns("cluster_alpha"), "alpha", 0.0001)
    }else if(input$clustering_method == 'Rphenograph'){
      x[[1]] <- numericInput(ns("k_param"), "k", 100)
    }
    tagList(x)
  })
  
  ##########################################################################################################
  # Observe functions for Clustering
  
  observeEvent(input$start_clustering, {
    
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
    
    #print(input$gate_sub_sample)
    
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
    
    if( length(input$clustering_variables_table_rows_selected)==0){
      showModal(modalDialog(
        title = "No variable selected",
        paste("Please select variables before proceeding", sep=""),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    validate(
      need(length(input$clustering_variables_table_rows_selected) >0, "No variables selected")
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
    
    y_trans <- switch(input$y_trans_clustering,
                      "log10" = log10_trans(),
                      "asinh" = asinh_trans(),
                      "identity" = identity_trans(),
                      NULL)
    
    progress$set(message = "Clustering...", value = 0)
    
    df_raw <- get_data_gs(gs = rval$gating_set,
                          sample = selected$samples,
                          subset = selected$gate,
                          spill = rval$spill,
                          return_comp_data = FALSE,
                          Ncells = NULL,
                          updateProgress = updateProgress)
    
    rval_mod$df_cluster <- get_data_gs(gs = rval$gating_set,
                                   sample = selected$samples,
                                   subset = selected$gate,
                                   spill = rval$spill,
                                   return_comp_data = TRUE,
                                   Ncells = NULL,
                                   updateProgress = updateProgress)
    
    
    
    #print(summary(rval$df_tsne))
    
    progress$set(message = "Clustering...", value = 50)
    
    
    res <- get_cluster(df=rval_mod$df_cluster, 
                       yvar = rval$parameters$name[input$clustering_variables_table_rows_selected],
                       y_trans = y_trans,
                       transformation = transformation,
                       method = input$clustering_method,
                       dc = ifelse(is.null(input$cluster_dc), 5, input$cluster_dc), 
                       alpha = ifelse(is.null(input$cluster_alpha), 0.0001, input$cluster_alpha),
                       k = ifelse(is.null(input$k_param), 100, input$k_param)
    )
    
    rval_mod$df_cluster <- res$df
    df <- cbind(df_raw[res$keep, ], rval_mod$df_cluster[c("cluster")])
    
    fs <- build_flowset_from_df(df = df, origin = rval$flow_set_list[[rval$flow_set_selected]])
    rval_mod$flow_set_cluster <- fs
    
    # delete previous cluster gates
    
    idx_cluster_gates <- grep("^/cluster[0-9]+", names(rval$gates_flowCore))
    
    if(length(idx_cluster_gates)>0){
      rval$gates_flowCore <- rval$gates_flowCore[-idx_cluster_gates]
    }
    
    # create one gate per cluster
    
    uclust <- unique(rval_mod$df_cluster$cluster)
    uclust <- uclust[ order(as.numeric(uclust), decreasing = FALSE) ]
    
    for(i in 1:length(uclust)){
      filterID <- paste("cluster", uclust[i], sep = "")
      polygon <- matrix(c(as.numeric(uclust[i])-0.25, 
                          as.numeric(uclust[i])+0.25, 
                          range(rval_mod$df_cluster[[rval_mod$flow_set_cluster@colnames[1]]])
      ), 
      ncol = 2)
      row.names(polygon) <- c("min", "max")
      colnames(polygon) <- c("cluster", rval_mod$flow_set_cluster@colnames[1])
      g <- rectangleGate(.gate = polygon, filterId=filterID)
      rval$gates_flowCore[[paste("/",filterID, sep="")]] <- list(gate = g, parent = "root")
    }
    
    rval$flow_set_list[[input$fs_name]] <- list(flow_set = fs, 
                                                par = lapply(1:length(fs), function(x){parameters(fs[[x]])}),
                                                desc = lapply(1:length(fs), function(x){description(fs[[x]])}),
                                                name = input$fs_name, 
                                                parent = rval$flow_set_selected,
                                                gates = rval$gates_flowCore,
                                                spill = rval$df_spill,
                                                transformation = rval$transformation,
                                                trans_parameters = rval$trans_parameters)
    
    rval$flow_set_selected <- input$fs_name

    
  })
  
  
  output$clustering_variables_table <- DT::renderDataTable({
    
    validate(
      need(rval$parameters, "No data imported")
    )
    
    df <- rval$parameters
    df[["chanel_name"]] <- df$name_long
    
    DT::datatable(
      df[, c("chanel_name", "transform", "transform parameters")], 
      rownames = FALSE)
  })
  
  output$summary_cluster <- renderPrint({
    if(!is.null(rval_mod$df_cluster)){
      print(paste("Number of unique clusters :", length(unique(rval_mod$df_cluster$cluster))))
    }else{
      "No clustering performed yet"
    }
  })
  
  return( rval )
  
}