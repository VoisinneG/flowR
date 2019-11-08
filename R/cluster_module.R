#' @title clusterUI and cluster
#' @description  A shiny Module for cluster identification
#' @param id shiny id
#' @importFrom shinydashboard tabBox
#' @import shiny
#' @importFrom DT DTOutput
clusterUI <- function(id) {
  
  ns <- NS(id)
  
    fluidRow(
      column(width = 6,
             tabBox(title = "",
                    width = NULL, height = NULL,
                    tabPanel("Sample/Subset",
                             selectionInput(ns("selection_module"), multiple_subset = TRUE)
                    ),
                    tabPanel("Variables",
                             div(style = 'overflow-x: scroll', DT::DTOutput(ns("clustering_variables_table")))
                    ),
                    tabPanel("Options",
                             selectInput(ns("y_trans_clustering"),
                                         label = "Transform variables:",
                                         choices = c("log10", "asinh", "identity", "default"),
                                         selected = "default"),
                             selectInput(ns("clustering_method"), 
                                         label = "method", 
                                         choices = c("FlowSOM", "ClusterX", "Rphenograph"), 
                                         selected = "FlowSOM"),
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
      ),
      column(width = 6,
             uiOutput(ns("cluster_plot_ui"))
             )
    )
}


#' cluster server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param rval A reactive values object
#' @return The updated reactiveValues object \code{rval}
#' @import shiny
#' @importFrom flowCore rectangleGate
#' @importFrom shinydashboard tabBox
#' @importFrom DT renderDT datatable
#' @importFrom FlowSOM UpdateNodeSize PlotPies PlotStars PlotMarker
#' @importFrom scales identity_trans log10_trans
#' @rdname clusterUI
cluster <- function(input, output, session, rval) {
  
  selected <- callModule(selection, "selection_module", rval)
  
  rval_mod <- reactiveValues( flow_set_cluster = NULL, parameters = NULL )
  callModule(simpleDisplay, "simple_display_module", plot_fSOM)
  
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
    }else if(input$clustering_method == 'FlowSOM'){
      x[[1]] <- numericInput(ns("k_meta"), "k", 7)
      x[[2]] <- checkboxInput(ns("scale"), "scale", FALSE)
    }
    tagList(x)
  })
  
  output$cluster_plot_ui <- renderUI({
    
    ns <- session$ns

    if(input$clustering_method == 'FlowSOM'){
      tabBox(title = "FlowSOM",
             width = NULL, height = NULL,
             tabPanel("Plot",
                      simpleDisplayUI(ns("simple_display_module"), save = FALSE)
                      #plotOutput(ns("plot_fSOM"))
             ),
             tabPanel("Options",
                      selectInput(ns("fSOM_plot_type"), "Plot type", choices = c("stars", "pies", "marker"), selected = "pies"),
                      selectInput(ns("cellTypes"), "Pie variable", choices = c("name", "subset"), selected = "subset"),
                      checkboxInput(ns("scale_node_size"), "Scale node size", TRUE),
                      checkboxInput(ns("show_background"), "Show background", TRUE),
                      selectInput(ns("color_var"), "Color variable", 
                                  choices = rval_mod$parameters, 
                                  selected = NULL)
             )
      )
    }

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
                      "log10" = scales::log10_trans(),
                      "asinh" = asinh_trans(),
                      "identity" = scales::identity_trans(),
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
    
    progress$set(message = "Clustering...", value = 50)
    
    rval_mod$parameters <- rval$parameters$name_long[input$clustering_variables_table_rows_selected]
    
    res <- try(get_cluster(df=rval_mod$df_cluster, 
                       yvar = rval$parameters$name[input$clustering_variables_table_rows_selected],
                       y_trans = y_trans,
                       transformation = transformation,
                       method = input$clustering_method,
                       dc = ifelse(is.null(input$cluster_dc), 5, input$cluster_dc), 
                       alpha = ifelse(is.null(input$cluster_alpha), 0.0001, input$cluster_alpha),
                       k = ifelse(is.null(input$k_param), 100, input$k_param),
                       k_meta = ifelse(is.null(input$k_meta), 7, input$k_meta),
                       scale = ifelse(is.null(input$scale), FALSE, input$scale)
    ), silent = TRUE)
    
    if(class(res) == "try-error"){
      showModal(modalDialog(
        title = "Error during clustering",
        print(res),
        easyClose = TRUE,
        footer = NULL
      ))
    }else{
      if("fSOM" %in% names(res)){
        rval_mod$fSOM <- res$fSOM
      }
      
      rval_mod$df_cluster <- res$df
      if("cluster" %in% names(rval_mod$df_cluster)){
        df <- cbind(df_raw[res$keep, setdiff(names(df_raw), "cluster")], rval_mod$df_cluster[c("cluster")])
      }else{
        df <- df_raw[res$keep, ]
      }
      
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
      if(length(uclust) > 0){
        for(i in 1:length(uclust)){
          filterID <- paste("cluster", uclust[i], sep = "")
          polygon <- matrix(c(as.numeric(uclust[i])-0.25, 
                              as.numeric(uclust[i])+0.25, 
                              range(rval_mod$df_cluster[[colnames(rval_mod$flow_set_cluster)[1]]])
          ), 
          ncol = 2)
          row.names(polygon) <- c("min", "max")
          colnames(polygon) <- c("cluster", colnames(rval_mod$flow_set_cluster)[1])
          g <- flowCore::rectangleGate(.gate = polygon, filterId=filterID)
          rval$gates_flowCore[[paste("/",filterID, sep="")]] <- list(gate = g, parent = "root")
        }
      }
      
      rval$flow_set_list[[input$fs_name]] <- list(flow_set = fs, 
                                                  name = input$fs_name, 
                                                  parent = rval$flow_set_selected,
                                                  gates = rval$gates_flowCore,
                                                  spill = rval$df_spill,
                                                  transformation = rval$transformation,
                                                  trans_parameters = rval$trans_parameters)
      
      rval$flow_set_selected <- input$fs_name
    }
    
    

    
  })
  
  
  plot_fSOM <- reactive({
    validate(need(input$clustering_method == "FlowSOM", "No plot to display"))
    validate(need("fSOM" %in% names(rval_mod), "No plot to display"))
    
    fSOM <- rval_mod$fSOM
    
    if(!input$scale_node_size){
      fSOM <- FlowSOM::UpdateNodeSize(fSOM, reset=TRUE)
    }
    
    backgroundValues <- NULL
    if(input$show_background){
      backgroundValues <- as.factor(fSOM$metaClustering)
    }
    
    
    plot.new()
    
    if(input$fSOM_plot_type == "pies"){
      FlowSOM::PlotPies(fSOM,
                        cellTypes=rval_mod$df_cluster[[input$cellTypes]], 
                        backgroundValues = backgroundValues
      )
    }else if(input$fSOM_plot_type == "stars"){
      FlowSOM::PlotStars(fSOM, 
                         backgroundValues = backgroundValues)
    }else if(input$fSOM_plot_type == "marker"){
      color_var <- rval$parameters$name[match(input$color_var, rval$parameters$name_long)]
      print(color_var)
      FlowSOM::PlotMarker(fSOM, marker =  color_var)
    }
  })
  
  output$plot_fSOM <- renderPlot({

    plot_fSOM()
    
  })
  
  
  output$clustering_variables_table <- DT::renderDT({
    
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