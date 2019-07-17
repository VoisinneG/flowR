#' @import shiny
#' @import flowWorkspace
#' @import flowCore
#' @import ncdfFlow
#' @import gridExtra
#' @import DT
#' @import plotly
#' @import heatmaply
#' @import CytoML
#' @import Rgraphviz
#' @import scales
#' @import tools
#' @import utils
#' @import sp
#' @import viridis
#' @export
flowR_server <- function(session, input, output) {
  
  `%then%` <- shiny:::`%OR%`
  
  rval <- reactiveValues()
  
  # rval <- reactiveValues(df_files = NULL,
  #                        datasets = list(),
  #                        flow_set_imported = NULL,
  #                        flow_set_filter = NULL,
  #                        flow_set_sample = NULL,
  #                        flow_set_tsne = NULL,
  #                        flow_set_cluster = NULL,
  #                        flow_set = NULL,
  #                        gating_set = NULL,
  #                        idx_ff_gate = NULL,
  #                        parameters = NULL,
  #                        gates = list(),
  #                        gate_focus = NULL,
  #                        df_gate_focus = NULL,
  #                        gates_flowCore = list(),
  #                        min_val = NULL,
  #                        max_val = NULL,
  #                        transformation = list(),
  #                        trans_parameters = list(),
  #                        keywords = NULL,
  #                        plot_var = NULL,
  #                        gate = NULL,
  #                        data_range = NULL,
  #                        pdata = NULL,
  #                        pdata_original = NULL,
  #                        df_tot = NULL,
  #                        df_meta = NULL,
  #                        df_meta_imported = NULL,
  #                        df_meta_mapped = NULL,
  #                        df_keywords = NULL,
  #                        df_sample = NULL,
  #                        df_tsne = NULL,
  #                        df_cluster = NULL,
  #                        df_spill_original = NULL,
  #                        df_spill = NULL,
  #                        df_spill_imported = NULL,
  #                        spill = NULL,
  #                        Ncells_tot = 0,
  #                        flow_set_names = NULL,
  #                        pos_values = list(),
  #                        neg_values = list(),
  #                        time_step = 1,
  #                        dim_red_var = NULL
  # )
  
  gate <- reactiveValues(x = NULL, y = NULL)
  
  
  # Import module : import flowSet, gates from fcs files and workspace
  rval <- callModule(import, "import_module")
  
  # Metadata module
  rval <- callModule(metadata, "metadata_module", rval)

  # Transform module
  rval <- callModule(transform, "transform_module", rval)
  
  # Compensation module
  rval <- callModule(compensation, "compensation_module", rval)
  
  # Subsample module
  rval <- callModule(subsample, "subsample_module", rval)
  
  # Dimensionality reduction module
  rval <- callModule(dimRed, "dim_reduction_module", rval)
  
  # Display module
  plot_display <- callModule(display, "display_module", rval)
  
  observeEvent(input$apply_trans, {
    rval$apply_trans <- input$apply_trans
  })
  
  observeEvent(input$apply_comp, {
    rval$apply_comp <- input$apply_comp
  })
  
  ##########################################################################################################
  # Create gating set
  
  observeEvent(rval$flow_set_selected, {
    updateSelectInput(session, "flow_set", choices = rval$flow_set_names, selected = rval$flow_set_selected)
  })
  
  observe({
    validate(
      need(input$flow_set, "No flow set selected")
    )
    
    rval$flow_set <- switch(input$flow_set,
                            "imported" = rval$flow_set_imported,
                            "filter" = rval$flow_set_filter,
                            "sub-sample" = rval$flow_set_sample,
                            "dim-reduction" = rval$flow_set_dim_red,
                            "cluster" = rval$flow_set_cluster
    )
    
  })
  
  observe({
    validate(
      need(rval$flow_set, "No flow set available")
    )
    
    fs <- rval$flow_set
    rval$Ncells_tot <- sum( sapply(1:length(fs), function(x){dim(fs[[x]]@exprs)[1]}) )
    
  })
  
  
  observeEvent(input$flow_set, {
    
    validate(
      need(rval$flow_set, "No flow set available")
    )
    
    fs <- rval$flow_set
    params <- parameters(fs[[1]])$name
    
    min_val <- as.data.frame(fsApply(fs, each_col, min, na.rm = TRUE))
    min_val_all <- apply(min_val, 2, min)
    max_val <- as.data.frame(fsApply(fs, each_col, max,  na.rm = TRUE))
    max_val_all <- apply(max_val, 2, max)
    
    rval$data_range <- lapply(params, function(x){
      c(min_val_all[[x]] , max_val_all[[x]])
    })
    names(rval$data_range) <- params
    
  })
  
  observe({
    
    validate(
      need(rval$flow_set, "No flow set available")
    )
    
    rval$gating_set <- GatingSet(rval$flow_set)
    
    # add gates
    print(names(rval$gates_flowCore))
    rval$gating_set <- add_gates_flowCore(rval$gating_set, rval$gates_flowCore)

    # update gates
    
    gate_names <- getNodes(rval$gating_set)
    
    #updateSelectInput(session, "gate_selected", choices = gate_names)
    updateSelectInput(session, "gate", choices = gate_names, selected = "root")
    updateSelectInput(session, "gate_stat", choices = gate_names, selected = "root")
    #updateSelectInput(session, "gate_trans", choices = gate_names, selected = "root")
    #updateSelectInput(session, "gate_comp", choices = gate_names, selected = "root")
    updateSelectInput(session, "gate_to_delete", choices = setdiff(gate_names,"root"))
    updateSelectInput(session, "gate_sub_sample", choices = gate_names, selected = "root")
    
  })
  
  observe({
    updateSelectInput(session, "gate_stat",
                      selected = names(rval$gates_flowCore)[grep("^/cluster[0-9]+", names(rval$gates_flowCore))])
  })

  
  ##########################################################################################################
  # Observe functions for gating
  
  observeEvent(input$plot_click, {
    
    xvar <- rval$parameters$name[match(input$xvar_gate, rval$parameters$name_long)]
    yvar <- rval$parameters$name[match(input$yvar_gate, rval$parameters$name_long)]
    
    gate$x <- c(gate$x, rval$transformation[[xvar]]$inverse(input$plot_click$x))
    gate$y <- c(gate$y, rval$transformation[[yvar]]$inverse(input$plot_click$y))
    
  })
  
  observeEvent(input$plot_brush, {
    brush <- input$plot_brush
    
    if (!is.null(brush)) {
      
      xvar <- rval$parameters$name[match(input$xvar_gate, rval$parameters$name_long)]
      yvar <- rval$parameters$name[match(input$yvar_gate, rval$parameters$name_long)]
      
      gate$x <- rval$transformation[[xvar]]$inverse(c(brush$xmin, brush$xmax, brush$xmax, brush$xmin))
      gate$y <- rval$transformation[[yvar]]$inverse(c(brush$ymin, brush$ymin, brush$ymax, brush$ymax))
      
      session$resetBrush("plot_brush")
      
    }
  })
  
  observeEvent(input$plot_dblclick, {
    gate$x <- NULL
    gate$y <- NULL
    #cat("dblclick")
    session$resetBrush("plot_brush")
  })
  
  observeEvent(input$reset_gate, {
    gate$x <- NULL
    gate$y <- NULL
    rval$gate <- NULL
    session$resetBrush("plot_brush")
  })
  
  observeEvent(input$create_gate, {
    
    if(input$gate_name %in% basename(getNodes(rval$gating_set))){
      showModal(modalDialog(
        title = "Error",
        "Gate name already exists! Please choose another name.",
        easyClose = TRUE,
        footer = NULL
      ))
    }else{
      if(!is.null(gate$x)){
        polygon <- data.frame(x =gate$x, y = gate$y)
        hpts <- chull(polygon)
        #hpts <- c(hpts, hpts[1])
        polygon <- polygon[hpts, ]
        polygon <- as.matrix(polygon)
        
        var_names <- c(rval$parameters$name[match(input$xvar_gate, rval$parameters$name_long)],
                       rval$parameters$name[match(input$yvar_gate, rval$parameters$name_long)])
        names(var_names)<-NULL
        colnames(polygon) <- var_names
        
        poly_gate <- polygonGate(.gate = polygon, filterId=input$gate_name)
        rval$gate <- poly_gate
        rval$gates_flowCore[[input$gate_name]] <- list(gate = poly_gate, parent = rval$gate_focus)
        
        
        add(rval$gating_set, poly_gate, parent = rval$gate_focus)
        recompute(rval$gating_set)
        
        updateSelectInput(session, "gate_selected", choices = getNodes(rval$gating_set), selected = input$gate_name)
        updateSelectInput(session, "gate_to_delete", choices = setdiff(getNodes(rval$gating_set), "root"))
        updateSelectInput(session, "gate", choices = getNodes(rval$gating_set), selected = "root")
        updateSelectInput(session, "gate_stat", choices = getNodes(rval$gating_set), selected = "root")
        updateSelectInput(session, "gate_trans", choices = getNodes(rval$gating_set), selected = "root")
        updateSelectInput(session, "gate_comp", choices = getNodes(rval$gating_set), selected = "root")
        updateSelectInput(session, "gate_sub_sample", choices = getNodes(rval$gating_set), selected = "root")
        
        
        gate$x <- NULL
        gate$y <- NULL
        
      }
    }
    
  })
  
  
  observeEvent(input$gate_selected, {
    rval$gate_focus <- input$gate_selected
  })
  
  observeEvent(input$delete_gate, {
    if(input$gate_to_delete != "root"){
      
      idx_gh <- which( getNodes(rval$gating_set) == input$gate_to_delete )
      target_gate <- getNodes(rval$gating_set)[idx_gh]
      child_gates <- getChildren(rval$gating_set[[1]], target_gate)
      idx_delete <- which( names(rval$gates_flowCore) %in% c(target_gate, child_gates) )
      rval$gates_flowCore <- rval$gates_flowCore[-idx_delete]
      
      Rm(target_gate, rval$gating_set)
      recompute(rval$gating_set)
      
      updateSelectInput(session, "gate_selected", choices = getNodes(rval$gating_set), selected = "root")
      updateSelectInput(session, "gate_to_delete", choices = setdiff(getNodes(rval$gating_set), "root"))
      # updateSelectInput(session, "gate", choices = getNodes(rval$gating_set), selected = "root")
      updateSelectInput(session, "gate_stat", choices = getNodes(rval$gating_set), selected = "root")
      updateSelectInput(session, "gate_trans", choices = getNodes(rval$gating_set), selected = "root")
      updateSelectInput(session, "gate_comp", choices = getNodes(rval$gating_set), selected = "root")
      
    }
    
  })
  
  observeEvent(input$show_gate, {
    if(input$gate_selected != "root"){
      
      rval$gate <- rval$gates_flowCore[[input$gate_selected]]$gate
      gate_params <- names(rval$gate@parameters)
      
      params <- rval$parameters$name_long[match(gate_params, rval$parameters$name)]
      
      if(length(params) > 0){
        updateSelectInput(session, "xvar_gate", selected = params[1])
      }
      if(length(params) > 1){
        updateSelectInput(session, "yvar_gate", selected = params[2])
      }
      
      gate$x <- NULL
      gate$y <- NULL
      
      updateSelectInput(session, "gate_selected",  
                        selected = rval$gates_flowCore[[input$gate_selected]]$parent)
      
    }
    
    
  })
  
  
  # ##########################################################################################################
  # # Observe functions for sub-sampling
  # 
  # observeEvent(input$compute_data, {
  #   
  #   # Create a Progress object
  #   progress <- shiny::Progress$new(min = 0, max = 100)
  #   on.exit(progress$close())
  #   progress$set(message = "Computing...", value = 0)
  #   updateProgress <- function(value = NULL, detail = NULL) {
  #     progress$set(value = value, detail = detail)
  #   }
  #   
  #   
  #   if( length(input$sub_sample_table_rows_selected)==0){
  #     showModal(modalDialog(
  #       title = "No sample selected",
  #       paste("Please select samples before proceeding", sep=""),
  #       easyClose = TRUE,
  #       footer = NULL
  #     ))
  #   }
  #   
  #   validate(
  #     need(length(input$sub_sample_table_rows_selected)>0, "No sample selected")
  #   )
  #   
  #   #print(input$gate_sub_sample)
  #   
  #   if( nchar(input$gate_sub_sample) == 0 ){
  #     showModal(modalDialog(
  #       title = "No subset selected",
  #       paste("Please select a subset before proceeding", sep=""),
  #       easyClose = TRUE,
  #       footer = NULL
  #     ))
  #   }
  #   
  #   validate(
  #     need(input$gate_sub_sample, "No subset selected")
  #   )
  #   
  #   sample = rval$pdata$name[input$sub_sample_table_rows_selected]
  #   
  #   rval$df_sample <- get_data_gs(gs = rval$gating_set,
  #                                 sample = sample, 
  #                                 subset = input$gate_sub_sample,
  #                                 spill = rval$spill,
  #                                 Ncells = input$ncells_per_sample,
  #                                 return_comp_data = FALSE,
  #                                 updateProgress = updateProgress)
  #   #print(rval$df_sample)
  #   
  #   if( length(rval$df_sample) == 0 ){
  #     showModal(modalDialog(
  #       title = "No cells in selection",
  #       paste("Please modify selection", sep=""),
  #       easyClose = TRUE,
  #       footer = NULL
  #     ))
  #   }
  #   
  #   validate(
  #     need(length(rval$df_sample)>0, "No cells in selection")
  #   )
  #   
  #   rval$flow_set_sample <- build_flowset_from_df(rval$df_sample, fs = rval$flow_set)
  #   print("OK")
  #   print(dim(rval$df_sample))
  #   rval$flow_set_names <- unique(c(rval$flow_set_names, "sub-sample"))
  #   rval$flow_set_selected <- "sub-sample"
  # })
  
  
  # ##########################################################################################################
  # # Observe functions for t-SNE
  # 
  # 
  # observeEvent(input$compute_tsne, {
  #   
  #   validate(
  #     need(rval$flow_set, "Empty flow set") 
  #   )
  #   
  #   
  #   if( length(input$tSNE_variables_table_rows_selected)==0){
  #     showModal(modalDialog(
  #       title = "No variable selected",
  #       paste("Please select variables before proceeding", sep=""),
  #       easyClose = TRUE,
  #       footer = NULL
  #     ))
  #   }
  #   
  #   validate(
  #     need(length(input$tSNE_variables_table_rows_selected) >0, "No variables selected")
  #   )
  #   
  #   # Create a Progress object
  #   progress <- shiny::Progress$new(min = 0, max = 100)
  #   on.exit(progress$close())
  #   
  #   updateProgress <- function(value = NULL, detail = NULL) {
  #     progress$set(value = value, detail = detail)
  #   }
  #   
  #   transformation <- NULL
  #   if(input$apply_trans){
  #     transformation <- rval$transformation
  #   }
  #   
  #   y_trans <- switch(input$y_trans_tsne,
  #                     "log10" = log10_trans(),
  #                     "asinh" = asinh_trans(),
  #                     "identity" = identity_trans(),
  #                     NULL)
  #   
  #   progress$set(message = "Getting data...", value = 0)
  #   
  #   df_raw <- get_data_gs(gs = rval$gating_set,
  #                         sample = pData(rval$gating_set)$name, 
  #                         subset = "root",
  #                         spill = rval$spill,
  #                         Ncells = NULL,
  #                         return_comp_data = FALSE,
  #                         updateProgress = updateProgress)
  #   
  #   rval$df_tsne <- get_data_gs(gs = rval$gating_set,
  #                               sample = pData(rval$gating_set)$name, 
  #                               subset = "root",
  #                               spill = rval$spill,
  #                               Ncells = NULL,
  #                               return_comp_data = TRUE,
  #                               updateProgress = updateProgress)
  #   
  #   progress$set(message = paste("Performing", input$dim_red_method, "..."), value = 0)
  #   
  #   res <- dim_reduction(df = rval$df_tsne,
  #                        yvar = rval$parameters$name[input$tSNE_variables_table_rows_selected], 
  #                        Ncells = input$ncells_tsne, 
  #                        y_trans = y_trans,
  #                        transformation = transformation,
  #                        method = input$dim_red_method,
  #                        perplexity = input$perplexity)
  #   rval$df_tsne <- res$df
  #   
  #   df <- cbind( df_raw[res$keep, ], rval$df_tsne[ , setdiff(names(rval$df_tsne), names(df_raw))])
  #   
  #   rval$dim_red_var <- res$vars
  #   
  #   if(!is.null(rval$df_tsne)){
  #     rval$flow_set_tsne <- build_flowset_from_df(df = df, fs = rval$flow_set)
  #     rval$flow_set_names <- unique(c(rval$flow_set_names, "dim-reduction"))
  #     rval$flow_set_selected <- "dim-reduction"
  #     
  #     #updateSelectInput(session, "flow_set", choices = rval$flow_set_names, selected = "dim-reduction")
  #   }
  #   
  #   
  # })
  
  ##########################################################################################################
  # Observe functions for Clustering
  
  
  observeEvent(input$start_clustering, {
    
    validate(
      need(rval$flow_set, "Empty flow set")
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
    
    # Create a Progress object
    progress <- shiny::Progress$new(min = 0, max = 100)
    on.exit(progress$close())
    
    updateProgress <- function(value = NULL, detail = NULL) {
      progress$set(value = value, detail = detail)
    }
    
    transformation <- NULL
    if(input$apply_trans){
      transformation <- rval$transformation
    }
    
    y_trans <- switch(input$y_trans_clustering,
                      "log10" = log10_trans(),
                      "asinh" = asinh_trans(),
                      "identity" = identity_trans(),
                      NULL)
    
    progress$set(message = "Clustering...", value = 0)
    
    df_raw <- get_data_gs(gs = rval$gating_set,
                          sample = pData(rval$gating_set)$name, 
                          subset = "root",
                          spill = rval$spill,
                          return_comp_data = FALSE,
                          Ncells = NULL,
                          updateProgress = updateProgress)
    
    rval$df_cluster <- get_data_gs(gs = rval$gating_set,
                                   sample = pData(rval$gating_set)$name, 
                                   subset = "root",
                                   spill = rval$spill,
                                   return_comp_data = TRUE,
                                   Ncells = NULL,
                                   updateProgress = updateProgress)
    
    
    
    #print(summary(rval$df_tsne))
    
    progress$set(message = "Clustering...", value = 50)
    
    res <- get_cluster(df=rval$df_cluster, 
                       yvar = rval$parameters$name[input$clustering_variables_table_rows_selected],
                       y_trans = y_trans,
                       transformation = transformation,
                       dc = input$cluster_dc, 
                       alpha = input$cluster_alpha
    )
    rval$df_cluster <- res$df
    df <- cbind(df_raw[res$keep, ], rval$df_cluster[c("cluster")])
    
    rval$flow_set_cluster <- build_flowset_from_df(df, fs = rval$flow_set)
    
    # delete previous cluster gates
    
    idx_cluster_gates <- grep("^/cluster[0-9]+", names(rval$gates_flowCore))
    
    if(length(idx_cluster_gates)>0){
      rval$gates_flowCore <- rval$gates_flowCore[-idx_cluster_gates]
    }
    
    # create one gate per cluster
    
    uclust <- unique(rval$df_cluster$cluster)
    uclust <- uclust[ order(as.numeric(uclust), decreasing = FALSE) ]
    
    for(i in 1:length(uclust)){
      filterID <- paste("cluster", uclust[i], sep = "")
      polygon <- matrix(c(as.numeric(uclust[i])-0.25, 
                          as.numeric(uclust[i])+0.25, 
                          range(rval$df_cluster[[rval$flow_set_cluster@colnames[1]]])
      ), 
      ncol = 2)
      row.names(polygon) <- c("min", "max")
      colnames(polygon) <- c("cluster", rval$flow_set_cluster@colnames[1])
      g <- rectangleGate(.gate = polygon, filterId=filterID)
      rval$gates_flowCore[[paste("/",filterID, sep="")]] <- list(gate = g, parent = "root")
    }
    
    rval$flow_set_names <- unique(c(rval$flow_set_names, "cluster"))
    rval$flow_set_selected <- "cluster"
    
    #updateSelectInput(session, "flow_set", choices = rval$flow_set_names, selected = "cluster")
    
  })
  
  ##########################################################################################################
  ##########################################################################################################
  # Output 
  
  
  
  
  ##########################################################################################################
  # Output Tables
  
  # output$spill_imported <- DT::renderDataTable({
  #   validate(
  #     need(rval$df_spill_imported, "No spillover data imported")
  #   )
  #   as.data.frame(rval$df_spill_imported)
  # })
  
  # output$meta <- DT::renderDataTable({
  #   validate(
  #     need(rval$df_meta_imported, "No meta data imported")
  #   )
  #   as.data.frame(rval$df_meta_imported)
  # })
  
  # output$files_table <- DT::renderDataTable({
  #   validate(
  #     need(rval$df_files, "Please select a file to import")
  #   )
  #   df <- rval$df_files[ ,c("name", "size")]
  #   df$new_name <- basename(rval$df_files$datapath)
  #   df$dir_name <- dirname(rval$df_files$datapath)
  #   df
  # })
  
  # output$parameters_table <- DT::renderDataTable({
  #   
  #   validate(
  #     need(rval$parameters, "No data imported")
  #   )
  #   df <- rval$parameters
  #   df$minRange <- format(df$minRange, digits = 2)
  #   df$maxRange <- format(df$maxRange, digits = 2)
  #   df[["chanel_name"]] <- df$name_long
  #   DT::datatable(
  #     df[, c("chanel_name", "transform", "transform parameters", "display", "range", "minRange", "maxRange", "name", "desc")], 
  #     rownames = FALSE)
  #   
  # })
  
  
  # output$tSNE_variables_table <- DT::renderDataTable({
  #   
  #   validate(
  #     need(rval$parameters, "No data imported")
  #   )
  #   
  #   df <- rval$parameters
  #   df[["chanel_name"]] <- df$name_long
  #   
  #   DT::datatable(
  #     df[, c("chanel_name", "transform", "transform parameters")], 
  #     rownames = FALSE)
  # })
  
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
  
  # output$pData <- DT::renderDataTable({
  #   if(!is.null(rval$flow_set)){
  #     DT::datatable(rval$pdata, rownames = FALSE)
  #   }
  # })
  # 
  
  # output$files_selection_table <- DT::renderDataTable({
  #   if(!is.null(rval$flow_set)){
  #     data.frame("name" = rval$pdata$name, row.names = NULL)
  #   }
  # })
  
  output$samples_stat <- DT::renderDataTable({
    if(!is.null(rval$flow_set)){
      data.frame("name" = rval$pdata$name, row.names = NULL)
    }
  })
  
  # output$sub_sample_table <- DT::renderDataTable({
  #   if(!is.null(rval$flow_set)){
  #     data.frame("name" = rval$pdata$name, row.names = NULL)
  #   }
  # })
  
  # output$spill_table <- DT::renderDataTable({
  #   
  #   validate(
  #     need(rval$df_spill, "No spillover matrix")
  #   )
  #   
  #   df <- rval$df_spill
  #   format(df, digits =3)
  # })
  
  # output$message <- renderText({
  #   paste("You have loaded", length(rval$flow_set), "items")
  # })
  
  
  ##########################################################################################################
  # Output value boxes
  
  output$progressBox <- renderValueBox({
    valueBox(
      length(rval$flow_set), "samples",icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$progressBox2 <- renderValueBox({
    ngates <- 0
    if(!is.null(rval$gating_set)){
      ngates <- length(setdiff(getNodes(rval$gating_set), "root"))
    }
    
    valueBox(
      ngates, "gates", icon = icon("list"),
      color = "yellow"
    )
  })
  
  output$progressBox3 <- renderValueBox({
    ncells <- 0
    if(!is.null(rval$Ncells_tot)){
      ncells <- rval$Ncells_tot
    }
    valueBox(
      format(ncells, digits = 2), "cells", icon = icon("list"),
      color = "green"
    )
  })
  
  output$progressBox4 <- renderValueBox({
    nparams <- 0
    if(!is.null(rval$flow_set)){
      nparams <- length(rval$flow_set@colnames)
    }
    
    valueBox(
      nparams, "parameters",icon = icon("list"),
      color = "red"
    )
  })
  
  # output$progressBoxSub <- renderValueBox({
  #   valueBox(
  #     length(rval$flow_set_sample), "samples",icon = icon("list"),
  #     color = "purple"
  #   )
  # })
  # 
  # output$progressBoxSub2 <- renderValueBox({
  #   ncells <- 0
  #   if(!is.null(rval$flow_set_sample)){
  #     fs <- rval$flow_set_sample
  #     ncells <- sum( sapply(1:length(fs), function(x){dim(fs[[x]]@exprs)[1]}) )
  #   }
  #   
  #   valueBox(
  #     ncells, "cells", icon = icon("list"),
  #     color = "green"
  #   )
  # })
  
  # output$progressBoxTSNE <- renderValueBox({
  #   valueBox(
  #     length(rval$flow_set_tsne), "samples",icon = icon("list"),
  #     color = "purple"
  #   )
  # })
  # 
  # output$progressBoxTSNE2 <- renderValueBox({
  #   ncells <- 0
  #   if(!is.null(rval$flow_set_tsne)){
  #     fs <- rval$flow_set_tsne
  #     ncells <- sum( sapply(1:length(fs), function(x){dim(fs[[x]]@exprs)[1]}) )
  #   }
  #   
  #   valueBox(
  #     ncells, "cells", icon = icon("list"),
  #     color = "green"
  #   )
  # })
  
  
  ##########################################################################################################
  # Output messages
  
  # output$summary_sub_sample <- renderPrint({
  #   if(!is.null(rval$df_sample)){
  #     print(summary(rval$df_sample[, c("name", "subset")]))
  #   }else{
  #     "No sub-sampling performed yet"
  #   }
  # })
  
  # output$summary_tsne <- renderPrint({
  #   if(!is.null(rval$df_tsne)){
  #     print(summary(rval$df_tsne[, c("name", "subset")]))
  #   }else{
  #     "No t-SNE performed yet"
  #   }
  # })
  
  output$summary_cluster <- renderPrint({
    if(!is.null(rval$df_cluster)){
      print(paste("Number of unique clusters :", length(unique(rval$df_cluster$cluster))))
    }else{
      "No clustering performed yet"
    }
  })
  
  output$message_gate <- renderPrint({
    print(gate$x)
  })
  
  # output$message_transform <- renderPrint({
  #   if(!is.null(rval$parameters)){
  #     var_show <- rval$parameters$name[ match(input$xvar_show, rval$parameters$name_long) ]
  #     print(rval$transformation[[var_show]])
  #   }
  #   
  # })
  
  
  ##########################################################################################################
  # Output plots
  
  output$flow_set_tree <- renderPlot({
    
  })
  
  output$tree <- renderPlot({
    
    validate(
      need(rval$gating_set, "Empty gating set")
    )
    
    validate(
      need(length(setdiff(getNodes(rval$gating_set), "root"))>0, "No gates in gating set")
    )
    
    p <- plot(rval$gating_set)
    
    renderGraph(p)
    
  })
  
  output$plot_gh <- renderPlot({
    plot_all_gates()
  })
  
  plot_all_gates <- reactive({
    
    validate(
      need(rval$gating_set, "Empty gating set") %then%
        need(length(getNodes(rval$gating_set))>1, "No gates to display") %then%
        need(rval$idx_ff_gate, "Please select a sample")
    )
    
    #cat("sample_selected : \n")
    #print(rval$idx_ff_gate)
    
    axis_labels <- rval$parameters$name_long
    names(axis_labels) <- rval$parameters$name
    
    transformation <- NULL
    if(input$apply_trans){
      transformation <- rval$transformation
    }
    
    data_range <- NULL
    if(input$freeze_limits){
      data_range <- rval$data_range
    }
    
    if(input$color_var_gate %in% rval$parameters$name_long){
      color_var <- rval$parameters$name[match(input$color_var_gate, rval$parameters$name_long)]
    }else{
      color_var <- input$color_var_gate
    }
    
    if(input$plot_type_gate != "histogram"){
      type <- input$plot_type_gate
    }
    
    p <- plot_gh(df = rval$df_tot,
                 gs = rval$gating_set,
                 sample = rval$pdata$name[rval$idx_ff_gate],
                 spill = rval$spill,
                 transformation = transformation,
                 bins = input$bin_number_gate,
                 color_var = color_var,
                 facet_vars = NULL,
                 axis_labels = axis_labels,
                 data_range = data_range,
                 type = type,
                 alpha = input$alpha_gate,
                 size = input$size_gate,
                 show.legend = FALSE)
    
    n <- length(p)
    
    ncol <- ceiling(n/input$nrow)
    g <- marrangeGrob(p, nrow = input$nrow, ncol = ncol, top = input$sample_selected)
    
    # if(n>2){
    #   ncol <- n%/%input$nrow + n%%input$nrow
    #   ncol <- ceiling(n/input$nrow)
    #   g <- marrangeGrob(p, nrow = input$nrow, ncol = ncol, top = input$sample_selected)
    # }else{
    #   g <- marrangeGrob(p, nrow = 1, ncol = n  , top = input$sample_selected)
    # }
    
    g
    
  })
  
  # output$plot_trans <- renderPlot({
  # 
  #   validate(
  #     need(rval$gating_set, "Empty gating set") %then%
  #       need(input$sample_selected_trans, "Please select a sample") %then%
  #       need(input$gate_trans, "Please select a subset")
  #   )
  # 
  #   idx_x <- match(input$xvar_trans, rval$parameters$name_long)
  #   idx_y <- match(input$yvar_trans, rval$parameters$name_long)
  #   xvar <- rval$parameters$name[idx_x]
  #   yvar <- rval$parameters$name[idx_y]
  # 
  #   if(input$color_var_trans %in% rval$parameters$name_long){
  #     color_var <- rval$parameters$name[match(input$color_var_trans, rval$parameters$name_long)]
  #   }else{
  #     color_var <- input$color_var
  #   }
  # 
  #   #color_var <- rval$parameters$name[match(input$color_var_gate, rval$parameters$name_long)]
  #   #color_var <- input$color_var
  # 
  #   axis_labels <- rval$parameters$name_long
  #   names(axis_labels) <- rval$parameters$name
  # 
  #   transformation <- NULL
  #   if(input$apply_trans){
  #     transformation <- rval$transformation
  #   }
  # 
  #   p <- plot_gs(df = rval$df_tot,
  #                gs = rval$gating_set,
  #                sample = input$sample_selected_trans,
  #                subset = input$gate_trans,
  #                spill = rval$spill,
  #                xvar = xvar,
  #                yvar = yvar,
  #                color_var = color_var,
  #                gate = NULL,
  #                type = input$plot_type_trans,
  #                bins = input$bin_number_trans,
  #                alpha = input$alpha_trans,
  #                size = input$size_trans,
  #                norm_density = input$norm_trans,
  #                smooth = input$smooth_trans,
  #                transformation =  transformation,
  #                show.legend = input$legend_trans,
  #                axis_labels = axis_labels)
  # 
  #   if(!is.null(p)){
  #     p <- p + xlab(input$xvar_trans)
  #     if(input$plot_type_trans != "histogram"){
  #       p <- p + ylab(input$yvar_trans)
  #     }
  #   }
  # 
  #   p
  # 
  # })
  
  
  # update_data_plot_focus <- eventReactive(input$update_plot, {
  #   data_plot_focus()
  # })
  # 
  # data_plot_focus <- reactive({
  #   validate(
  #     need(rval$gating_set, "Empty gating set") %then%
  #       need(input$files_selection_table_rows_selected, "Please select samples") %then%
  #       need(input$gate, "Please select subsets")
  #   )
  #   
  #   print("get data plot_focus")
  #   df <- get_data_gs(gs = rval$gating_set, 
  #                     sample = rval$pdata$name[input$files_selection_table_rows_selected],
  #                     subset = input$gate, 
  #                     spill = rval$spill)
  #   return(df)
  #   
  # })
  # 
  # 
  # 
  # output$plot_focus <- renderPlot({
  #   plot_focus()
  # })
  # 
  # 
  # 
  # 
  # plot_focus <- eventReactive(input$update_plot, {
  #   
  #   # validate(
  #   #   need(rval$gating_set, "Empty gating set") %then%
  #   #     need(input$files_selection_table_rows_selected, "Please select samples") %then%
  #   #     need(input$gate, "Please select subsets")
  #   # )
  #   
  #   
  #   idx_x <- match(input$xvar, rval$parameters$name_long)
  #   idx_y <- match(input$yvar, rval$parameters$name_long)
  #   xvar <- rval$parameters$name[idx_x]
  #   yvar <- rval$parameters$name[idx_y]
  #   
  #   color_var <- input$color_var
  #   if(!is.null(input$color_var)){
  #     
  #     for(i in 1:length(input$color_var)){
  #       if(input$color_var[i] %in% rval$parameters$name_long){
  #         color_var[i] <- rval$parameters$name[match(input$color_var[i], rval$parameters$name_long)]
  #       }else{
  #         color_var[i] <- input$color_var[i]
  #       }
  #     }
  #   }
  #   
  #   
  #   
  #   #color_var <- rval$parameters$name[match(input$color_var_gate, rval$parameters$name_long)]
  #   #color_var <- input$color_var
  #   
  #   axis_labels <- rval$parameters$name_long
  #   names(axis_labels) <- rval$parameters$name
  #   
  #   transformation <- NULL
  #   if(input$apply_trans){
  #     transformation <- rval$transformation
  #   }
  #   
  #   plist <- list()
  #   
  #   split_var <- switch(input$split_variable, 
  #                       "x variable" = "xvar",
  #                       "y variable" = "yvar",
  #                       "color variable" = "color_var"
  #   )
  #   
  #   for(i in 1:length(input[[split_var]])){
  #     
  #     color_var_int <- color_var[1]
  #     xvar_int <- xvar[1]
  #     yvar_int <- yvar[1]
  #     
  #     if(split_var == "color_var"){
  #       color_var_int <- color_var[i]
  #     }else if(split_var == "xvar"){
  #       xvar_int <- xvar[i]
  #     }else if(split_var == "yvar"){
  #       yvar_int <- yvar[i]
  #     }
  #     
  #     
  #     p <- plot_gs(df = update_data_plot_focus(),
  #                  gs = rval$gating_set, 
  #                  sample = rval$pdata$name[input$files_selection_table_rows_selected],
  #                  subset = input$gate, 
  #                  spill = rval$spill,
  #                  xvar = xvar_int, 
  #                  yvar = yvar_int, 
  #                  color_var = color_var_int, 
  #                  #gate = NULL, 
  #                  type = input$plot_type, 
  #                  bins = input$bin_number,
  #                  alpha = input$alpha,
  #                  size = input$size,
  #                  norm_density = input$norm,
  #                  smooth = input$smooth,
  #                  ridges = input$ridges,
  #                  transformation =  transformation,
  #                  facet_vars = input$facet_var,
  #                  group_var = input$group_var,
  #                  yridges_var = input$yridges_var,
  #                  show.legend = input$legend,
  #                  axis_labels = axis_labels,
  #                  legend.position = input$legend_pos)
  #     
  #     if(!is.null(p)){
  #       p <- p + xlab(input$xvar) 
  #       if(input$plot_type != "histogram"){
  #         p <- p + ylab(input$yvar)
  #       }
  #     }
  #     
  #     plist[[i]] <- p
  #     
  #   }
  #   
  #   
  #   n <- length(plist)
  #   
  #   nrow <- min(n, input$nrow_split)
  #   ncol <- ceiling(n/nrow)
  #   g <- marrangeGrob(plist, nrow = nrow, ncol = ncol, top = "")
  #   
  #   # if(input$split_direction == "horizontal"){
  #   #   g <- marrangeGrob(plist, nrow = n, ncol = 1)
  #   # }else{
  #   #   g <- marrangeGrob(plist, nrow = 1, ncol = n)
  #   # }
  #   
  #   g
  #   
  # })
  
  output$plotGate <- renderPlot({
    plotGate()
  })
  
  plotGate <- reactive({
    
    validate(
      need(rval$gating_set, "Empty gating set") %then%
        need(rval$idx_ff_gate, "Please select a sample") %then%
        need(rval$gate_focus, "Please select a subset")
    )
    
    #print(input$xvar_gate)
    
    idx_x <- match(input$xvar_gate, rval$parameters$name_long)
    idx_y <- match(input$yvar_gate, rval$parameters$name_long)
    
    xvar <- rval$parameters$name[idx_x]
    yvar <- rval$parameters$name[idx_y]
    
    
    data_range <- NULL
    if(input$freeze_limits){
      data_range <- rval$data_range
    }
    
    #print(rval$data_range)
    if(input$color_var_gate %in% rval$parameters$name_long){
      color_var <- rval$parameters$name[match(input$color_var_gate, rval$parameters$name_long)]
    }else{
      color_var <- input$color_var_gate
    }
    
    #color_var <- input$color_var_gate
    
    axis_labels <- rval$parameters$name_long
    names(axis_labels) <- rval$parameters$name
    
    polygon_gate <- data.frame(x = gate$x, y=gate$y)
    
    transformation <- NULL
    if(input$apply_trans){
      transformation <- rval$transformation
    }
    
    p <- plot_gs(df = rval$df_tot,
                 gs = rval$gating_set, 
                 sample = rval$pdata$name[rval$idx_ff_gate],
                 subset = rval$gate_focus, 
                 spill = rval$spill,
                 xvar = xvar, 
                 yvar = yvar, 
                 color_var = color_var, 
                 data_range = data_range,
                 axis_labels = axis_labels,
                 gate = rval$gate,
                 polygon_gate = polygon_gate,
                 type = input$plot_type_gate, 
                 bins = input$bin_number_gate,
                 alpha = input$alpha_gate,
                 size = input$size_gate,
                 norm_density = input$norm_gate,
                 smooth = input$smooth_gate,
                 transformation = transformation,
                 show.legend = input$legend_gate)
    
    if(!is.null(p)){
      p <- p + xlab(input$xvar_gate)
      if(input$plot_type_gate != "histogram"){
        p <- p + ylab(input$yvar_gate)
      }
    }
    
    
    
    p
    
  })
  
  output$plotStat <- renderPlot({
    plotStat()
  })
  
  plotStat <- eventReactive(input$update_plot_stat, {
    
    validate(
      need(rval$gating_set, "Empty gating set") %then%
        need(input$samples_stat_rows_selected, "Please select a sample") %then%
        need(input$gate_stat, "Please select subsets")
    )
    
    idx_y <- match(input$yvar_stat, rval$parameters$name_long)
    yvar <- rval$parameters$name[idx_y]
    
    transformation <- NULL
    if(input$apply_trans){
      transformation <- rval$transformation
    }
    
    axis_labels <- rval$parameters$name_long
    names(axis_labels) <- rval$parameters$name
    
    y_trans <- switch(input$y_trans,
                      "log10" = log10_trans(),
                      "asinh" = asinh_trans(),
                      "identity" = identity_trans(),
                      NULL)
    
    p <- plot_stat(df = rval$df_tot,
                   gs = rval$gating_set,
                   sample =  rval$pdata$name[input$samples_stat_rows_selected],
                   subset = input$gate_stat,
                   spill = rval$spill,
                   yvar = yvar,
                   type = input$plot_type_stat,
                   transformation = transformation,
                   axis_labels = axis_labels,
                   default_trans = identity_trans(),
                   scale_values = input$scale_values,
                   max_scale = input$max_scale,
                   free_y_scale = input$free_y_scale,
                   color_var = input$color_var_stat, 
                   facet_vars = input$facet_var_stat,
                   group_var = input$group_var_stat,
                   expand_factor = input$expand_factor,
                   stat_function = input$stat_function,
                   show.legend = input$legend_stat,
                   y_trans = y_trans,
                   strip.text.y.angle = input$strip_text_angle)
    
    p                          
    
  })
  
  # output$plot_comp <- renderPlot({
  #   
  #   validate(
  #     need(rval$gating_set, "Empty gating set") %then%
  #       need(input$sample_selected_comp, "Please select a sample") %then%
  #       need(input$gate_comp, "Please select a subset")
  #     
  #   )
  #   
  #   
  #   idx_x <- match(input$xvar_comp, rval$parameters$name_long)
  #   idx_y <- match(input$yvar_comp, rval$parameters$name_long)
  #   yvar <- rval$parameters$name[idx_x]
  #   xvar <- rval$parameters$name[idx_y]
  #   
  #   
  #   if(input$color_var_comp %in% rval$parameters$name_long){
  #     color_var <- rval$parameters$name[match(input$color_var_comp, rval$parameters$name_long)]
  #   }else{
  #     color_var <- input$color_var
  #   }
  #   
  #   #color_var <- rval$parameters$name[match(input$color_var_gate, rval$parameters$name_long)]
  #   #color_var <- input$color_var
  #   
  #   axis_labels <- rval$parameters$name_long
  #   names(axis_labels) <- rval$parameters$name
  #   axis_labels[[xvar]] <- paste(axis_labels[[xvar]], "(fluo)")
  #   axis_labels[[yvar]] <- paste(axis_labels[[yvar]], "(chanel)")
  #   
  #   transformation <- NULL
  #   if(input$apply_trans){
  #     transformation <- rval$transformation
  #   }
  #   
  #   print(rval$spill)
  #   
  #   p <- plot_gs(df = rval$df_tot,
  #                gs = rval$gating_set, 
  #                sample = input$sample_selected_comp,
  #                subset = input$gate_comp, 
  #                spill = rval$spill,
  #                xvar = xvar, 
  #                yvar = yvar, 
  #                color_var = color_var, 
  #                gate = NULL, 
  #                type = input$plot_type_comp, 
  #                bins = input$bin_number_comp,
  #                alpha = input$alpha_comp,
  #                size = input$size_comp,
  #                norm_density = input$norm_comp,
  #                smooth = input$smooth_comp,
  #                transformation =  transformation,
  #                show.legend = input$legend_comp,
  #                axis_labels = axis_labels)
  #   
  #   #p <- p + xlab(paste(input$yvar_comp, "(fluo)")) + ylab(paste(input$xvar_comp, "(chanel)"))
  #   
  #   p
  #   
  # })
  
  
  # output$heatmap_spill <- renderPlotly({
  #   
  #   validate(
  #     need(rval$df_spill, "No spillover matrix")
  #   )
  #   
  #   df <- rval$df_spill
  #   df[df == 0] <- NA
  #   df_log <- log10(df)
  #   p <- heatmaply(df,
  #                  #colors = c(rgb(1,1,1), rgb(1,0,0)),
  #                  #colors= viridis,
  #                  plot_method="ggplot",
  #                  scale_fill_gradient_fun = scale_fill_viridis(trans = log10_trans(), name = "spillover"),
  #                  Rowv = NULL,
  #                  Colv = NULL,
  #                  column_text_angle = 90,
  #                  xlab = "detection chanel",
  #                  ylab = "emitting fluorophore",
  #                  fontsize_row = 6,
  #                  fontsize_col = 6,
  #                  cellnote_size = 6,
  #                  hide_colorbar = TRUE,
  #                  main = "spillover matrix",
  #                  margins = c(50, 50, 50, 0)
  #   )
  #   p$x$source <- "select_heatmap"
  #   p
  # })
  
  ##########################################################################################################
  #Output Download functions
  
  # output$download_plot <- downloadHandler(
  #   filename = "plot.pdf",
  #   content = function(file) {
  #     pdf(file, width = input$width_plot, height = input$height_plot)
  #     print(plot_focus())
  #     dev.off()
  #   }
  # )
  
  output$download_plot_stat <- downloadHandler(
    filename = "stat.pdf",
    content = function(file) {
      pdf(file, width = input$width_plot_stat, height = input$height_plot_stat)
      print(plotStat())
      dev.off()
    }
  )
  
  output$download_plot_gh <- downloadHandler(
    filename = paste("gates_", input$sample_selected, ".pdf", sep = ""),
    content = function(file) {
      pdf(file, width = input$width_plot_gh, height = input$height_plot_gh)
      print(plot_all_gates())
      dev.off()
    }
  )
  
  # output$download_spill <- downloadHandler(
  #   filename = "spillover_matrix.txt",
  #   content = function(file) {
  #     write.table(rval$df_spill, file = file, row.names = FALSE, quote = FALSE, sep = "\t")
  #   }
  # )
  
  output$export_gating_set <- downloadHandler(
    
    filename = function(){
      switch(input$export_format,
             "FlowJo" = "workspace_flowJo.wsp",
             "Cytobank" = "workspace_cytobank.xml")
    },
    
    content = function(file) {
      print(input$export_format)
      gs <- GatingSet(rval$flow_set)
      
      ####################################################
      #transform
      if(input$export_format == "FlowJo"){
        trans.def <- trans_new("flowJo_linear", 
                               transform = function(x){x}, 
                               inverse = function(x){x})
      }else if(input$export_format == "Cytobank"){
        trans.def <- asinhtGml2_trans()
      }
      
      
      trans_list <- rval$transformation
      
      for(i in 1:length(trans_list)){
        trans_list[[i]] <- trans.def
      }
      
      trans <- transformerList(colnames(gs), trans_list)
      
      gs <- transform(gs, trans)
      print(gs@transformation)
      
      ####################################################
      #compensate
      
      if(input$apply_comp & !is.null(rval$df_spill)){
        comp <- rval$df_spill
      }else{
        comp <- diag( length(rval$flow_set@colnames) )
        colnames(comp) <- rval$flow_set@colnames
        row.names(comp) <- colnames(comp)
      }
      comp <- compensation(comp)
      gs <- compensate(gs, comp)
      print(gs@compensation)
      
      
      ####################################################
      #add gates
      
      # print(getNodes(gs))
      gates <- transform_gates(gates = rval$gates_flowCore, 
                               transformation = trans_list,
                               pattern = "", 
                               replacement = "",
                               time_step = 1/rval$time_step
      )
      
      gs <- add_gates_flowCore(gs, gates)
      
      g <- getGate(gs, "/live")
      print(g[[1]]@boundaries)
      
      #gs <- rval$gating_set
      
      
      if(input$export_format == "FlowJo"){
        CytoML::GatingSet2flowJo(gs = gs, outFile = file)
      }else if(input$export_format == "Cytobank"){
        CytoML::GatingSet2cytobank(gs = gs, outFile = file, cytobank.default.scale = FALSE)
      }
      
    }
  )
  
  ##########################################################################################################
  #Dynamic ui output
  
  # output$filter_meta <- renderUI({
  #   
  #   validate(
  #     need(rval$pdata, "No meta data")
  #   )
  #   
  #   x <- list()
  #   
  #   for(meta_var in names(rval$pdata)){
  #     uvar <- unique(rval$pdata[[meta_var]])
  #     
  #     x[[meta_var]] <- selectizeInput(meta_var, meta_var, 
  #                                     choices = uvar, 
  #                                     selected = uvar,
  #                                     multiple = TRUE)
  #     
  #   }
  #   
  #   if(length(x)>0){
  #     x[["apply_filter_meta"]] <- actionButton("apply_filter_meta", "apply filter")
  #   }
  #   
  #   tagList(x)
  #   
  # })
  
  # plot_height <- eventReactive(input$update_plot,{
  #   split_var <- switch(input$split_variable, 
  #                       "x variable" = "xvar",
  #                       "y variable" = "yvar",
  #                       "color variable" = "color_var"
  #   )
  #   
  #   min(input$nrow_split, length(input[[split_var]])) * input$row_size + 50
  #   
  # })
  # 
  # output$ui_plot <- renderUI({
  #   if(input$update_plot){
  #     # split_var <- switch(input$split_variable, 
  #     #                     "x variable" = "xvar",
  #     #                     "y variable" = "yvar",
  #     #                     "color variable" = "color_var"
  #     # )
  #     plotOutput("plot_focus", height = plot_height())
  #   }
  #   
  #   
  # })
  
  plot_height_stat <- eventReactive(input$update_plot_stat,{
    
    length(input$yvar_stat) * input$row_size_stat + 50
    
  })
  
  output$ui_plot_stat <- renderUI({
    
    plotOutput("plotStat", height = plot_height_stat())
    
  })
  
  # output$ui_compute_spill <- renderUI({
  #   
  #   validate(
  #     need(rval$flow_set, "No flow set available")
  #   )
  #   
  #   tagList(
  #     selectInput("fluo", 
  #                 "fluorophore",
  #                 choices = rval$flow_set@colnames,
  #                 selected = rval$flow_set@colnames[1]),
  #     
  #     selectInput("sample_pos",
  #                 "sample pos",
  #                 choices = pData(rval$flow_set)$name,
  #                 selected = pData(rval$flow_set)$name[1]),
  #     
  #     selectInput("gate_pos",
  #                 "gate pos",
  #                 choices = getNodes(rval$gating_set),
  #                 selected = "root"),
  #     
  #     selectInput("sample_neg", 
  #                 "sample neg",
  #                 choices = pData(rval$flow_set)$name,
  #                 selected = pData(rval$flow_set)$name[1]),
  #     
  #     selectInput("gate_neg",
  #                 "gate neg",
  #                 choices = getNodes(rval$gating_set),
  #                 selected = "root")
  #   )
  #   
  #   # x <- list()
  #   # 
  #   # for(meta_var in names(rval$pdata)){
  #   #   uvar <- unique(rval$pdata[[meta_var]])
  #   #   
  #   #   x[[meta_var]] <- selectizeInput(meta_var, meta_var, 
  #   #                                   choices = uvar, 
  #   #                                   selected = uvar,
  #   #                                   multiple = TRUE)
  #   #   
  #   # }
  #   # 
  #   # if(length(x)>0){
  #   #   x[["apply_filter_meta"]] <- actionButton("apply_filter_meta", "apply filter")
  #   # }
  #   # 
  #   # tagList(x)
  #   
  # })
  
  
}