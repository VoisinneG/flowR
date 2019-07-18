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
  
  # Clustering module
  rval <- callModule(cluster, "cluster_module", rval)
  
  # Display module
  plot_display <- callModule(display, "plot_module", rval, module_server_name = "plotGatingSet", simple_plot = FALSE)
  
  # stat module
  plot_statistics <- callModule(display, "statistics_module", rval, module_server_name = "plotStat")
  
  # save module
  callModule(saveWorkspace, "save_module", rval)
    
  ##########################################################################################################
  # General controls
  
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
  
  
  
  ##########################################################################################################
  ##########################################################################################################
  # Output 

  
  # output$samples_stat <- DT::renderDataTable({
  #   if(!is.null(rval$flow_set)){
  #     data.frame("name" = rval$pdata$name, row.names = NULL)
  #   }
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
  
  
  ##########################################################################################################
  # Output messages
  
  
  # output$summary_cluster <- renderPrint({
  #   if(!is.null(rval$df_cluster)){
  #     print(paste("Number of unique clusters :", length(unique(rval$df_cluster$cluster))))
  #   }else{
  #     "No clustering performed yet"
  #   }
  # })
  
  output$message_gate <- renderPrint({
    print(gate$x)
  })
  
  
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
  
  # output$plotStat <- renderPlot({
  #   plotStat()
  # })
  # 
  # plotStat <- eventReactive(input$update_plot_stat, {
  #   
  #   validate(
  #     need(rval$gating_set, "Empty gating set") %then%
  #       need(input$samples_stat_rows_selected, "Please select a sample") %then%
  #       need(input$gate_stat, "Please select subsets")
  #   )
  #   
  #   idx_y <- match(input$yvar_stat, rval$parameters$name_long)
  #   yvar <- rval$parameters$name[idx_y]
  #   
  #   transformation <- NULL
  #   if(input$apply_trans){
  #     transformation <- rval$transformation
  #   }
  #   
  #   axis_labels <- rval$parameters$name_long
  #   names(axis_labels) <- rval$parameters$name
  #   
  #   y_trans <- switch(input$y_trans,
  #                     "log10" = log10_trans(),
  #                     "asinh" = asinh_trans(),
  #                     "identity" = identity_trans(),
  #                     NULL)
  #   
  #   p <- plot_stat(df = rval$df_tot,
  #                  gs = rval$gating_set,
  #                  sample =  rval$pdata$name[input$samples_stat_rows_selected],
  #                  subset = input$gate_stat,
  #                  spill = rval$spill,
  #                  yvar = yvar,
  #                  type = input$plot_type_stat,
  #                  transformation = transformation,
  #                  axis_labels = axis_labels,
  #                  default_trans = identity_trans(),
  #                  scale_values = input$scale_values,
  #                  max_scale = input$max_scale,
  #                  free_y_scale = input$free_y_scale,
  #                  color_var = input$color_var_stat, 
  #                  facet_vars = input$facet_var_stat,
  #                  group_var = input$group_var_stat,
  #                  expand_factor = input$expand_factor,
  #                  stat_function = input$stat_function,
  #                  show.legend = input$legend_stat,
  #                  y_trans = y_trans,
  #                  strip.text.y.angle = input$strip_text_angle)
  #   
  #   p                          
  #   
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
  
  # output$export_gating_set <- downloadHandler(
  #   
  #   filename = function(){
  #     switch(input$export_format,
  #            "FlowJo" = "workspace_flowJo.wsp",
  #            "Cytobank" = "workspace_cytobank.xml")
  #   },
  #   
  #   content = function(file) {
  #     print(input$export_format)
  #     gs <- GatingSet(rval$flow_set)
  #     
  #     ####################################################
  #     #transform
  #     if(input$export_format == "FlowJo"){
  #       trans.def <- trans_new("flowJo_linear", 
  #                              transform = function(x){x}, 
  #                              inverse = function(x){x})
  #     }else if(input$export_format == "Cytobank"){
  #       trans.def <- asinhtGml2_trans()
  #     }
  #     
  #     
  #     trans_list <- rval$transformation
  #     
  #     for(i in 1:length(trans_list)){
  #       trans_list[[i]] <- trans.def
  #     }
  #     
  #     trans <- transformerList(colnames(gs), trans_list)
  #     
  #     gs <- transform(gs, trans)
  #     print(gs@transformation)
  #     
  #     ####################################################
  #     #compensate
  #     
  #     if(input$apply_comp & !is.null(rval$df_spill)){
  #       comp <- rval$df_spill
  #     }else{
  #       comp <- diag( length(rval$flow_set@colnames) )
  #       colnames(comp) <- rval$flow_set@colnames
  #       row.names(comp) <- colnames(comp)
  #     }
  #     comp <- compensation(comp)
  #     gs <- compensate(gs, comp)
  #     print(gs@compensation)
  #     
  #     
  #     ####################################################
  #     #add gates
  #     
  #     # print(getNodes(gs))
  #     gates <- transform_gates(gates = rval$gates_flowCore, 
  #                              transformation = trans_list,
  #                              pattern = "", 
  #                              replacement = "",
  #                              time_step = 1/rval$time_step
  #     )
  #     
  #     gs <- add_gates_flowCore(gs, gates)
  #     
  #     g <- getGate(gs, "/live")
  #     print(g[[1]]@boundaries)
  #     
  #     #gs <- rval$gating_set
  #     
  #     
  #     if(input$export_format == "FlowJo"){
  #       CytoML::GatingSet2flowJo(gs = gs, outFile = file)
  #     }else if(input$export_format == "Cytobank"){
  #       CytoML::GatingSet2cytobank(gs = gs, outFile = file, cytobank.default.scale = FALSE)
  #     }
  #     
  #   }
  # )
  
  ##########################################################################################################
  #Dynamic ui output
  
  
  
  
  
  
}