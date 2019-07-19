#' @title   gatingUI and gating
#' @description  A shiny Module that deals with metadata
#' @param id shiny id
#' @importFrom shinydashboard box tabBox
#' @import shiny
#' @import DT
gatingUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(
    column(width = 4,
           box(width = NULL, height = NULL, title = "Parameters",
               plotGatingSetInput(id = ns("plot_module"), simple_plot = TRUE)
           ),
           tabBox(title = "Gates",
                  width = NULL, height = NULL,
                  tabPanel("Add",
                           textInput(ns("gate_name"), label = "Enter gate name", value = ""),
                           actionButton(ns("create_gate"), "create gate"),
                           actionButton(ns("reset_gate"), "reset gate")
                  ),
                  tabPanel("Delete",
                           selectInput(ns("gate_to_delete"), 
                                       label = "Delete gate", 
                                       choices = NULL, 
                                       selected = NULL),
                           actionButton(ns("delete_gate"), "Delete")
                  ) 
           ),
           box(title = "Message_gate",
               width = NULL, height = NULL,
               verbatimTextOutput(ns("message_gate"))
           )
    ),
    column(width = 8,
           tabBox(title = "",
                  width = NULL, height = NULL,
                  tabPanel(title = "Plot",
                           plotOutput(ns("plot_gate"),
                                      brush = ns("plot_brush"),
                                      click = ns("plot_click"),
                                      dblclick = ns("plot_dblclick"))
                  ),
                  tabPanel(title = "Save",
                           numericInput(ns("width_plot"), label = "width", value = 5),
                           numericInput(ns("height_plot"), label = "height", value = 5),
                           downloadButton(ns("download_plot"), "Save plot")
                  )
           ),
           tabBox(title = "Gating hierarchy",
                  width = NULL, height = NULL,
                  tabPanel("Tree",
                           plotOutput(ns("tree"))
                  )
                  # tabPanel("Plot gates",
                  #          plotOutput("plot_gh")),
                  # tabPanel("Options",
                  #          numericInput("nrow", label = "Number of rows", value = 2)),
                  # tabPanel("Save",
                  #          numericInput("width_plot_gh", label = "width", value = 7),
                  #          numericInput("height_plot_gh", label = "height", value = 7),
                  #          downloadButton("download_plot_gh", "Save plot")     
                  # )
                  
           )
    )
    
  )
  
}


#' gating server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @return a reactivevalues object with values "flow_set", "parameters" and "gates_flowCore"
#' @import flowWorkspace
#' @import flowCore
#' @import shiny
#' @import DT
#' @export
#' @rdname gatingUI
gating <- function(input, output, session, rval) {
  
  `%then%` <- shiny:::`%OR%`

  plot_params <- reactiveValues()
  gate <- reactiveValues()
  
  # observe({
  #   plot_params$xvar <- input$xvar_comp
  #   plot_params$yvar <- input$yvar_comp
  # })
  
  res <- callModule(plotGatingSet, "plot_module", rval, plot_params, simple_plot = TRUE)
  
  output$plot_gate <- renderPlot({
    res$plot()[[1]]
  })
  

  ##########################################################################################################
  # Observe functions for gating
  
  observeEvent(input$plot_click, {
    
    xvar <- rval$parameters$name[match(res$params$xvar, rval$parameters$name_long)]
    yvar <- rval$parameters$name[match(res$params$yvar, rval$parameters$name_long)]
    
    gate$x <- c(gate$x, rval$transformation[[xvar]]$inverse(input$plot_click$x))
    gate$y <- c(gate$y, rval$transformation[[yvar]]$inverse(input$plot_click$y))
    
  })
  
  observeEvent(input$plot_brush, {
    brush <- input$plot_brush
    
    if (!is.null(brush)) {
      
      xvar <- rval$parameters$name[match(res$params$xvar, rval$parameters$name_long)]
      yvar <- rval$parameters$name[match(res$params$yvar, rval$parameters$name_long)]
      
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
        
        var_names <- c(rval$parameters$name[match(res$params$xvar, rval$parameters$name_long)],
                       rval$parameters$name[match(res$params$xvar, rval$parameters$name_long)])
        names(var_names)<-NULL
        colnames(polygon) <- var_names
        
        poly_gate <- polygonGate(.gate = polygon, filterId=input$gate_name)
        rval$gate <- poly_gate
        rval$gates_flowCore[[input$gate_name]] <- list(gate = poly_gate, parent = res$params$gate)
        
        print("add1")
        add(rval$gating_set, poly_gate, parent = res$params$gate)
        recompute(rval$gating_set)
        
        updateSelectInput(session, "gate_to_delete", choices = setdiff(getNodes(rval$gating_set), "root"))

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
      
      plot_params$gate <- "root"
      updateSelectInput(session, "gate_to_delete", choices = setdiff(getNodes(rval$gating_set), "root"))
      
    }
    
  })
  
  observeEvent(input$show_gate, {
    if(input$gate_selected != "root"){
      
      rval$gate <- rval$gates_flowCore[[res$params$gate]]$gate
      gate_params <- names(rval$gate@parameters)
      
      params <- rval$parameters$name_long[match(gate_params, rval$parameters$name)]

      
      if(length(params) > 0){
        plot_params$xvar <- params[1]
      }
      if(length(params) > 1){
        plot_params$xvar <- params[2]
      }
      
      gate$x <- NULL
      gate$y <- NULL
      
      plot_params$gate <- rval$gates_flowCore[[res$params$gate]]$parent
      
    }
    
    
  })
  
  ##########################################################################################################
  # Output messages
  
  
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
  
  # output$plot_gh <- renderPlot({
  #   plot_all_gates()
  # })
  # 
  # plot_all_gates <- reactive({
  #   
  #   validate(
  #     need(rval$gating_set, "Empty gating set") %then%
  #       need(length(getNodes(rval$gating_set))>1, "No gates to display") %then%
  #       need(rval$idx_ff_gate, "Please select a sample")
  #   )
  #   
  #   #cat("sample_selected : \n")
  #   #print(rval$idx_ff_gate)
  #   
  #   axis_labels <- rval$parameters$name_long
  #   names(axis_labels) <- rval$parameters$name
  #   
  #   transformation <- NULL
  #   if(input$apply_trans){
  #     transformation <- rval$transformation
  #   }
  #   
  #   data_range <- NULL
  #   if(input$freeze_limits){
  #     data_range <- rval$data_range
  #   }
  #   
  #   if(input$color_var_gate %in% rval$parameters$name_long){
  #     color_var <- rval$parameters$name[match(input$color_var_gate, rval$parameters$name_long)]
  #   }else{
  #     color_var <- input$color_var_gate
  #   }
  #   
  #   if(input$plot_type_gate != "histogram"){
  #     type <- input$plot_type_gate
  #   }
  #   
  #   p <- plot_gh(df = NULL,
  #                gs = rval$gating_set,
  #                sample = rval$pdata$name[rval$idx_ff_gate],
  #                spill = rval$spill,
  #                transformation = transformation,
  #                bins = input$bin_number_gate,
  #                color_var = color_var,
  #                facet_vars = NULL,
  #                axis_labels = axis_labels,
  #                data_range = data_range,
  #                type = type,
  #                alpha = input$alpha_gate,
  #                size = input$size_gate,
  #                show.legend = FALSE)
  #   
  #   n <- length(p)
  #   
  #   ncol <- ceiling(n/input$nrow)
  #   g <- marrangeGrob(p, nrow = input$nrow, ncol = ncol, top = input$sample_selected)
  #   
  #   # if(n>2){
  #   #   ncol <- n%/%input$nrow + n%%input$nrow
  #   #   ncol <- ceiling(n/input$nrow)
  #   #   g <- marrangeGrob(p, nrow = input$nrow, ncol = ncol, top = input$sample_selected)
  #   # }else{
  #   #   g <- marrangeGrob(p, nrow = 1, ncol = n  , top = input$sample_selected)
  #   # }
  #   
  #   g
  #   
  # })
  
  
  # output$plotGate <- renderPlot({
  #   plotGate()
  # })
  # 
  # plotGate <- reactive({
  #   
  #   validate(
  #     need(rval$gating_set, "Empty gating set") %then%
  #       need(rval$idx_ff_gate, "Please select a sample") %then%
  #       need(rval$gate_focus, "Please select a subset")
  #   )
  #   
  #   #print(input$xvar_gate)
  #   
  #   idx_x <- match(input$xvar_gate, rval$parameters$name_long)
  #   idx_y <- match(input$yvar_gate, rval$parameters$name_long)
  #   
  #   xvar <- rval$parameters$name[idx_x]
  #   yvar <- rval$parameters$name[idx_y]
  #   
  #   
  #   data_range <- NULL
  #   if(input$freeze_limits){
  #     data_range <- rval$data_range
  #   }
  #   
  #   #print(rval$data_range)
  #   if(input$color_var_gate %in% rval$parameters$name_long){
  #     color_var <- rval$parameters$name[match(input$color_var_gate, rval$parameters$name_long)]
  #   }else{
  #     color_var <- input$color_var_gate
  #   }
  #   
  #   #color_var <- input$color_var_gate
  #   
  #   axis_labels <- rval$parameters$name_long
  #   names(axis_labels) <- rval$parameters$name
  #   
  #   polygon_gate <- data.frame(x = gate$x, y=gate$y)
  #   
  #   transformation <- NULL
  #   if(input$apply_trans){
  #     transformation <- rval$transformation
  #   }
  #   
  #   p <- plot_gs(df = NULL,
  #                gs = rval$gating_set, 
  #                sample = res()$params$samples,
  #                subset = rval$gate_focus, 
  #                spill = rval$spill,
  #                xvar = xvar, 
  #                yvar = yvar, 
  #                color_var = color_var, 
  #                data_range = data_range,
  #                axis_labels = axis_labels,
  #                gate = rval$gate,
  #                polygon_gate = polygon_gate,
  #                type = input$plot_type_gate, 
  #                bins = input$bin_number_gate,
  #                alpha = input$alpha_gate,
  #                size = input$size_gate,
  #                norm_density = input$norm_gate,
  #                smooth = input$smooth_gate,
  #                transformation = transformation,
  #                show.legend = input$legend_gate)
  #   
  #   if(!is.null(p)){
  #     p <- p + xlab(input$xvar_gate)
  #     if(input$plot_type_gate != "histogram"){
  #       p <- p + ylab(input$yvar_gate)
  #     }
  #   }
  #   
  #   
  #   
  #   p
  #   
  # })
  
  ##########################################################################################################
  #Output Download functions
  
  output$download_plot <- downloadHandler(
    filename = paste("plot_gate.pdf", sep = ""),
    content = function(file) {
      pdf(file, width = input$width_plot, height = input$height_plot)
      print(plot_gate())
      dev.off()
    }
  )
  
  
  return(rval)
  
  
}