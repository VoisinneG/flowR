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
               actionButton(ns("show_gate"), label = "Show gate"),
               br(),
               br(),
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
                           simpleDisplayUI(ns("simple_display_module"))
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
                  ),
                  tabPanel(title = "Gates",
                           simpleDisplayUI(ns("simple_display_module_2"))
                  ),
                  tabPanel(title = "Stats",
                           br(),
                           div(style = 'overflow-x: scroll', DT::dataTableOutput(ns("pop_stats"))),
                           br()  
                      
                  )
                  
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
  
  observe({
    
    validate(
      need(rval$plot_var, "No plotting parameters")
    )
    
    idx_x <- grep("FSC", rval$plot_var)
    if(length(idx_x)>0){
      xvar <- rval$plot_var[idx_x[1]]
    }else{
      xvar <- rval$plot_var[1]
    }
    
    idx_y <- grep("SSC", rval$plot_var)
    if(length(idx_y)>0){
      yvar <- rval$plot_var[idx_y[1]]
    }else{
      yvar <- rval$plot_var[2]
    }
    
    plot_params$xvar <- xvar
    plot_params$yvar <- yvar
    plot_params$plot_type <- "dots"
    plot_params$color_var <- NULL
    plot_params$gate <- "root"
    
  })
  
  res <- callModule(plotGatingSet, "plot_module", rval, plot_params, simple_plot = TRUE, show_gates = TRUE, polygon_gate = gate)
  res_display <- callModule(simpleDisplay, "simple_display_module", res$plot, gate = gate)
  plot_all_gates <- callModule(plotGatingHierarchy, "plot_hierarchy_module", rval, plot_params = res$params)
  callModule(simpleDisplay, "simple_display_module_2", plot_all_gates)
  
  # callModule(simpleDisplay, "simple_display_module", res$plot())
  
  observe({
    
    for(var in intersect( names(res$params), c("xvar", "yvar", "gate", "samples") )){
      if(!is.null(res$params[[var]])){
        print("res$params[[var]] comp")
        print(res$params[[var]])
        if(res$params[[var]] != "") {
          plot_params[[var]] <- res$params[[var]]
        }
      }else{
        plot_params[[var]] <- res$params[[var]]
      }
    }
    
  })
  
  
  output$plot_gate <- renderPlot({
    res$plot()[[1]]
  })
  
  
  
  
  ##########################################################################################################
  # Observe functions for gating
  
  
  
  
  
  observeEvent(res_display$params$plot_click, {
    
    xvar <- rval$parameters$name[match(res$params$xvar, rval$parameters$name_long)]
    yvar <- rval$parameters$name[match(res$params$yvar, rval$parameters$name_long)]
    
    gate$x <- c(gate$x, rval$transformation[[xvar]]$inverse(res_display$params$plot_click$x))
    gate$y <- c(gate$y, rval$transformation[[yvar]]$inverse(res_display$params$plot_click$y))
    
    idx <- chull(gate$x, gate$y)
    
    gate$x <- gate$x[idx]
    gate$y <- gate$y[idx]
    
  })
  
  observeEvent(res_display$params$plot_brush, {
    brush <- res_display$params$plot_brush
    
    if (!is.null(brush)) {
      
      xvar <- rval$parameters$name[match(res$params$xvar, rval$parameters$name_long)]
      yvar <- rval$parameters$name[match(res$params$yvar, rval$parameters$name_long)]
      
      gate$x <- rval$transformation[[xvar]]$inverse(c(brush$xmin, brush$xmax, brush$xmax, brush$xmin))
      gate$y <- rval$transformation[[yvar]]$inverse(c(brush$ymin, brush$ymin, brush$ymax, brush$ymax))
      
      #session$resetBrush("plot_brush")
      
    }
  })
  
  observeEvent(res_display$params$plot_dblclick, {
    gate$x <- NULL
    gate$y <- NULL
    #cat("dblclick")
    #session$resetBrush("plot_brush")
  })
  
  observeEvent(input$reset_gate, {
    gate$x <- NULL
    gate$y <- NULL
    rval$gate <- NULL
    #session$resetBrush("plot_brush")
  })
  
  observeEvent(res$params$xvar, {
    gate$x <- NULL
    gate$y <- NULL
  })
  
  observeEvent(res$params$yvar, {
    gate$x <- NULL
    gate$y <- NULL
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
                       rval$parameters$name[match(res$params$yvar, rval$parameters$name_long)])
        names(var_names)<-NULL
        colnames(polygon) <- var_names
        
        poly_gate <- polygonGate(.gate = polygon, filterId=input$gate_name)
        rval$gate <- poly_gate
        
        if(res$params$gate != "root"){
          gate_name <- paste(res$params$gate, "/", input$gate_name, sep = "")
        }else{
          gate_name <- paste("/", input$gate_name, sep = "")
        }
          
        rval$gates_flowCore[[gate_name]] <- list(gate = poly_gate, parent = res$params$gate)
        
        print("add1")
        add(rval$gating_set, poly_gate, parent = res$params$gate)
        recompute(rval$gating_set)
        print(getNodes(rval$gating_set))
        print(names(rval$gates_flowCore))
        print(rval$gates_flowCore)
        
        #updateSelectInput(session, "gate_to_delete", choices = setdiff(getNodes(rval$gating_set), "root"))

        gate$x <- NULL
        gate$y <- NULL
        
      }
    }
    
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
      
      
    }
    
  })
  
  observeEvent(rval$gates_flowCore, {
    validate(need(rval$gates_flowCore, "no gating set"))
    updateSelectInput(session, "gate_to_delete", choices = names(rval$gates_flowCore), selected = "root")
  })
  
  observeEvent(input$show_gate, {
    if(res$params$gate != "root"){

      rval$gate <- rval$gates_flowCore[[res$params$gate]]$gate
      gate_params <- names(rval$gate@parameters)

      params <- rval$parameters$name_long[match(gate_params, rval$parameters$name)]


      if(length(params) > 0){
        plot_params$xvar <- params[1]
      }
      if(length(params) > 1){
        plot_params$yvar <- params[2]
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
  
  output$tree <- renderPlot({
    
    validate(
      need(rval$gates_flowCore, "Empty gating set")
    )
    
    print("gates tree")
    print(getNodes(rval$gating_set))
    
    validate(
      need(length(setdiff(getNodes(rval$gating_set), "root"))>0, "No gates in gating set")
    )
    
    p <- plot(rval$gating_set)
    
    renderGraph(p)
    
  })
  
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
  
  output$pop_stats <- DT::renderDataTable({
    validate(need(rval$gating_set, "No gating set available"))
    df <- getPopStats(rval$gating_set)
    df <- df[df$name %in% res$params$samples, ]
    df[['%']] <- format(df$Count / df$ParentCount * 100, digits = 1)
    DT::datatable(
      df[, c("name", "Population", "Parent", "%", "Count", "ParentCount")],
      rownames = FALSE)
  })
  
  return(rval)
  
  
}