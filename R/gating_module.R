#' @title   gatingUI and gating
#' @description  A shiny Module that deals with gating
#' @param id shiny id
#' @import shiny
#' @importFrom shinydashboard box tabBox
#' @importFrom DT DTOutput
gatingUI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(width = 4,
           box(width = NULL, height = NULL, title = "Parameters", collapsible = TRUE, collapsed = FALSE,
               actionButton(ns("show_gate"), label = "Show defining gate"),
               br(),
               br(),
               plotGatingSet2Input(id = ns("plot_module"), simple_plot = TRUE)
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
                                       label = "Select gate", 
                                       choices = NULL, 
                                       selected = NULL),
                           actionButton(ns("delete_gate"), "Delete")
                  ),
                  tabPanel("Rename",
                           selectInput(ns("gate_to_rename"), 
                                       label = "Select gate", 
                                       choices = NULL, 
                                       selected = NULL),
                           textInput(ns("new_name"), label = "Enter new name", value = ""),
                           actionButton(ns("rename_gate"), "rename gate")
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
                  )
           ),
           tabBox(title = "Gating hierarchy",
                  width = NULL, height = NULL,
                  tabPanel("Tree",
                           checkboxInput(ns("show_all_subsets"), "Show all subsets (including clusters and bins)", FALSE),
                           plotOutput(ns("tree"))
                  ),
                  tabPanel(title = "Gates",
                           simpleDisplayUI(ns("simple_display_module_2"), nrow = 2, size = 200)
                  ),
                  tabPanel(title = "Stats",
                           br(),
                           downloadButton(ns("download_data")),
                           br(),
                           br(),
                           div(style = 'overflow-x: scroll', DT::DTOutput(ns("pop_stats"))),
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
#' @param rval A reactive values object
#' @return The updated reactiveValues object \code{rval}
#' @import shiny
#' @importFrom flowWorkspace gs_pop_add Rm setNode recompute gs_get_pop_paths
#' @importFrom flowCore polygonGate
#' @importFrom graph addEdge
#' @importFrom Rgraphviz renderGraph layoutGraph
#' @importFrom methods new
#' @importFrom grDevices chull
#' @importFrom DT datatable renderDT
#' @importFrom dplyr rename
#' @importFrom utils write.table
#' @rdname gatingUI
gating <- function(input, output, session, rval) {
  
  plot_params <- reactiveValues() # parameters controlling the main plot
  plot_params_gh <- reactiveValues() # parameters controlling the gating hierarchy plot
  gate <- reactiveValues() # polygon gate represented on plot
  rval_mod <- reactiveValues(init = TRUE) # a local (module specific) reactiveValues object
  
  #Plot initialization
  observe({
    
    validate(need(rval$plot_var, "No plotting parameters"))
    validate(need(rval$pdata, "No metadata available"))
    
    if(rval_mod$init){
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
      
      plot_params$samples <- rval$pdata$name[1]
      plot_params$gate <- "root"
      plot_params$xvar <- xvar
      plot_params$yvar <- yvar
      plot_params$plot_type <- "hexagonal"
      plot_params$color_var <- NULL
      plot_params$use_all_cells <- FALSE
      rval_mod$init <- FALSE
    }

  })
  
  # Calling other modules
  res <- callModule(plotGatingSet2, "plot_module", rval, plot_params, 
                    simple_plot = TRUE, show_gates = TRUE, polygon_gate = gate)
  res_display <- callModule(simpleDisplay, "simple_display_module", res$plot)
  
  plot_all_gates <- callModule(plotGatingHierarchy, "plot_hierarchy_module", rval, plot_params = plot_params_gh)
  callModule(simpleDisplay, "simple_display_module_2", plot_all_gates)
  
  #Passing plot parameters to the gating hierarchy plot
  observe({
    for(var in setdiff(names(res$params), c("xvar",
                                            "yvar",
                                            "group_var",
                                            "facet_var",
                                            "norm", 
                                            "smooth", 
                                            "ridges", 
                                            "yridges_var",
                                            "show_label")) ){
      plot_params_gh[[var]] <- res$params[[var]]
    }
  })
  
  
  ##########################################################################################################
  # Observe functions for gating

  #update polygon coordinates upon mouse click on main plot
  observeEvent(res_display$params$plot_click, {
    
    xvar <- rval$parameters$name[match(res$params$xvar, rval$parameters$name_long)]
    yvar <- rval$parameters$name[match(res$params$yvar, rval$parameters$name_long)]
    
    gate$x <- c(gate$x, rval$transformation[[xvar]]$inverse(res_display$params$plot_click$x))
    gate$y <- c(gate$y, rval$transformation[[yvar]]$inverse(res_display$params$plot_click$y))
    
    idx <- grDevices::chull(gate$x, gate$y)
    
    gate$x <- gate$x[idx]
    gate$y <- gate$y[idx]
    
  })
  
  #update polygon coordinates upon mouse brush on main plot
  observeEvent(res_display$params$plot_brush, {
    brush <- res_display$params$plot_brush
    
    if (!is.null(brush)) {
      xvar <- rval$parameters$name[match(res$params$xvar, rval$parameters$name_long)]
      yvar <- rval$parameters$name[match(res$params$yvar, rval$parameters$name_long)]
      
      x <- try(rval$transformation[[xvar]]$inverse(c(brush$xmin, brush$xmax, brush$xmax, brush$xmin)), silent = TRUE)
      y <- try(rval$transformation[[yvar]]$inverse(c(brush$ymin, brush$ymin, brush$ymax, brush$ymax)), silent = TRUE)
      
      if(class(x) != "try-error"){
        gate$x <- x
      }
      if(class(y) != "try-error"){
        gate$y <- y
      }
    }
    
  })
  
  #reset polygon upon mouse double click on main plot
  observeEvent(res_display$params$plot_dblclick, {
    gate$x <- NULL
    gate$y <- NULL
  })
  
  #reset polygon using action button
  observeEvent(input$reset_gate, {
    gate$x <- NULL
    gate$y <- NULL
    rval$gate <- NULL
  })
  
  #reset polygon when plot x axis is modified
  observeEvent(res$params$xvar, {
    gate$x <- NULL
    gate$y <- NULL
  })
  
  #reset polygon when plot y axis is modified
  observeEvent(res$params$yvar, {
    gate$x <- NULL
    gate$y <- NULL
  })
  
  #Create gate from polygon, update rval$gates_flowCore and ravl$gating_set
  observeEvent(input$create_gate, {
    
    if(input$gate_name %in% basename(flowWorkspace::gs_get_pop_paths(rval$gating_set))){
      showModal(modalDialog(
        title = "Error",
        "Gate name already exists! Please choose another name.",
        easyClose = TRUE,
        footer = NULL
      ))
    }else{
      if(!is.null(gate$x)){
        polygon <- data.frame(x =gate$x, y = gate$y)
        hpts <- grDevices::chull(polygon)
        polygon <- polygon[hpts, ]
        polygon <- as.matrix(polygon)
        
        var_names <- c(rval$parameters$name[match(res$params$xvar, rval$parameters$name_long)],
                       rval$parameters$name[match(res$params$yvar, rval$parameters$name_long)])
        names(var_names)<-NULL
        colnames(polygon) <- var_names
        
        poly_gate <- flowCore::polygonGate(.gate = polygon, filterId=input$gate_name)
        rval$gate <- poly_gate
        
        if(res$params$gate != "root"){
          gate_name <- paste(res$params$gate, "/", input$gate_name, sep = "")
        }else{
          gate_name <- paste("/", input$gate_name, sep = "")
        }
          
        rval$gates_flowCore[[gate_name]] <- list(gate = poly_gate, parent = res$params$gate)
        
        flowWorkspace::gs_pop_add(rval$gating_set, poly_gate, parent = res$params$gate)
        flowWorkspace::recompute(rval$gating_set)
        
        #updateSelectInput(session, "gate_to_delete", choices = setdiff(flowWorkspace::gs_get_pop_paths(rval$gating_set), "root"))

        gate$x <- NULL
        gate$y <- NULL

        #reset plot parameters (only non null parameters will be updated)
        for(var in names(reactiveValuesToList(plot_params))){
          plot_params[[var]] <- NULL
        }
        plot_params$gate <- gate_name
      }
    }
    
  })
  
  #Delete selected gate, update rval$gates_flowCore and rval$gating_set
  observeEvent(input$delete_gate, {
    if(input$gate_to_delete != "root"){
      
      idx_gh <- which( flowWorkspace::gs_get_pop_paths(rval$gating_set) == input$gate_to_delete )
      target_gate <- flowWorkspace::gs_get_pop_paths(rval$gating_set)[idx_gh]
      
      child_gates <- get_all_descendants(rval$gates_flowCore, target_gate)
      #child_gates <- getChildren(rval$gating_set[[1]], target_gate)
      if(length(child_gates)==0){
        child_gates <- NULL
      }
      
      idx_delete <- which( names(rval$gates_flowCore) %in% c(target_gate, child_gates) )
      rval$gates_flowCore <- rval$gates_flowCore[-idx_delete]
      
      flowWorkspace::Rm(target_gate, rval$gating_set)
      flowWorkspace::recompute(rval$gating_set)

      #reset plot parameters (only non null parameters will be updated)
      for(var in names(reactiveValuesToList(plot_params))){
        plot_params[[var]] <- NULL
      }
      plot_params$gate <- "root"
      
    }
    
  })
  
  observeEvent(rval$gates_flowCore, {
    validate(need(rval$gates_flowCore, "no gating set"))
    updateSelectInput(session, "gate_to_delete", choices = names(rval$gates_flowCore))
  })
  
  #Rename selected gate, update rval$gates_flowCore and rval$gating_set
  observeEvent(input$rename_gate, {
    
    if(input$gate_to_rename != "root"){
      
      idx_gh <- which( flowWorkspace::gs_get_pop_paths(rval$gating_set) == input$gate_to_rename )
      target_gate <- flowWorkspace::gs_get_pop_paths(rval$gating_set)[idx_gh]

      flowWorkspace::setNode(rval$gating_set, target_gate, input$new_name)

      idx <- which( names(rval$gates_flowCore) == input$gate_to_rename )

      parent_name <- dirname(input$gate_to_rename)
      if(parent_name == "/"){
        parent_name <- ""
      }


      names(rval$gates_flowCore)[idx] <- paste(parent_name, input$new_name, sep = "/")
      rval$gates_flowCore[[idx]]$gate@filterId <- input$new_name

      child_gates <- get_all_descendants(rval$gates_flowCore, input$gate_to_rename)
      new_name <- paste(parent_name, input$new_name, sep = "/")
      for(child in child_gates){
        rval$gates_flowCore[[child]]$parent <- gsub(input$gate_to_rename, new_name, rval$gates_flowCore[[child]]$parent, fixed = TRUE)
        idx <- which(names(rval$gates_flowCore) == child)
        names(rval$gates_flowCore)[idx] <- gsub(input$gate_to_rename, new_name, names(rval$gates_flowCore)[idx], fixed = TRUE)
      }
      
    }
    
  })

  observeEvent(rval$gates_flowCore, {
    validate(need(rval$gates_flowCore, "no gating set"))
    updateSelectInput(session, "gate_to_rename", choices = names(rval$gates_flowCore))
  })
  
  #Update plot parameters to show defining gate
  observeEvent(input$show_gate, {
    
    print(res$params$gate)
    
    if(res$params$gate != "root"){

      rval$gate <- rval$gates_flowCore[[res$params$gate]]$gate
      gate_params <- names(rval$gate@parameters)

      params <- rval$parameters$name_long[match(gate_params, rval$parameters$name)]
      
      #reset plot parameters (only non null parameters will be updated)
      for(var in names(reactiveValuesToList(plot_params))){
        plot_params[[var]] <- NULL
      }
      
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
    gates <- rval$gates_flowCore
    validate(need(names(gates), "Empty gating set"))
    
    if(!input$show_all_subsets){
      idx_cluster <- grep("^cluster[0-9]+", basename(names(gates)))
      idx_bins <- grep("^bin[0-9]+", basename(names(gates)))
      idx_hide <- union(idx_cluster, idx_bins)
      if(length(idx_hide)>0){
        gates <- gates[-idx_hide]
      }
      
      
    }
    
    plot_params$selected_subsets <- names(gates)
    
    gR = methods::new("graphNEL", nodes = union("root", names(gates)), edgemode = "directed")

    for(i in 1:length(gates)){
      if(!is.null(gates[[i]]$parent)){
        gR = graph::addEdge(gates[[i]]$parent,  names(gates)[i], gR)
      }
    }

    nAttrs <- list()
    nodeNames <- basename(graph::nodes(gR))
    names(nodeNames) <- graph::nodes(gR)
    nAttrs$label <- nodeNames
    
    Rgraphviz::renderGraph(Rgraphviz::layoutGraph(gR,
                            nodeAttrs=nAttrs,
                            attrs=list(graph=list(rankdir="LR", page=c(8.5,11)),
                                       node=list(fixedsize = FALSE,
                                                 fillcolor = "gray",
                                                 fontsize = 12,
                                                 shape = "ellipse")
                            )
    )
    )
    
  })
  
  ##########################################################################################################
  #Output Download functions
  
  pop_stats <- reactive({
    validate(need(rval$gating_set, "No gating set available"))
    df <- getPopStatsPlus(rval$gating_set, spill = rval$df_spill)
    df <- df[df$name %in% res$params$samples, ]
    df[['% parent']] <- sprintf("%.1f", df$Count / df$ParentCount * 100)
    df <- df[, c("name", "Population", "Parent", "% parent", "Count", "ParentCount")] 
    df <- dplyr::rename(df, subset = "Population")
    df <- df[df$subset %in% plot_params$selected_subsets, ]
    df
  })
  
  output$pop_stats <- DT::renderDT({
    DT::datatable(pop_stats(), rownames = FALSE)
  })
  
  output$download_data <- downloadHandler(
    filename = "pop_stats.txt",
    content = function(file) {
      utils::write.table(pop_stats(), file = file, row.names = FALSE, quote = FALSE, sep = "\t")
    }
  )
  
  return(rval)
  
  
}