#' Edit, visualize and show statistics from a gating hierarchy
#' @param id shiny id
#' @import shiny
#' @importFrom shinydashboard box tabBox
#' @importFrom DT DTOutput
#' @export
GatingUI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(width = 4,
           box(width = NULL, height = NULL, title = "Parameters", 
               collapsible = TRUE, collapsed = FALSE,
               actionButton(ns("show_gate"), label = "Show defining gate"),
               br(),
               br(),
               plotGatingSetInput(id = ns("plot_module"))
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
                           checkboxInput(ns("show_all_subsets"), 
                                         "Show all subsets (including clusters and bins)", FALSE),
                           plotOutput(ns("tree"))
                  ),
                  tabPanel(title = "Gates",
                           simpleDisplayUI(ns("simple_display_module_2"))
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


#' Gating module server function
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
#' @export
#' @rdname GatingUI
Gating <- function(input, output, session, rval) {
  
  
  plot_params <- reactiveValues() # parameters controlling the main plot
  plot_params_gh <- reactiveValues() # parameters controlling the gating hierarchy plot
  gate <- reactiveValues(x = NULL, y = NULL) # polygon gate represented on plot
  rval_mod <- reactiveValues() # a local (module specific) reactiveValues object
  
  #Plot initialization
  observe({
    
    if(class(rval$gating_set)=="GatingSet"){
      rval$gates_flowCore <- get_gates_from_gs(rval$gating_set)
      if(!setequal(names(rval$gating_set@transformation), colnames(rval$gating_set))){
        transformation <- lapply(colnames(rval$gating_set), function(x){return(identity_trans())})
        names(transformation) <- colnames(rval$gating_set)
        rval$gating_set@transformation <- transformation
      }else{
        rval$transformation <- rval$gating_set@transformation
      }
    }
      
  })
  
  # Call modules
  res <- callModule(plotGatingSet, "plot_module", 
                    rval=rval, 
                    plot_params=plot_params,
                    simple_plot = TRUE, 
                    show_gates = TRUE,
                    polygon_gate = gate)
  
  res_display <- callModule(simpleDisplay, "simple_display_module", plot_list = res$plot)
  
  plot_all_gates <- callModule(plotGatingHierarchy, "plot_hierarchy_module", 
                               rval = rval, plot_params = plot_params_gh)
  
  callModule(simpleDisplay, "simple_display_module_2", 
             plot_list = plot_all_gates, 
             nrow = 2, size = 200)
  
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
    
    xvar <- res$params$xvar
    yvar <- res$params$yvar
    
    gate$x <- c(gate$x, rval$transformation[[xvar]]$inverse(
      res_display$params$plot_click$x))
    gate$y <- c(gate$y, rval$transformation[[yvar]]$inverse(
      res_display$params$plot_click$y))
    
    idx <- grDevices::chull(gate$x, gate$y)

    gate$x <- gate$x[idx]
    gate$y <- gate$y[idx]
    
  })
  
  #update polygon coordinates upon mouse brush on main plot
  observeEvent(res_display$params$plot_brush, {
    brush <- res_display$params$plot_brush
    
    if (!is.null(brush)) {
      
      xvar <- res$params$xvar
      yvar <- res$params$yvar
      
      x <- try(rval$transformation[[xvar]]$inverse(
        c(brush$xmin, brush$xmax, brush$xmax, brush$xmin)), silent = TRUE)
      
      y <- try(rval$transformation[[yvar]]$inverse(
        c(brush$ymin, brush$ymin, brush$ymax, brush$ymax)), silent = TRUE)
      
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
  
  #Create gate from polygon, update rval$gates_flowCore and rval$gating_set
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
        
        var_names <- c(res$params$xvar, res$params$yvar)
          
        names(var_names)<-NULL
        colnames(polygon) <- var_names
        
        poly_gate <- flowCore::polygonGate(.gate = polygon, filterId=input$gate_name)
        rval$gate <- poly_gate
        
        if(res$params$subset != "root"){
          gate_name <- paste(res$params$subset, "/", input$gate_name, sep = "")
        }else{
          gate_name <- paste("/", input$gate_name, sep = "")
        }
          
        rval$gates_flowCore[[gate_name]] <- list(gate = poly_gate, parent = res$params$subset)
        
        flowWorkspace::gs_pop_add(rval$gating_set, poly_gate, parent = res$params$subset)
        flowWorkspace::recompute(rval$gating_set)
        rval$update_gs <- rval$update_gs + 1
        
        #updateSelectInput(session, "gate_to_delete", choices = setdiff(flowWorkspace::gs_get_pop_paths(rval$gating_set), "root"))

        gate$x <- NULL
        gate$y <- NULL

        #reset plot parameters (only non null parameters will be updated)
        # for(var in names(reactiveValuesToList(plot_params))){
        #  plot_params[[var]] <- NULL
        # }
        
        plot_params$subset <- gate_name
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
      # for(var in names(reactiveValuesToList(plot_params))){
      #  plot_params[[var]] <- NULL
      # }
      
      plot_params$subset <- "root"
      
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
    
    print(res$params$subset)
    
    if(res$params$subset != "root"){

      rval$gate <- rval$gates_flowCore[[res$params$subset]]$gate
      gate_params <- names(rval$gate@parameters)

      #reset plot parameters (only non null parameters will be updated)
      # for(var in names(reactiveValuesToList(plot_params))){
      #  plot_params[[var]] <- NULL
      # }
      
      if(length(gate_params) > 0){
        plot_params$xvar <- gate_params[1]
      }
      if(length(gate_params) > 1){
        plot_params$yvar <- gate_params[2]
      }
      
      gate$x <- NULL
      gate$y <- NULL

      plot_params$subset <- rval$gates_flowCore[[res$params$subset]]$parent
      
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
    validate(need(class(rval$gating_set)=="GatingSet", "No GatingSet available"))
    
    spill <- rval$gating_set@compensation
    if(!is.null(rval$apply_comp)){
      if(!rval$apply_comp){
        spill <- NULL
      }
    }
    
    df <- getPopStatsPlus(rval$gating_set, spill = spill)
    df <- df[df$name %in% res$params$sample, ]
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

##################################################################################
# Tests
##################################################################################

# library(shiny)
# library(shinydashboard)
# library(flowWorkspace)
# library(flowCore)
# library(viridis)
# library(scales)
# library(ggplot2)
# library(ggrepel)
# library(plotly)
# library(ggridges)
# 
# if (interactive()){
# 
#   ui <- dashboardPage(
#     dashboardHeader(title = "plotting2"),
#     sidebar = dashboardSidebar(disable = TRUE),
#     body = dashboardBody(
#       gating2UI("module")
#     )
#   )
# 
#   server <- function(input, output, session) {
# 
#     rval <- reactiveValues()
#     plot_params <- reactiveValues()
# 
#     observe({
#       #utils::data("GvHD", package = "flowCore")
#       #rval$gating_set <- GatingSet(GvHD)
#       gs <- load_gs("./inst/ext/gs")
#       rval$gating_set <- gs
#     })
# 
#     res <- callModule(gating, "module", rval = rval)
# 
#   }
# 
#   shinyApp(ui, server)
# 
# }

