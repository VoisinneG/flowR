#' Edit, visualize and show statistics from a gating hierarchy
#' @param id shiny id
#' @import shiny
#' @importFrom shinydashboard box tabBox
#' @importFrom DT DTOutput
#' @export
#' @examples
#' \dontrun{
#' library(shiny)
#' library(shinydashboard)
#' library(flowWorkspace)
#' library(flowCore)
#' library(viridis)
#' library(scales)
#' library(ggplot2)
#' library(ggrepel)
#' library(plotly)
#' library(ggridges)
#' 
#' if (interactive()){
#' 
#'   ui <- dashboardPage(
#'     dashboardHeader(title = "Gating"),
#'     sidebar = dashboardSidebar(disable = TRUE),
#'     body = dashboardBody(
#'       GatingUI("module")
#'     )
#'   )
#' 
#'   server <- function(input, output, session) {
#' 
#'     rval <- reactiveValues()
#'     observe({
#'       utils::data("GvHD", package = "flowCore")
#'       rval$gating_set <- GatingSet(GvHD)
#'     })
#'     res <- callModule(Gating, "module", rval = rval)
#'   }
#' 
#'   shinyApp(ui, server)
#' 
#' }
#' }
GatingUI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(width = 4,
           box(title = "Gates", width = NULL, height = NULL, collapsible = TRUE, collapsed = FALSE,
               actionButton(ns("show_gate"), label = "Show defining gate"),
               br(),
               br(),
               box(title = "Select",width = NULL, height = NULL, collapsible = TRUE, collapsed = TRUE,
                   selectInput(ns("gate_selection"), 
                               label = "Select gate", 
                               choices = NULL, 
                               selected = NULL),
               ),
               box(title = "Add",width = NULL, height = NULL, collapsible = TRUE, collapsed = TRUE,
                   textInput(ns("gate_name"), label = "Enter gate name", value = ""),
                   actionButton(ns("create_gate"), "create gate"),
                   actionButton(ns("reset_gate"), "reset gate")
               ),
               box(title = "Delete",width = NULL, height = NULL, collapsible = TRUE, collapsed = TRUE,
                   selectInput(ns("gate_to_delete"), 
                               label = "Select gate", 
                               choices = NULL, 
                               selected = NULL),
                   actionButton(ns("delete_gate"), "Delete")
               ),
               box(title = "Rename",width = NULL, height = NULL, collapsible = TRUE, collapsed = TRUE,
                   selectInput(ns("gate_to_rename"), 
                               label = "Select gate", 
                               choices = NULL, 
                               selected = NULL),
                   textInput(ns("new_name"), label = "Enter new name", value = ""),
                   actionButton(ns("rename_gate"), "rename gate")
               )
                  
                   
           ),
           box(width = NULL, height = NULL, title = "Parameters", 
               collapsible = TRUE, collapsed = FALSE,
               plotGatingSetInput(id = ns("plot_module"))
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
                           downloadButton(ns("download_data"), label = "Download Stats"),
                           br(),
                           br(),
                           div(style = 'overflow-x: scroll', DT::DTOutput(ns("pop_stats"))),
                           br() 
                  ),
                  tabPanel(title = "Import/Export",
                           br(),
                           downloadButton(ns("export_gh"), label = "Export Gating Hierarchy"),
                           br(),
                           br(),
                           fileInput(inputId = ns("import_gh"),
                                     label = "Import Gating Hierarchy",
                                     multiple = FALSE),
                           textInput(ns("pattern"), label = "Pattern", value  = "[\\<|\\>]"),
                           textInput(ns("replacement"), label = "Replacement", value  = ""),
                           numericInput(ns("time_step"), label = "Time step", value = 1),
                           verbatimTextOutput(ns("import_gh_summary")),
                           actionButton(ns("apply_gh"), label = "Apply Gating Hierarchy")
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
#' @importFrom flowWorkspace gs_pop_add gs_pop_remove setNode recompute gs_get_pop_paths
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
  display_params <- reactiveValues()
  rval_mod <- reactiveValues()
  
  observe({
    rval$update_gs <- 0
  })
  
  # Call modules
  res <- callModule(plotGatingSet, "plot_module", 
                    rval=rval,
                    plot_params=plot_params,
                    simple_plot = TRUE, 
                    show_gates = TRUE,
                    polygon_gate = gate)
  
  res_display <- callModule(simpleDisplay, "simple_display_module", plot_list = res$plot, size = 350)
  
  plot_all_gates <- callModule(plotGatingHierarchy, "plot_hierarchy_module", 
                               rval = rval, 
                               plot_params = plot_params_gh)
  
  callModule(simpleDisplay, "simple_display_module_2", 
             plot_list = plot_all_gates,
             nrow = 2, size = 200,
             params = display_params)
  
  
  ##########################################################################################################
  # Observe functions
  
  #Passing plot parameters to the gating hierarchy plot
  observe({
    # for(var in setdiff(names(res$params), c("xvar",
    #                                         "yvar",
    #                                         "subset",
    #                                         "group_var",
    #                                         "facet_var",
    #                                         "norm", 
    #                                         "smooth", 
    #                                         "ridges", 
    #                                         "yridges_var",
    #                                         "show_label")) ){
      for(var in intersect(names(res$params), c("sample",
                                              "plot_type",
                                              "auto_focus",
                                              "legend.position",
                                              "theme",
                                              "bins",
                                              "size", 
                                              "alpha", 
                                              "option",
                                              "color_var",
                                              "show_outliers")) ){
      plot_params_gh[[var]] <- res$params[[var]]
      #print(reactiveValuesToList(plot_params_gh))
    }
    display_params$top <- paste(res$params$sample, collapse = " + ")
  })
  
  observe({
    rval$update_gs
    validate(need(class(rval$gating_set)=="GatingSet", "no gating set"))
    gates <- gs_get_pop_paths(rval$gating_set)
    updateSelectInput(session, "gate_to_delete", choices = setdiff(gates, "root"))
    updateSelectInput(session, "gate_to_rename", choices = setdiff(gates, "root"))
    updateSelectInput(session, "gate_selection", choices = gates)
  })
  
  observeEvent(input$gate_selection, {
    #update plot_params
    for(var in names(plot_params)){
      plot_params[[var]] <- res$params[[var]]
    }
    plot_params$subset <- input$gate_selection
  })
  
  observe({
    updateSelectInput(session, "gate_selection", selected = res$params$subset)
  })
  
  ##########################################################################################################
  # Observe functions for gating

  #update polygon coordinates upon mouse click on main plot
  observeEvent(res_display$params$plot_click, {
    
    xvar <- res$params$xvar
    yvar <- res$params$yvar
    
    x_coord <- res_display$params$plot_click$x
    if(xvar %in% names(rval$gating_set@transformation)){
      x_coord <- rval$gating_set@transformation[[xvar]]$inverse(x_coord)
    }
    gate$x <- c(gate$x, x_coord)
    
    y_coord <- res_display$params$plot_click$y
    if(yvar %in% names(rval$gating_set@transformation)){
      y_coord <- rval$gating_set@transformation[[yvar]]$inverse(y_coord)
    }
    gate$y <- c(gate$y, y_coord)

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
      
      x_coord <- c(brush$xmin, brush$xmax, brush$xmax, brush$xmin)
      if(xvar %in% names(rval$gating_set@transformation)){
        x_coord <- rval$gating_set@transformation[[xvar]]$inverse(x_coord)
      }
      gate$x <- x_coord
      
      y_coord <- c(brush$ymin, brush$ymin, brush$ymax, brush$ymax)
      if(yvar %in% names(rval$gating_set@transformation)){
        y_coord <- rval$gating_set@transformation[[yvar]]$inverse(y_coord)
      }
      gate$y <- y_coord
      
      # x <- try(rval$transformation[[xvar]]$inverse(
      #   c(brush$xmin, brush$xmax, brush$xmax, brush$xmin)), silent = TRUE)
      # 
      # y <- try(rval$transformation[[yvar]]$inverse(
      #   c(brush$ymin, brush$ymin, brush$ymax, brush$ymax)), silent = TRUE)
      # 
      # if(class(x) != "try-error"){
      #   gate$x <- x
      # }
      # if(class(y) != "try-error"){
      #   gate$y <- y
      # }
      
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
        names(var_names) <- NULL
        colnames(polygon) <- var_names
        
        poly_gate <- flowCore::polygonGate(.gate = polygon, filterId=input$gate_name)
        rval$gate <- poly_gate
          
        flowWorkspace::gs_pop_add(rval$gating_set, poly_gate, parent = res$params$subset)
        flowWorkspace::recompute(rval$gating_set)
        rval$update_gs <- rval$update_gs + 1
        
        gate$x <- NULL
        gate$y <- NULL

        if(res$params$subset != "root"){
          gate_name <- paste(res$params$subset, "/", input$gate_name, sep = "")
        }else{
          gate_name <- paste("/", input$gate_name, sep = "")
        }
        #update plot_params
        for(var in names(plot_params)){
          plot_params[[var]] <- res$params[[var]]
        }
        plot_params$subset <- gate_name
      }
    }
    
  })
  
  #Delete selected gate, update rval$gates_flowCore and rval$gating_set
  observeEvent(input$delete_gate, {
    if(input$gate_to_delete != "root"){
      
      idx_gh <- which( flowWorkspace::gs_get_pop_paths(rval$gating_set) == input$gate_to_delete )
      target_gate <- flowWorkspace::gs_get_pop_paths(rval$gating_set)[idx_gh]

      flowWorkspace::gs_pop_remove(gs = rval$gating_set, node = target_gate)
      flowWorkspace::recompute(rval$gating_set)
      rval$update_gs <- rval$update_gs + 1
      #update plot_params
      for(var in names(plot_params)){
        plot_params[[var]] <- res$params[[var]]
      }
      plot_params$subset <- "root"
      
    }
    
  })
  
  
  #Rename selected gate, update rval$gates_flowCore and rval$gating_set
  observeEvent(input$rename_gate, {
    
    if(input$gate_to_rename != "root"){
      
      idx_gh <- which( flowWorkspace::gs_get_pop_paths(rval$gating_set) == input$gate_to_rename )
      target_gate <- flowWorkspace::gs_get_pop_paths(rval$gating_set)[idx_gh]

      flowWorkspace::setNode(rval$gating_set, target_gate, input$new_name)
      rval$update_gs <- rval$update_gs + 1
      
    }
    
  })

  #Update plot parameters to show defining gate
  observeEvent(input$show_gate, {
    
    if(res$params$subset != "root"){
      
      gates <- get_gates_from_gs(gs = rval$gating_set)
      rval$gate <- gates[[res$params$subset]]$gate
      
      #update plot_params
      for(var in names(plot_params)){
        plot_params[[var]] <- res$params[[var]]
      }
      plot_params$subset <- gates[[res$params$subset]]$parent
      
      gate_params <- names(rval$gate@parameters)
      
      if(length(gate_params) > 0){
        plot_params$xvar <- gate_params[1]
      }
      if(length(gate_params) > 1){
        plot_params$yvar <- gate_params[2]
      }
      
      gate$x <- NULL
      gate$y <- NULL

    }

  })
  
  
  
  ##########################################################################################################
  # Output messages
  
  
  output$message_gate <- renderPrint({
    print(gate$x)
  })
  
  
  ##########################################################################################################
  # Output plots
  
  gate_list <- reactive({
    rval$update_gs
    validate(need(class(rval$gating_set)=="GatingSet", "No GatingSet available"))
    gates <- get_gates_from_gs(rval$gating_set)
    validate(need(names(gates), "Empty GatingSet"))
    
    if(!input$show_all_subsets){
      idx_cluster <- grep("^cluster[0-9]+", basename(names(gates)))
      idx_bins <- grep("^bin[0-9]+", basename(names(gates)))
      idx_hide <- union(idx_cluster, idx_bins)
      if(length(idx_hide)>0){
        gates <- gates[-idx_hide]
      }
    }
    gates
  })
  
  observe({
    plot_params_gh$selected_subsets <- names(gate_list())
  })
  
  output$tree <- renderPlot({
    gates <- gate_list()

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
    df <- df[df$subset %in% plot_params_gh$selected_subsets, ]
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
  
  ##########################################################################################################
  #Import/Export Gating Hierarchy
  
  output$export_gh <- downloadHandler(
    filename = "gates.rda",
    content = function(file) {
      gates <- get_gates_from_gs(rval$gating_set)
      save(gates, file=file)
    }
  )
  
  observeEvent(input$import_gh, {
    validate(
      need(input$import_gh$datapath, "Please select a file")
    )
    file_path <- input$import_gh$datapath
    if(file_ext(file_path) %in% c("wsp") ){
      rval_mod$gating_hierarchy <- get_gates_from_ws(ws_path = file_path)
    }else if(file_ext(file_path) %in% c("rda", "Rda") ){
      res_name <- load(file_path)
      res <- get(res_name)
      if(class(res) == "list"){
        if("gate" %in% names(res[[1]])){
          rval_mod$gating_hierarchy <- res
        }
      }
    }
  })
  
  output$import_gh_summary <- renderPrint({
    print(paste(length(rval_mod$gating_hierarchy), "gates imported"))
    print(rval_mod$gating_hierarchy)
  })
  
  observe({
    validate(need(class(rval$gating_set)=="GatingSet", "No GatingSet available"))
    ff <- rval$gating_set@data[[1]]
    time_step <- as.numeric(description(ff)[["$TIMESTEP"]])
    updateNumericInput(session, "time_step", value = time_step)
  })
  
  
  observeEvent(input$apply_gh, {
    validate(need(length(rval_mod$gating_hierarchy)>0, "No gating hierarchy to apply"))
    
    old_gates <- gs_pop_get_children(obj = rval$gating_set[[1]], y = "root")
    #print(old_gates)
    for(gate in old_gates){
        flowWorkspace::gs_pop_remove(gs = rval$gating_set, node = gate)
    }
    #print("OK remove")
    #print(gs_get_pop_paths(rval$gating_set))
    new_gates <- rval_mod$gating_hierarchy
    new_gates <- transform_gates(gates = new_gates, 
                                 transformation = NULL,
                                 pattern = input$pattern, 
                                 replacement = input$replacement,
                                 time_step = input$time_step)
    
    add_gates_flowCore(gs = rval$gating_set, gates = new_gates)
    #print("OK add")
    #print(gs_get_pop_paths(rval$gating_set))
    rval$update_gs <- rval$update_gs + 1

  })
    
  return(rval)
  
  
}

##################################################################################
# Tests
##################################################################################
# 
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
#     dashboardHeader(title = "Gating"),
#     sidebar = dashboardSidebar(disable = TRUE),
#     body = dashboardBody(
#       GatingUI("module")
#     )
#   )
# 
#   server <- function(input, output, session) {
# 
#     rval <- reactiveValues()
# 
#     observe({
#       #load("../flowR_utils/demo-data/Rafa2Gui/analysis/cluster.rda")
#       #fs <- build_flowset_from_df(df = res$cluster$data, origin = res$cluster$flow_set)
#       #gs <- GatingSet(fs)
#       #gs@transformation <-  res$cluster$transformation
#       #add_gates_flowCore(gs, res$cluster$gates)
#       #rval$gating_set <- gs
#       #plot_params$sample <- pData(gs)$name[1]
#       utils::data("GvHD", package = "flowCore")
#       rval$gating_set <- GatingSet(GvHD)
#       #gs <- load_gs("./inst/ext/gs")
#       #rval$gating_set <- gs
#     })
# 
#     res <- callModule(Gating, "module", rval = rval)
# 
#   }
# 
#   shinyApp(ui, server)
# 
# }

