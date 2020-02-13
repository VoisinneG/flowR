#' Automatic cluster identification
#' @param id shiny id
#' @importFrom shinydashboard tabBox
#' @import shiny
#' @importFrom DT DTOutput
#' @export
#' @examples 
#' \dontrun{
#' library(shiny)
#' library(shinydashboard)
#' library(flowWorkspace)
#' library(flowCore)
#' 
#' if (interactive()){
#' 
#'   ui <- dashboardPage(
#'     dashboardHeader(title = "Clustering"),
#'     sidebar = dashboardSidebar(disable = TRUE),
#'     body = dashboardBody(
#'       ClusteringUI("module")
#'     )
#'   )
#' 
#'   server <- function(input, output, session) {
#' 
#'     rval <- reactiveValues()
#'     observe({
#'       utils::data("GvHD", package = "flowCore")
#'       gs <- GatingSet(GvHD)
#'       rval$gating_set <- gs
#'     })
#'     rval <- callModule(Clustering, "module", rval = rval)
#'   }
#' 
#'   shinyApp(ui, server)
#' 
#' }
#' }
ClusteringUI <- function(id) {
  
  ns <- NS(id)
  
    fluidRow(
      column(width = 6,
             tabBox(title = "",
                    width = NULL, height = NULL,
                    tabPanel("Sample/Subset",
                             selectionInput(ns("selection_module"))
                    ),
                    tabPanel("Variables",
                              checkboxInput(ns("select_all"), "Select all", value = FALSE),
                              br(),
                              div(style = 'overflow-x: scroll', 
                                  DT::DTOutput(ns("variables_table")))
                    ),
                    tabPanel("Options",
                             selectInput(ns("y_trans"),
                                         label = "Transform variables:",
                                         choices = c("log10", 
                                                     "asinh", 
                                                     "identity", 
                                                     "default"),
                                         selected = "default"),
                             selectInput(ns("clustering_method"), 
                                         label = "method", 
                                         choices = c("FlowSOM"), 
                                         selected = "FlowSOM"),
                             uiOutput(ns("method_ui"))
                    ),
                    tabPanel("Cluster",
                             textInput(ns("gs_name"), "GatingSet name", "cluster"),
                             actionButton(ns("start"), "Start"),
                             br(),
                             br(),
                             "Summary",
                             br(),
                             verbatimTextOutput(ns("summary"))
                    )
             )
      ),
      column(width = 6,
             uiOutput(ns("fsom_plot_ui"))
             )
    )
}


#' Clustering module server function
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
#' @importFrom graphics plot.new
#' @export
#' @rdname ClusteringUI
Clustering <- function(input, output, session, rval) {
  
  selected <- callModule(selection, "selection_module", rval)
  rval_mod <- reactiveValues( gs = NULL, parameters = NULL, fSOM = NULL)
  
  observe({ 
    if(! "update_gs" %in% names(rval)){
      rval$update_gs <- 0
    }
  })
  
  observe({
    # if(!is.null(rval$gating_set_selected)){
    #   if("fSOM" %in% names(rval$gating_set_list[[rval$gating_set_selected]])){
    #     rval_mod$fSOM <- rval$gating_set_list[[rval$gating_set_selected]]$fSOM
    #   }
    # }
    if(!is.null(rval$gating_set_selected)){
      rval_mod$fSOM <- rval$gating_set_list[[rval$gating_set_selected]]$fSOM
    }
    
  })
  
  ### Call modules #########################################################################
  
  selected <- callModule(selection, "selection_module", rval)
  callModule(simpleDisplay, "simple_display_module", 
             plot_list = plot_fSOM, 
             params = reactiveValues(width = 500, height = 500))
  
  ### Build UI with options ##################################################################
  
  output$method_ui <- renderUI({
    ns <- session$ns
    x <- list()
    # if(input$clustering_method == 'ClusterX'){
    #   x[[1]] <- numericInput(ns("cluster_dc"), "dc", 5)
    #   x[[2]] <- numericInput(ns("cluster_alpha"), "alpha", 0.0001)
    # }else if(input$clustering_method == 'Rphenograph'){
    #   x[[1]] <- numericInput(ns("k_param"), "k", 100)
    # }else if(input$clustering_method == 'FlowSOM'){
    #   x[[1]] <- numericInput(ns("k_meta"), "k", 7)
    #   x[[2]] <- checkboxInput(ns("scale"), "scale", FALSE)
    # }
    x[[1]] <- numericInput(ns("k_meta"), "k", 7)
    x[[2]] <- checkboxInput(ns("scale"), "scale", FALSE)
    tagList(x)
  })
  
  ### Build UI with FlowSOM plot ##############################################################
  
  output$fsom_plot_ui <- renderUI({
    
    validate(need(rval_mod$fSOM, ""))
    
    ns <- session$ns

    if(input$clustering_method == 'FlowSOM'){
      tabBox(title = "FlowSOM",
             width = NULL, height = NULL,
             tabPanel("Plot",
                      simpleDisplayUI(ns("simple_display_module"))
                      #plotOutput(ns("plot_fSOM"))
             ),
             tabPanel("Options",
                      selectInput(ns("fSOM_plot_type"), "Plot type",
                                  choices = c("stars", "pies", "marker"),
                                  selected = "pies"),
                      selectInput(ns("cellTypes"), "Pie variable", 
                                  choices = c("name", "subset"), 
                                  selected = "subset"),
                      checkboxInput(ns("scale_node_size"), "Scale node size", TRUE),
                      checkboxInput(ns("show_background"), "Show background", TRUE),
                      selectInput(ns("color_var"), "Color variable", 
                                  choices = choices()$plot_var[
                                    choices()$plot_var %in% colnames(rval_mod$fSOM$data)], 
                                  selected = NULL)
             )
      )
    }

  })
  
  ### Get parameters from GatingSet ########################################################
  
  choices <- reactive({
    rval$update_gs
    validate(need(class(rval$gating_set) == "GatingSet", "No GatingSet available"))
    get_parameters_gs(rval$gating_set)
  })
  
  ### Perform Clustering ######################################################################
  
  observeEvent(input$start, {

    
    if( length(selected$sample) ==0 ){
      showModal(modalDialog(
        title = "No sample selected",
        paste("Please select samples before proceeding", sep=""),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    validate(need(length(selected$sample)>0, "No sample selected"))
    
    
    if( nchar(selected$subset) == 0 ){
      showModal(modalDialog(
        title = "No subset selected",
        paste("Please select a subset before proceeding", sep=""),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    validate(need(selected$subset, "No subset selected"))
    
    if( length(input$variables_table_rows_selected)==0){
      showModal(modalDialog(
        title = "No variable selected",
        paste("Please select variables before proceeding", sep=""),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    validate( need(length(input$variables_table_rows_selected) >0, "No variables selected"))
    
    if( input$gs_name %in% names(rval$gating_set_list) ){
      showModal(modalDialog(
        title = "Name already exists",
        paste("Please choose another name", sep=""),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    validate(need(! input$gs_name %in% names(rval$gating_set_list), "Name already exists" ))
    
    # Create a Progress object
    progress <- shiny::Progress$new(min = 0, max = 100)
    on.exit(progress$close())
    
    updateProgress <- function(value = NULL, detail = NULL) {
      progress$set(value = value, detail = detail)
    }
    
    spill <- choices()$compensation
    if(!is.null(rval$apply_comp)){
      if(!rval$apply_comp){
        spill <- NULL
      }
    }
    
    transformation <- choices()$transformation
    if(!is.null(rval$apply_trans)){
      if(!rval$apply_trans){
        transformation <- NULL
      }
    }
    if(input$y_trans != "default"){
      transformation <- NULL
    }
    
    y_trans <- switch(input$y_trans,
                      "log10" = scales::log10_trans(),
                      "asinh" = asinh_trans(),
                      "identity" = scales::identity_trans(),
                      scales::identity_trans())
    
    progress$set(message = "Clustering...", value = 0)
    
    df_raw <- get_data_gs(gs = rval$gating_set,
                          sample = selected$sample,
                          subset = selected$subset,
                          spill = spill,
                          return_comp_data = FALSE,
                          Ncells = NULL,
                          updateProgress = updateProgress)
    
    df_cluster <- get_data_gs(gs = rval$gating_set,
                                   sample = selected$sample,
                                   subset = selected$subset,
                                   spill = spill,
                                   return_comp_data = TRUE,
                                   Ncells = NULL,
                                   updateProgress = updateProgress)
    
    progress$set(message = "Clustering...", value = 50)
    
    print("OK1")
    print(transformation)
    
    res <- try(get_cluster(df=df_cluster, 
                       yvar = parameters_table()$name[input$variables_table_rows_selected],
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
      
      df <- df_raw[res$keep, ]
      for(var in res$var_names){
        print(var)
        print(class(res$df[[var]]))
        df[[var]] <- res$df[[var]]
      }
      #df_raw <- df_raw[ , setdiff(names(df_raw), c("cluster"))]
      #df <- cbind(df_raw[res$keep, ], res$df[ , setdiff(names(res$df), names(df_raw))])
      
      rval_mod$gs <- build_gatingset_from_df(df = df, gs_origin = rval$gating_set)
      
      print(rval_mod$gs@transformation)
      params <- colnames(rval_mod$gs)[colnames(rval_mod$gs) %in% names(rval$trans_parameters)]
      
      rval$gating_set_list[[input$gs_name]] <- list(gating_set = rval_mod$gs,
                                                    parent = rval$gating_set_selected,
                                                    trans_parameters = rval$trans_parameters[params],
                                                    fSOM = res$fSOM)
      rval$gating_set_selected <- input$gs_name
      
      # fs <- build_flowset_from_df(df = df, 
      #                             origin = rval$gating_set@data)
      # 
      # rval_mod$gs <- GatingSet(fs)
      # gates <- get_gates_from_gs(rval$gating_set)
      # add_gates_flowCore(gs = rval_mod$gs, gates = gates)
      # rval_mod$gs@compensation <- choices()$compensation
      # rval_mod$gs@transformation <- choices()$transformation
      # 
      # rval$gating_set_list[[input$gs_name]] <- list(gating_set = rval_mod$gs,
      #                                               parent = rval$gating_set_selected,
      #                                               fSOM = res$fSOM)
      # rval$gating_set_selected <- input$gs_name
      
      rval$gating_set <- rval_mod$gs
      rval$update_gs <- rval$update_gs + 1
      
    }
    
    

    
  })
  
  ### Build FlowSOM plot ####################################################################
  
  plot_fSOM <- reactive({
    
    validate(need(input$clustering_method == "FlowSOM", "No plot to display"))
    validate(need(rval_mod$fSOM, "No plot to display"))
    
    fSOM <- rval_mod$fSOM
    
    if(!input$scale_node_size){
      fSOM <- FlowSOM::UpdateNodeSize(fSOM, reset=TRUE)
    }
    
    backgroundValues <- NULL
    if(input$show_background){
      backgroundValues <- as.factor(fSOM$metaClustering)
    }
    
    graphics::plot.new()
    
    if(input$fSOM_plot_type == "pies"){
      print("OK pies")
      FlowSOM::PlotPies(fSOM,
                        cellTypes=get_data_gs(gs = rval$gating_set, subset = "root")[[input$cellTypes]], 
                        backgroundValues = backgroundValues
      )
    }else if(input$fSOM_plot_type == "stars"){
      print("OK stars")
      FlowSOM::PlotStars(fSOM, 
                         backgroundValues = backgroundValues)
    }else if(input$fSOM_plot_type == "marker"){
      print("OK marker")
      FlowSOM::PlotMarker(fSOM, marker = input$color_var)
    }
  })
  
  # output$plot_fSOM <- renderPlot({
  #   print("OK render")
  #   plot_fSOM()
  # })
  

  ### Display available variables ##########################################################
  
  parameters_table <- reactive({
    
    transformation <- choices()$transformation
    trans_parameters <- rval$trans_parameters
    
    trans_name <- sapply(choices()$params$name, function(x){
      ifelse(!is.null(transformation[[x]]$name), transformation[[x]]$name , NA)
    })
    
    trans_param <- sapply(choices()$params$name, function(x){
      params <- trans_parameters[[x]]
      name <- paste( paste(names(params), as.character(params), sep = ": "), collapse="; ")
      ifelse(!is.null(name), name, NA)
    })
    
    df <- data.frame("name" = choices()$params$name, 
                     "desc" = choices()$params$desc, 
                     "transform" = unlist(trans_name), 
                     "transform parameters" = unlist(trans_param), check.names = FALSE)
    df
  })
  
  
  output$variables_table <- DT::renderDT({
    
    df <- parameters_table()
    
    selected <- NULL
    if(input$select_all){
      selected <- 1:length(df$name)
    }
    
    DT::datatable(
      df, 
      rownames = FALSE, selection = list(target = 'row', selected = selected))
  })
  
  ### Summary #############################################################################
  
  output$summary <- renderPrint({
    if("cluster" %in% colnames(rval$gating_set)){
      fs <- rval$gating_set@data
      cluster <- lapply(1:length(fs), function(x){return(unique(fs[[x]]@exprs[ , "cluster"]))})
      print(paste("Number of unique clusters :", length(unique(unlist(cluster)))))
    }else{
      "No clustering performed yet"
    }
  })
  
  return( rval )
  
}


### Tests #################################################################################
# 
# library(shiny)
# library(shinydashboard)
# library(flowWorkspace)
# library(flowCore)
# 
# if (interactive()){
# 
#   ui <- dashboardPage(
#     dashboardHeader(title = "Clustering"),
#     sidebar = dashboardSidebar(disable = TRUE),
#     body = dashboardBody(
#       ClusteringUI("module")
#     )
#   )
# 
#   server <- function(input, output, session) {
# 
#     rval <- reactiveValues()
#     observe({
#       utils::data("GvHD", package = "flowCore")
#       gs <- GatingSet(GvHD)
#       rval$gating_set <- gs
#     })
#     rval <- callModule(Clustering, "module", rval = rval)
#   }
# 
#   shinyApp(ui, server)
# 
# }
