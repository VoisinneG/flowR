#' Perform dimensionality reduction using t-SNE or UMAP
#' @param id shiny id
#' @import shiny
#' @importFrom shinydashboard tabBox valueBoxOutput
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
#'     dashboardHeader(title = "Dim_reduction"),
#'     sidebar = dashboardSidebar(disable = TRUE),
#'     body = dashboardBody(
#'       Dim_reductionUI("module")
#'     )
#'   )
#' 
#'   server <- function(input, output, session) {
#' 
#'     rval <- reactiveValues()
#' 
#'     observe({
#'       utils::data("GvHD", package = "flowCore")
#'       gs <- GatingSet(GvHD)
#'       transformation <- lapply(colnames(GvHD), function(x){logicle_trans()} )
#'       names(transformation) <- colnames(GvHD)
#'       print(transformation)
#'       gs@transformation <- transformation
#'       rval$gating_set <- gs
#'     })
#' 
#'     rval <- callModule(Dim_reduction, "module", rval = rval)
#' 
#'   }
#' 
#'   shinyApp(ui, server)
#' 
#' }
#' }
Dim_reductionUI <- function(id) {
  
  # Create a namespace function using the provided id
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
                      selectInput(ns("dim_red_method"), 
                                  label = "method", 
                                  choices = c("tSNE" , "umap"), 
                                  selected = "tSNE"),
                      uiOutput(ns("method_ui"))
             ),
             tabPanel("Compute",
                      numericInput(ns("ncells"), "Number of cells", 1000),
                      textInput(ns("gs_name"), "GatingSet name", "dim-reduction"),
                      actionButton(ns("compute"), "Start"),
                      br(),
                      br(),
                      "Summary",
                      br(),
                      verbatimTextOutput(ns("summary"))
             )
      ),
      fluidRow(
        valueBoxOutput(ns("progressBox"), width = 6),
        valueBoxOutput(ns("progressBox2"), width = 6)
      )
    ),
    column(width = 6,
           tabBox(title = "",
                  width = NULL, height = NULL,
                  tabPanel(title = "Plot",
                           simpleDisplayUI(ns("simple_display_module"))
                  ),
                  tabPanel(title = "Parameters",
                           plotGatingSetInput(id = ns("plot_module"))
                  )
           )
    )
  )
}


#' Dim_reduction module  server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param rval A reactive values object
#' @return The updated reactiveValues object \code{rval}
#' @import shiny
#' @importFrom flowWorkspace pData gs_get_pop_paths
#' @importFrom shinydashboard renderValueBox
#' @importFrom DT renderDT datatable
#' @importFrom scales identity_trans log10_trans
#' @export
#' @rdname Dim_reductionUI
Dim_reduction <- function(input, output, session, rval) {
  
  plot_params <- reactiveValues()
  rval_mod <- reactiveValues( gs = NULL )
  
  observe({ 
    if(! "update_gs" %in% names(rval)){
      rval$update_gs <- 0
    }
  })
  
  ### Call modules #########################################################################
  
  selected <- callModule(selection, "selection_module", rval)
  res <- callModule(plotGatingSet, "plot_module", 
                    rval = rval, plot_params = plot_params, simple_plot = FALSE)
  callModule(simpleDisplay, "simple_display_module", plot_list = res$plot)
  
  ### Build UI with options ##############################################################
  
  output$method_ui <- renderUI({
    ns <- session$ns
    x <- list()
    if(input$dim_red_method == 'tSNE'){
      x[[1]] <- numericInput(ns("perplexity"), "perplexity", 50)
      x[[2]] <- numericInput(ns("dims"), "# dimensions", 2)
      x[[3]] <- checkboxInput(ns("check_duplicates"), "check duplicates", FALSE)
    }
    tagList(x)
  })
  
  ### Set plot parameters ###################################################################
  
  observe({
    
    rval$update_gs
    
    plot_params$plot_type <- "dots"
    plot_params$use_all_cells <- TRUE
    plot_params$xvar <- NULL
    if("tSNE1" %in% choices()$params$name){
      plot_params$xvar <- "tSNE1"
    }
    if("tSNE2" %in% choices()$params$name){
      plot_params$yvar <- "tSNE2"
    } 
    if("UMAP1" %in% choices()$params$name){
      plot_params$xvar <- "UMAP1"
    }
    if("UMAP2" %in% choices()$params$name){
      plot_params$yvar <- "UMAP2"
    } 
  })
  
  ### Get parameters from GatingSet ########################################################
  
  choices <- reactive({
    rval$update_gs
    validate(need(class(rval$gating_set) == "GatingSet", "No GatingSet available"))
    get_parameters_gs(rval$gating_set)
  })
  
  ### Compute Dim. Red. #################################################################
  
  observeEvent(input$compute, {

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

    validate(
      need(length(input$variables_table_rows_selected) >0, "No variables selected")
    )
    
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

    progress$set(message = "Getting data...", value = 0)

    df_raw <- get_data_gs(gs = rval$gating_set,
                          sample = selected$sample,
                          subset = selected$subset,
                          spill = spill,
                          Ncells = NULL,
                          return_comp_data = FALSE,
                          updateProgress = updateProgress)

    rval_mod$df_dim_red <- get_data_gs(gs = rval$gating_set,
                                   sample = selected$sample,
                                   subset = selected$subset,
                                   spill = spill,
                                   Ncells = NULL,
                                   return_comp_data = TRUE,
                                   updateProgress = updateProgress)


    progress$set(message = paste("Performing", input$dim_red_method, "..."), value = 0)
    
    res <- try(dim_reduction(df = rval_mod$df_dim_red,
                         yvar = parameters_table()$name[input$variables_table_rows_selected],
                         Ncells = input$ncells,
                         y_trans = y_trans,
                         transformation = transformation,
                         method = input$dim_red_method,
                         perplexity = ifelse(is.null(input$perplexity), 50, input$perplexity),
                         dims = ifelse(is.null(input$dims), 2, input$dims),
                         check_duplicates = ifelse(is.null(input$check_duplicates), 2, input$check_duplicates)
                         ), silent = TRUE)
    
    if(class(res) == "try-error"){
      showModal(modalDialog(
        title = "Error",
        print(res),
        easyClose = TRUE,
        footer = NULL
      ))
    }else{
      rval_mod$df_dim_red <- res$df
      
      df <- cbind( df_raw[res$keep, ], rval_mod$df_dim_red[ , setdiff(names(rval_mod$df_dim_red), names(df_raw))])
      
      rval$dim_red_var <- res$vars
      
      if(!is.null(rval_mod$df_dim_red)){

        df <- cbind(df_raw[res$keep, setdiff(names(df_raw), names(res$df))], res$df)
        
        rval_mod$gs <- build_gatingset_from_df(df = df, gs_origin = rval$gating_set)
        params <- colnames(rval_mod$gs)[colnames(rval_mod$gs) %in% names(rval$trans_parameters)]
        
        rval$gating_set_list[[input$gs_name]] <- list(gating_set = rval_mod$gs,
                                                      parent = rval$gating_set_selected,
                                                      trans_parameters = rval$trans_parameters[params]
                                                      )
        rval$gating_set_selected <- input$gs_name
        
        # fs <- build_flowset_from_df(rval_mod$df_dim_red, 
        #                             origin = rval$gating_set@data)
        # 
        # rval_mod$gs <- GatingSet(fs)
        # gates <- get_gates_from_gs(rval$gating_set)
        # add_gates_flowCore(gs = rval_mod$gs, gates = gates)
        # rval_mod$gs@compensation <- choices()$compensation
        # rval_mod$gs@transformation <- choices()$transformation
        # 
        # rval$gating_set_list[[input$gs_name]] <- list(gating_set = rval_mod$gs,
        #                                               parent = rval$gating_set_selected)
        # rval$gating_set_selected <- input$gs_name
        
        rval$gating_set <- rval_mod$gs
        rval$update_gs <- rval$update_gs + 1
      }
    
    }


  })
  
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
  
  ### Value boxes #########################################################################
  
  output$progressBox <- renderValueBox({
    Nsamples <- 0
    if(!is.null(rval_mod$gs)){
      Nsamples <- length(flowWorkspace::pData(rval_mod$gs)$name)
    }
    
    valueBox(
      Nsamples, "samples",icon = icon("list"),
      color = "purple"
    )
  })
  
  output$progressBox2 <- renderValueBox({
    ncells <- 0
    if(!is.null(rval_mod$gs)){
      fs <- rval_mod$gs@data
      ncells <- sum( sapply(1:length(fs), function(x){dim(fs[[x]]@exprs)[1]}) )
    }
    
    valueBox(
      ncells, "cells", icon = icon("list"),
      color = "green"
    )
  })
  
  ### Summary #############################################################################
  
  output$summary <- renderPrint({
    validate(need(rval_mod$df_dim_red, "no dim-reduction performed"))
    print(summary(rval_mod$df_dim_red[, c("name", "subset")]))
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
#     dashboardHeader(title = "Dim_reduction"),
#     sidebar = dashboardSidebar(disable = TRUE),
#     body = dashboardBody(
#       Dim_reductionUI("module")
#     )
#   )
# 
#   server <- function(input, output, session) {
# 
#     rval <- reactiveValues()
# 
#     observe({
#       utils::data("GvHD", package = "flowCore")
#       gs <- GatingSet(GvHD)
#       transformation <- lapply(colnames(GvHD), function(x){logicle_trans()} )
#       names(transformation) <- colnames(GvHD)
#       print(transformation)
#       gs@transformation <- transformation
#       rval$gating_set <- gs
#     })
# 
#     rval <- callModule(Dim_reduction, "module", rval = rval)
# 
#   }
# 
#   shinyApp(ui, server)
# 
# }
