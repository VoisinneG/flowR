#' @title   plotStatInput and plotStat
#' @description  A shiny Module to build plots with data aggregation
#' @param id shiny id
#' @importFrom shinydashboard box tabBox
#' @import shiny
plotStatInput <- function(id) {

  ns <- NS(id)
  
  tagList(
    actionButton(ns("update"), "update"),
    br(),
    br(),
    box(collapsible = TRUE, collapsed = TRUE, width = NULL, height = NULL,
        title = "Sample/Subset",
        selectionInput(ns("selection_module"), multiple_subset = TRUE)
    ),
    box(collapsible = TRUE, collapsed = FALSE, width = NULL, height = NULL,
        title ="Stat",
        selectInput(ns("stat_function"), 
                    label = "statistics", 
                    choices = c("mean", "median", "sd", "cell count", "percentage"), 
                    selected = "mean"),
        uiOutput(ns("stat_options"))
    ),
    box(collapsible = TRUE, collapsed = TRUE, width = NULL, height = NULL,
        title ="Plot variables",
        
        selectizeInput(ns("group_var"), 
                       multiple = FALSE,
                       label = "group variable (x-axis)",
                       choices = NULL,
                       selected = NULL),
        selectizeInput(ns("facet_var"), 
                       multiple = TRUE,
                       label = "facet variables",
                       choices = NULL,
                       selected = NULL
        ),
        selectInput(ns("color_var"), "color variable",
                    choices = NULL,
                    selected = NULL)
    ),
    box(collapsible = TRUE, collapsed = TRUE, width = NULL, height = NULL,
        title ="Options",
        selectInput(ns("plot_type"), label = "plot type",
                    choices = c("bar", 
                                "tile", 
                                "heatmap",
                                "pca"),
                    selected = "bar"),
        uiOutput(ns("plot_options"))
    )
    
  )
  
}


#' plotStat server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param rval A reactive values object
#' @return a list containing the plot, the plot data, and the corresponding plot parameters
#' @import shiny
#' @importFrom scales identity_trans log10_trans
#' @export
#' @rdname plotStatUI
plotStat <- function(input, output, session, rval) {
  
  `%then%` <- shiny:::`%OR%`
  
  rval_plot <- reactiveValues()
  rval_mod <- reactiveValues()
  selected <- callModule(selection, "selection_module", rval)
  
  
  
  observe({
    
    ns <- session$ns
    x <- list()
    x[["legend"]] <- checkboxInput(ns("legend"), "show legend", value = TRUE)
    x[["theme"]] <- selectInput(ns("theme"), 
                label = "plot theme", 
                choices = c("gray", "light", "minimal", "classic", "bw", "dark", "void"), 
                selected = "gray")
    x[["scales"]] <- selectInput(ns("scales"), 
                                 label = "facet scales", 
                                 choices = c("fixed", "free", "free_x", "free_y"), 
                                 selected = "fixed")
    x[["scale_values"]] <- checkboxInput(ns("scale_values"), "scale values by row", value = FALSE)
    x[["max_scale"]] <- numericInput(ns("max_scale"), label = "scale limit", value = 0)
    x[["expand_factor"]] <- numericInput(ns("expand_factor"), label = "expand factor", value = 0.1)
    x[["strip_text_angle"]] <- numericInput(ns("strip_text_angle"), label = "y strip text angle", value = 0)
    x[["cluster_x"]] <- checkboxInput(ns("cluster_x"), "cluster x variables", value = TRUE)
    x[["cluster_y"]] <- checkboxInput(ns("cluster_y"), "cluster y variables", value = TRUE)
    
    rval_mod$plot_options <- x
    
  })
  
  output$plot_options <- renderUI({
    ns <- session$ns
    # x <- rval_mod$plot_options
    if(input$plot_type == 'bar'){
      vars <- c("legend", "theme", "scales", "scale_values", "max_scale", "expand_factor", "strip_text_angle")
    }else if (input$plot_type == 'tile'){
      vars <- c("legend", "theme", "scale_values", "max_scale", "strip_text_angle" )
    }else if (input$plot_type == 'heatmap'){
      vars <- c("scale_values", "max_scale", "cluster_x", "cluster_y")
    }else if (input$plot_type == 'pca'){
      vars <- c("scale_values")
      rval_mod$plot_options[["scale_values"]] <- checkboxInput(ns("scale_values"), "scale values by row", value = TRUE)
    }
    tagList(rval_mod$plot_options[vars])
    
  })
  
  output$stat_options <- renderUI({

    validate(
      need(rval$plot_var, "No plotting parameters")
    )

    if( ! input$stat_function %in% c("cell count", "percentage") ){
      ns <- session$ns
      tagList(
        selectInput(ns("y_trans"),
                    label = "Transform",
                    choices = c("log10", "asinh", "identity", "default"),
                    selected = "default"),
        selectizeInput(ns("yvar"),
                       multiple = TRUE,
                       label = "variable (y-axis)",
                       choices = rval$plot_var,
                       selected = rval$plot_var[1]),
        box(title = "Select using pattern", width = NULL, height = NULL, collapsible = TRUE, collapsed = TRUE,
            textInput(ns("pattern"), "Pattern"),
            checkboxInput(ns("match_as_is"), "use pattern as regular expression", TRUE),
            actionButton(ns("select_var"), "Select variable (y-axis)")
        )
      )
    }

  })
  

  observe({
    
    validate(
      need(rval$pdata, "No metadata available")
    )

    facet_var_default <- "name"
    color_var_default <- "subset"
    group_var_default <- "subset"
   
    extra_facet_var <- rval$parameters$name[rval$parameters$name %in% c("cluster", "bin")]
    if(length(extra_facet_var) == 0){
      extra_facet_var <- NULL
    }
    
    updateSelectInput(session, "color_var", 
                      choices = c("subset", names(rval$pdata)), 
                      selected = color_var_default)
    
    updateSelectInput(session, "group_var", 
                      choices = c("subset", names(rval$pdata)), 
                      selected = color_var_default)
    
    updateSelectInput(session, "facet_var",
                        choices = c("subset", names(rval$pdata), extra_facet_var), 
                        selected = facet_var_default )
    
  })
  
  
  observeEvent(input$select_var, {

    var_name <- "yvar"
    choices <- rval$plot_var
    var_selected <- NULL
    
    idx_selected <- try(grep(input$pattern, choices, fixed = input$match_as_is), silent = TRUE)
    
    if(class(idx_selected) == "try-error"){
      showModal(modalDialog(
        title = "Error",
        print(idx_selected),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    if(length(idx_selected)>0){
      var_selected <- choices[idx_selected]
    }
    updateSelectInput(session, var_name, choices = choices, selected = var_selected)
    
  })
  

  observeEvent(input$update, {
    for(var in names(input)){
      rval_plot[[var]] <- input[[var]]
    }
    rval_plot$gate <- selected$gate
    rval_plot$samples <- selected$samples
  }) 
  
  update_data_plot_stat <- eventReactive(input$update, {
    data_plot_stat()
  })

  data_plot_stat <- reactive({
    validate(
      need(rval$gating_set, "Empty gating set") %then%
        need(selected$samples, "Please select samples") %then%
        need(selected$gate, "Please select subsets")
    )

    df <- get_data_gs(gs = rval$gating_set,
                      sample = selected$samples,
                      subset = selected$gate,
                      spill = rval$spill)
    
    return(df)

  })
  
  
  plot_statistics <- eventReactive(input$update, {
    
    validate(
      need(rval$gating_set, "Empty gating set") %then%
        need(selected$samples, "Please select samples") %then%
        need(selected$gate, "Please select subsets")
        
    )
    
    if(!input$stat_function %in% c("cell count", "percentage")){
      validate(need(input$yvar, "Please select y variables"))
    }

    
    idx_y <- match(input$yvar, rval$parameters$name_long)
    yvar <- rval$parameters$name[idx_y]
    
    transformation <- NULL
    if(rval$apply_trans){
      transformation <- rval$transformation
    }
    
    axis_labels <- rval$parameters$name_long
    names(axis_labels) <- rval$parameters$name
    
    y_trans <- switch(input$y_trans,
                      "log10" = scales::log10_trans(),
                      "asinh" = asinh_trans(),
                      "identity" = scales::identity_trans(),
                      NULL)

    plot_args <- list()
    options <- list()
    for(var in names(input) ){
      plot_args[[var]] <- input[[var]]
      options[[var]] <- input[[var]]
    }
    
    scale <- FALSE
    if(!is.null(input$scale_values)){
      scale <- input$scale_values
    }
    
    p <- try(plot_stat(df = update_data_plot_stat(),
                   gs = rval$gating_set,
                   metadata = rval$pdata,
                   sample = selected$samples,
                   subset = selected$gate, 
                   spill = rval$spill,
                   yvar = yvar,
                   transformation = transformation,
                   stat_function = input$stat_function,
                   y_trans = y_trans,
                   apply_inverse = TRUE,
                   scale = scale,
                   plot_type = input$plot_type,
                   plot_args = plot_args,
                   options = options
                   ), silent = TRUE)
    
    if(class(p) == "try-error"){
      showModal(modalDialog(
        title = "Error",
        p,
        easyClose = TRUE,
        footer = NULL
      ))
      NULL
    }else{
      p
    }
    
  })
  
  plot <- reactive({
    p <- plot_statistics()$plot
    print(names(p))
    p
  })
  
  data <- reactive({
    plot_statistics()$data
  })
  
  
  return( list(plot = plot, data = data, params = rval_plot) )
  
}