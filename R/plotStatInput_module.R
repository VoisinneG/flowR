#' @title   plotStatInput and plotStat
#' @description  A shiny Module that deals with metadata
#' @param id shiny id
#' @importFrom shinydashboard box tabBox
#' @import shiny
#' @import DT
plotStatInput <- function(id) {

  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    actionButton(ns("update"), "update"),
    br(),
    br(),
    box(collapsible = TRUE, collapsed = TRUE, width = NULL, height = NULL,
        title = "Sample/Subset",
        selectionInput(ns("selection_module"), multiple_subset = TRUE)
    ),
    # box(collapsible = TRUE, collapsed = TRUE, width = NULL, height = NULL,
    #     title = "Sample/Subset",
    #     checkboxInput(ns("all_samples"), "Select all samples", FALSE),
    #     selectizeInput(ns("samples"), 
    #                    label = "samples",
    #                    choices = NULL,
    #                    selected = NULL,
    #                    multiple = TRUE),
    #     selectizeInput(ns("gate"), 
    #                    label = "subset",
    #                    choices = "root",
    #                    selected = "root",
    #                    multiple = TRUE)
    # ),
    box(collapsible = TRUE, collapsed = FALSE, width = NULL, height = NULL,
        title ="Stat",
        selectInput(ns("stat_function"), 
                    label = "statistics", 
                    choices = c("mean", "median", "sd"), 
                    selected = "mean"),
        selectInput(ns("y_trans"), 
                    label = "Transform", 
                    choices = c("log10", "asinh", "identity", "default"), 
                    selected = "default")
    ),
    box(collapsible = TRUE, collapsed = FALSE, width = NULL, height = NULL,
        title ="Variables",
        selectizeInput(ns("yvar"), 
                       multiple = TRUE,
                       label = "variable (y-axis)", 
                       choices = NULL, 
                       selected = NULL),
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
                    choices = c("bar", "tile", "heatmap"),
                    selected = "bar"),
        # checkboxInput(ns("legend"), "show legend", value = TRUE),
        # selectInput(ns("theme"), 
        #             label = "plot theme", 
        #             choices = c("gray", "light", "minimal", "classic", "bw", "dark", "void"), 
        #             selected = "gray"),
        uiOutput(ns("plot_options"))
        
        # checkboxInput(ns("free_y_scale"), "free y scale", value = TRUE),
        # checkboxInput(ns("scale_values"), "scale values by row", value = FALSE),
        # numericInput(ns("max_scale"), label = "scale limit", value = 2),
        # numericInput(ns("expand_factor"), label = "expand factor", value = 0.1),
        # numericInput(ns("strip_text_angle"), label = "strip text angle", value = 0)
        
    )
    
  )
  
}


#' plotStat server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @return a reactivevalues object with values "df_files", "flow_set_imported" and "gates_flowCore"
#' @import flowWorkspace
#' @import flowCore
#' @import shiny
#' @import DT
#' @import plotly
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
    x[["free_y_scale"]] <- checkboxInput(ns("free_y_scale"), "free y scale", value = TRUE)
    x[["scale_values"]] <- checkboxInput(ns("scale_values"), "scale values by row", value = FALSE)
    x[["max_scale"]] <- numericInput(ns("max_scale"), label = "scale limit", value = 2)
    x[["expand_factor"]] <- numericInput(ns("expand_factor"), label = "expand factor", value = 0.1)
    x[["strip_text_angle"]] <- numericInput(ns("strip_text_angle"), label = "strip text angle", value = 0)
    x[["cluster_x"]] <- checkboxInput(ns("cluster_x"), "cluster x variables", value = TRUE)
    x[["cluster_y"]] <- checkboxInput(ns("cluster_y"), "cluster y variables", value = TRUE)

    rval_mod$plot_options <- x
    
  })
  
  output$plot_options <- renderUI({
    x <- rval_mod$plot_options
    if(input$plot_type == 'bar'){
      vars <- c("legend", "theme", "free_y_scale", "scale_values", "max_scale", "expand_factor")
    }else if (input$plot_type == 'tile'){
      vars <- c("legend", "theme", "scale_values", "max_scale", "strip_text_angle" )
    }else if (input$plot_type == 'heatmap'){
      vars <- c("scale_values", "max_scale", "cluster_x", "cluster_y")
    }
    tagList(rval_mod$plot_options[vars])
    
  })
  
  
  

  observe({
    
    validate(
      need(rval$pdata, "No metadata available")
    )

    facet_var_default <- "name"
    color_var_default <- "subset"
    group_var_default <- "subset"
   
    updateSelectInput(session, "color_var", 
                      choices = c("subset", names(rval$pdata)), 
                      selected = color_var_default)
    
    updateSelectInput(session, "group_var", 
                      choices = c("subset", names(rval$pdata)), 
                      selected = color_var_default)
    
    updateSelectInput(session, "facet_var",
                        choices = c("subset", names(rval$pdata)), 
                        selected = facet_var_default )
    
  })
  
  observe({
    
    validate(
      need(rval$plot_var, "No plotting parameters")
    )
    
    yvar_default <- rval$plot_var[1]
  
    updateSelectInput(session, "yvar", choices = rval$plot_var, selected = yvar_default)
    
  })
  
  # observe({
  #   updateSelectInput(session, "gate", choices = union("root", names(rval$gates_flowCore)), selected = "root")
  # })
  # 
  # observe({
  #   updateSelectInput(session, "samples", choices = rval$pdata$name, selected = rval$pdata$name[1])
  # })
  # 
  # observeEvent(input$all_samples, {
  #   updateSelectInput(session, "samples", choices = rval$pdata$name, selected = rval$pdata$name)
  # })

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
        need(selected$gate, "Please select subsets") %then%
        need(input$yvar, "Please select y variables")
    )
    
    if(input$plot_type == "heatmap"){
      name_x_var <- switch(input$group_var,
                           "subset" = "gate",
                           "name" = "samples")
      validate(need(length(selected[[name_x_var]])>1,
                    paste("Please select more vriables on x axis (", name_x_var, ")", sep = "")))
      validate(need(length(input$yvar)>1, "Please select more variables on y axis"))
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
                      "log10" = log10_trans(),
                      "asinh" = asinh_trans(),
                      "identity" = identity_trans(),
                      NULL)
    
    p <- plot_stat(df = update_data_plot_stat(),
                   gs = rval$gating_set,
                   sample = selected$samples,
                   subset = selected$gate, 
                   spill = rval$spill,
                   yvar = yvar,
                   type = input$plot_type,
                   transformation = transformation,
                   axis_labels = axis_labels,
                   default_trans = identity_trans(),
                   scale_values = input$scale_values,
                   max_scale = input$max_scale,
                   free_y_scale = input$free_y_scale,
                   color_var = input$color_var, 
                   facet_vars = input$facet_var,
                   group_var = input$group_var,
                   expand_factor = input$expand_factor,
                   stat_function = input$stat_function,
                   show.legend = input$legend,
                   y_trans = y_trans,
                   strip.text.y.angle = input$strip_text_angle,
                   theme_name = paste("theme_", input$theme, sep = ""),
                   Rowv = input$cluster_y,
                   Colv = input$cluster_x
                   )
    
    p                          
    
  })
  
  plot <- reactive({
    plot_statistics()$plot
  })
  
  data <- reactive({
    plot_statistics()$data
  })
  
  
  return( list(plot = plot, data = data, params = rval_plot) )
  
}