#' @title plotGatingSetInput and plotGatingSet
#' @description  A shiny Module that deals with metadata
#' @param id shiny id
#' @importFrom shinydashboard box tabBox
#' @import shiny
#' @import DT
plotGatingSetInput <- function(id, simple_plot = TRUE, auto_update = TRUE) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    if(!auto_update){
      tagList(
        actionButton(ns("update_plot"), "update"),
        br(),
        br()
      )
    },
    box(collapsible = TRUE, collapsed = TRUE, width = NULL, height = NULL,
        title = "Sample/Subset",
        selectionInput(ns("selection_module"), multiple_subset = !simple_plot)
    ),
    box(collapsible = TRUE, collapsed = FALSE, width = NULL, height = NULL,
        title ="Variables",
        selectizeInput(ns("xvar"), 
                        multiple = !simple_plot,
                        label = "x variable", 
                        choices = NULL,
                        selected = NULL),
        selectizeInput(ns("yvar"),
                       multiple = !simple_plot,
                       label = "y variable",
                       choices = NULL,
                       selected = NULL),
        uiOutput(ns("plot_variables"))
    ),
    box(collapsible = TRUE, collapsed = TRUE, width = NULL, height = NULL,
        title ="Options",
        selectInput(ns("plot_type"), label = "plot type",
                    choices = c("hexagonal", "histogram", "dots", "contour"),
                    selected = "dots"),
        checkboxInput(ns("legend"), "show legend", value = TRUE),
        selectInput(ns("legend_pos"), label = "legend position",
                    choices = c("right", "top", "left", "bottom"),
                    selected = "right"),
        selectInput(ns("theme"), 
                    label = "plot theme", 
                    choices = c("gray", "light", "minimal", "classic", "bw", "dark", "void"), 
                    selected = "gray"),
        uiOutput(ns("plot_options"))
    )
                 
  )
  
}


#'  plotGatingSet server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @return a reactivevalues object with values "df_files", "flow_set_imported" and "gates_flowCore"
#' @import flowWorkspace
#' @import flowCore
#' @import shiny
#' @import DT
#' @export
#' @rdname plotGatingSetInput
plotGatingSet <- function(input, output, session, 
                          rval, 
                          plot_params = reactiveValues(), 
                          simple_plot = TRUE,
                          auto_update = TRUE,
                          show_gates = FALSE,
                          polygon_gate = NULL) {
  
  `%then%` <- shiny:::`%OR%`
  #ns <- session$ns
  
  rval_plot_default <- reactiveValues(norm = TRUE,
                              smooth = FALSE,
                              ridges = FALSE,
                              yridges_var = "subset",
                              bin_number = 100,
                              alpha = 0.1,
                              size = 0.1,
                              xvar = NULL,
                              yvar = NULL,
                              color_var = "none",
                              facet_var = NULL,
                              split_variable = "x_variable")
  
  observeEvent(input$plot_type, {
    rval_plot_default$alpha <- switch(input$plot_type, 
                                      "contour" = 0.75,
                                      "dots" = 0.1,
                                      0.1)
    
    rval_plot_default$size <- switch(input$plot_type, 
                                      "contour" = 0.2,
                                      0.1)
  })
  
  rval_plot <- reactiveValues()
  
  rval_mod <- reactiveValues()
  
  selected <- callModule(selection, "selection_module", rval, params = plot_params)

  observe({
    
    validate(need(rval$pdata, "No metadata available"))
    validate(need(rval$plot_var, "No plotting variables"))
    
    ns <- session$ns
    x <- list()
    
    x[["morm"]] <- checkboxInput(ns("norm"), "normalize (set max to 1)", value = rval_plot_default[["norm"]])
    x[["smooth"]] <- checkboxInput(ns("smooth"), "smooth", value = rval_plot_default[["smooth"]])
    
    x[["ridges"]] <- checkboxInput(ns("ridges"), "ridges", value = rval_plot_default[["ridges"]])
    
    x[["yridges_var"]] <- selectizeInput(ns("yridges_var"), 
                                         multiple =FALSE,
                                         label = "y ridges variable", 
                                         choices = c("subset", names(rval$pdata)), 
                                         selected = rval_plot_default[["yridges_var"]])
    x[["bin_number"]] <- numericInput(ns("bin_number"), label = "number of bins", value = rval_plot_default[["bin_number"]])
    x[["alpha"]] <- numericInput(ns("alpha"), label = "alpha", value = rval_plot_default[["alpha"]])
    x[["size"]] <- numericInput(ns("size"), label = "size", value = rval_plot_default[["size"]])
    
    rval_mod$plot_options <- x
    
  })
  
  output$plot_options <- renderUI({
    x <- rval_mod$plot_options
    if(input$plot_type == 'histogram'){
      vars <- names(x)
    }else if (input$plot_type == 'hexagonal'){
      vars <- "bin_number"
    }else if (input$plot_type == 'dots'){
      vars <- c("alpha", "size")
    }else if (input$plot_type == 'contour'){
      vars <- c("bin_number", "alpha", "size")
    }
    
    tagList(rval_mod$plot_options[vars])
    
  })
  
  observe({
    
    validate(need(rval$plot_var, "No plotting variables"))
    
    updateSelectInput(session, "xvar", choices = rval$plot_var, selected = rval$plot_var[1])
    updateSelectInput(session, "yvar", choices = rval$plot_var, selected = rval$plot_var[2]) 
    
  })
  
  observe({
    
    validate(need(rval$pdata, "No metadata available"))
    validate(need(rval$plot_var, "No plotting variables"))
    
    ns <- session$ns
    x <- list()
    
    if(input$plot_type %in% c('histogram', "contour")){
      x[["color_var"]] <- selectizeInput(ns("color_var"), 
                                         multiple = !simple_plot,
                                         label = "color variable",
                                         choices = c("none", "subset", names(rval$pdata)),
                                         selected = rval_plot_default[["color_var"]])
    
    }else{
      x[["color_var"]] <- selectizeInput(ns("color_var"), 
                                         multiple = !simple_plot,
                                         label = "color variable",
                                         choices = c("none", "subset", names(rval$pdata), rval$plot_var),
                                         selected = rval_plot_default[["color_var"]])
    }
    
    if(!simple_plot){
        x[["facet_var"]] <- selectizeInput(ns("facet_var"), 
                       multiple =TRUE,
                       label = "facet variables",
                       choices = c("subset", names(rval$pdata)),
                       selected = rval_plot_default[["facet_var"]]
        )
        x[["split_variable"]] <- selectInput(ns("split_variable"),
                    label = "select variable used to split plots",
                    choices = c("x variable", "y variable", "color variable"),
                    selected = rval_plot_default[["split_variable"]]
        )
    }

    rval_mod$plot_variables <- x
    
  })
  
  output$plot_variables <- renderUI({
    x <- rval_mod$plot_variables
    if(input$plot_type == 'histogram'){
      vars <- names(x)
    }else if (input$plot_type == 'hexagonal'){
      vars <- setdiff(names(x), "color_var")
    }else if (input$plot_type == 'dots'){
      vars <- names(x)
    }else if (input$plot_type == 'contour'){
      vars <- names(x)
    }
    tagList(rval_mod$plot_variables[vars])
  })
  
  
  observe({
    
    #print(names(plot_params))
    for(var in intersect( names(plot_params), c("xvar", "yvar", "color_var", "gate", "samples", "plot_type") )){
      if(!is.null(plot_params[[var]])){
          if(plot_params[[var]]!=""){
            updateSelectInput(session, var, selected = plot_params[[var]])
          }
      }else{
        updateSelectInput(session, var, selected = plot_params[[var]])
      }
    }
  })
  
  observe({
    for(var in names(input)){
        rval_plot[[var]] <- input[[var]]
    }
    rval_plot$gate <- selected$gate
    rval_plot$samples <- selected$samples
    #rval_plot$all_samples <-  selected$all_samples
  }) 

  
  split_var <- reactive({
    if("split_variable" %in% names(input)){
      switch(input$split_variable, 
             "x variable" = "xvar",
             "y variable" = "yvar",
             "color variable" = "color_var"
      )
    }else{
      "xvar"
    }
    
  })

  update <- reactive({
    if(!auto_update){
      input$update_plot
    }else{
      update_params <- NULL
      var_update <- c("plot_type",
                      "xvar",
                      "yvar",
                      "color_var",
                      "facet_var", 
                      "theme",
                      "legend",
                      "legend_pos",
                      "bin_number",
                      "size", 
                      "alpha", 
                      "norm", 
                      "smooth", 
                      "ridges", 
                      "yridges_var")
      var_update <- var_update[var_update %in% names(input)]
      for(var in var_update){
        update_params <- c(update_params, input[[var]])
      }
      
      update_params <- c(update_params,
                         selected$samples,
                         selected$gate,
                         rval$apply_trans,
                         rval$flow_set,
                         rval$gating_set,
                         rval$spill,
                         rval$parameters,
                         split_var())
      
      #if(!is.null(rval$apply_trans)){
        if(rval$apply_trans){
          update_params <- c(update_params, rval$transformation)
        }
      #}
      
      if(show_gates){
        update_params <- c(update_params, 
                           rval$gates_flowCore,
                           polygon_gate$x)
      }
      update_params
    }
  })
  
  update_data_plot_focus <- eventReactive(update(), {
    data_plot_focus()
  })
  
  data_plot_focus <- reactive({
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
  
  plot_focus <- eventReactive(update(), {
    
    validate(
        need(rval$gating_set, "Empty gating set") %then%
        need(selected$samples, "Please select samples") %then%
        need(selected$gate, "Please select subsets")  %then%
        need(input$xvar, "Please select a x-axis variable") 
        
    )
    
    idx_x <- match(input$xvar, rval$parameters$name_long)
    xvar <- rval$parameters$name[idx_x]
    name_xvar <- input$xvar
    if(!is.null(input$yvar)){
      if(input$yvar != ""){
        idx_y <- match(input$yvar, rval$parameters$name_long)
        yvar <- rval$parameters$name[idx_y]
        name_yvar <- input$yvar
      }else{
        yvar <- xvar
        name_yvar <- name_xvar
      }
    }else{
      yvar <- xvar
      name_yvar <- name_xvar
    }
    
    color_var <- input$color_var
   
    if(!is.null(input$color_var)){
      
      for(i in 1:length(input$color_var)){
        if(input$color_var[i] %in% rval$parameters$name_long){
          color_var[i] <- rval$parameters$name[match(input$color_var[i], rval$parameters$name_long)]
        }else{
          color_var[i] <- input$color_var[i]
        }
      }
    }

    axis_labels <- rval$parameters$name_long
    names(axis_labels) <- rval$parameters$name
    
    transformation <- NULL
    if(rval$apply_trans){
      transformation <- rval$transformation
    }
    
    plist <- list()
    
    if(!simple_plot){
      facet_vars <- input$facet_var
    }else{
      facet_vars <- NULL
    }
    
    for(i in 1:length(input[[split_var()]])){
      
      color_var_int <- color_var[1]
      xvar_int <- xvar[1]
      yvar_int <- yvar[1]
      
      if(split_var() == "color_var"){
        color_var_int <- color_var[i]
      }else if(split_var() == "xvar"){
        xvar_int <- xvar[i]
      }else if(split_var() == "yvar"){
        yvar_int <- yvar[i]
      }
      
      gate <- NULL
      
      if(show_gates){
        child_gates <- getChildren(rval$gating_set[[1]], selected$gate)
        if(length(child_gates) > 0){
          gate <- child_gates
        }

        #gate <- setdiff(names(rval$gates_flowCore), "root")
          # gates_non_root <- setdiff(getNodes(rval$gating_set), "root")
          # if(length(gates_non_root)>0){
          #   gate <- gates_non_root
          # }
      }
      
      p <- plot_gs(df = update_data_plot_focus(),
                   gs = rval$gating_set, 
                   sample = selected$samples,
                   subset = selected$gate, 
                   spill = rval$spill,
                   xvar = xvar_int, 
                   yvar = yvar_int, 
                   color_var = color_var_int, 
                   gate = gate,
                   polygon_gate = polygon_gate,
                   type = input$plot_type, 
                   bins = ifelse(is.null(input$bin_number), rval_plot_default$bin_number, input$bin_number),
                   alpha = ifelse(is.null(input$alpha), rval_plot_default$alpha, input$alpha),
                   size = ifelse(is.null(input$size), rval_plot_default$size, input$size),
                   norm_density = ifelse(is.null(input$norm), TRUE, input$norm),
                   smooth = ifelse(is.null(input$smooth), FALSE, input$smooth),
                   ridges = ifelse(is.null(input$ridges), FALSE, input$ridges),
                   transformation =  transformation,
                   facet_vars = facet_vars,
                   metadata = rval$pdata,
                   #group_var = input$group_var,
                   yridges_var = input$yridges_var,
                   show.legend = input$legend,
                   axis_labels = axis_labels,
                   legend.position = input$legend_pos,
                   theme_name = paste("theme_", input$theme, sep = ""))
      
      if(!is.null(p)){
        p <- p + xlab(xvar) 
        if(input$plot_type != "histogram"){
          p <- p + ylab(yvar)
        }
      }
      
      plist[[i]] <- p
      
    }
    
    plist
    
  })
  
  
  
  
  return( list(plot = plot_focus, params = rval_plot) )
  
}