#' @title plotGatingSetInput and plotGatingSet
#' @description  A shiny Module that deals with metadata
#' @param id shiny id
#' @importFrom shinydashboard box tabBox
#' @import shiny
#' @import DT
plotGatingSetInput <- function(id, simple_plot = TRUE) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    if(!simple_plot){
      tagList(
        actionButton(ns("update_plot"), "update"),
        br(),
        br()
      )
    },
    box(collapsible = TRUE, collapsed = TRUE, width = NULL, height = NULL,
        title = "Sample/Subset",
        selectionInput(ns("selection_module"), multiple_subset = !simple_plot)
        # checkboxInput(ns("all_samples"), "Select all samples", FALSE),
        # selectizeInput(ns("samples"), 
        #                label = "samples",
        #                choices = NULL,
        #                selected = NULL,
        #                multiple = TRUE),
        # selectizeInput(ns("gate"), 
        #                label = "subset",
        #                choices = "root",
        #                selected = "root",
        #                multiple = !simple_plot)
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
        selectizeInput(ns("color_var"), 
                       multiple = !simple_plot,
                       label = "color variable",
                       choices = "none",
                       selected = "none"),
        if(!simple_plot){
          tagList(
            selectizeInput(ns("facet_var"), 
                           multiple =TRUE,
                           label = "facet variables",
                           choices = "name",
                           selected = "name"
            ),
            selectInput(ns("split_variable"),
                        label = "select variable used to split plots",
                        choices = c("x variable", "y variable", "color variable"),
                        selected = "x variable"
            )
          )
        }
    ),
    box(collapsible = TRUE, collapsed = TRUE, width = NULL, height = NULL,
        title ="Options",
        selectInput(ns("plot_type"), label = "plot type",
                    choices = c("hexagonal", "histogram", "dots", "contour"),
                    selected = "histogram"),
        checkboxInput(ns("legend"), "show legend", value = TRUE),
        uiOutput(ns("histo_options")),
        selectInput(ns("legend_pos"), label = "legend position",
                    choices = c("right", "top", "left", "bottom"),
                    selected = "right"),
        numericInput(ns("bin_number"), label = "number of bins", value = 50),
        numericInput(ns("alpha"), label = "alpha", value = 0.5),
        numericInput(ns("size"), label = "size", value = 1)
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
                          show_gates = FALSE,
                          polygon_gate = NULL) {
  
  `%then%` <- shiny:::`%OR%`
  #ns <- session$ns
  
  rval_plot <- reactiveValues()
  
  selected <- callModule(selection, "selection_module", rval)
  
  observe({
    for(var in names(input)){
      rval_plot[[var]] <- input[[var]]
    }
    rval_plot$gate <- selected$gate
    rval_plot$samples <- selected$samples
  })
  
  output$histo_options <- renderUI({
    
    ns <- session$ns
    x <- list()
    if(input$plot_type == 'histogram'){
      x[[1]] <- checkboxInput(ns("norm"), "normalize (set max to 1)", value = TRUE)
      x[[2]] <- checkboxInput(ns("smooth"), "smooth", value = FALSE)
      
      x[[3]] <- checkboxInput(ns("ridges"), "ridges", value = FALSE)
      
      x[[4]] <- selectizeInput(ns("yridges_var"), 
                                   multiple =FALSE,
                                   label = "y ridges variable", 
                                   choices = c("name","subset"), 
                                   selected = "subset")
    }
    tagList(x)
  })
  
  
  observe({
    
    validate(
      need(rval$pdata, "No metadata available")
    )
    validate(
      need(rval$plot_var, "No plotting variables")
    )
    
    facet_var_default <- "name"
    color_var_default <- "subset"
    plot_type_default <- "hexagonal"
    
    if("facet_var" %in% names(plot_params)){
      facet_var_default <- plot_params$facet_var
    }
    if("color_var" %in% names(plot_params)){
      color_var_default <- plot_params$color_var
    }
    if("plot_type" %in% names(plot_params)){
      plot_type_default <- plot_params$plot_type
    }
    
    
    updateSelectInput(session, "plot_type", selected = plot_type_default)

    updateSelectInput(session, "yridges_var", 
                      choices = c("subset", names(rval$pdata)), 
                      selected = color_var_default)
    
    updateSelectInput(session, "color_var", 
                      choices = c("subset", names(rval$pdata), rval$plot_var), 
                      selected = color_var_default)
    
    if(!simple_plot){
      updateSelectInput(session, "facet_var", 
                        choices = c("subset", names(rval$pdata)), 
                        selected = facet_var_default )
      
    }
    
  })
  
 
  observe({
    
    validate(
      need(rval$plot_var, "No plotting parameters")
    )
    
    print(rval$plot_var)

    xvar_default <- rval$plot_var[1]
    yvar_default <- rval$plot_var[2]
    
    if("xvar" %in% names(plot_params)){
      xvar_default <- plot_params$xvar
    }
    if("yvar" %in% names(plot_params)){
      yvar_default <- plot_params$yvar
    }
    
    updateSelectInput(session, "xvar", choices = rval$plot_var, selected = xvar_default)
    updateSelectInput(session, "yvar", choices = rval$plot_var, selected = yvar_default)
    
  })
  
  # observe({
  #   rval_plot$xvar <- input$xvar
  #   rval_plot$yvar <- input$yvar
  #   rval_plot$gate <- input$gate
  #   rval_plot$samples <- input$samples
  # })
  
  # observe({
  #   validate(need(rval$gates_flowCore, "no gating set"))
  #   updateSelectInput(session, "gate", choices = union("root", names(rval$gates_flowCore)), selected = "root")
  # })
  # 
  # observe({
  #   if("gate" %in% names(plot_params)){
  #     updateSelectInput(session, "gate", choices = union("root", names(rval$gates_flowCore)), selected = plot_params$gate)
  #   }
  # })
  # 
  # observe({
  #   updateSelectInput(session, "samples", choices = rval$pdata$name, selected = rval$pdata$name[1])
  # })
  # 
  # observeEvent(input$all_samples, {
  #   updateSelectInput(session, "samples", choices = rval$pdata$name, selected = rval$pdata$name)
  # })
  
  split_var <- reactive({
    if(!simple_plot){
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
    if(!simple_plot){
      input$update_plot
    }else{
      update_params <- c(input$xvar,
                         input$yvar,
                         input$color_var,
                         selected$samples,
                         selected$gate,
                         input$plot_type,
                         input$legend,
                         input$legend_pos,
                         input$bin_number,
                         input$size,
                         input$alpha,
                         input$norm,
                         input$smooth,
                         input$ridges,
                         input$yridges_var,
                         rval$apply_trans,
                         rval$flow_set,
                         rval$gating_set,
                         rval$spill,
                         rval$parameters,
                         split_var())
      if(rval$apply_trans){
        update_params <- c(update_params, rval$transformation)
      }
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
    
    print("get data plot_focus")
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
        need(input$xvar, "Please select a x-axis variable")  %then%
        need(input$yvar, "Please select a y-axis variable")
    )
    
    print("gates print")
    print(getNodes(rval$gating_set))
    print(names(rval$gates_flowCore))
    
    idx_x <- match(input$xvar, rval$parameters$name_long)
    idx_y <- match(input$yvar, rval$parameters$name_long)
    xvar <- rval$parameters$name[idx_x]
    yvar <- rval$parameters$name[idx_y]
    
    
    
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
        gate <- setdiff(names(rval$gates_flowCore), "root")
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
                   bins = input$bin_number,
                   alpha = input$alpha,
                   size = input$size,
                   norm_density = input$norm,
                   smooth = input$smooth,
                   ridges = input$ridges,
                   transformation =  transformation,
                   facet_vars = facet_vars,
                   #group_var = input$group_var,
                   yridges_var = input$yridges_var,
                   show.legend = input$legend,
                   axis_labels = axis_labels,
                   legend.position = input$legend_pos)
      
      if(!is.null(p)){
        p <- p + xlab(input$xvar) 
        if(input$plot_type != "histogram"){
          p <- p + ylab(input$yvar)
        }
      }
      
      plist[[i]] <- p
      
    }
    
    plist
    
  })
  
  return( list(plot = plot_focus, params = rval_plot) )
  
}