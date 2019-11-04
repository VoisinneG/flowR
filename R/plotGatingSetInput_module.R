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
        uiOutput(ns("plot_variables")),
        if(!simple_plot){
          box(title = "Select using pattern", width = NULL, height = NULL, collapsible = TRUE, collapsed = TRUE,
              textInput(ns("pattern"), "Pattern"),
              checkboxInput(ns("match_as_is"), "use pattern as regular expression", TRUE),
              selectizeInput(ns("var_name"),
                             multiple = FALSE,
                             label = "variable",
                             choices = c("x variable", "y variable", "color variable"),
                             selected = "x variable"),
              actionButton(ns("select_var"), "Select variable")
          )
        }
    ),
    box(collapsible = TRUE, collapsed = TRUE, width = NULL, height = NULL,
        title ="Options",
        selectInput(ns("plot_type"), label = "plot type",
                    choices = c("hexagonal", "histogram", "dots", "contour"),
                    selected = "hexagonal"),
        #checkboxInput(ns("legend"), "show legend", value = TRUE),
        checkboxInput(ns("use_all_cells"), "Use all cells", FALSE),
        selectInput(ns("legend.position"), label = "legend position",
                    choices = c("none", "right", "top", "left", "bottom"),
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
  
  rval_plot <- reactiveValues(show_gates = FALSE,
                                      norm = TRUE,
                                      smooth = FALSE,
                                      ridges = FALSE,
                                      yridges_var = "subset",
                                      bins = 50,
                                      alpha = 0.3,
                                      size = 0.3,
                                      xvar = NULL,
                                      yvar = NULL,
                                      color_var = "none",
                                      group_var = "none",
                                      facet_var = NULL,
                                      split_variable = "x_variable",
                                      show_label = FALSE,
                                      show_outliers = FALSE)
  
                              
  # observeEvent(input$plot_type, {
  #   rval_plot_default$alpha <- switch(input$plot_type,
  #                                     "contour" = 0.75,
  #                                     "dots" = 0.1,
  #                                     0.1)
  #   
  #   rval_plot_default$size <- switch(input$plot_type, 
  #                                     "contour" = 0.2,
  #                                     0.1)
  #   
  #   rval_plot_default$bins <- switch(input$plot_type, 
  #                                    "contour" = 10,
  #                                    100)
  # })
  
  #rval_plot <- reactiveValues()
  
  rval_mod <- reactiveValues(plot_list = list(), count_raw = 0, count_format = 0)
  
  selected <- callModule(selection, "selection_module", rval, params = plot_params)

  observeEvent(c(input$plot_type, names(rval$pdata)) , {
    
    validate(need(rval$pdata, "No metadata available"))
    
    ns <- session$ns
    x <- list()
    
    
      x[["show_gates"]] <- checkboxInput(ns("show_gates"), "show defining gates", value = rval_plot[["show_gates"]])
      x[["norm_density"]] <- checkboxInput(ns("norm_density"), "normalize (set max to 1)", value = rval_plot[["norm"]])
      x[["smooth"]] <- checkboxInput(ns("smooth"), "smooth", value = rval_plot[["smooth"]])
      
      x[["ridges"]] <- checkboxInput(ns("ridges"), "ridges", value = rval_plot[["ridges"]])
      
      x[["yridges_var"]] <- selectizeInput(ns("yridges_var"), 
                                           multiple =FALSE,
                                           label = "y ridges variable", 
                                           choices = c("subset", names(rval$pdata)), 
                                           selected = rval_plot[["yridges_var"]])
      x[["bins"]] <- numericInput(ns("bins"), label = "number of bins", value = rval_plot[["bins"]])
      x[["alpha"]] <- numericInput(ns("alpha"), label = "alpha", value = rval_plot[["alpha"]])
      x[["size"]] <- numericInput(ns("size"), label = "size", value = rval_plot[["size"]])
      x[["show_label"]] <- checkboxInput(ns("show_label"), "show labels", value = rval_plot[["show_label"]])
      x[["show_outliers"]] <- checkboxInput(ns("show_outliers"), "show outliers", value = rval_plot[["show_outliers"]])
      
      rval_mod$plot_options <- x
    
    
    
  })
  
  output$plot_options <- renderUI({
    x <- rval_mod$plot_options
    if(input$plot_type == 'histogram'){
      vars <- setdiff(names(x), "show_gates")
    }else if (input$plot_type == 'hexagonal'){
      vars <- c("show_gates", "bins")
    }else if (input$plot_type == 'dots'){
      vars <- c("show_gates","alpha", "size", "show_label")
    }else if (input$plot_type == 'contour'){
      vars <- c("show_gates", "show_outliers",  "bins", "alpha", "size")
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
    
    x[["group_var"]] <- selectizeInput(ns("group_var"), 
                                       multiple = !simple_plot,
                                       label = "group variable",
                                       choices = c("none", "subset", names(rval$pdata)),
                                       selected = rval_plot[["group_var"]])
    
    
    if(input$plot_type %in% c('histogram', "contour")){
      x[["color_var"]] <- selectizeInput(ns("color_var"), 
                                         multiple = !simple_plot,
                                         label = "color variable",
                                         choices = c("none", "subset", names(rval$pdata)),
                                         selected = rval_plot[["color_var"]])
    
    }else{
      x[["color_var"]] <- selectizeInput(ns("color_var"),
                                         multiple = !simple_plot,
                                         label = "color variable",
                                         choices = c("none", "subset", names(rval$pdata), rval$plot_var),
                                         selected = rval_plot[["color_var"]])
    }
    
    if(!simple_plot){
      
        extra_facet_var <- rval$parameters$name[rval$parameters$name %in% c("cluster", "bin")]
        if(length(extra_facet_var) == 0){
          extra_facet_var <- NULL
        }
      
        x[["facet_var"]] <- selectizeInput(ns("facet_var"), 
                       multiple =TRUE,
                       label = "facet variables",
                       choices = c("subset", names(rval$pdata), extra_facet_var),
                       selected = rval_plot[["facet_var"]]
        )
        x[["split_variable"]] <- selectInput(ns("split_variable"),
                    label = "select variable used to split plots",
                    choices = c("x variable", "y variable", "color variable"),
                    selected = rval_plot[["split_variable"]]
        )
    }

    rval_mod$plot_variables <- x
    
  })
  
  output$plot_variables <- renderUI({
    x <- rval_mod$plot_variables
    if(input$plot_type == 'histogram'){
      vars <- names(x)
    }else if (input$plot_type == 'hexagonal'){
      vars <- setdiff(names(x), c("color_var", "group_var"))
    }else if (input$plot_type == 'dots'){
      vars <- names(x)
    }else if (input$plot_type == 'contour'){
      vars <- names(x)
    }
    tagList(rval_mod$plot_variables[vars])
  })
  
  ######################################################################################
  #Initialization of plot parameters
  observe({
    
    #print(names(plot_params))
    for(var in intersect( names(plot_params), c("xvar", 
                                                "yvar", 
                                                "color_var", 
                                                "group_var",  
                                                "gate", 
                                                "samples", 
                                                "plot_type") )){
      if(!is.null(plot_params[[var]])){
          if(plot_params[[var]]!=""){
            updateSelectInput(session, var, selected = plot_params[[var]])
          }
      }else{
        updateSelectInput(session, var, selected = plot_params[[var]])
      }
    }
    
    if("use_all_cells" %in% names(plot_params)){
      updateCheckboxInput(session, "use_all_cells", value = plot_params[["use_all_cells"]])
    }
    
  })
  ######################################################################################
  
  
  observeEvent(input$select_var, {
    
    var_name <- switch(input$"var_name",
                       "x variable" = "xvar", 
                       "y variable" = "yvar", 
                       "color variable" = "color_var",
                       "group variable" = "group_var")
    
    choices <- rval$plot_var
    if(var_name == "color_variable"){
      choices <- c("none", "subset", names(rval$pdata), rval$plot_var)
    }
    if(var_name == "group_variable"){
      choices <- c("none", "subset", names(rval$pdata))
    }
    
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
  
  observe({
    for(var in names(input)){
        rval_plot[[var]] <- input[[var]]
    }
    rval_plot$gate <- selected$gate
    rval_plot$samples <- selected$samples
  })

  
  split_var <- reactive({
    if("split_variable" %in% names(input)){
      switch(input$split_variable, 
             "x variable" = "xvar",
             "y variable" = "yvar",
             "color variable" = "color_var",
             "group variable" = "group_var"
      )
    }else{
      "xvar"
    }
    
  })

  params_update_data <- reactive({
    if(!auto_update){
      input$update_plot
    }else{
      update_params <- c(selected$samples,
                         selected$gate,
                         rval$spill,
                         rval$flow_set_selected,
                         # rval$gating_set,
                         # rval$parameters,
                         rval$gates_flowCore,
                         rval$plot_var,
                         rval$pdata,
                         rval_plot$use_all_cells
      )
      update_params
    }
  })
  
  params_update_plot_raw <- reactive({
    if(!auto_update){
      input$update_plot
    }else{
      update_params <- NULL
      var_update <- c("plot_type",
                      "xvar",
                      "yvar",
                      "color_var",
                      "group_var",
                      "bins",
                      "size", 
                      "alpha", 
                      "norm", 
                      "smooth", 
                      "ridges", 
                      "yridges_var",
                      "show_label",
                      "show_outliers")
      var_update <- var_update[var_update %in% names(rval_plot)]
      for(var in var_update){
        update_params <- c(update_params, rval_plot[[var]])
      }
      update_params
    }
  })
  
  params_update_plot_format <- reactive({
    
    validate(need(rval$apply_trans, "No transformation options defined"))
    
    if(!auto_update){
      input$update_plot
    }else{
      update_params <- rval_mod$count_raw
      var_update <- c("facet_var", 
                      "theme",
                      "legend",
                      "legend.position")
      var_update <- var_update[var_update %in% names(rval_plot)]
      for(var in var_update){
        update_params <- c(update_params, rval_plot[[var]])
      }
      
      update_params <- c(update_params,
                         rval$apply_trans)
      
      if(rval$apply_trans){
        update_params <- c(update_params, rval$transformation)
      }
      
      if(show_gates){
        update_params <- c(update_params, 
                           rval$gates_flowCore)
      }
      update_params
    }
  })
  
  
  data_plot_focus <- eventReactive(params_update_data(), {
    
    print("data")
    
    validate(
      need(rval$gating_set, "Empty gating set") %then%
        need(selected$samples, "Please select samples") %then%
        need(all(selected$samples %in% pData(rval$gating_set)$name), "Samples not found in gating set") %then%
        need(selected$gate, "Please select subsets")
    )
    
    Ncells <- 30000
    if(rval_plot$use_all_cells){
      Ncells <- NULL
    }
    
    df <- get_plot_data(gs = rval$gating_set,
                      sample = selected$samples,
                      subset = selected$gate,
                      spill = rval$spill,
                      metadata = rval$pdata,
                      Ncells = Ncells)
    
    print(names(df))
    
    return(df)
    
  })
  
  observeEvent(c(params_update_plot_raw(),  data_plot_focus()), {
    
    print("call plot")
    df <- data_plot_focus()

    idx_x <- match(input$xvar, rval$parameters$name_long)
    xvar <- rval$parameters$name[idx_x]
    
    idx_y <- match(input$yvar, rval$parameters$name_long)
    yvar <- rval$parameters$name[idx_y]
    
    validate(need(xvar, "x variable not available"))
    validate(need(yvar, "y variable not available"))
    
    color_var <- input$color_var
    group_var <- input$group_var

    
    if(!is.null(input$color_var)){
      for(i in 1:length(input$color_var)){
        if(input$color_var[i] %in% rval$parameters$name_long){
          color_var[i] <- rval$parameters$name[match(input$color_var[i], rval$parameters$name_long)]
        }else{
          color_var[i] <- input$color_var[i]
        }
      }
    }
    
    rval_mod$plot_list <- list()
    
    plot_args <- list()
    for(var in names(rval_plot) ){
      plot_args[[var]] <- rval_plot[[var]]
    }
    
    for(i in 1:length(input[[split_var()]])){

          plot_args[["color_var"]] <- color_var[1]
          plot_args[["group_var"]] <- group_var[1]
          plot_args[["xvar"]] <- xvar[1]
          plot_args[["yvar"]] <- yvar[1]

          if(split_var() == "color_var"){
            plot_args[["color_var"]] <- color_var[i]
          }else if(split_var() == "group_var"){
            plot_args[["group_var"]] <- group_var[i]
          }else if(split_var() == "xvar"){
            plot_args[["xvar"]] <- xvar[i]
          }else if(split_var() == "yvar"){
            plot_args[["yvar"]] <- xvar[i]
          }
    
          #args <- reactiveValuesToList(rval_plot)
          
         
          
          rval_mod$plot_list[[i]] <- call_plot_function(df=df,
                                                        plot_type = input$plot_type,
                                                        plot_args = plot_args)
                                     
          
          
    }
    
    rval_mod$count_raw <- rval_mod$count_raw + 1
    
  })
  
  observeEvent(params_update_plot_format(),  {
    
    print("format_plot")
    
    axis_labels <- rval$parameters$name_long
    names(axis_labels) <- rval$parameters$name
    
    transformation <- NULL
    if(rval$apply_trans){
      transformation <- rval$transformation
    }
     
    if(!simple_plot){
      facet_var <- rval_plot$facet_var
    }else{
      facet_var <- NULL
    }
    
    options <- list(theme = rval_plot$theme,
                    transformation = transformation,
                    axis_labels = axis_labels,
                    facet_var = facet_var,
                    legend.position = rval_plot[["legend.position"]])
    
    plist <- lapply( rval_mod$plot_list, 
                     function(p){
                       format_plot(p,
                                   options = options)
                     })
    
    rval_mod$plot_list <- plist
    rval_mod$count_format <- rval_mod$count_format + 1

  })
  
  observeEvent(rval_mod$count_format, {
    
    validate(need(rval$gating_set, "No gating set"))
    
    print("show gate")
    gate <- NULL
    
    if(show_gates){
      if(selected$gate %in% getNodes(rval$gating_set[[1]])){
        child_gates <- getChildren(rval$gating_set[[1]], selected$gate)
        if(length(child_gates) > 0){
          gate <- child_gates
        }
      }
    }
    
    plist <- lapply( rval_mod$plot_list,
                     function(p){
                       if(!is.null(gate)){
                         for(gate_name in setdiff(gate, "root")){
                           gate_int <- getGate(rval$gating_set[[1]], gate_name)
                           p <- add_gate(p = p, gate = gate_int)
                         }
                       }
                       return(p)
                     })
    
    rval_mod$plot_list <- plist
    
    
  })
  
  plot_gate <- reactive({
    
    print("plot polygon gate")
    
      gate <- NULL
      if(!is.null(rval_plot$show_gates)){
        if(rval_plot$show_gates){
          gate <- selected$gate
        }
      }
      
      polygon <- data.frame(x = polygon_gate$x, 
                            y = polygon_gate$y)
    
      plist <- lapply(rval_mod$plot_list,
                      function(p){
                        if(!is.null(polygon$x)){
                         
                          p <- add_polygon_layer(p, polygon = polygon)
                        }
                        
                        if(!is.null(gate)){
                          for(gate_name in setdiff(gate, "root")){
                            g <- rval$gates_flowCore[[gate_name]]$gate
                            p <- add_gate(p, g)
                          }
                        }
                        return(p)
                      })
      plist
      
  })
  
  return( list(plot = plot_gate, params = rval_plot) )
  
}