#' @title plotGatingSetInput2
#' @description  A shiny Module (ui function) to build plots from a GatingSet
#' @param id shiny id
#' @param simple_plot logical, disable a number of plot options
#' @param auto_update Should plot update be automatic? 
#' If FALSE, plot update is controlled by an action button.
#' @import shiny
#' @importFrom shinydashboard box 
plotGatingSetInput <- function(id, simple_plot = TRUE, auto_update = TRUE) {
  
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


#' plotGatingSet2 server function
#' @title plotGatingSet
#' @description  A shiny module (server function) to build plots from a GatingSet
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param rval a reactivevalues object with the following elements :
#' \describe{
#'   \item{gating_set}{: a GatingSet object}
#'}
#' @param plot_params reactivevalues object used to initialize plot parameters 
#' with elements (not mandatory):
#' \describe{
#'   \item{samples}{: initially selected samples}
#'   \item{gate}{: initially selected subsets}
#'  }
#' @param simple_plot logical, disable a number of plot options 
#' (such as faceting, multiple plots with different x, y or color variables)
#' @param auto_update Should plot update be automatic? 
#' If FALSE, plot update is controlled by an action button.
#' @param show_gates Should gates with coordinates matching plot coordinates be displayed
#' @param polygon_gate a reactiveValues object with polygon coordinates to be plotted as an additionnal layer.
#' Must conatain elements :
#' \describe{
#'   \item{x}{vector of x coordinates}
#'   \item{y}{vector of y coordinates}
#' }
#' @return A reactivevalues object with the following elements :
#' \describe{
#'   \item{plot}{a plot or a list of plots}
#'   \item{params}{plot parameters}
#' }
#' @importFrom flowWorkspace gs_pop_get_children gh_pop_get_gate gs_get_pop_paths
#' @import shiny
plotGatingSet <- function(input, output, session, 
                          rval, 
                          plot_params = reactiveValues(), 
                          simple_plot = TRUE,
                          auto_update = TRUE,
                          show_gates = FALSE,
                          polygon_gate = NULL) {
  
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
                                      

  
  rval_mod <- reactiveValues(plot_list = list(), count_raw = 0, count_format = 0)
  
  selected <- callModule(selection, "selection_module", rval, params = plot_params)

  ################################################################################################################
  
  choices <- reactive({
    validate(need(class(rval$gating_set) == "GatingSet", "input is not a GatingSet"))
    
    plot_var <- colnames(rval$gating_set)
    extra_facet_var <- plot_var[plot_var %in% c("cluster", "bin")]
    
    if(length(extra_facet_var) == 0){
      extra_facet_var <- NULL
    }
    
    return( 
      list(samples = pData(rval$gating_set)$name,
           subsets = gs_get_pop_paths(rval$gating_set),
           plot_var = plot_var,
           meta_var = names(pData(rval$gating_set)),
           extra_facet_var = extra_facet_var
      )
    )
  })
  
  # Define and initialize plot options
  observeEvent(c(input$plot_type, choices()$meta_var) , {
    
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
                                         choices = c("subset", choices()$meta_var), 
                                         selected = rval_plot[["yridges_var"]])
    x[["bins"]] <- numericInput(ns("bins"), label = "number of bins", value = rval_plot[["bins"]])
    x[["alpha"]] <- numericInput(ns("alpha"), label = "alpha", value = rval_plot[["alpha"]])
    x[["size"]] <- numericInput(ns("size"), label = "size", value = rval_plot[["size"]])
    x[["show_label"]] <- checkboxInput(ns("show_label"), "show labels", value = rval_plot[["show_label"]])
    x[["show_outliers"]] <- checkboxInput(ns("show_outliers"), "show outliers", value = rval_plot[["show_outliers"]])
    x[["option"]] <- selectizeInput(ns("option"), 
                                    multiple =FALSE,
                                    label = "color palette", 
                                    choices = c("viridis", "magma", "plasma",  "inferno", "cividis"), 
                                    selected = "viridis")
    
    rval_mod$plot_options <- x

  })
  
  output$plot_options <- renderUI({
    x <- rval_mod$plot_options
    if(input$plot_type == 'histogram'){
      vars <- setdiff(names(x), "show_gates")
    }else if (input$plot_type == 'hexagonal'){
      vars <- c("show_gates", "bins", "option")
    }else if (input$plot_type == 'dots'){
      vars <- c("show_gates","alpha", "size", "show_label", "option")
    }else if (input$plot_type == 'contour'){
      vars <- c("show_gates", "show_outliers",  "bins", "alpha", "size")
    }
    
    tagList(rval_mod$plot_options[vars])
    
  })
  
  ################################################################################################################
  # Define and initialize plot variables
  
  observe({
    #validate(need(rval$plot_var, "No plotting variables"))
    updateSelectInput(session, "xvar", choices = choices()$plot_var, selected = choices()$plot_var[1])
    updateSelectInput(session, "yvar", choices = choices()$plot_var, selected = choices()$plot_var[2]) 
  })
  
  observe({
    
    #validate(need(rval$pdata, "No metadata available"))
    #validate(need(rval$plot_var, "No plotting variables"))
    
    ns <- session$ns
    x <- list()
    
    x[["group_var"]] <- selectizeInput(ns("group_var"), 
                                       multiple = !simple_plot,
                                       label = "group variable",
                                       choices = c("none", "subset", choices()$meta_var),
                                       selected = rval_plot[["group_var"]])
    
    
    if(input$plot_type %in% c('histogram', "contour")){
      x[["color_var"]] <- selectizeInput(ns("color_var"), 
                                         multiple = !simple_plot,
                                         label = "color variable",
                                         choices = c("none", "subset", choices()$meta_var),
                                         selected = rval_plot[["color_var"]])
    
    }else{
      x[["color_var"]] <- selectizeInput(ns("color_var"),
                                         multiple = !simple_plot,
                                         label = "color variable",
                                         choices = c("none", "subset", choices()$meta_var, choices()$plot_var),
                                         selected = rval_plot[["color_var"]])
    }
    
    if(!simple_plot){
      

      
        x[["facet_var"]] <- selectizeInput(ns("facet_var"), 
                       multiple =TRUE,
                       label = "facet variables",
                       choices = c("subset", choices()$meta_var, choices()$extra_facet_var),
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
  # Control of plot parameters using 'plot_params'
  
  observe({
    
    for(var in intersect( names(plot_params), c("xvar", 
                                                "yvar", 
                                                "color_var",
                                                "group_var",
                                                "facet_var",
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
  # Select plot variables using a pattern
  
  observeEvent(input$select_var, {
    
    var_name <- switch(input$var_name,
                       "x variable" = "xvar", 
                       "y variable" = "yvar", 
                       "color variable" = "color_var",
                       "group variable" = "group_var")
    
    choices <- choices()$plot_var
    if(var_name == "color_var"){
      choices <- c("none", "subset", choices()$meta_var, choices()$plot_var)
    }
    if(var_name == "group_var"){
      choices <- c("none", "subset", choices()$meta_var)
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
  
  
  ##########################################################################################################3
  # store plot parameters in rval_plot (available for the upstream shiny module)
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

  ##########################################################################################################3
  # Control update of plot data
  
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
                         choices()$plot_var,
                         rval$pdata,
                         rval_plot$use_all_cells
      )
      update_params
    }
  })
  
  ##########################################################################################################3
  # Control update of raw plot
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
                      "show_outliers",
                      "option")
      var_update <- var_update[var_update %in% names(rval_plot)]
      for(var in var_update){
        update_params <- c(update_params, rval_plot[[var]])
      }
      update_params
    }
  })
  
  ##########################################################################################################3
  # Control update of formatted plot
  params_update_plot_format <- reactive({
    
    validate(need("apply_trans" %in% names(rval), "No transformation options defined"))
    
    if(!auto_update){
      input$update_plot
    }else{
      update_params <- rval_mod$count_raw
      var_update <- c("facet_var",
                      "color_var",
                      "theme",
                      "legend",
                      "legend.position",
                      "option")
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
  
  
  ##########################################################################################################3
  # Get plot data
  data_plot_focus <- eventReactive(params_update_data(), {
    
    validate(need(rval$gating_set, "Empty gating set"))
    validate(need(selected$samples, "Please select samples"))
    validate(need(all(selected$samples %in% pData(rval$gating_set)$name), "Samples not found in gating set"))
    validate(need(selected$gate, "Please select subsets"))

    Ncells <- 30000
    if(rval_plot$use_all_cells){
      Ncells <- NULL
    }
    
    spill <- NULL
    df <- get_plot_data(gs = rval$gating_set,
                      sample = selected$samples,
                      subset = selected$gate,
                      spill = spill,
                      metadata = rval$pdata,
                      Ncells = Ncells)
    
    return(df)
    
  })
  
  ##########################################################################################################3
  # Build raw plot
  observeEvent(c(params_update_plot_raw(),  data_plot_focus()), {
    
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
            plot_args[["yvar"]] <- yvar[i]
          }

          rval_mod$plot_list[[i]] <- call_plot_function(df=df,
                                                        plot_type = input$plot_type,
                                                        plot_args = plot_args)
          
    }
    
    rval_mod$count_raw <- rval_mod$count_raw + 1
    
  })
  
  ##########################################################################################################3
  # Format plot
  observeEvent(params_update_plot_format(),  {
    
    print("format")
    
    axis_labels <- rval$parameters$name_long
    if(!is.null(axis_labels)){
      names(axis_labels) <- rval$parameters$name
    }
    
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
                    legend.position = rval_plot[["legend.position"]],
                    option = rval_plot[["option"]])
    
    plist <- lapply( rval_mod$plot_list, 
                     function(p){
                       format_plot(p,
                                   options = options)
                     })
    
    rval_mod$plot_list <- plist
    rval_mod$count_format <- rval_mod$count_format + 1

  })
  
  ##########################################################################################################3
  # Add gates corresponding to plot coordinates
  observeEvent(rval_mod$count_format, {
    
    validate(need(rval$gating_set, "No gating set"))

    gate <- NULL
    
    if(show_gates){
      if(selected$gate %in% flowWorkspace::gs_get_pop_paths(rval$gating_set[[1]])){
        child_gates <- flowWorkspace::gs_pop_get_children(rval$gating_set[[1]], selected$gate)
        if(length(child_gates) > 0){
          gate <- child_gates
        }
      }
    }
    
    plist <- lapply( rval_mod$plot_list,
                     function(p){
                       if(!is.null(gate)){
                         for(gate_name in setdiff(gate, "root")){
                           gate_int <- flowWorkspace::gh_pop_get_gate(rval$gating_set[[1]], gate_name)
                           p <- add_gate(p = p, gate = gate_int)
                         }
                       }
                       return(p)
                     })
    
    rval_mod$plot_list <- plist
    

  })
  
  ##########################################################################################################3
  # Add polygonal plot layer
  plot_gate <- reactive({
    
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