#' Build plots from a GatingSet
#' @param id shiny id
#' @import shiny
#' @importFrom shinydashboard box
#' @export
plotCytoUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("ui_update")),
    box(collapsible = TRUE, collapsed = TRUE, width = NULL, height = NULL,
        title = "Sample/Subset",
        selectionInput(ns("selection_module"))
    ),
    box(collapsible = TRUE, collapsed = TRUE, width = NULL, height = NULL,
        title ="Variables",
        uiOutput(ns("plot_variables")),
        uiOutput(ns("ui_pattern"))
    ),
    box(collapsible = TRUE, collapsed = TRUE, width = NULL, height = NULL,
        title ="Options",
        uiOutput(ns("plot_options"))
    )
                 
  )
  
}

#' plotCyto module server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param rval a reactivevalues object with the following elements :
#' \describe{
#'   \item{gating_set}{: a GatingSet object}
#'   \item{apply_trans}{: logical; apply transformations defined in \code{rval$gating_set}}
#'   \item{apply_comp}{: logical; apply compensation defined in \code{rval$gating_set}}
#'}
#' @param plot_params reactivevalues object used to initialize plot parameters. 
#' Amongst others it can contain the following elements (not mandatory):
#' \describe{
#'   \item{plot_type}{: type of plot}
#'   \item{sample}{: names of samples}
#'   \item{subset}{: names of subsets}
#'   \item{xvar}{: x-axis variables}
#'   \item{yvar}{: y-axis variables}
#'   \item{color_var}{: color aesthetic}
#'   \item{group_var}{: group aesthetic}
#'   \item{facet_var}{: variable used to show different plot facets}
#'   \item{split_var}{: variable used to create multiple plots}
#'  }
#' @param simple_plot logical, disable a number of plot options 
#' (such as faceting, multiple plots with different x, y or color variables)
#' @param auto_update Should plot update be automatic? 
#' If FALSE, plot update is controlled by an action button.
#' @param show_gates Should gates with coordinates matching plot coordinates be displayed
#' @param polygon_gate a reactiveValues object with polygon coordinates to be plotted as 
#' an additionnal layer.
#' Must contain elements :
#' \describe{
#'   \item{x}{vector of x coordinates}
#'   \item{y}{vector of y coordinates}
#' }
#' @param use_ggcyto logical, use the ggcyto package to generate plots
#' @return A reactivevalues object with the following elements :
#' \describe{
#'   \item{plot}{a plot or a list of plots}
#'   \item{params}{plot parameters}
#' }
#' @importFrom flowWorkspace gs_pop_get_children gs_pop_get_data sampleNames
#' @importFrom flowCore parameters compensate rectangleGate Subset
#' @import shiny
#' @export
#' @rdname plotCytoUI
plotCyto <- function(input, output, session,
                     rval,
                     plot_params = reactiveValues(),
                     simple_plot = TRUE,
                     auto_update = TRUE,
                     show_gates = FALSE,
                     polygon_gate = NULL,
                     use_ggcyto = FALSE
                     ) {
                          
  
  ### module specific reactiveValues ###############################################################
  rval_mod <- reactiveValues(plot_list = list())
  
  ### Default plot parameters ######################################################################
  rval_plot <- reactiveValues(plot_type = "dots",
                              use_all_cells = FALSE,
                              auto_focus = FALSE,
                              zoom_on_data_points = FALSE,
                              legend.position = "right",
                              theme = "gray",
                              show_gates = FALSE,
                              norm_density = TRUE,
                              smooth = FALSE,
                              ridges = FALSE,
                              yridges_var = "name",
                              bins = 50,
                              alpha = 0.3,
                              size = 0.3,
                              adjust = 1,
                              color_var = "none",
                              group_var = "none",
                              facet_var = "name",
                              split_var = "xvar",
                              show_label = FALSE,
                              show_outliers = FALSE,
                              option = "viridis")
  

  ### Initialization of plot parameters ############################################################
  
  observe({
    rval_plot$xvar <- choices()$plot_var[1]
    
    if(length(choices()$plot_var)>1){
      rval_plot$yvar <- choices()$plot_var[2]
    }else{
      rval_plot$yvar <- choices()$plot_var[1]
    }
    
    idx_xvar <- grep("FSC-A", choices()$plot_var)
    if(length(idx_xvar)>0){
      rval_plot$xvar <- choices()$plot_var[idx_xvar]
    }
    idx_yvar <- grep("SSC-A", choices()$plot_var)
    if(length(idx_yvar)>0){
      rval_plot$yvar <- choices()$plot_var[idx_yvar]
    }
    
    rval_plot$sample <- choices()$sample[1]
    rval_plot$subset <- choices()$subset[1]
  })
  
  ### Control of plot parameters using 'plot_params' ###############################################
  observeEvent(reactiveValuesToList(plot_params), {
    
    for(var in names(rval_input)){
      rval_plot[[var]] <- rval_input[[var]]
    }
    
    for(var in names(plot_params) ){
      rval_plot[[var]] <- plot_params[[var]]
    }

  })

  ### Sample and subset selection module ###########################################################
  
  selected <- callModule(selection, "selection_module", rval, 
                         params = plot_params, multiple_subset = !(simple_plot | use_ggcyto) )

  ### Get parameters from GatingSet ################################################################
  
  choices <- reactive({
    rval$update_gs
    validate(need(class(rval$gating_set) == "GatingSet", "input is not a GatingSet"))
    get_parameters_gs(rval$gating_set)
  })
  
  ### Build UI #####################################################################################
  
  output$ui_update <- renderUI({
    ns <- session$ns
    if(!auto_update){
      tagList(
        actionButton(ns("update_plot"), "update"),
        br(),
        br()
      )
    }
  })
  
  ### Define and initialize plot options ###########################################################
  observe({
    
    ns <- session$ns
    x <- list()
    
    if(use_ggcyto){
      plot_types <- c("dots", "histogram", "contour")
    }else{
      plot_types <- c("dots", "histogram", "contour", "hexagonal")
    }
    x[["plot_type"]] <- selectInput(ns("plot_type"), label = "plot type",
                choices = plot_types,
                selected = rval_plot[["plot_type"]])
    
    x[["use_all_cells"]] <- checkboxInput(ns("use_all_cells"), "Use all cells", rval_plot[["use_all_cells"]])
    
    x[["auto_focus"]] <- checkboxInput(ns("auto_focus"), "Auto-focus", rval_plot[["auto_focus"]])
    
    x[["zoom_on_data_points"]] <- checkboxInput(ns("zoom_on_data_points"), "Zoom on data points", rval_plot[["zoom_on_data_points"]])

    x[["legend.position"]] <- selectInput(ns("legend.position"), label = "legend position",
                choices = c("none", "right", "top", "left", "bottom"),
                selected = rval_plot[["legend.position"]])
    
    x[["theme"]] <- selectInput(ns("theme"), 
                label = "plot theme", 
                choices = c("gray", "light", "minimal", "classic", 
                            "bw", "dark", "void"), 
                selected = rval_plot[["theme"]])
    
    x[["show_gates"]] <- checkboxInput(ns("show_gates"), "show defining gates", 
                                       value = rval_plot[["show_gates"]])
    
    x[["norm_density"]] <- checkboxInput(ns("norm_density"), "normalize (set max to 1)", 
                                         value = rval_plot[["norm_density"]])
    
    x[["smooth"]] <- checkboxInput(ns("smooth"), "smooth", value = rval_plot[["smooth"]])
    
    x[["ridges"]] <- checkboxInput(ns("ridges"), "ridges", value = rval_plot[["ridges"]])
    
    x[["yridges_var"]] <- selectizeInput(ns("yridges_var"),
                                         multiple =FALSE,
                                         label = "y ridges variable", 
                                         choices = c("subset", choices()$meta_var), 
                                         selected = rval_plot[["yridges_var"]])
    
    x[["bins"]] <- numericInput(ns("bins"), label = "number of bins", 
                                value = rval_plot[["bins"]])
    
    x[["alpha"]] <- numericInput(ns("alpha"), label = "alpha", 
                                 value = rval_plot[["alpha"]])
    
    x[["size"]] <- numericInput(ns("size"), label = "size", 
                                value = rval_plot[["size"]])
    
    x[["adjust"]] <- numericInput(ns("adjust"), label = "adjust", 
                                value = rval_plot[["adjust"]])
    
    x[["show_label"]] <- checkboxInput(ns("show_label"), "show labels", 
                                       value = rval_plot[["show_label"]])
    
    x[["show_outliers"]] <- checkboxInput(ns("show_outliers"), "show outliers", 
                                          value = rval_plot[["show_outliers"]])
    
    x[["option"]] <- selectizeInput(ns("option"), 
                                    multiple =FALSE,
                                    label = "color palette", 
                                    choices = c("viridis", "magma", "plasma",  "inferno", "cividis"), 
                                    selected = rval_plot[["option"]])
    
    rval_mod$plot_options <- x

  })
  
  output$plot_options <- renderUI({
    validate(need(rval_input$plot_type, "No plot type selected"))  
    
    if(rval_input$plot_type == 'histogram'){
      vars <- c("norm_density", "smooth", "ridges", "yridges_var",
                "bins","alpha", "size")
    }else if (rval_input$plot_type == 'hexagonal'){
      vars <- c("show_gates", "bins", "option")
    }else if (rval_input$plot_type == 'dots'){
      vars <- c("show_gates","alpha", "size", "show_label", "option")
      if("density" %in% rval_input$color_var){
        vars <- c(vars, "adjust")
      }
    }else if (rval_input$plot_type == 'contour'){
      vars <- c("show_gates", "show_outliers",  "bins", "alpha", "size")
    }
    vars <- c("plot_type", "use_all_cells", "auto_focus", "zoom_on_data_points",
              "legend.position", "theme", vars)
    
    tagList(rval_mod$plot_options[vars])
    
  })
  

  ### Define and initialize plot variables ##########################################################
  
  observeEvent(c(rval_input$plot_type, 
                 rval_plot[["color_var"]], 
                 choices()$meta_var, 
                 choices()$plot_var), {
    
    validate(need(rval_input$plot_type, "No plot type selected"))   
                   
    for(var in names(rval_input)){
      rval_plot[[var]] <- rval_input[[var]]
    }
    
    if(rval_input$plot_type == "histogram"){
      rval_plot$auto_focus <- TRUE
    }
    
                  
    color_var_choices <- switch(rval_input$plot_type,
                                "dots" = c("none", "density", "subset", choices()$meta_var, 
                                           choices()$plot_var),
                                c("none", "subset", 
                                  choices()$meta_var, 
                                  choices()$plot_var[choices()$params$vartype != "numeric"]))
    
    if(use_ggcyto){
      color_var_choices <- switch(rval_input$plot_type,
                                  "dots" = c("none", "density", "subset", choices()$meta_var),
                                  c("none", "subset", choices()$meta_var))
    }
    
    
    if(is.null(rval_plot[["color_var"]])){
      rval_plot[["color_var"]] <- color_var_choices[1]
    }
    
    if(! rval_plot[["color_var"]] %in% color_var_choices ){
      rval_plot[["color_var"]] <- color_var_choices[1]
    }
    
    ns <- session$ns
    rval_mod$plot_variables[["color_var"]] <-  selectizeInput(ns("color_var"),
                                                              multiple = !simple_plot,
                                                              label = "color variable",
                                                              choices = color_var_choices,
                                                              selected = rval_plot[["color_var"]])
  })
  
  observe({
    ns <- session$ns
    
    rval_mod$plot_variables[["xvar"]] <- selectizeInput(ns("xvar"),
                   multiple = !simple_plot,
                   label = "x variable", 
                   choices = choices()$plot_var,
                   selected = rval_plot[["xvar"]])
    
    rval_mod$plot_variables[["yvar"]] <- selectizeInput(ns("yvar"),
                   multiple = !simple_plot,
                   label = "y variable",
                   choices = choices()$plot_var,
                   selected = rval_plot[["yvar"]])
    
    if(use_ggcyto){
      rval_mod$plot_variables[["group_var"]] <- selectizeInput(ns("group_var"),
                                                               multiple = !simple_plot,
                                                               label = "group variable",
                                                               choices = c("none",  choices()$meta_var),
                                                               selected = rval_plot[["group_var"]])

      rval_mod$plot_variables[["facet_var"]] <- selectizeInput(ns("facet_var"),
                                                               multiple =TRUE,
                                                               label = "facet variables",
                                                               choices = c(choices()$meta_var),
                                                               selected = rval_plot[["facet_var"]])
    }else{
      rval_mod$plot_variables[["group_var"]] <- selectizeInput(ns("group_var"), 
                                                               multiple = !simple_plot,
                                                               label = "group variable",
                                                               choices = c("none", "subset", 
                                                                           choices()$meta_var,
                                                                           choices()$plot_var[choices()$params$vartype != "numeric"]),
                                                               selected = rval_plot[["group_var"]])
      
      rval_mod$plot_variables[["facet_var"]] <- selectizeInput(ns("facet_var"),
                                                               multiple =TRUE,
                                                               label = "facet variables",
                                                               choices = c("subset", 
                                                                           choices()$meta_var, 
                                                                           choices()$plot_var[choices()$params$vartype != "numeric"]),
                                                               selected = rval_plot[["facet_var"]])
    }
    
    split_choices <- c("xvar", "yvar", "color_var")
    names(split_choices) <- c("x variable", "y variable", "color variable")
    rval_mod$plot_variables[["split_var"]] <- selectInput(ns("split_var"),
                                         label = "select variable used to split plots",
                                         choices = split_choices,
                                         selected = rval_plot[["split_var"]])
    
  })
  
  output$plot_variables <- renderUI({
    validate(need(rval_input$plot_type, "No plot type selected"))  
    x <- rval_mod$plot_variables
    vars <- names(x)
    hidden_vars <- switch(rval_input$plot_type,
                          "histogram" = "yvar",
                          "hexagonal" = c("color_var", "group_var"),
                          NULL)
    if(simple_plot){
      hidden_vars <- union(hidden_vars, c("split_var"))
    }
    
    vars <- setdiff(vars, hidden_vars)

    tagList(rval_mod$plot_variables[vars])
  })
  
  output$ui_pattern <- renderUI({
    ns <- session$ns
    if(!simple_plot){
      tagList(
        box(title = "Select using a pattern", 
            width = NULL, height = NULL, 
            collapsible = TRUE, collapsed = TRUE,
            patternSelectionInput(ns("pattern_module"))
        )
      )
    }
  })
  
  ### Select plot variables using a pattern #########################################################
  
  choices_pattern <- reactiveValues()
  
  observe({
    
    validate(need(rval_input$plot_type, "No plot type selected"))
    
    choices_color_var <- switch(rval_input$plot_type,
                                "dots" = c("none", "subset", choices()$meta_var, 
                                           choices()$plot_var),
                                c("none", "subset", choices()$meta_var))
    
    for(var_name in c("x variable", "y variable", "color variable")){
      choices_pattern[[var_name]] <- switch(var_name,
                                            "x variable" = choices()$plot_var,
                                            "y variable" = choices()$plot_var,
                                            "color variable" = choices_color_var
      )
    }
    
  })
  
  res_pattern <- callModule(patternSelection, "pattern_module", 
                            choices = choices_pattern)
  
  observe({
    
    if(!is.null(res_pattern$variable)){
      pattern_var <- switch(res_pattern$variable,
                            "x variable" = "xvar",
                            "y variable" = "yvar",
                            "color variable" = "color_var")
      
      updateSelectizeInput(session, 
                           pattern_var, 
                           choices = choices_pattern[[res_pattern$variable]], 
                           selected = res_pattern$values)
    }
  })
  
  ### store plot parameters in rval_input (available for the upstream shiny module) #################
                                  
  rval_input <- reactiveValues()
  
  observe({
    for(var in names(rval_plot)){
      rval_input[[var]] <- rval_plot[[var]]
    }
  })
  
  observe({
    for(var in names(input)){
        rval_input[[var]] <- input[[var]]
    }
    if(!is.null( selected$subset )){
      rval_input$subset <- selected$subset
    }
    if(!is.null( selected$sample )){
      rval_input$sample <- selected$sample
    }
  })

  
  ### Control update of plot data ##################################################################
  
  params_update_data <- reactive({

    #print("update data")

    if(!auto_update){
      input$update_plot
    }else{
      update_params <- c(rval$update_gs,
                         rval_input$sample,
                         rval_input$subset,
                         choices()$params,
                         choices()$metadata,
                         choices()$gates,
                         rval_input$use_all_cells
      )

      update_params <- c(update_params, choices()$compensation, rval$apply_comp)

      update_params
    }
  })
  
  ### Control update of raw plot ###################################################################
  
  params_update_plot_raw <- reactive({

    #print("update_raw")

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
                      "adjust",
                      "norm_density",
                      "smooth",
                      "ridges",
                      "yridges_var",
                      "show_label",
                      "show_outliers",
                      "option",
                      "split_var")
      var_update <- var_update[var_update %in% names(rval_input)]
      for(var in var_update){
        update_params <- c(update_params, rval_input[[var]])
      }
      update_params
    }
  })
  
  ### Control update of formatted plot #############################################################
  
  params_update_plot_format <- reactive({

    #print("update_format")

    if(!auto_update){
      input$update_plot
    }else{
      update_params <- NULL
      var_update <- c("facet_var",
                      "theme",
                      "legend",
                      "legend.position",
                      "option",
                      "auto_focus",
                      "zoom_on_data_points")
      var_update <- var_update[var_update %in% names(rval_input)]
      for(var in var_update){
        update_params <- c(update_params, rval_input[[var]])
      }

      update_params <- c(update_params,
                         choices()$transformation,
                         rval$apply_trans,
                         choices()$labels,
                         choices()$axis_limits)

      if(show_gates){
        update_params <- c(update_params,
                           choices()$gates)
      }
      update_params
    }
  })
  
  ### Get plot data ################################################################################
  
  data_plot_focus <- eventReactive(params_update_data(), {

    #print("data")

    validate(need(class(rval$gating_set) =="GatingSet", "No GatingSet"))
    validate(need(rval_input$sample, "Please select samples"))
    validate(need(all(rval_input$sample %in% choices()$sample),
                  "All samples not found in GatingSet"))
    validate(need(rval_input$subset, "Please select subsets"))
    validate(need(all(rval_input$subset %in% choices()$subset),
                  "All subsets not found in GatingSet"))


    Ncells <- 5e4
    if(!is.null(rval_input$use_all_cells)){
      if(rval_input$use_all_cells){
        Ncells <- NULL
      }
    }


    spill <- choices()$compensation
    if(!is.null(rval$apply_comp)){
      if(!rval$apply_comp){
        spill <- NULL
      }
    }

    vartype <- choices()$params$vartype
    names(vartype) <- choices()$params$name

    df <- get_plot_data(gs = rval$gating_set,
                      sample = rval_input$sample,
                      subset = rval_input$subset,
                      spill = spill,
                      metadata = choices()$metadata,
                      Ncells = Ncells,
                      vartype = vartype)
    return(df)

  })
  
  ### Build raw plot ###############################################################################
  
  plot_raw <- eventReactive(c(params_update_plot_raw(),  data_plot_focus()),{
    if(use_ggcyto){
      return(list())
    }
    #print("raw")
    
    df <- data_plot_focus()
    plot_list <- list()
    
    validate(need(df, "no cells in selection"))
    validate(need(rval_input$xvar %in% choices()$plot_var, "Please select x variable"))
    validate(need(rval_input$plot_type, "Please select plot type"))
    if(!is.null(rval_input$plot_type)){
      if(rval_input$plot_type != "histogram"){
        validate(need(rval_input$yvar %in% choices()$plot_var, "Please select y variable"))
      }
    }
    
    
    plot_args <- reactiveValuesToList(rval_input)
    
    split_variable  <- "xvar"
    if("split_var" %in% names(rval_input)){
      split_variable <- rval_input$split_var
    }
    
    mono_var <- setdiff(c("xvar", "yvar", "color_var"), split_variable)
    
    for(var in mono_var){
      plot_args[[var]] <- rval_input[[var]][1]
    }
    
    plot_args[["transform_function"]] <- "identity"
    transformation <- choices()$transformation
    if(!is.null(rval$apply_trans)){
      if(rval$apply_trans){
        if( ! "identity" %in% transformation[[rval_input$xvar]]$name |
            ! "identity" %in% transformation[[rval_input$yvar]]$name){
          plot_args[["transform_function"]] <- "log"
        }
      }
    }
    
    for(var in rval_input[[split_variable]]){
      
      plot_args[[split_variable]] <- var
      
      plot_list[[var]] <- call_plot_function(data=df,
                                             plot_type = rval_input$plot_type,
                                             plot_args = plot_args)
    }
    
    return(plot_list)
  })
  
  ### Build raw plot using ggcyto #########################################################
  
  plot_raw_ggcyto <- reactive({
      
    if(!use_ggcyto){
      return(list())
    }
    #print("raw ggcyto")
    
    plot_list <- list()
    
    
    validate(need(class(rval$gating_set) =="GatingSet", "No GatingSet"))
    validate(need(rval_input$sample, "Please select samples"))
    validate(need(all(rval_input$sample %in% choices()$sample),
                  "All samples not found in GatingSet"))
    validate(need(rval_input$subset, "Please select subsets"))
    validate(need(all(rval_input$subset %in% choices()$subset),
                  "All subsets not found in GatingSet"))
    validate(need(rval_input$xvar %in% choices()$plot_var, "Please select x variable"))
    validate(need(rval_input$plot_type, "Please select plot type"))
    if(!is.null(rval_input$plot_type)){
      if(rval_input$plot_type != "histogram"){
        validate(need(rval_input$yvar %in% choices()$plot_var, "Please select y variable"))
        validate(need(all(!rval_input$yvar %in% rval_input$xvar), "Please select different x and y variables"))
      }
    }
    
    spill <- choices()$compensation

    if(!is.null(rval$apply_comp)){
      if(!rval$apply_comp){
        spill <- NULL
      }
    }
      
    fs <- gs_get_fs_subset(gs = rval$gating_set[rval_input$sample], 
                           spill =spill,
                           subset = rval_input$subset)

    # gate data based on plot limits
    if(!rval_input$auto_focus){
      
      gate_var <- NULL
      if(!is.null(choices()$axis_limits[[rval_input$xvar]])){
        gate_var <- rval_input$xvar
      }
      
      if(!is.null(rval_input$yvar)){
        if(!is.null(choices()$axis_limits[[rval_input$yvar]])){
          gate_var <- c(gate_var, rval_input$yvar)
        }
      }
      
      if(length(gate_var) > 0 ){
        rectGate <- flowCore::rectangleGate(filterId="focus", choices()$axis_limits[gate_var])
        fs <- flowCore::Subset(fs, rectGate)
      }
      
    }
    
    plot_args <- reactiveValuesToList(rval_input)
    
    split_variable  <- "xvar"
    if("split_var" %in% names(rval_input)){
      split_variable <- rval_input$split_var
    }
    
    mono_var <- setdiff(c("xvar", "yvar", "color_var"), split_variable)
    
    for(var in mono_var){
      plot_args[[var]] <- rval_input[[var]][1]
    }
    
    # set the maximum number of cells to include in plot (default to 30000)
    plot_args[["max_nrow_to_plot"]] <- 3e4
    if(!is.null(rval_input$use_all_cells)){
      if(rval_input$use_all_cells){
        plot_args[["max_nrow_to_plot"]] <- Inf
      }
    }
    
    # set parameter for geom_pointdensity (only for "dots" plot)
    plot_args[["transform_function"]] <- "identity"
    transformation <- choices()$transformation
    if(!is.null(rval$apply_trans)){
      if(rval$apply_trans){
        if( ! "identity" %in% transformation[[rval_input$xvar]]$name |
            ! "identity" %in% transformation[[rval_input$yvar]]$name){
          plot_args[["transform_function"]] <- "log"
        }
      }
    }

    
    for(var in rval_input[[split_variable]]){
      
      plot_args[[split_variable]] <- var
      
      plot_list[[var]] <- call_plot_function(data=fs,
                                             plot_type = rval_input$plot_type,
                                             plot_args = plot_args)
    }
    
    return(plot_list)
    
  })
  
  ### Format plot ##################################################################################

  plot_format <- eventReactive( c(params_update_plot_format(), 
                                  draw_gates()), {
    
    #print("format")
    
    plist <- draw_gates()
      
    axis_labels <- choices()$labels
    
    transformation <- choices()$transformation
    if(!is.null(rval$apply_trans)){
      if(!rval$apply_trans){
        transformation <- list()
      }
    }
     
    options <- reactiveValuesToList(rval_input)
    options$transformation <- transformation
    options$axis_labels <- axis_labels

    if(!rval_input$auto_focus){
      options$axis_limits <- choices()$axis_limits
    }
    
    if(use_ggcyto){
      plist <- lapply( plist, function(p){
        p <- as.ggplot(p)
        return(p)} )
    }
   
    
    plist <- lapply( plist,
                     function(p){
                       
                       if(length(rval_input$subset)==1){
                         options$title <- rval_input$subset
                       }
                       
                       options$axis_limits <- list()
                       if(!rval_input$auto_focus){
                         options$axis_limits <- choices()$axis_limits
                       }
                       if(rval_input$zoom_on_data_points){
                         data_range <- get_plot_data_range(p)
                         options$axis_limits[names(data_range)] <- data_range
                       }
                       print(options$axis_limits)
                       p <- format_plot(p,
                                   options = options)
                       return(p)
                     })
   return(plist)
  })
  
  ### Add gates corresponding to plot coordinates ##################################################
  
  draw_gates <- eventReactive(c(plot_raw(), 
                              plot_raw_ggcyto()), {
    
    if(!use_ggcyto){
      return(plot_raw())
    }else{
      plist <- plot_raw_ggcyto()
    }
    
    validate(need(rval_input$subset, "Please select subsets"))
    validate(need(all(rval_input$subset %in% choices()$subset), 
                  "All subsets not found in GatingSet"))
    
    #print("gate")
    
    gate <- NULL

    if(show_gates){
      if(!is.null(rval_input$subset)){
        if(rval_input$subset %in% choices()$subset){
          child_gates <- flowWorkspace::gs_pop_get_children(rval$gating_set[[1]],
                                                            rval_input$subset)
          if(length(child_gates) > 0){
            gate <- child_gates
          }
        }
      }
    }
    
    plist <- lapply( plist,
                     function(p){
                       if(!is.null(gate)){
                         for(gate_name in setdiff(gate, "root")){
                           gate_int <- flowWorkspace::gs_pop_get_gate(rval$gating_set,
                                                                      gate_name)
                           p <- add_gate_to_plot(p, gate_int)
                         }
                       }
                       return(p)
                     })

    return(plist)
  })
  
  ### Add polygon layer ############################################################################

  draw_polygon <- reactive({

    #print("poly")
 
    polygon <- data.frame(x = polygon_gate$x,
                          y = polygon_gate$y
                          )
    
    plist <- lapply( plot_format(),
                     function(p){
                       if(!is.null(polygon$x)){
                         p <- add_polygon_layer(p, 
                                                polygon = polygon, 
                                                idx_selected = polygon_gate$idx_selected)
                       }
                       if(rval_input$zoom_on_data_points){
                         data_range <- get_plot_data_range(p)
                         xlim <- NULL
                         ylim <- NULL
                         xlim <- data_range[[1]]
                         if(length(data_range)>1){
                           ylim <- data_range[[2]]
                         }
                         p <- p + coord_cartesian(xlim = xlim, ylim = ylim, expand = TRUE)
                       }
                       return(p)
                     })
    
    
  })
  
  return( list(plot =  draw_polygon, params = rval_input) )
  
}

### Tests ##########################################################################################
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
#     dashboardHeader(title = "plotCyto"),
#     sidebar = dashboardSidebar(disable = TRUE),
#     body = dashboardBody(
#       fluidRow(
#         column(4, box(width = NULL, title = "Parameters", collapsible = TRUE, collapsed = TRUE,
#                       plotCytoUI("module"))),
#         column(8, box(width = NULL, simpleDisplayUI("simple_display_module")))
#       )
#     )
#   )
# 
#   server <- function(input, output, session) {
# 
#     rval <- reactiveValues()
#     plot_params <- reactiveValues()
# 
#     observe({
#       #dataDir <- system.file("extdata",package="flowWorkspaceData")
#       #gs <- load_gs(list.files(dataDir, pattern = "gs_bcell_auto",full = TRUE))
#       utils::data("GvHD", package = "flowCore")
#       gs <- GatingSet(GvHD)
#       #data("Bcells")
#       #gs <- GatingSet(Bcells)
#        rval$gating_set <- gs
#       #plot_params$plot_type <- "histogram"
#       #plot_params$xvar <- "cluster"
#       #plot_params$yvar <- "FL4-H"
#       #plot_params$sample <- pData(gs)$name[3]
#       #plot_params$subset <- gs_get_pop_paths(gs)[1]
#       #plot_params$auto_focus <- FALSE
#       #plot_params$plot_type = "histogram"
#       #plot_params$xvar = "FL2-H"
#       #plot_params$option = "magma"
# 
#     })
# 
#     res <- callModule(plotCyto, "module",
#                       rval = rval,
#                       plot_params = plot_params,
#                       simple_plot = FALSE,
#                       show_gates = TRUE,
#                       use_ggcyto = FALSE
#                       )
# 
#     callModule(simpleDisplay, "simple_display_module", res$plot)
# 
#   }
# 
#   shinyApp(ui, server)
# 
# }
  