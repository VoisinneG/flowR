#' Build plots from a GatingSet
#' @param id shiny id
#' @import shiny
#' @importFrom shinydashboard box
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
#'     dashboardHeader(title = "plotGatingSet"),
#'     sidebar = dashboardSidebar(disable = TRUE),
#'     body = dashboardBody(
#'       fluidRow(
#'         column(4, box(width = NULL, plotGatingSetInput("module"))),
#'         column(8, box(width = NULL, simpleDisplayUI("simple_display_module")))
#'       )
#'     )
#'   )
#'   
#'   server <- function(input, output, session) {
#'     
#'     rval <- reactiveValues()
#'     plot_params <- reactiveValues()
#'     
#'     observe({
#'       utils::data("GvHD", package = "flowCore")
#'       rval$gating_set <- GatingSet(GvHD)
#'       plot_params$plot_type <- "histogram"
#'     })
#'     
#'     res <- callModule(plotGatingSet, "module",
#'                       rval = rval,
#'                       plot_params = plot_params)
#'     
#'     callModule(simpleDisplay, "simple_display_module", res$plot)
#'     
#'   }
#'   
#'   shinyApp(ui, server)
#'   
#' }}
plotGatingSetInput <- function(id) {
  
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
        selectInput(ns("plot_type"), label = "plot type",
                    choices = c("hexagonal", "histogram", "dots", "contour"),
                    selected = "hexagonal"),
        checkboxInput(ns("use_all_cells"), "Use all cells", FALSE),
        checkboxInput(ns("auto_focus"), "Auto-focus", FALSE),
        selectInput(ns("legend.position"), label = "legend position",
                    choices = c("none", "right", "top", "left", "bottom"),
                    selected = "right"),
        selectInput(ns("theme"), 
                    label = "plot theme", 
                    choices = c("gray", "light", "minimal", "classic", 
                                "bw", "dark", "void"), 
                    selected = "gray"),
        uiOutput(ns("plot_options"))
    )
                 
  )
  
}


#' plotGatingSet module server function
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
#' @return A reactivevalues object with the following elements :
#' \describe{
#'   \item{plot}{a plot or a list of plots}
#'   \item{params}{plot parameters}
#' }
#' @importFrom flowWorkspace gs_pop_get_children gh_pop_get_gate gs_get_pop_paths pData
#' @importFrom flowCore parameters
#' @import shiny
#' @export
#' @rdname plotGatingSetInput
plotGatingSet <- function(input, output, session,
                          rval,
                          plot_params = reactiveValues(),
                          simple_plot = TRUE,
                          auto_update = TRUE,
                          show_gates = FALSE,
                          polygon_gate = NULL) {
  
  # module specific reactiveValues
  rval_mod <- reactiveValues(plot_list = list(), 
                             count_raw = 0, count_format = 0, count_gate = 0)
  
  # rval_plot stores default values
  rval_plot <- reactiveValues(plot_type = "hexagonal",
                              use_all_cells = FALSE,
                              auto_focus = FALSE,
                              legend.position = "right",
                              theme = "gray",
                              show_gates = FALSE,
                              norm = TRUE,
                              smooth = FALSE,
                              ridges = FALSE,
                              yridges_var = "subset",
                              bins = 50,
                              alpha = 0.3,
                              size = 0.3,
                              color_var = "none",
                              group_var = "none",
                              facet_var = NULL,
                              split_var = "xvar",
                              show_label = FALSE,
                              show_outliers = FALSE)
  
  ######################################################################################
  # Initialization of plot parameters
  
  # sample and subset
  
  # observe({
  #   rval_plot$sample <- choices()$sample[1]
  #   rval_plot$subset <- choices()$subset[1]
  #   
  #   if("sample" %in% names(plot_params)){
  #     rval_plot$sample <- plot_params$sample
  #   }
  #   if("subset" %in% names(plot_params)){
  #     rval_plot$subset <- plot_params$subset
  #   }
  # })
  
  #xvar and yvar
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
    
    
  })
  
  #other variables
  observeEvent(reactiveValuesToList(plot_params), {
    
    print("update rval_plot")
    
    for(var in names(rval_input)){
      rval_plot[[var]] <- rval_input[[var]]
    }
    for(var in names(plot_params) ){
      rval_plot[[var]] <- plot_params[[var]]
    }
  })
  
  # observeEvent(plot_params[["plot_type"]], {
  #   if("plot_type" %in% names(plot_params)){
  #     updateCheckboxInput(session, "plot_type", value = plot_params[["plot_type"]])
  #     rval_plot[["plot_type"]] <- plot_params[["plot_type"]]
  #   }
  # })
  # observeEvent(plot_params[["use_all_cells"]], {
  #   if("use_all_cells" %in% names(plot_params)){
  #     updateCheckboxInput(session, "use_all_cells", value = plot_params[["use_all_cells"]])
  #     rval_plot[["use_all_cells"]] <- plot_params[["use_all_cells"]]
  #   }
  # })
  # observeEvent(plot_params[["auto_focus"]], {
  #   if("auto_focus" %in% names(plot_params)){
  #     updateCheckboxInput(session, "auto_focus", value = plot_params[["auto_focus"]])
  #     rval_plot[["auto_focus"]] <- plot_params[["auto_focus"]]
  #   }
  # })
  
  ######################################################################################
  # Sample and subset selection module
  
  selected <- callModule(selection, "selection_module", rval, 
                         params = plot_params, multiple_subset = !simple_plot)

  ######################################################################################
  # get parameters from GatingSet
  choices <- reactive({
    rval$update_gs
    validate(need(class(rval$gating_set) == "GatingSet", "input is not a GatingSet"))
    
    plot_var <- parameters(rval$gating_set@data[[1]])$name
    
    validate(need(length(plot_var)>0, "No variables in GatingSet"))
    
    desc <- parameters(rval$gating_set@data[[1]])$desc
    minRange <- parameters(rval$gating_set@data[[1]])@data$minRange
    maxRange <- parameters(rval$gating_set@data[[1]])@data$maxRange
    
    axis_limits <- lapply(1:length(plot_var), function(x){
      return(as.numeric(c(minRange[x], maxRange[x])))})
    
    names(axis_limits) <- plot_var
      
    labels <- sapply(1:length(plot_var), function(x){
      if(is.na(desc[x])){
        plot_var[x]
      }else{
        paste(plot_var[x], "(", desc[x], ")")
      }
    })
    names(plot_var) <- labels
    names(labels) <- plot_var 
    
    extra_facet_var <- plot_var[plot_var %in% c("cluster", "bin")]
    
    if(length(extra_facet_var) == 0){
      extra_facet_var <- NULL
    }
    
    return( 
      list(sample = pData(rval$gating_set)$name,
           subset = gs_get_pop_paths(rval$gating_set),
           plot_var = plot_var,
           labels = labels,
           axis_limits = axis_limits,
           metadata = pData(rval$gating_set),
           parameters = parameters(rval$gating_set@data[[1]]),
           meta_var = names(pData(rval$gating_set)),
           extra_facet_var = extra_facet_var,
           transformation = rval$gating_set@transformation,
           compensation = rval$gating_set@compensation,
           gates = get_gates_from_gs(rval$gating_set)
      )
    )
  })
  
  ######################################################################################
  #                               Build UI
  ######################################################################################
  
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
  
  ######################################################################################
  # Define and initialize plot options
  observe({
    
    ns <- session$ns
    x <- list()
    
    x[["plot_type"]] <- selectInput(ns("plot_type"), label = "plot type",
                choices = c("hexagonal", "histogram", "dots", "contour"),
                selected = rval_plot[["plot_type"]])
    
    x[["use_all_cells"]] <- checkboxInput(ns("use_all_cells"), "Use all cells", rval_plot[["use_all_cells"]])
    
    x[["auto_focus"]] <- checkboxInput(ns("auto_focus"), "Auto-focus", rval_plot[["auto_focus"]])
    
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
                                         value = rval_plot[["norm"]])
    
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
    
    x[["show_label"]] <- checkboxInput(ns("show_label"), "show labels", 
                                       value = rval_plot[["show_label"]])
    
    x[["show_outliers"]] <- checkboxInput(ns("show_outliers"), "show outliers", 
                                          value = rval_plot[["show_outliers"]])
    
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
    vars <- c("plot_type", "use_all_cells", "auto_focus", "legend.position", "theme", vars)
    
    tagList(rval_mod$plot_options[vars])
    
  })
  
  ######################################################################################
  # Define and initialize plot variables
  
  
  observeEvent(input$plot_type, {
    
    for(var in names(rval_input)){
      rval_plot[[var]] <- rval_input[[var]]
    }
    
    ns <- session$ns
    color_var_choices <- switch(input$plot_type,
                                "dots" = c("none", "subset", choices()$meta_var, 
                                           choices()$plot_var),
                                c("none", "subset", choices()$meta_var))
    
    rval_mod$plot_variables[["color_var"]] <-  selectizeInput(ns("color_var"),
                                                              multiple = !simple_plot,
                                                              label = "color variable",
                                                              choices = color_var_choices,
                                                              selected = rval_plot[["color_var"]])
    
  })
  
  observe({
  
    # for(var in names(rval_input)){
    #   rval_plot[[var]] <- rval_input[[var]]
    # }
    
    # validate(need(input$plot_type, "Not plot type selected"))
                   
    ns <- session$ns
    
    # color_var_choices <- switch(input$plot_type,
    #                             "dots" = c("none", "subset", choices()$meta_var, 
    #                                        choices()$plot_var),
    #                             c("none", "subset", choices()$meta_var))
    
    #x <- list()
    
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
    
    rval_mod$plot_variables[["group_var"]] <- selectizeInput(ns("group_var"), 
                                       multiple = !simple_plot,
                                       label = "group variable",
                                       choices = c("none", "subset", choices()$meta_var),
                                       selected = rval_plot[["group_var"]])
    
    # x[["color_var"]] <- selectizeInput(ns("color_var"),
    #                                    multiple = !simple_plot,
    #                                    label = "color variable",
    #                                    choices = color_var_choices,
    #                                    selected = rval_input[["color_var"]])
    
    rval_mod$plot_variables[["facet_var"]] <- selectizeInput(ns("facet_var"),
                                       multiple =TRUE,
                                       label = "facet variables",
                                       choices = c("subset", 
                                                   choices()$meta_var, 
                                                   choices()$extra_facet_var),
                                       selected = rval_plot[["facet_var"]]
    )
    
    split_choices <- c("xvar", "yvar", "color_var")
    names(split_choices) <- c("x variable", "y variable", "color variable")
    rval_mod$plot_variables[["split_var"]] <- selectInput(ns("split_var"),
                                         label = "select variable used to split plots",
                                         choices = split_choices,
                                         selected = rval_plot[["split_var"]]
    )

    #rval_mod$plot_variables <- x
    
  })
  
  output$plot_variables <- renderUI({
    
    x <- rval_mod$plot_variables
    vars <- names(x)
    hidden_vars <- switch(input$plot_type,
                          "histogram" = "yvar",
                          "hexagonal" = c("color_var", "group_var"),
                          NULL)
    if(simple_plot){
      hidden_vars <- union(hidden_vars, c("facet_var", "split_var"))
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
  
  ###############################   End Build UI  #####################################

  ######################################################################################
  # Select plot variables using a pattern
  
  
  choices_pattern <- reactiveValues()
  
  observe({
    
    validate(need(input$plot_type, "Not plot type selected"))
    
    choices_color_var <- switch(input$plot_type,
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
  
  ######################################################################################
  # store plot parameters in rval_input (available for the upstream shiny module)
                                  
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

  ######################################################################################
  #                               Update plot
  ######################################################################################
  
  
  ######################################################################################
  # Control update of plot data
  
  params_update_data <- reactive({
    #print("update_data")
    if(!auto_update){
      input$update_plot
    }else{
      update_params <- c(rval_input$sample,
                         rval_input$subset,
                         #rval$spill,
                         #rval$flow_set_selected,
                         # rval$gating_set,
                         # rval$parameters,
                         #rval$gates_flowCore,
                         choices()$parameters,
                         choices()$metadata,
                         choices()$gates,
                         #rval$pdata,
                         rval_input$use_all_cells
      )
      
      update_params <- c(update_params, choices()$compensation, rval$apply_comp)

      update_params
    }
  })
  
  ######################################################################################3
  # Control update of raw plot
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
                      "norm", 
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
  
  ######################################################################################3
  # Control update of formatted plot
  params_update_plot_format <- reactive({
    #print("update_format")
    
    if(!auto_update){
      input$update_plot
    }else{
      update_params <- rval_mod$count_raw
      var_update <- c("facet_var",
                      "color_var",
                      "theme",
                      "legend",
                      "legend.position",
                      "option",
                      "auto_focus")
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
  
  
  ######################################################################################3
  # Get plot data
  data_plot_focus <- eventReactive(params_update_data(), {
    
    #print("data")
    
    validate(need(rval$gating_set, "Empty GatingSet"))
    validate(need(rval_input$sample, "Please select samples"))
    validate(need(all(rval_input$sample %in% pData(rval$gating_set)$name), 
                  "All samples not found in GatingSet"))
    validate(need(rval_input$subset, "Please select subsets"))
    validate(need(all(rval_input$subset %in% gs_get_pop_paths(rval$gating_set)), 
                  "All subsets not found in GatingSet"))
   
    
    Ncells <- 30000
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
    
    df <- get_plot_data(gs = rval$gating_set,
                      sample = rval_input$sample,
                      subset = rval_input$subset,
                      spill = spill,
                      metadata = choices()$metadata,
                      Ncells = Ncells)
    
    return(df)
    
  })
  
  ######################################################################################3
  # Build raw plot
  observeEvent(c(params_update_plot_raw(),  data_plot_focus()), {
    
    #print("raw")
    
    df <- data_plot_focus()
    rval_mod$plot_list <- list()
    
    validate(need(rval_input$xvar, "Please select x variable"))
    validate(need(input$plot_type, "Please select plot type"))
    
    if(input$plot_type != "histogram"){
      validate(need(rval_input$yvar, "Please select y variable"))
    }
    
    plot_args <- reactiveValuesToList(rval_input)

    split_variable  <- "xvar"
    if("split_var" %in% names(rval_input)){
      split_variable <- rval_input$split_var
    }

    for(var in rval_input[[split_variable]]){
          
          plot_args[[split_variable]] <- var

          rval_mod$plot_list[[var]] <- call_plot_function(df=df,
                                                        plot_type = input$plot_type,
                                                        plot_args = plot_args)
    
    }
    
    rval_mod$count_raw <- rval_mod$count_raw + 1
  })
  
  ######################################################################################
  # Format plot
  observeEvent(params_update_plot_format(),  {
    
    #print("format")
    
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

    if(!input$auto_focus){
      options$axis_limits <- choices()$axis_limits
    }
    
    plist <- lapply( rval_mod$plot_list,
                     function(p){
                       format_plot(p,
                                   options = options)
                     })
    
    #plist <- rval_mod$plot_list
   
    rval_mod$plot_list <- plist
    rval_mod$count_format <- rval_mod$count_format + 1
    #print("OK format")
  })
  
  ######################################################################################
  # Add gates corresponding to plot coordinates
  draw_gates <- eventReactive(rval_mod$count_format, {
    
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
    
    plist <- rval_mod$plot_list
    #print(gate)
    
    plist <- lapply( plist,
                     function(p){
                       if(!is.null(gate)){
                         for(gate_name in setdiff(gate, "root")){
                           gate_int <- flowWorkspace::gh_pop_get_gate(rval$gating_set[[1]],
                                                                      gate_name)
                           #print(gate_int)
                           p <- add_gate(p = p, gate = gate_int)
                         }
                       }
                       return(p)
                     })


    #rval_mod$plot_list <- plist
    #rval_mod$count_gate <- rval_mod$count_gate + 1
    #print("OK gate")
    return(plist)
  })
  
  ######################################################################################
  # Add polygonal plot layer
  draw_polygon <- reactive({
    
    #print("poly")
    
      gate <- NULL
      if(!is.null(rval_input$show_gates)){
        if(rval_input$show_gates){
          gate <- rval_input$subset
        }
      }

      polygon <- data.frame(x = polygon_gate$x,
                            y = polygon_gate$y)

      plist <- draw_gates()
      
      plist <- lapply(plist,
                      function(p){
                        if(!is.null(polygon$x)){
                          p <- add_polygon_layer(p, polygon = polygon)
                        }
                        if(!is.null(gate)){
                          for(gate_name in setdiff(gate, "root")){
                            g <- choices()$gates[[gate_name]]$gate
                            p <- add_gate(p, g)
                          }
                        }
                        return(p)
                      })

      #print("OK poly")
      plist
      
      
  })
  
  return( list(plot = draw_polygon, params = rval_input) )
  
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
#     dashboardHeader(title = "plotGatingSet"),
#     sidebar = dashboardSidebar(disable = TRUE),
#     body = dashboardBody(
#       fluidRow(
#         column(4, box(width = NULL, title = "Parameters", collapsible = TRUE, collapsed = TRUE,
#                       plotGatingSetInput("module"))),
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
#       load("../flowR_utils/demo-data/Rafa2Gui/analysis/cluster.rda")
#       fs <- build_flowset_from_df(df = res$cluster$data, origin = res$cluster$flow_set)
#       gs <- GatingSet(fs)
#       #gs@transformation <- res$cluster$transformation
#       add_gates_flowCore(gs, res$cluster$gates)
#       rval$gating_set <- gs
#       #plot_params$sample <- pData(gs)$name
#       # utils::data("GvHD", package = "flowCore")
#       # gs <- GatingSet(GvHD)
#       # rval$gating_set <- gs
#       #plot_params$plot_type <- "histogram"
#       #plot_params$xvar <- "cluster"
#       #plot_params$yvar <- "FL4-H"
#       #plot_params$sample <- pData(gs)$name[3]
#       #plot_params$subset <- gs_get_pop_paths(gs)[1]
#       plot_params$auto_focus <- FALSE
#     })
# 
#     res <- callModule(plotGatingSet, "module",
#                       rval = rval,
#                       plot_params = plot_params,
#                       show_gates = TRUE,
#                       auto_update = TRUE)
# 
#     callModule(simpleDisplay, "simple_display_module", res$plot)
# 
#   }
# 
#   shinyApp(ui, server)
# 
# }