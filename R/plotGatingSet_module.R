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
#' @importFrom flowWorkspace gs_pop_get_children gh_pop_get_gate
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
  
  ### module specific reactiveValues ###############################################################
  rval_mod <- reactiveValues(plot_list = list(), 
                             count_raw = 0, count_format = 0, count_gate = 0)
  
  ### Default plot parameters ######################################################################
  rval_plot <- reactiveValues(plot_type = "hexagonal",
                              use_all_cells = FALSE,
                              auto_focus = FALSE,
                              legend.position = "right",
                              theme = "gray",
                              show_gates = FALSE,
                              norm_density = TRUE,
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
                         params = plot_params, multiple_subset = !simple_plot)

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
    }else if (rval_input$plot_type == 'contour'){
      vars <- c("show_gates", "show_outliers",  "bins", "alpha", "size")
    }
    vars <- c("plot_type", "use_all_cells", "auto_focus", "legend.position", "theme", vars)
    
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
                                "dots" = c("none", "subset", choices()$meta_var, 
                                           choices()$plot_var),
                                c("none", "subset", 
                                  choices()$meta_var, 
                                  choices()$plot_var[choices()$params$vartype != "double"]))
    
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
    
    rval_mod$plot_variables[["group_var"]] <- selectizeInput(ns("group_var"), 
                                       multiple = !simple_plot,
                                       label = "group variable",
                                       choices = c("none", "subset", 
                                                   choices()$meta_var,
                                                   choices()$plot_var[choices()$params$vartype != "double"]),
                                       selected = rval_plot[["group_var"]])
    
    rval_mod$plot_variables[["facet_var"]] <- selectizeInput(ns("facet_var"),
                                       multiple =TRUE,
                                       label = "facet variables",
                                       choices = c("subset", 
                                                   choices()$meta_var, 
                                                   choices()$plot_var[choices()$params$vartype != "double"]),
                                       selected = rval_plot[["facet_var"]]
    )
    
    split_choices <- c("xvar", "yvar", "color_var")
    names(split_choices) <- c("x variable", "y variable", "color variable")
    rval_mod$plot_variables[["split_var"]] <- selectInput(ns("split_var"),
                                         label = "select variable used to split plots",
                                         choices = split_choices,
                                         selected = rval_plot[["split_var"]]
    )
    
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
      update_params <- rval_mod$count_raw
      var_update <- c("facet_var",
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
  
  ### Get plot data ################################################################################
  
  data_plot_focus <- eventReactive(params_update_data(), {
    
    #print("data")
    
    validate(need(rval$gating_set, "Empty GatingSet"))
    validate(need(rval_input$sample, "Please select samples"))
    validate(need(all(rval_input$sample %in% choices()$sample), 
                  "All samples not found in GatingSet"))
    validate(need(rval_input$subset, "Please select subsets"))
    validate(need(all(rval_input$subset %in% choices()$subset), 
                  "All subsets not found in GatingSet"))
   
    
    Ncells <- 30000
    if(!is.null(rval_input$use_all_cells)){
      if(rval_input$use_all_cells){
        Ncells <- NULL
      }
    }
    
    
    spill <- choices()$compensation
    #print(spill)
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
    
    for(var in rval_input[[split_variable]]){
          
          plot_args[[split_variable]] <- var

          plot_list[[var]] <- call_plot_function(df=df,
                                                        plot_type = rval_input$plot_type,
                                                        plot_args = plot_args)
    }
    
    return(plot_list)
  })
  
  ### Format plot ##################################################################################
  
  plot_format <- eventReactive( c(params_update_plot_format(), plot_raw()), {
    
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

    if(!rval_input$auto_focus){
      options$axis_limits <- choices()$axis_limits
    }
    
    plist <- lapply( plot_raw(),
                     function(p){
                       format_plot(p,
                                   options = options)
                     })
   return(plist)
  })
  
  ### Add gates corresponding to plot coordinates ##################################################
  
  draw_gates <- eventReactive(plot_format(), {
    
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
    
    plist <- lapply( plot_format(),
                     function(p){
                       if(!is.null(gate)){
                         for(gate_name in setdiff(gate, "root")){
                           gate_int <- flowWorkspace::gh_pop_get_gate(rval$gating_set[[1]],
                                                                      gate_name)
                           p <- add_gate(p = p, gate = gate_int)
                         }
                       }
                       return(p)
                     })

    return(plist)
  })
  
  ### Add polygon layer ############################################################################
  
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
    
    
    plist <- lapply( draw_gates(),
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
    
    return(plist)
    
  })
  
  return( list(plot = draw_polygon, params = rval_input) )
  
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
#        utils::data("GvHD", package = "flowCore")
#        gs <- GatingSet(GvHD)
#        rval$gating_set <- gs
#       #plot_params$plot_type <- "histogram"
#       #plot_params$xvar <- "cluster"
#       #plot_params$yvar <- "FL4-H"
#       #plot_params$sample <- pData(gs)$name[3]
#       #plot_params$subset <- gs_get_pop_paths(gs)[1]
#       plot_params$auto_focus <- FALSE
#       plot_params$plot_type = "histogram"
#       plot_params$xvar = "FL2-H"
#       plot_params$option = "magma"
# 
#     })
# 
#     res <- callModule(plotGatingSet, "module",
#                       rval = rval,
#                       plot_params = plot_params,
#                       show_gates = FALSE,
#                       auto_update = TRUE,
#                       simple_plot=TRUE)
# 
#     callModule(simpleDisplay, "simple_display_module", res$plot)
# 
#   }
# 
#   shinyApp(ui, server)
# 
# }