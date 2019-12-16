#' @title plotGatingSet2Input
#' @description  A shiny Module (ui function) to build plots from a GatingSet
#' @param id shiny id
#' @param simple_plot logical, disable a number of plot options
#' @param auto_update Should plot update be automatic? 
#' If FALSE, plot update is controlled by an action button.
#' @import shiny
#' @importFrom shinydashboard box 
plotGatingSet2Input <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("ui_update")),
    box(collapsible = TRUE, collapsed = FALSE, width = NULL, height = NULL,
        title = "Sample/Subset",
        selection2Input(ns("selection_module"))
    ),
    box(collapsible = TRUE, collapsed = FALSE, width = NULL, height = NULL,
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
#' @title plotGatingSet2
#' @description  A shiny module (server function) to build plots from a GatingSet
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param rval a reactivevalues object with the following elements :
#' \describe{
#'   \item{gating_set}{: a GatingSet object}
#' @param plot_params reactivevalues object used to initialize plot parameters 
#' with elements (not mandatory):
#' \describe{
#'   \item{sample}{: initially selected samples}
#'   \item{subset}{: initially selected subsets}
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
#' @importFrom flowWorkspace gs_pop_get_children gh_pop_get_gate gs_get_pop_paths pData
#' @importFrom flowCore parameters
#' @import shiny
plotGatingSet2 <- function(input, output, session,
                          rval,
                          plot_params = reactiveValues(),
                          simple_plot = TRUE,
                          auto_update = TRUE,
                          show_gates = FALSE,
                          polygon_gate = NULL,
                          apply_trans = TRUE,
                          apply_comp = TRUE) {
  
  #default values
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
                              split_var = "yvar",
                              show_label = FALSE,
                              show_outliers = FALSE)
  
  
  
  rval_mod <- reactiveValues(plot_list = list(), count_raw = 0, count_format = 0)
  
  selected <- callModule(selection2, "selection_module", rval, params = plot_params, multiple_subset = !simple_plot)

  ################################################################################################################
  # get plot parameters from GatingSet
  choices <- reactive({
    validate(need(class(rval$gating_set) == "GatingSet", "input is not a GatingSet"))
    
    plot_var <- parameters(rval$gating_set@data[[1]])$name
    desc <- parameters(rval$gating_set@data[[1]])$desc
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
  observeEvent(c(input$plot_type, choices()$meta_var) , {
    
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
  
  ######################################################################################
  # Define and initialize plot variables
  
  # observe({
  #   #validate(need(rval$plot_var, "No plotting variables"))
  #   updateSelectInput(session, "xvar", choices = choices()$plot_var, selected = choices()$plot_var[1])
  #   updateSelectInput(session, "yvar", choices = choices()$plot_var, selected = choices()$plot_var[2]) 
  # })
  
  observe({
  
    ns <- session$ns
    x <- list()
    color_var_choices <- switch(input$plot_type,
                                "dots" = c("none", "subset", choices()$meta_var, choices()$plot_var),
                                c("none", "subset", choices()$meta_var))
    
    
    x[["xvar"]] <- selectizeInput(ns("xvar"),
                   multiple = !simple_plot,
                   label = "x variable", 
                   choices = choices()$plot_var,
                   selected = rval_plot[["xvar"]])
    
    x[["yvar"]] <- selectizeInput(ns("yvar"),
                   multiple = !simple_plot,
                   label = "y variable",
                   choices = choices()$plot_var,
                   selected = rval_plot[["yvar"]])
    
    x[["group_var"]] <- selectizeInput(ns("group_var"), 
                                       multiple = !simple_plot,
                                       label = "group variable",
                                       choices = c("none", "subset", choices()$meta_var),
                                       selected = rval_plot[["group_var"]])
    
    x[["color_var"]] <- selectizeInput(ns("color_var"), 
                                       multiple = !simple_plot,
                                       label = "color variable",
                                       choices = c("none", "subset", choices()$meta_var),
                                       selected = rval_plot[["color_var"]])
    
    x[["facet_var"]] <- selectizeInput(ns("facet_var"),
                                       multiple =TRUE,
                                       label = "facet variables",
                                       choices = c("subset", choices()$meta_var, choices()$extra_facet_var),
                                       selected = rval_plot[["facet_var"]]
    )
    
    split_choices <- c("xvar", "yvar", "color_var")
    names(split_choices) <- c("x variable", "y variable", "color variable")
    x[["split_var"]] <- selectInput(ns("split_var"),
                                         label = "select variable used to split plots",
                                         choices = split_choices,
                                         selected = rval_plot[["split_var"]]
    )

    rval_mod$plot_variables <- x
    
  })
  
  output$plot_variables <- renderUI({
    
    x <- rval_mod$plot_variables
    vars <- names(x)
    hidden_vars <- switch(input$plot_type,
                          'histogram' = "yvar",
                          'hexagonal' = c("color_var", "group_var"),
                          NULL)
    if(simple_plot){
      hidden_vars <- union(hidden_vars, c("facet_var, split_var"))
    }
    
    vars <- setdiff(vars, hidden_vars)
    
    tagList(rval_mod$plot_variables[vars])
  })
  
  output$ui_pattern <- renderUI({
    ns <- session$ns
    if(!simple_plot){
      tagList(
        box(title = "Select using a pattern", width = NULL, height = NULL, collapsible = TRUE, collapsed = TRUE,
            patternSelectionInput(ns("pattern_module"))
        )
      )
    }
  })
  

  ###############################   End Build UI  #####################################

  
  ######################################################################################
  # Control of plot parameters using 'plot_params'
  observe({
    
    for(var in intersect( names(plot_params), c("xvar", 
                                                "yvar", 
                                                "color_var",
                                                "group_var",
                                                "facet_var") )){
      rval_plot[[var]] <- plot_params[[var]]
      
    }
    
    if("plot_type" %in% names(plot_params)){
      updateCheckboxInput(session, "plot_type", value = plot_params[["plot_type"]])
    }
    
    if("use_all_cells" %in% names(plot_params)){
      updateCheckboxInput(session, "use_all_cells", value = plot_params[["use_all_cells"]])
    }
    
  })
  
  ######################################################################################
  # Select plot variables using a pattern
  
  
  choices_pattern <- reactiveValues()
  
  observe({
    
    choices_color_var <- switch(input$plot_type,
                                "dots" = c("none", "subset", choices()$meta_var, choices()$plot_var),
                                c("none", "subset", choices()$meta_var))
    
    for(var_name in c("x variable", "y variable", "color variable")){
      choices_pattern[[var_name]] <- switch(var_name,
                                            "x variable" = choices()$plot_var,
                                            "y variable" = choices()$plot_var,
                                            "color variable" = choices_color_var
      )
    }
    
  })
  
  res_pattern <- callModule(patternSelection, "pattern_module", choices = choices_pattern)
  
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
  
  #########################################################################################################
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
    rval_input$subset <- selected$subset
    rval_input$sample <- selected$sample
  })

  ##########################################################################################################
  # Control update of plot data
  
  params_update_data <- reactive({
    if(!auto_update){
      input$update_plot
    }else{
      update_params <- c(selected$sample,
                         selected$subset,
                         #rval$spill,
                         #rval$flow_set_selected,
                         # rval$gating_set,
                         # rval$parameters,
                         #rval$gates_flowCore,
                         choices()$parameters,
                         choices()$metadata,
                         #rval$pdata,
                         rval_input$use_all_cells
      )
      
      if(apply_comp){
        update_params <- c(update_params, choices()$compensation)
      }
      
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
      var_update <- var_update[var_update %in% names(rval_input)]
      for(var in var_update){
        update_params <- c(update_params, rval_input[[var]])
      }
      update_params
    }
  })
  
  ##########################################################################################################3
  # Control update of formatted plot
  params_update_plot_format <- reactive({
    
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
      var_update <- var_update[var_update %in% names(rval_input)]
      for(var in var_update){
        update_params <- c(update_params, rval_input[[var]])
      }
      
      
      
      if(apply_trans){
        update_params <- c(update_params, choices()$transformation)
      }
      
      if(show_gates){
        update_params <- c(update_params, 
                           choices()$gates)
      }
      update_params
    }
  })
  
  
  ##########################################################################################################3
  # Get plot data
  data_plot_focus <- eventReactive(params_update_data(), {
    
    print("data")
    
    validate(need(rval$gating_set, "Empty gating set"))
    validate(need(selected$sample, "Please select samples"))
    validate(need(all(selected$sample %in% pData(rval$gating_set)$name), "Samples not found in gating set"))
    validate(need(selected$subset, "Please select subsets"))

    Ncells <- 30000
    if(rval_input$use_all_cells){
      Ncells <- NULL
    }
    
    spill <- NULL
    if(apply_comp){
      spill <- choices()$compensation
    }
    df <- get_plot_data(gs = rval$gating_set,
                      sample = selected$sample,
                      subset = selected$subset,
                      spill = spill,
                      metadata = choices()$metadata,
                      Ncells = Ncells)
    return(df)
    
  })
  
  ##########################################################################################################3
  # Build raw plot
  observeEvent(c(params_update_plot_raw(),  data_plot_focus()), {
    
    print("raw")
    
    df <- data_plot_focus()
    
    validate(need(input$xvar, "x variable not available"))
    validate(need(input$yvar, "y variable not available"))
    
    xvar <- input$xvar
    yvar <- input$yvar
    
    color_var <- input$color_var
    group_var <- input$group_var
    
    rval_mod$plot_list <- list()
    
    plot_args <- reactiveValuesToList(rval_input)
    # plot_args <- list()
    # for(var in names(rval_input) ){
    #   plot_args[[var]] <- rval_input[[var]]
    # }
    
    split_var  <- "xvar"
    if(!is.null(input$split_var)){
      split_var <- input$split_var
    }
    
    for(i in 1:length(input[[split_var]])){

          plot_args[["color_var"]] <- color_var[1]
          plot_args[["group_var"]] <- group_var[1]
          plot_args[["xvar"]] <- xvar[1]
          plot_args[["yvar"]] <- yvar[1]

          if(split_var == "color_var"){
            plot_args[["color_var"]] <- color_var[i]
          }else if(split_var == "group_var"){
            plot_args[["group_var"]] <- group_var[i]
          }else if(split_var == "xvar"){
            plot_args[["xvar"]] <- xvar[i]
          }else if(split_var == "yvar"){
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
    
    axis_labels <- choices()$labels
    
    transformation <- NULL
    if(apply_trans){
      transformation <- choices()$transformation
    }
     
    if(!simple_plot){
      facet_var <- rval_input$facet_var
    }else{
      facet_var <- NULL
    }
    
    options <- list(theme = rval_input$theme,
                    transformation = transformation,
                    axis_labels = axis_labels,
                    facet_var = facet_var,
                    legend.position = rval_input[["legend.position"]],
                    option = rval_input[["option"]])
    
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
    
    print("gate")
    
    validate(need(rval$gating_set, "No gating set"))

    gate <- NULL
    
    if(show_gates){
      if(selected$subset %in% flowWorkspace::gs_get_pop_paths(rval$gating_set[[1]])){
        child_gates <- flowWorkspace::gs_pop_get_children(rval$gating_set[[1]], selected$subset)
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
    print("poly")
    
      gate <- NULL
      if(!is.null(rval_input$show_gates)){
        if(rval_input$show_gates){
          gate <- selected$subset
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
                            g <- choices()$gates[[gate_name]]$gate
                            p <- add_gate(p, g)
                          }
                        }
                        return(p)
                      })
      
      plist
      
      
  })
  
  return( list(plot = plot_gate, params = rval_input) )
  
}



##################################################################################
# Tests
##################################################################################

library(shiny)
library(shinydashboard)

if (interactive()){

  ui <- dashboardPage(
    dashboardHeader(title = "plotGatingSet2"),
    sidebar = dashboardSidebar(disable = TRUE),
    body = dashboardBody(
      fluidRow(
        column(4, box(width = NULL, plotGatingSet2Input("module"))),
        column(8, box(width = NULL, simpleDisplayUI("simple_display_module")))
      )
    )
  )
  
  server <- function(input, output, session) {

    rval <- reactiveValues()
    plot_params <- reactiveValues()

    observe({
      gs <- load_gs("./inst/ext/gs")
      rval$gating_set <- gs
      plot_params$xvar <- "Time"
      plot_params$plot_type <- "histogram"
      plot_params$sample <- pData(gs)$name[2]
      plot_params$subset <- gs_get_pop_paths(gs)[3]
    })

    res <- callModule(plotGatingSet2, "module",
                      rval = rval,
                      plot_params = plot_params,
                      simple_plot = FALSE,
                      apply_trans = TRUE,
                      apply_comp = FALSE,
                      auto_update = TRUE
                      )
    
    callModule(simpleDisplay, "simple_display_module", res$plot)
    
  }

  shinyApp(ui, server)

}





