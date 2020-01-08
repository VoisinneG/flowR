#' Edit flow parameter transformation and description
#' @param id shiny id
#' @importFrom shinydashboard tabBox
#' @import shiny
#' @importFrom DT DTOutput
#' @export
TransformUI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    
    column(width = 6,
           tabBox(title = "Channels",
                  width = NULL, height = NULL,
                  tabPanel(title = "Table",
                           "Select channels",
                           br(),
                           br(),
                           div(style = 'overflow-x: scroll', DT::DTOutput(ns("parameters_table")))
                           
                  ),
                  tabPanel(title = "Transform",
                           selectInput(ns("trans"), "transformation", 
                                       choices = c("identity", "logicle", "asinh", "flowJo_asinh", "log"), 
                                       selected = "identity"),
                           uiOutput(ns("trans_param_ui")),
                           br(),
                           actionButton(ns("apply_transformation"), label = "apply to selected chanels"),
                           br()
                  ),
                  tabPanel(title = "Edit",
                           "Edit table (column 'desc' only)",
                           br(),
                           br(),
                           div(style = 'overflow-x: scroll', DT::DTOutput(ns("parameters")))
                  )
           )
    ),
    column(width = 6,
           tabBox(title = "",
               width = NULL, height = NULL,
               tabPanel(title = "Plot",
                  simpleDisplayUI(ns("simple_display_module"))
               ),
               tabPanel(title = "Parameters",
                 plotGatingSetInput(id = ns("plot_module"))
               )
           )
    )
  )
  
}


#' Transform module server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param rval A reactive values object
#' @return The updated reactiveValues object \code{rval}
#' @import shiny
#' @importFrom flowWorkspace logicle_trans flowjo_fasinh_trans
#' @importFrom scales log_trans identity_trans
#' @importFrom flowCore parameters description
#' @importFrom DT renderDT dataTableProxy editData replaceData
#' @export
#' @rdname TransformUI
Transform <- function(input, output, session, rval) {

  rval_mod <- reactiveValues(parameters = NULL)
  plot_params <- reactiveValues()
  
  output$trans_param_ui <- renderUI({
    ns <- session$ns
    x <- list()
    if(input$trans == 'asinh'){
      x[[1]] <- h5("Parameters")
      x[[2]] <- numericInput(ns("base_asinh"), label = "base", value = 1)
    } else if(input$trans == 'flowjo_fasinh'){
      x[[1]] <- h5("Parameters")
      x[[2]] <- numericInput(ns("m"), label = "m", value = 5)
      x[[3]] <- numericInput(ns("t"), label = "t", value = 12000)
      x[[4]] <- numericInput(ns("a"), label = "a", value = 0.7)
      x[[5]] <- numericInput(ns("length"), label = "length", value = 256)
    } else if(input$trans == 'logicle'){
      x[[1]] <- h5("Parameters")
      x[[2]] <- numericInput(ns("w_logicle"), label = "w", value = 0.5)
      x[[3]] <- numericInput(ns("t_logicle"), label = "t", value = 262144)
      x[[4]] <- numericInput(ns("m_logicle"), label = "m", value = 4.5)
      x[[5]] <- numericInput(ns("a_logicle"), label = "a", value = 0)
    } else if(input$trans == 'log'){
      x[[1]] <- h5("Parameters")
      x[[2]] <- numericInput(ns("base_log"), label = "base", value = 10)
    }
    tagList(x)
  })
  

  observe({
      
      plot_params$plot_type <- "histogram"
      plot_params$color_var <- NULL
      plot_params$use_all_cells <- FALSE
  })
  
  observeEvent(input$parameters_table_rows_selected, {

    if(length(input$parameters_table_rows_selected)>0){
      
      #reset plot parameters (only non null parameters will be updated)
      for(var in names(reactiveValuesToList(plot_params))){
        plot_params[[var]] <- NULL
      }
      
      plot_params$xvar <- rval_mod$parameters$name[input$parameters_table_rows_selected[1]]
      if(length(input$parameters_table_rows_selected)>1){
        plot_params$yvar <- rval_mod$parameters$name[input$parameters_table_rows_selected[2]]
      }else{
        plot_params$yvar <- rval_mod$parameters$name[1]
      }
    }
  })
  
  
  
  res <- callModule(plotGatingSet, "plot_module", rval, plot_params, simple_plot = TRUE)
  callModule(simpleDisplay, "simple_display_module", res$plot)

  
  ##########################################################################################################
  # Observe functions for data transformation
  
  #get parameters information from flow set
  observe({
    
    validate(need(class(rval$gating_set) == "GatingSet", "No GatingSet available"))
    
    ff <- rval$gating_set@data[[1]]
    
    params <- flowCore::parameters(ff)
    
    desc <- as.character(params$desc)
    name <- as.character(params$name)
    
    
    display <- unlist(sapply(rownames(params@data), FUN = function(x){
      kw <- substr(x, start = 2, stop = nchar(x))
      kw <- paste(kw, "DISPLAY", sep = "")
      disp <- ff@description[[kw]]
      if(is.null(disp)){
        disp <- "NA"
      }
      return(disp)
    }))
    
    names(display) <- params@data$name
    
    rval_mod$parameters <- data.frame(name = name,
                                      desc = desc,
                                      display = display[match(name, names(display))],
                                      range = params@data$range,
                                      minRange = params@data$minRange,
                                      maxRange = params@data$maxRange,
                                      stringsAsFactors = FALSE)
    #plot_params$xvar <- name[1]
    #plot_params$yvar <- name[2]
  })
  
  
  # observe({
  #   validate(need(rval$parameters, "no parameters"))
  #   desc <- as.character(rval$parameters$desc)
  #   name <- as.character(rval$parameters$name)
  #   name_long <- name
  #   name_long[!is.na(desc)] <- paste(name[!is.na(desc)], " (", desc[!is.na(desc)], ")", sep = "")
  #   rval$parameters$name_long <- name_long
  # })
  # 
  # observeEvent(rval$parameters, {
  #   validate(need(rval$parameters, "No parameters"))
  #   rval$plot_var <- rval$parameters$name_long
  #   names(rval$plot_var) <- NULL
  # })
  
  # Initialization of transformation for new parameters
  observe({
    
    validate(need(class(rval$gating_set) == "GatingSet", "No GatingSet available"))
    
    transformation <- rval$gating_set@transformation
    trans_parameters <- rval$trans_parameters
    
    new_par <- setdiff(colnames(rval$gating_set), names(transformation))
    idx_new <- match(new_par, colnames(rval$gating_set))
    
    if(length(new_par)>0){
      for(i in 1:length(new_par)){
        transformation[[new_par[i]]] <- switch(rval_mod$parameters$display[idx_new[i]],
                                                    "LOG" = flowWorkspace::logicle_trans(w=0.5, 
                                                                          m=4.5, 
                                                                          t = 262144, 
                                                                          a = 0),
                                                    scales::identity_trans())
        
        trans_parameters[[new_par[i]]] <- switch(rval_mod$parameters$display[idx_new[i]],
                                                      "LOG" = list(w=0.5, 
                                                                   m=4.5, 
                                                                   t = 262144, 
                                                                   a = 0),
                                                      list())
      }
    }
    
    rval$trans_parameters <- trans_parameters
    rval$gating_set@transformation <- transformation
    #print(class(rval$gating_set))
    #rval$update_gs <- rval$update_gs + 1
    
  })
  
  
  observeEvent(input$apply_transformation, {

    transformation <- rval$gating_set@transformation
    trans_parameters <- rval$trans_parameters
    
    if(length(input$parameters_table_rows_selected)>0){
      
      var_name <- rval_mod$parameters$name[input$parameters_table_rows_selected]
      
      trans_params <- switch(input$trans,
                             "identity" = list(),
                             "asinh" = list(base = input$base_asinh),
                             "log" = list(base = input$base_log),
                             "flowjo_fasinh" = list(m=input$m,
                                                   t = input$t,
                                                   a = input$a,
                                                   length = input$length),
                             "logicle" = list(w=input$w_logicle,
                                              m=input$m_logicle,
                                              t = input$t_logicle,
                                              a = input$a_logicle))
      
      trans <- switch(input$trans,
                      "identity" = scales::identity_trans(),
                      "log" = scales::log_trans(base = input$base_log),
                      "asinh" = asinh_trans(b = input$base_asinh),
                      "flowjo_fasinh" = flowWorkspace::flowjo_fasinh_trans(m=input$m,
                                                           t = input$t,
                                                           a = input$a,
                                                           length = input$length),
                      "logicle" = flowWorkspace::logicle_trans(w=input$w_logicle,
                                                m=input$m_logicle,
                                                t = input$t_logicle,
                                                a = input$a_logicle))
      
      
      
      for(i in 1:length(var_name)){
        transformation[[var_name[i]]] <- trans
        trans_parameters[[var_name[i]]] <- trans_params
      }
      
    }
    
    rval$trans_parameters <- trans_parameters
    rval$gating_set@transformation <- transformation
    #rval$update_gs <- rval$update_gs + 1
    
  })
  
  observe({
    validate(need(class(rval$gating_set) == "GatingSet", "No GatingSet available"))
    transformation <- rval$gating_set@transformation
    trans_parameters <- rval$trans_parameters
    # validate(
    #   need(rval$transformation, "No transformation defined")
    # )
    # 
    # validate(
    #   need(rval$parameters, "No parameters")
    # )
    
    trans_name <- sapply(transformation, function(x){x$name})
    trans_param <- sapply(trans_parameters, function(x){
      paste( paste(names(x), as.character(x), sep = ": "), collapse="; ")})
    
    idx_match <- match(rval_mod$parameters$name, names(transformation))
    rval_mod$parameters$transform <- trans_name[idx_match]
    
    idx_match_trans <- match(rval_mod$parameters$name, names(trans_parameters))
    rval_mod$parameters[["transform parameters"]] <- trans_param[idx_match_trans]
    
    
  })
  
  output$parameters_table <- DT::renderDT({
    validate(
      need(rval_mod$parameters, "No parameters defined")
    )
    df <- rval_mod$parameters
    df$minRange <- format(df$minRange, digits = 2)
    df$maxRange <- format(df$maxRange, digits = 2)
    
    DT::datatable(
      df[, c("name", "desc", "transform", "transform parameters", "minRange", "maxRange",  "range", "display" )],
      rownames = FALSE)
  })
  
  #Edit data table
  output$parameters <- renderDT({validate(need(rval_mod$parameters, "No metadata")); rval_mod$parameters},
                           rownames = FALSE,
                           selection = 'none',
                           editable = 'cell',
                           server = TRUE)
  
  proxy = dataTableProxy('parameters')
  
  observeEvent(input$parameters_cell_edit, {
    info = input$parameters_cell_edit
    info$col <- info$col + 1
    if(info$col == 2){
      rval_mod$parameters <<- editData(rval_mod$parameters, info)
      replaceData(proxy, rval_mod$parameters, resetPaging = FALSE)
    }
  })
  
  return(rval)
  
  
}

##################################################################################
# Tests
##################################################################################
#
# library(shiny)
# library(shinydashboard)
# 
# if (interactive()){
# 
#   ui <- dashboardPage(
#     dashboardHeader(title = "plotting"),
#     sidebar = dashboardSidebar(disable = TRUE),
#     body = dashboardBody(
#       TransformUI("module")
#     )
#   )
# 
#   server <- function(input, output, session) {
# 
#     rval <- reactiveValues()
#     plot_params <- reactiveValues()
# 
#     observe({
#       utils::data("GvHD", package = "flowCore")
#       rval$gating_set <- GatingSet(GvHD)
#     })
# 
#     res <- callModule(Transform, "module", rval = rval)
# 
#   }
# 
#   shinyApp(ui, server)
# 
# }
