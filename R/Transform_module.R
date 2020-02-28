#' Edit flow parameter transformation and description
#' @param id shiny id
#' @importFrom shinydashboard tabBox
#' @import shiny
#' @importFrom DT DTOutput
#' @export
#' @examples 
#' \dontrun{
#' library(shiny)
#' library(shinydashboard)
#' 
#' if (interactive()){
#'   
#'   ui <- dashboardPage(
#'     dashboardHeader(title = "Transform"),
#'     sidebar = dashboardSidebar(disable = TRUE),
#'     body = dashboardBody(
#'       TransformUI("module")
#'     )
#'   )
#'   
#'   server <- function(input, output, session) {
#'     rval <- reactiveValues()
#'     observe({
#'       utils::data("GvHD", package = "flowCore")
#'       rval$gating_set <- GatingSet(GvHD)
#'     })
#'     res <- callModule(Transform, "module", rval = rval)
#'   }
#'   
#'   shinyApp(ui, server)
#'   
#' }
#' }
TransformUI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    
    column(width = 6,
           tabBox(title = "Channels",
                  width = NULL, height = NULL,
                  tabPanel(title = "Table",
                           selectInput(ns("sample"), "Sample", choices = NULL, selected = NULL),
                           h4("Parameters"),
                           div(style = 'overflow-x: scroll', DT::DTOutput(ns("parameters_table")))
                           
                  ),
                  tabPanel(title = "Transform",
                           selectizeInput(ns("selected_params"), "Select parameters", 
                                          choices = NULL, selected = NULL, multiple = TRUE),
                           selectInput(ns("param_vartype"), "Type of variable", 
                                       choices = c("numeric", 
                                                   #"integer", 
                                                   "factor"), 
                                       selected = NULL),
                           textInput(ns("param_desc"), label = "Description (first parameter only)", value = ""),
                           selectInput(ns("trans"), "transformation",
                                       choices = c("identity", 
                                                   "logicle", 
                                                   "asinh",
                                                   "log"), 
                                       selected = "identity"),
                           uiOutput(ns("trans_param_ui")),
                           actionButton(ns("apply"), 
                                        label = "apply to selected parameters"),
                           br()
                  )
                  # tabPanel(title = "Edit",
                  #          selectizeInput(ns("selected_params_edit"), "Select parameters", 
                  #                         choices = NULL, selected = NULL, multiple = FALSE),
                  #          
                  #          actionButton(ns("apply_edit"), 
                  #                       label = "apply"),
                  #          br()
                  #          # "Edit table (column 'desc' only)",
                  #          # br(),
                  #          # br(),
                  #          # div(style = 'overflow-x: scroll', DT::DTOutput(ns("parameters")))
                  # )
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

  rval_mod <- reactiveValues(parameters = NULL, trans_parameters = NULL)
  
  ### Default transform parameters values
  observe({
    rval_mod$trans_parameters <- list("base" = 10,
                                      "scale" = 5,
                                      "w" = 0.5,
                                      "t" = 262144,
                                      "m" = 4.5,
                                      "a" = 0
                                      )
  })
  
  plot_params <- reactiveValues( "plot_type" = "histogram", 
                                 "color_var" = NULL, 
                                 "use_all_cells" = FALSE)
  
  
  ### Call modules ##########################################################################
  
  res <- callModule(plotGatingSet, "plot_module", 
                    rval = rval, plot_params = plot_params, simple_plot = TRUE)
  callModule(simpleDisplay, "simple_display_module", plot_list = res$plot)
  
  ### build UI for transform parameters ####################################################
  
  output$trans_param_ui <- renderUI({
    ns <- session$ns
    x <- list()
    if(input$trans %in% c('log')){
      x[[1]] <- numericInput(ns("base"), label = "base", 
                             value = rval_mod$trans_parameters[["base"]])
    }else if(input$trans %in% c('asinh')){
      x[[1]] <- numericInput(ns("scale"), label = "scale", 
                             value = rval_mod$trans_parameters[["scale"]])
    }else if(input$trans == 'logicle'){
      x[[1]] <- numericInput(ns("w"), label = "w", 
                             value = rval_mod$trans_parameters[["w"]])
      x[[2]] <- numericInput(ns("t"), label = "t", 
                             value = rval_mod$trans_parameters[["t"]])
      x[[3]] <- numericInput(ns("m"), label = "m", 
                             value = rval_mod$trans_parameters[["m"]])
      x[[4]] <- numericInput(ns("a"), label = "a", 
                             value = rval_mod$trans_parameters[["a"]])
    }
    
    box(title = "Transform parameters", width = NULL, collapsible = TRUE, collapsed = FALSE,
        tagList(x)
    )
    
  })
  
  observe({
    rval$update_gs <- 0
  })
  
  ### update plot parameters ###################################################################
  
  observeEvent(input$parameters_table_rows_selected, {

    if(length(input$parameters_table_rows_selected)>0){
      
      updateSelectizeInput(session, "selected_params",
                           selected = rval_mod$parameters$name[input$parameters_table_rows_selected])
    }
    
  })
  
  
  observeEvent(input$selected_params, {
    #update plot_params
    for(var in intersect(names(res$params), names(plot_params))){
      plot_params[[var]] <- res$params[[var]]
    }
    
    plot_params$xvar <- input$selected_params[1]
    if(length(input$selected_params)>1){
      plot_params$yvar <- input$selected_params[2]
    }
  
  })
      
  
  observeEvent(input$sample, {
    if(!is.null(input$sample)){
      #update plot_params
      for(var in intersect(names(res$params), names(plot_params))){
        plot_params[[var]] <- res$params[[var]]
      }
      plot_params$sample <- input$sample
    }
  })

  ### Get parameters from GatingSet ##################################################################
  choices <- reactive({
    rval$update_gs
    validate(need(class(rval$gating_set) == "GatingSet", "No GatingSet available"))
    get_parameters_gs(rval$gating_set)
  })
  
  observe({
    rval_mod$parameters <- choices()$params
  })

  ### Update UI ################################################################################
  
  observeEvent(choices()$sample, {
    updateSelectInput(session, "sample", 
                      choices = choices()$sample,
                      selected = choices()$sample[1])
  })
  
  observeEvent(choices()$plot_var, {
    updateSelectizeInput(session, "selected_params",
                         choices = choices()$plot_var, 
                         selected = input$selected_params)
  })
  
  observeEvent(input$selected_params, {
    
    idx <- match(input$selected_params[1], rval_mod$parameters$name)
    updateSelectInput(session, "trans",
                         selected = rval_mod$parameters$transform[idx])
    
    updateSelectInput(session, "param_vartype",
                         selected = rval_mod$parameters$vartype[idx])
    
    updateTextInput(session, "param_desc",
                         value = rval_mod$parameters$desc[idx])
    
    if(!is.null(rval$trans_parameters[[input$selected_params[1]]])){
      var_trans <- names(rval$trans_parameters[[input$selected_params[1]]])
      for(var in intersect(var_trans, names(rval_mod$trans_parameters))){
        rval_mod$trans_parameters[var] <- rval$trans_parameters[[input$selected_params[1]]][var]
      }
    }

  })
  
  ### Update transformation ################################################################
  
  observe({
    
    validate(need(rval_mod$parameters$name, "No parameters defined"))
    
    transformation <- choices()$transformation
    trans_parameters <- rval$trans_parameters
    
    new_par <- setdiff(rval_mod$parameters$name, names(transformation))
    idx_new <- match(new_par, rval_mod$parameters$name)
    
    
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
      
      rval$trans_parameters <- trans_parameters
      rval$gating_set@transformation <- transformation
      
      # update rval$gating_set_list
      if("gating_set_selected" %in% names(rval)){
        rval$gating_set_list[[rval$gating_set_selected]]$gating_set@transformation <- transformation
        rval$gating_set_list[[rval$gating_set_selected]]$trans_parameters <- trans_parameters
      }
      
      rval$update_gs <- rval$update_gs + 1
    }
    
    
    
  })
  
  ### Apply transformation ################################################################
  
  observeEvent(input$apply, {

    
    
    idx <- match(input$selected_params, rval_mod$parameters$name)
    
    # update parameters slot in GatingSet

    if(nchar(input$param_desc) >0){
      for(i in 1:length(rval$gating_set)){
        old_desc <- rval$gating_set@data[[i]]@parameters[["desc"]][idx[1]]
        if(old_desc != input$param_desc){
          rval$gating_set@data[[i]]@parameters[["desc"]][idx[1]] <- input$param_desc
        }
      }
    }

    # update description slot in GatingSet
    if(!is.null(input$param_vartype)){
      for(i in 1:length(rval$gating_set)){
        for(j in 1:length(idx)){
          desc_field <- paste("$P", idx[j], "VARTYPE", sep="")
          old_vartype <- rval$gating_set@data[[i]]@description[[desc_field]]
          if(!is.null(old_vartype)){
            if(old_vartype != input$param_vartype){
              rval$gating_set@data[[i]]@description[[desc_field]] <- input$param_vartype
            }
          }else{
            if(input$param_vartype!="numeric"){
              rval$gating_set@data[[i]]@description[[desc_field]] <- input$param_vartype
            }
          }
        }
      }
    }
    
    transformation <- choices()$transformation
    trans_parameters <- rval$trans_parameters
    
    if(length( input$selected_params )>0){
      
      var_name <- input$selected_params
      
      trans_params <- switch(input$trans,
                             "identity" = list(),
                             "asinh" = list(scale = input$scale),
                             "log" = list(base = input$base),
                             "logicle" = list(w=input$w,
                                              m=input$m,
                                              t = input$t,
                                              a = input$a))
      
      trans <- switch(input$trans,
                      "identity" = scales::identity_trans(),
                      "log" = scales::log_trans(base = input$base),
                      "asinh" = asinh_trans(scale = input$scale),
                      "logicle" = flowWorkspace::logicle_trans(w=input$w,
                                                m= input$m,
                                                t = input$t,
                                                a = input$a))
      
      for(i in 1:length(var_name)){
        transformation[[var_name[i]]] <- trans
        trans_parameters[[var_name[i]]] <- trans_params
      }
      
      rval$trans_parameters <- trans_parameters
      rval$gating_set@transformation <- transformation
      
      # update rval$gating_set_list
      if("gating_set_selected" %in% names(rval)){
        rval$gating_set_list[[rval$gating_set_selected]]$gating_set@transformation <- transformation
        rval$gating_set_list[[rval$gating_set_selected]]$trans_parameters <- trans_parameters
      }
      
      rval$update_gs <- rval$update_gs + 1
      
    }
    
    
    
  })
  
  ### add transform name and transfrom parameters to parameters table ##################################
  
  observe({
    
    validate(need(rval_mod$parameters, "No parameters defined"))
    
    transformation <- choices()$transformation
    trans_parameters <- rval$trans_parameters
    
    trans_name <- sapply(transformation, function(x){x$name})
    trans_param <- sapply(trans_parameters, function(x){
      paste( paste(names(x), as.character(x), sep = ": "), collapse="; ")})
    
    idx_match <- match(rval_mod$parameters$name, names(transformation))
    rval_mod$parameters[["transform"]] <- trans_name[idx_match]
    
    idx_match_trans <- match(rval_mod$parameters$name, names(trans_parameters))
    rval_mod$parameters[["transform parameters"]] <- trans_param[idx_match_trans]
    
    
  })
  
  params_table <- reactive({
    
    validate(need(rval_mod$parameters, "No parameters defined"))
    
    df <- rval_mod$parameters
    df$minRange <- format(df$minRange, digits = 2)
    df$maxRange <- format(df$maxRange, digits = 2)

    df
      
  })
  
  output$parameters_table <- DT::renderDT({
    DT::datatable(params_table(), rownames = TRUE, selection = list(mode = "multiple"))
  })
  
  
  return(rval)
  
  
}

### Tests ##############################################################################################
# library(shiny)
# library(shinydashboard)
# 
# if (interactive()){
# 
#   ui <- dashboardPage(
#     dashboardHeader(title = "Transform"),
#     sidebar = dashboardSidebar(disable = TRUE),
#     body = dashboardBody(
#       TransformUI("module")
#     )
#   )
# 
#   server <- function(input, output, session) {
#     rval <- reactiveValues()
#     observe({
#        # load("../flowR_utils/demo-data/Rafa2Gui/analysis/cluster.rda")
#        # fs <- build_flowset_from_df(df = res$cluster$data, origin = res$cluster$flow_set)
#        #        gs <- GatingSet(fs)
#        #        gs@transformation <-  res$cluster$transformation
#        #        add_gates_flowCore(gs, res$cluster$gates)
#        #        rval$gating_set <- gs
#       utils::data("GvHD", package = "flowCore")
#       rval$gating_set <- GatingSet(GvHD)
#     })
#     res <- callModule(Transform, "module", rval = rval)
#   }
# 
#   shinyApp(ui, server)
# 
# }
