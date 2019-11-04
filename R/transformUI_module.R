#' @title   transformUI and transform
#' @description  A shiny Module that deals with metadata
#' @param id shiny id
#' @importFrom shinydashboard box tabBox
#' @import shiny
#' @import DT
transformUI <- function(id) {
  # Create a namespace function using the provided id
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
                           "Edit table",
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
                 plotGatingSetInput(id = ns("plot_module"), simple_plot = TRUE)
               )
           )
    )
  )
  
}


#' transform server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @return a reactivevalues object with values "flow_set", "parameters" and "gates_flowCore"
#' @import flowWorkspace
#' @import flowCore
#' @import shiny
#' @import DT
#' @export
#' @rdname transformUI
transform <- function(input, output, session, rval) {
  
  `%then%` <- shiny:::`%OR%`
  
  rval_mod <- reactiveValues(init = TRUE)
  
  output$trans_param_ui <- renderUI({
    ns <- session$ns
    x <- list()
    if(input$trans == 'asinh'){
      x[[1]] <- h5("Parameters")
      x[[2]] <- numericInput(ns("base_asinh"), label = "base", value = 1)
    } else if(input$trans == 'flowJo_asinh'){
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
  
  plot_params <- reactiveValues()
  
  
  
  observe({
    
    validate( need(rval$plot_var, "No plotting parameters"))
    validate(need(rval$pdata, "No metadata available"))
    
    if(rval_mod$init){
      plot_params$samples <- rval$pdata$name[1]
      plot_params$gate <- "root"
      plot_params$xvar <- rval$plot_var[1]
      plot_params$yvar <- rval$plot_var[2]
      plot_params$plot_type <- "histogram"
      plot_params$color_var <- NULL
      plot_params$use_all_cells <- FALSE
      rval_mod$init <- FALSE
    }else{
      plot_params <- reactiveValues()
    }
    
  })
  
  observe({
    if(length(input$parameters_table_rows_selected)>0){
    plot_params$xvar <- rval$parameters$name_long[input$parameters_table_rows_selected[1]]
      if(length(input$parameters_table_rows_selected)>1){
        plot_params$yvar <- rval$parameters$name_long[input$parameters_table_rows_selected[2]]
      }
    }
  })
  
  
  
  res <- callModule(plotGatingSet, "plot_module", rval, plot_params, simple_plot = TRUE)
  callModule(simpleDisplay, "simple_display_module", res$plot)


  # observe({
  #   #plot_params <- res$params
  #   for(var in names(res$params)){
  #     plot_params[[var]] <- res$params[[var]]
  #   }
  # })
  
  
  
  
  ##########################################################################################################
  # Observe functions for data transformation
  
  #get parameters information from flow set
  observeEvent(rval$flow_set, {
    
    validate(
      need(rval$flow_set, "No flow set available")
    )
    
    ff <- rval$flow_set[[1]]
    
    #if(is.null(rval$parameters) | !setequal(rval$parameters$name, parameters(ff)$name)){
    if(is.null(rval$parameters)){
      
      desc <- as.character(parameters(ff)$desc)
      name <- as.character(parameters(ff)$name)
      # name_long <- name
      # name_long[!is.na(desc)] <- paste(name[!is.na(desc)], " (", desc[!is.na(desc)], ")", sep = "")
      
      display <- unlist(sapply(rownames(parameters(ff)@data), FUN = function(x){
        kw <- substr(x, start = 2, stop = nchar(x))
        kw <- paste(kw, "DISPLAY", sep = "")
        disp <- ff@description[[kw]]
        if(is.null(disp)){
          disp <- "NA"
        }
        return(disp)
      }))
      
      names(display) <- parameters(ff)@data$name
      
      rval$parameters <- data.frame(name = name,
                                    desc = desc,
                                    # name_long = name_long,
                                    display = display[match(name, names(display))],
                                    range = parameters(ff)@data$range,
                                    minRange = parameters(ff)@data$minRange,
                                    maxRange = parameters(ff)@data$maxRange,
                                    stringsAsFactors = FALSE)
    }
    
  })
  
  
  observe({
    validate(need(rval$parameters, "no parameters"))
    desc <- as.character(rval$parameters$desc)
    name <- as.character(rval$parameters$name)
    name_long <- name
    name_long[!is.na(desc)] <- paste(name[!is.na(desc)], " (", desc[!is.na(desc)], ")", sep = "")
    rval$parameters$name_long <- name_long
  })
  
  observeEvent(rval$parameters, {
    validate(need(rval$parameters, "No parameters"))
    rval$plot_var <- rval$parameters$name_long
    names(rval$plot_var) <- NULL
  })
  
  # Initialization of transformation for new parameters
  observe({
    
    validate(
      need(rval$parameters, "No parameters defined")
    )
    
    new_par <- setdiff(rval$parameters$name, names(rval$transformation))
    idx_new <- match(new_par, rval$parameters$name)
    
    if(length(new_par)>0){
      for(i in 1:length(new_par)){
        rval$transformation[[new_par[i]]] <- switch(rval$parameters$display[idx_new[i]],
                                                    "LOG" = logicle_trans(w=0.5, 
                                                                          m=4.5, 
                                                                          t = 262144, 
                                                                          a = 0),
                                                    identity_trans())
        rval$trans_parameters[[new_par[i]]] <- switch(rval$parameters$display[idx_new[i]],
                                                      "LOG" = list(w=0.5, 
                                                                   m=4.5, 
                                                                   t = 262144, 
                                                                   a = 0),
                                                      list())
      }
      
    }
    
  })
  
  
  observeEvent(input$apply_transformation, {
    
    if(length(input$parameters_table_rows_selected)>0){
      
      var_name <- rval$parameters$name[input$parameters_table_rows_selected]
      
      trans_params <- switch(input$trans,
                             "identity" = list(),
                             "asinh" = list(base = input$base_asinh),
                             "log" = list(base = input$base_log),
                             "flowJo_asinh" = list(m=input$m,
                                                   t = input$t,
                                                   a = input$a,
                                                   length = input$length),
                             "logicle" = list(w=input$w_logicle,
                                              m=input$m_logicle,
                                              t = input$t_logicle,
                                              a = input$a_logicle))
      
      trans <- switch(input$trans,
                      "identity" = identity_trans(),
                      "log" = log_trans(base = input$base_log),
                      "asinh" = asinh_trans(b = input$base_asinh),
                      "flowJo_asinh" = flowJo_fasinh_trans(m=input$m,
                                                           t = input$t,
                                                           a = input$a,
                                                           length = input$length),
                      "logicle" = logicle_trans(w=input$w_logicle,
                                                m=input$m_logicle,
                                                t = input$t_logicle,
                                                a = input$a_logicle))
      
      
      
      for(i in 1:length(var_name)){
        rval$transformation[[var_name[i]]] <- trans
        rval$trans_parameters[[var_name[i]]] <- trans_params
      }
      
    }
    
  })
  
  observe({
    
    validate(
      need(rval$transformation, "No transformation defined")
    )
    
    validate(
      need(rval$parameters, "No parameters")
    )
    
    trans_name <- sapply(rval$transformation, function(x){x$name})
    trans_param <- sapply(rval$trans_parameters, function(x){
      paste( paste(names(x), as.character(x), sep = ": "), collapse="; ")})
    
    idx_match <- match(rval$parameters$name, names(rval$transformation))
    
    rval$parameters$transform <- trans_name[idx_match]
    rval$parameters[["transform parameters"]] <- trans_param[idx_match]
    
    
  })
  
  output$parameters_table <- DT::renderDataTable({

    validate(
      need(rval$parameters, "No data imported")
    )
    df <- rval$parameters
    df$minRange <- format(df$minRange, digits = 2)
    df$maxRange <- format(df$maxRange, digits = 2)
    df[["channel_name"]] <- df$name_long
    DT::datatable(
      df[, c("name", "desc", "channel_name", "transform", "transform parameters", "minRange", "maxRange",  "range", "display" )],
      rownames = FALSE)
  })
  
  
  
  #Edit data table
  output$parameters <- renderDT({validate(need(rval$parameters, "No metadata")); rval$parameters},
                           rownames = FALSE,
                           selection = 'none',
                           editable = 'cell',
                           server = TRUE)
  
  proxy = dataTableProxy('parameters')
  observeEvent(input$parameters_cell_edit, {
    info = input$parameters_cell_edit
    info$col <- info$col + 1
    str(info)  # check what info looks like (a data frame of 3 columns)
    if(info$col == 2){
      rval$parameters <<- editData(rval$parameters, info)
      replaceData(proxy, rval$parameters, resetPaging = FALSE)
    }
      # important
    # the above steps can be merged into a single editData() call; see examples below
  })
  
  
  return(rval)
  
  
}