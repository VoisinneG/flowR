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
    
    column(width = 5,
           tabBox(title = "Chanels",
                  width = NULL, height = NULL,
                  tabPanel(title = "Table",
                           "Select chanels",
                           br(),
                           br(),
                           div(style = 'overflow-x: scroll', DT::dataTableOutput(ns("parameters_table")))
                           
                  ),
                  tabPanel(title = "Transform",
                           selectInput(ns("trans"), "transformation", 
                                       choices = c("identity", "logicle", "asinh", "flowJo_asinh", "log"), 
                                       selected = "identity"),
                           conditionalPanel(condition = "input.trans == 'asinh'",
                                            h5("Parameters"),
                                            numericInput(ns("base_asinh"), label = "base", value = 1)
                           ),
                           conditionalPanel(condition = "input.trans == 'flowJo_asinh'",
                                            h5("Parameters"),
                                            numericInput(ns("m"), label = "m", value = 5),
                                            numericInput(ns("t"), label = "t", value = 12000),
                                            numericInput(ns("a"), label = "a", value = 0.7),
                                            numericInput(ns("length"), label = "length", value = 256)
                           ),
                           conditionalPanel(condition = "input.trans == 'logicle'",
                                            h5("Parameters"),
                                            numericInput(ns("w_logicle"), label = "w", value = 0.5),
                                            numericInput(ns("t_logicle"), label = "t", value = 262144),
                                            numericInput(ns("m_logicle"), label = "m", value = 4.5),
                                            numericInput(ns("a_logicle"), label = "a", value = 0)
                           ),
                           conditionalPanel(condition = "input.trans == 'log'",
                                            h5("Parameters"),
                                            numericInput(ns("base_log"), label = "base", value = 10)
                           ),
                           br(),
                           actionButton(ns("apply_transformation"), label = "apply to selected chanels"),
                           br()
                  )
           )
    ),
    column(width = 7,
           tabBox(title = "Plot",
                  width = NULL, height = NULL,
                  tabPanel("Plot",
                           plotOutput(ns("plot_trans"))
                           
                  ),
                  tabPanel("Select",
                           selectInput(ns("sample_selected_trans"), label = "Sample", choices = NULL, selected = NULL),
                           actionButton(ns("previous_frame_trans"), "previous"),
                           actionButton(ns("next_frame_trans"), "next"),
                           br(),
                           br(),
                           selectizeInput(ns("gate_trans"), 
                                          label = "subset", 
                                          choices = "root", 
                                          selected = "root",
                                          multiple = FALSE),
                           selectInput(ns("xvar_trans"), label = "x variable", choices = NULL, selected = NULL),
                           selectInput(ns("yvar_trans"), label = "y variable", choices = NULL, selected = NULL)
                  ),
                  tabPanel("Options",
                           selectInput(ns("plot_type_trans"), label = "plot type",
                                       choices = c("hexagonal", "histogram", "dots", "contour"),
                                       selected = "histogram"),
                           checkboxInput(ns("legend_trans"), "show legend", value = FALSE),
                           checkboxInput(ns("norm_trans"), "normalize (set max to 1)", value = TRUE),
                           checkboxInput(ns("smooth_trans"), "smooth", value = FALSE),
                           selectInput(ns("color_var_trans"), "color variable",
                                       choices = "none",
                                       selected = "none"),
                           numericInput(ns("bin_number_trans"), label = "number of bins", value = 50),
                           numericInput(ns("alpha_trans"), label = "alpha", value = 0.5),
                           numericInput(ns("size_trans"), label = "size", value = 1)
                           
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
  
  #get parameters information from flow set
  observe({
    
    validate(
      need(rval$flow_set, "No flow set available")
    )
    
    ff <- rval$flow_set[[1]]
    
    if(!setequal(rval$parameters$name, parameters(ff)$name)){
      desc <- parameters(ff)$desc
      name <- parameters(ff)$name
      print(parameters(ff)$name)
      name_long <- name
      name_long[!is.na(desc)] <- paste(name[!is.na(desc)], " (", desc[!is.na(desc)], ")", sep = "")
      
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
                                    name_long = name_long,
                                    display = display[match(name, names(display))],
                                    range = parameters(ff)@data$range,
                                    minRange = parameters(ff)@data$minRange,
                                    maxRange = parameters(ff)@data$maxRange,
                                    stringsAsFactors = FALSE)
    }
    
  })
  
  observeEvent(rval$parameters, {
    validate(need(rval$parameters, "No parameters"))
    rval$plot_var <- rval$parameters$name_long
    names(rval$plot_var) <- NULL
  })
  
  #Update available variables
  observe({
    
    validate(
      need(rval$parameters, "No parameters defined")
    )
    
    updateSelectInput(session, "xvar_trans", choices = rval$parameters$name_long, selected = rval$parameters$name_long[1])
    updateSelectInput(session, "yvar_trans", choices = rval$parameters$name_long, selected = rval$parameters$name_long[2])
  })
  
  ##########################################################################################################
  # Observe functions for data transformation
  
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
                                                    "LOG" = logicle_trans(w=input$w_logicle, 
                                                                          m=input$m_logicle, 
                                                                          t = input$t_logicle, 
                                                                          a = input$a_logicle),
                                                    identity_trans())
        rval$trans_parameters[[new_par[i]]] <- switch(rval$parameters$display[idx_new[i]],
                                                      "LOG" = list(w=input$w_logicle, 
                                                                   m=input$m_logicle, 
                                                                   t = input$t_logicle, 
                                                                   a = input$a_logicle),
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
  
  observe({
    updateSelectInput(session, "xvar_trans", 
                      selected = rval$parameters$name_long[input$parameters_table_rows_selected[1]])
    if(length(input$parameters_table_row_selected)>1){
      updateSelectInput(session, "yvar_trans", 
                        selected = rval$parameters$name_long[input$parameters_table_rows_selected[2]])
    }
  })
  
  observe({
    validate(
      need(rval$flow_set, "No flow set available")
    )

    updateSelectInput(session, "sample_selected_trans",
                      choices = pData(rval$flow_set)$name,
                      selected = pData(rval$flow_set)$name[1])
    
  })
  
  observeEvent(input$next_frame_trans, {
    if(!is.null(rval$flow_set)){
      idx <- which(rval$pdata$name == input$sample_selected_trans)
      idx <- idx +1
      if(idx > length(rval$pdata$name)){
        idx <- 1
      }
      updateSelectInput(session, "sample_selected_trans", selected = rval$pdata$name[idx])
      
    }
  })
  
  observeEvent(input$previous_frame_trans, {
    if(!is.null(rval$flow_set)){
      idx <- which(rval$pdata$name == input$sample_selected_trans)
      idx <- idx - 1
      if(idx < 1){
        idx <- length(rval$pdata$name)
      }
      updateSelectInput(session, "sample_selected_trans", selected = rval$pdata$name[idx])
    }
  })
  
  output$parameters_table <- DT::renderDataTable({
    
    validate(
      need(rval$parameters, "No data imported")
    )
    df <- rval$parameters
    df$minRange <- format(df$minRange, digits = 2)
    df$maxRange <- format(df$maxRange, digits = 2)
    df[["chanel_name"]] <- df$name_long
    DT::datatable(
      df[, c("chanel_name", "transform", "transform parameters", "display", "range", "minRange", "maxRange", "name", "desc")], 
      rownames = FALSE)
    
  })
  
  output$plot_trans <- renderPlot({
    
    validate(
      need(rval$gating_set, "Empty gating set") %then%
        need(input$sample_selected_trans, "Please select a sample") %then%
        need(input$gate_trans, "Please select a subset")
    )
    
    idx_x <- match(input$xvar_trans, rval$parameters$name_long)
    idx_y <- match(input$yvar_trans, rval$parameters$name_long)
    xvar <- rval$parameters$name[idx_x]
    yvar <- rval$parameters$name[idx_y]
    
    if(input$color_var_trans %in% rval$parameters$name_long){
      color_var <- rval$parameters$name[match(input$color_var_trans, rval$parameters$name_long)]
    }else{
      color_var <- input$color_var
    }
    
    #color_var <- rval$parameters$name[match(input$color_var_gate, rval$parameters$name_long)]
    #color_var <- input$color_var
    
    axis_labels <- rval$parameters$name_long
    names(axis_labels) <- rval$parameters$name
    
    transformation <- NULL
    if(rval$apply_trans){
      transformation <- rval$transformation
    }
    
    p <- plot_gs(df = rval$df_tot,
                 gs = rval$gating_set, 
                 sample = input$sample_selected_trans,
                 subset = input$gate_trans, 
                 spill = rval$spill,
                 xvar = xvar, 
                 yvar = yvar, 
                 color_var = color_var, 
                 gate = NULL, 
                 type = input$plot_type_trans, 
                 bins = input$bin_number_trans,
                 alpha = input$alpha_trans,
                 size = input$size_trans,
                 norm_density = input$norm_trans,
                 smooth = input$smooth_trans,
                 transformation =  transformation,
                 show.legend = input$legend_trans,
                 axis_labels = axis_labels)
    
    if(!is.null(p)){
      p <- p + xlab(input$xvar_trans) 
      if(input$plot_type_trans != "histogram"){
        p <- p + ylab(input$yvar_trans)
      }
    }
    
    p
    
  })
  
  return(rval)
  
  
}