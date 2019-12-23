#' @title CompensationUI and Compensation
#' @description  A shiny Module that deals with compensation
#' @param id shiny id
#' @import shiny
#' @importFrom shinydashboard box tabBox
#' @importFrom plotly plotlyOutput
#' @importFrom DT DTOutput
#' @export
CompensationUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
    fluidRow(
      
      column(width = 6,
             tabBox(title = "",
                    width = NULL, height = NULL,
                    tabPanel(title = "Matrix",
                             plotly::plotlyOutput(ns("heatmap_spill")),
                             checkboxInput(ns("show_all_channels"), "Show all channels", FALSE)
                    ),
                    tabPanel("Set",
                             selectInput(ns("xvar_comp"), label = "column (chanel)", choices = NULL, selected = NULL),
                             selectInput(ns("yvar_comp"), label = "row (fluorophore)", choices = NULL, selected = NULL),
                             numericInput(ns("spill_value"), 
                                          label = "spillover value", 
                                          value = 0, 
                                          min = 0, 
                                          max = 2, 
                                          step = 0.01),
                             numericInput(ns("step_size"), label = "step size", value = 0.01),
                             actionButton(ns("set_spill_value"), "set value")
                    ),
                    tabPanel(title = "Table",
                             actionButton(ns("reset_comp"), "reset"),
                             downloadButton(ns("download_spill")),
                             br(),
                             br(),
                             div(style = 'overflow-x: scroll', DT::DTOutput(ns("spill_table"))),
                             br()    
                    )
             ),
             tabBox(title = "",
                    width = NULL, height = NULL,   
                    tabPanel("Import",
                             width = NULL, height = NULL,
                             fileInput(inputId = ns("spill_file"), 
                                       label = "Choose spillover matrix file", 
                                       multiple = FALSE),
                             selectInput(ns("sep_spill"), "column separator", choices = c("comma", "semi-column", "tab", "space"), selected = "tab"),
                             div(style = 'overflow-x: scroll', DT::DTOutput(ns("spill_imported"))),
                             br(),
                             actionButton(ns("set_spillover_matrix"), "Set spillover matrix")
                    ),
                    tabPanel("Compute",
                             width = NULL, height = NULL,
                             uiOutput(ns("ui_compute_spill")),
                             actionButton(ns("add_spill_param"), "Add parameter"),
                             br(),
                             br(),
                             selectizeInput(ns("spill_params"), "Spillover parameters", choices = NULL, selected = NULL, multiple = TRUE),
                             actionButton(ns("compute_spillover_matrix"), "Compute spillover matrix")
                    )
             )
    ),
    column(width = 6,
           tabBox(title = "",
                  width = NULL, height = NULL,
                  tabPanel(title = "Plot",
                           simpleDisplayUI(ns("simple_display_module"), nrow = 2, size = 200)
                  ),
                  tabPanel(title = "Parameters",
                           plotGatingSetInput(id = ns("plot_module"), simple_plot = FALSE)
                  )
           )
           
    )
  )
  
}

#' Compensation server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param rval A reactive values object
#' @return The updated reactiveValues object \code{rval}
#' @import shiny
#' @importFrom flowCore parameters description
#' @importFrom heatmaply heatmaply
#' @importFrom plotly renderPlotly event_data
#' @importFrom DT renderDT
#' @importFrom utils read.table
#' @importFrom  stats median
#' @export
#' @rdname CompensationUI
Compensation <- function(input, output, session, rval) {

  plot_params <- reactiveValues()
  rval_mod <- reactiveValues(init = TRUE)
  
  observe({
    
    validate( need(rval$df_spill, "No plotting parameters"))
    validate(need(rval$pdata, "No metadata available"))
    
    if(rval_mod$init){
      plot_params$samples <- rval$pdata$name[1]
      plot_params$gate <- "root"
      plot_params$xvar <- colnames(rval$df_spill)[1]
      plot_params$yvar <- colnames(rval$df_spill)[2]
      plot_params$plot_type <- "hexagonal"
      plot_params$color_var <- NULL
      plot_params$use_all_cells <- FALSE
      rval_mod$init <- FALSE
    }
    
  })

  observeEvent(c(input$xvar_comp, input$yvar_comp, input$show_all_channels), {
    
    #reset plot parameters (only non null parameters will be updated)
    for(var in names(reactiveValuesToList(plot_params))){
      plot_params[[var]] <- NULL
    }
    
    if(input$show_all_channels){
      channels <- rval$parameters$name_long[match(colnames(rval$df_spill), rval$parameters$name)]
      plot_params$xvar <- setdiff(channels, input$yvar_comp)
    }else{
      plot_params$xvar <- input$xvar_comp
    }
    plot_params$yvar <- input$yvar_comp
  })
  
  res <- callModule(plotGatingSet, "plot_module", rval, plot_params, simple_plot = FALSE)
  callModule(simpleDisplay, "simple_display_module", res$plot)
  
  ##########################################################################################################
  # Observe functions for compensation
  
  observeEvent(rval$flow_set, {
    
    validate(need(rval$flow_set, "no flow-set available"))
    validate(need(length(flowCore::parameters(rval$flow_set[[1]])$name) < 100, "Maximum number of parameters exceeded (100)"))
    
    
    if(is.null(rval$df_spill)){
      fs <- rval$flow_set
      params <- flowCore::parameters(fs[[1]])
      m <- diag( length(params$name) )
      colnames(m) <- params$name
      rval$df_spill <- as.data.frame(m)
      row.names(rval$df_spill) <- colnames(rval$df_spill)
      
      #rval$df_spill <- NULL
      
      for(i in 1:length(fs)){
        desc <- flowCore::description(fs[[i]])
        if("SPILL" %in% names(desc)){
          df <- as.data.frame(desc[["SPILL"]])
          is_identity <- sum(apply(df, MARGIN = 1, FUN = function(x){sum(x==0) == (length(x)-1)})) == dim(df)[1]
          if(!is_identity){
            rval$df_spill <- df
            row.names(rval$df_spill) <- colnames(rval$df_spill)
            break
          }
        }
      }
      
      rval$df_spill_original <- rval$df_spill
    }
    
  })
  
  
  
  ##################################################################################################
  # Computing comp matrix
  ##################################################################################################
  observeEvent(input$add_spill_param, {
    
    validate(
      need(rval$gating_set, "No gating set available")
    )
    
    
    df_pos <- get_data_gs(gs = rval$gating_set,
                          sample = input$sample_pos,
                          subset = input$gate_pos,
                          spill = NULL)
    df_pos <- df_pos[names(df_pos) %in% rval$flow_set@colnames]
    pos_values <- apply(df_pos, MARGIN = 2, FUN = stats::median,  na.rm = TRUE)
    pos_values <- pos_values[rval$flow_set@colnames]
    names(pos_values) <- rval$flow_set@colnames
    rval$pos_values[[input$fluo]] <- pos_values
    
    print(rval$pos_values)
    
    df_neg <- get_data_gs(gs = rval$gating_set,
                          sample = input$sample_neg,
                          subset = input$gate_neg,
                          spill = NULL)
    
    df_neg <- df_neg[names(df_neg) %in% rval$flow_set@colnames]
    neg_values <- apply(df_neg, MARGIN = 2, FUN = stats::median, na.rm = TRUE)
    neg_values <- neg_values[rval$flow_set@colnames]
    names(neg_values) <- rval$flow_set@colnames
    rval$neg_values[[input$fluo]] <- neg_values
    
    print(rval$pos_values)
    
  })
  
  observe({
    updateSelectInput(session, "spill_params", choices = names(rval$pos_values), selected = names(rval$pos_values))
  })
  
  observeEvent(input$compute_spillover_matrix, {
    validate(
      need(length(input$spill_params)>0, "Not enough parameters selected")
    )
    
    df_pos_tot <- data.frame(do.call(rbind, rval$pos_values[input$spill_params]), check.names = FALSE)
    row.names(df_pos_tot) <- input$spill_params
    df_pos_tot <- df_pos_tot[input$spill_params]
    #print(df_pos_tot)
    
    df_neg_tot <- data.frame(do.call(rbind, rval$neg_values[input$spill_params]), check.names = FALSE)
    row.names(df_neg_tot) <- input$spill_params
    df_neg_tot <- df_neg_tot[input$spill_params]
    #print(df_neg_tot)
    
    df_spill <- df_pos_tot
    for(i in 1:length(input$spill_params)){
      neg <- rval$neg_values[[input$spill_params[i]]][[input$spill_params[i]]]
      pos <- rval$pos_values[[input$spill_params[i]]][[input$spill_params[i]]]
      df_spill[input$spill_params[i]] <- (df_pos_tot[input$spill_params[i]] - neg)/(pos - neg)
    }
    
    #print(df_spill)
    rval$df_spill <- df_spill
    
  })
  
  ##################################################################################################
  
  observe({
    validate(
      need(rval$parameters, "No parameters available")
    )
    comp_params <- rval$parameters$name_long
    names(comp_params) <- NULL
    updateSelectInput(session, "xvar_comp", choices = comp_params, selected = comp_params[1])
    updateSelectInput(session, "yvar_comp", choices = comp_params, selected = comp_params[2])
  })
  
  observe({
    validate(
      need(input$spill_file$datapath, "Please select a file to import")
    )
    
    sep <- switch(input$sep_spill,
                  "comma" = ",",
                  "semi-column" = ";",
                  "tab" = "\t",
                  "space" = " ")
    
    rval$df_spill_imported <- utils::read.table(file = input$spill_file$datapath, 
                                         sep = sep,
                                         fill = TRUE,
                                         quote = "\"",
                                         header = TRUE,
                                         check.names = FALSE)
    
  })
  
  observeEvent(input$set_spillover_matrix,{
    df <- as.data.frame(rval$df_spill_imported)
    row.names(df) <- colnames(df)
    df <- df[row.names(df) %in% colnames(rval$flow_set), colnames(df) %in% colnames(rval$flow_set)]
    rval$df_spill <- df
    
  })
  
  
  observe({
    
    validate(
      need(length(input$file_comp)>0, "Please select a file to load")
    )
    
    sep <- switch(input$sep_comp,
                  "comma" = ",",
                  "semi-column" = ";",
                  "tab" = "\t")
    
    rval$df_comp <- read.csv(input$file_comp$datapath, sep = sep, header = TRUE, quote = "\"", fill = TRUE)
    names(rval$df_comp)[1] <- "name"
  })
  
  observeEvent(input$import_comp, {
    idx_match_row <- match(rval$pdata$name, rval$df_comp[,1])
    idx_match_col <- match(rval$pdata$name, names(rval$df_comp))
    rval$df_spill <- rval$df_comp[idx_match_row, idx_match_col]
  })
  
  observeEvent(input$reset_comp, {
    rval$df_spill <- rval$df_spill_original
  })
  
  observeEvent(input$set_spill_value, {
    
    xvar <- rval$parameters$name[match(input$xvar_comp, rval$parameters$name_long)]
    yvar <- rval$parameters$name[match(input$yvar_comp, rval$parameters$name_long)]
    idx_x <- match(xvar, names(rval$df_spill))
    idx_y <- match(yvar, names(rval$df_spill))
    rval$df_spill[idx_y, idx_x] <- input$spill_value
    
  })
  
  observe({
    rval$spill <- NULL
    if("apply_comp" %in% names(rval)){
      if(rval$apply_comp){
        rval$spill <- rval$df_spill
      }
    }
    
    
  })
  
  observe({
    df <- rval$df_spill
    event.data <- plotly::event_data("plotly_click", source = "select_heatmap")
    idx_y <- dim(df)[1] - event.data$pointNumber[[1]][1]
    idx_x <- event.data$pointNumber[[1]][2] + 1
    
    if(length(idx_x)>0){
      updateSelectInput(session, "xvar_comp", 
                        selected = rval$parameters$name_long[match(colnames(df)[idx_x], rval$parameters$name)])
    }
    if(length(idx_y)>0){
      updateSelectInput(session, "yvar_comp", 
                        selected = rval$parameters$name_long[match(row.names(df)[idx_y], rval$parameters$name)])
      
    }
    
  })
  
  observe({
    df <- rval$df_spill
    xvar <- rval$parameters$name[match(input$xvar_comp, rval$parameters$name_long)]
    yvar <- rval$parameters$name[match(input$yvar_comp, rval$parameters$name_long)]
    idx_x <- match(xvar, colnames(df))
    idx_y <- match(yvar, row.names(df))
    #if(length(idx_x)>0 & length(idx_y)>0){
    updateNumericInput(session, "spill_value", value = df[idx_y, idx_x])
    
    #}
    
  })
  
  observe({
    updateNumericInput(session, "spill_value", step = input$step_size)
  })
  
  output$spill_imported <- DT::renderDT({
    validate(
      need(rval$df_spill_imported, "No spillover data imported")
    )
    as.data.frame(rval$df_spill_imported)
  })
  
  output$spill_table <- DT::renderDT({
    
    validate(
      need(rval$df_spill, "No spillover matrix")
    )
    
    df <- rval$df_spill
    format(df, digits =3)
  })
  
  output$heatmap_spill <- plotly::renderPlotly({
    
    validate(
      need(rval$df_spill, "No spillover matrix")
    )
    
    df <- rval$df_spill
    df[df == 0] <- NA
    df_log <- log10(df)
    
    #print(df)
    
    p <- heatmaply::heatmaply(df_log,
                   #colors = c(rgb(1,1,1), rgb(1,0,0)),
                   colors= viridis,
                   plot_method="plotly",
                   #scale_fill_gradient_fun = scale_fill_viridis(trans = scales::log10_trans(), name = "spillover"),
                   Rowv = NULL,
                   Colv = NULL,
                   column_text_angle = 90,
                   xlab = "detection chanel",
                   ylab = "emitting fluorophore",
                   fontsize_row = 10,
                   fontsize_col = 10,
                   cellnote_size = 6,
                   hide_colorbar = TRUE,
                   main = "spillover matrix",
                   custom_hovertext = df,
                   margins = c(50, 50, 50, 0)
    )
    p$x$source <- "select_heatmap"
    
    p
    
  })
  
  output$download_spill <- downloadHandler(
    filename = "spillover_matrix.txt",
    content = function(file) {
      write.table(rval$df_spill, file = file, row.names = FALSE, quote = FALSE, sep = "\t")
    }
  )
  
  output$ui_compute_spill <- renderUI({

    ns <- session$ns
    
    validate(
      need(rval$flow_set, "No flow set available")
    )
    
    tagList(
      selectInput(ns("fluo"), 
                  "fluorophore",
                  choices = rval$flow_set@colnames,
                  selected = rval$flow_set@colnames[1]),
      
      selectInput(ns("sample_pos"),
                  "sample pos",
                  choices = pData(rval$flow_set)$name,
                  selected = pData(rval$flow_set)$name[1]),
      
      selectInput(ns("gate_pos"),
                  "gate pos",
                  choices = names(rval$gates_flowCore),
                  selected = "root"),
      
      selectInput(ns("sample_neg"), 
                  "sample neg",
                  choices = pData(rval$flow_set)$name,
                  selected = pData(rval$flow_set)$name[1]),
      
      selectInput(ns("gate_neg"),
                  "gate neg",
                  choices = names(rval$gates_flowCore),
                  selected = "root")
    )
    
  })
  
  return(rval)
  
  
}