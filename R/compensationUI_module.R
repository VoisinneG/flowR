#' @title   compensationUI and compensation
#' @description  A shiny Module that deals with metadata
#' @param id shiny id
#' @importFrom shinydashboard box tabBox
#' @import shiny
#' @import DT
compensationUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
    fluidRow(
      
      column(width = 6,
             tabBox(title = "",
                    width = NULL, height = NULL,
                    tabPanel(title = "Heatmap",
                             plotlyOutput(ns("heatmap_spill"))
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
                             div(style = 'overflow-x: scroll', DT::dataTableOutput(ns("spill_table"))),
                             br()    
                    )
             ),
             tabBox(title = "",
                    width = NULL, height = NULL,
                    # tabPanel("Save",
                    #          width = NULL, height = NULL,
                    #          "Save spillover matrix in .txt format :",
                    #          br(),
                    #          br(),
                    #          downloadButton("download_spill")
                    # ),               
                    tabPanel("Import",
                             width = NULL, height = NULL,
                             fileInput(inputId = ns("spill_file"), 
                                       label = "Choose spillover matrix file", 
                                       multiple = FALSE),
                             selectInput(ns("sep_spill"), "column separator", choices = c("comma", "semi-column", "tab", "space"), selected = "tab"),
                             div(style = 'overflow-x: scroll', DT::dataTableOutput(ns("spill_imported"))),
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
                           plotOutput(ns("plot_comp"))
                  ),
                  tabPanel(title = "Parameters",
                           plotGatingSetInput(id = ns("plot_module"), simple_plot = TRUE)
                  ),
                  tabPanel(title = "Save",
                           numericInput(ns("width_plot"), label = "width", value = 5),
                           numericInput(ns("height_plot"), label = "height", value = 5),
                           downloadButton(ns("download_plot"), "Save plot")
                  )
           )
           
    )
  )
  
}


#' compensation server function
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
compensation <- function(input, output, session, rval) {
  
  `%then%` <- shiny:::`%OR%`

  plot_params <- reactiveValues()
  
  observe({
    plot_params$xvar <- input$xvar_comp
    plot_params$yvar <- input$yvar_comp
  })
  
  plot_comp <- callModule(plotGatingSet, "plot_module", rval, plot_params, simple_plot = TRUE)$plot
  
  output$plot_comp <- renderPlot({
    plot_comp()
  })

  ##########################################################################################################
  # Observe functions for compensation
  
  observeEvent(rval$flow_set_imported, {
    fs <- rval$flow_set_imported
    m <- diag( length(parameters(fs[[1]])$name) )
    colnames(m) <- parameters(fs[[1]])$name
    rval$df_spill <- as.data.frame(m)
    row.names(rval$df_spill) <- colnames(rval$df_spill)
    
    #rval$df_spill <- NULL
    
    for(i in 1:length(fs)){
      if("SPILL" %in% names(description(fs[[i]]))){
        df <- as.data.frame(description(fs[[i]])[["SPILL"]])
        is_identity <- sum(apply(X=df, MARGIN = 1, FUN = function(x){sum(x==0) == (length(x)-1)})) == dim(df)[1]
        if(!is_identity){
          rval$df_spill <- df
          row.names(rval$df_spill) <- colnames(rval$df_spill)
          break
        }
      }
    }
    
    rval$df_spill_original <- rval$df_spill
    
  })
  
  
  observeEvent(input$add_spill_param, {
    
    validate(
      need(rval$gating_set, "No gating set available")
    )
    
    
    df_pos <- get_data_gs(gs = rval$gating_set,
                          sample = input$sample_pos,
                          subset = input$gate_pos,
                          spill = NULL)
    df_pos <- df_pos[names(df_pos) %in% rval$flow_set@colnames]
    pos_values <- apply(df_pos, MARGIN = 2, FUN = median,  na.rm = TRUE)
    pos_values <- pos_values[rval$flow_set@colnames]
    names(pos_values) <- rval$flow_set@colnames
    rval$pos_values[[input$fluo]] <- pos_values
    
    
    df_neg <- get_data_gs(gs = rval$gating_set,
                          sample = input$sample_neg,
                          subset = input$gate_neg,
                          spill = NULL)
    df_neg <- df_neg[names(df_neg) %in% rval$flow_set@colnames]
    neg_values <- apply(df_neg, MARGIN = 2, FUN = median, na.rm = TRUE)
    neg_values <- neg_values[rval$flow_set@colnames]
    names(neg_values) <- rval$flow_set@colnames
    rval$neg_values[[input$fluo]] <- neg_values
    
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
    print(df_pos_tot)
    
    df_neg_tot <- data.frame(do.call(rbind, rval$neg_values[input$spill_params]), check.names = FALSE)
    row.names(df_neg_tot) <- input$spill_params
    df_neg_tot <- df_neg_tot[input$spill_params]
    print(df_neg_tot)
    
    df_spill <- df_pos_tot
    for(i in 1:length(input$spill_params)){
      neg <- rval$neg_values[[input$spill_params[i]]][[input$spill_params[i]]]
      pos <- rval$pos_values[[input$spill_params[i]]][[input$spill_params[i]]]
      df_spill[input$spill_params[i]] <- (df_pos_tot[input$spill_params[i]] - neg)/(pos - neg)
    }
    
    print(df_spill)
    rval$df_spill <- df_spill
    
  })
  
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
    
    rval$df_spill_imported <- read.table(file = input$spill_file$datapath, 
                                         sep = sep, 
                                         fill = TRUE, 
                                         quote = "\"", 
                                         header = TRUE, 
                                         check.names = FALSE)
    
  })
  
  observeEvent(input$set_spillover_matrix,{
    df <- as.data.frame(rval$df_spill_imported)
    row.names(df) <- colnames(df)
    df <- df[row.names(df) %in% rval$flow_set@colnames, colnames(df) %in% rval$flow_set@colnames]
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
    event.data <- event_data("plotly_click", source = "select_heatmap")
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
  
  output$spill_imported <- DT::renderDataTable({
    validate(
      need(rval$df_spill_imported, "No spillover data imported")
    )
    as.data.frame(rval$df_spill_imported)
  })
  
  output$spill_table <- DT::renderDataTable({
    
    validate(
      need(rval$df_spill, "No spillover matrix")
    )
    
    df <- rval$df_spill
    format(df, digits =3)
  })
  
  output$heatmap_spill <- renderPlotly({
    
    validate(
      need(rval$df_spill, "No spillover matrix")
    )
    
    df <- rval$df_spill
    df[df == 0] <- NA
    df_log <- log10(df)
    p <- heatmaply(df,
                   #colors = c(rgb(1,1,1), rgb(1,0,0)),
                   #colors= viridis,
                   plot_method="ggplot",
                   scale_fill_gradient_fun = scale_fill_viridis(trans = log10_trans(), name = "spillover"),
                   Rowv = NULL,
                   Colv = NULL,
                   column_text_angle = 90,
                   xlab = "detection chanel",
                   ylab = "emitting fluorophore",
                   fontsize_row = 6,
                   fontsize_col = 6,
                   cellnote_size = 6,
                   hide_colorbar = TRUE,
                   main = "spillover matrix",
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
  
  output$download_plot <- downloadHandler(
    filename = "plot.pdf",
    content = function(file) {
      pdf(file, width = input$width_plot, height = input$height_plot)
      print(plot_comp())
      dev.off()
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
                  choices = getNodes(rval$gating_set),
                  selected = "root"),
      
      selectInput(ns("sample_neg"), 
                  "sample neg",
                  choices = pData(rval$flow_set)$name,
                  selected = pData(rval$flow_set)$name[1]),
      
      selectInput(ns("gate_neg"),
                  "gate neg",
                  choices = getNodes(rval$gating_set),
                  selected = "root")
    )
    
  })
  
  return(rval)
  
  
}