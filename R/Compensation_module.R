#' Visualize, import, export and edit compensation matrices associated with a GatingSet
#' @param id shiny id
#' @import shiny
#' @importFrom shinydashboard box tabBox
#' @importFrom plotly plotlyOutput
#' @importFrom DT DTOutput
CompensationUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
    fluidRow(
      
      column(width = 6,
             tabBox(title = "",
                    width = NULL, height = NULL,
                    tabPanel(title = "Matrix",
                             selectInput(ns("comp_mat"), "Select matrix", 
                                         choices = NULL, selected = NULL),
                             box(title = "Heatmap", width = NULL, height = NULL, collapsible = TRUE, collapsed = TRUE,
                                 plotly::plotlyOutput(ns("heatmap_spill"))
                                 ),
                             box(title = "Edit", width = NULL, height = NULL, collapsible = TRUE, collapsed = TRUE,
                                 selectInput(ns("xvar_comp"), label = "column (channel)", 
                                             choices = NULL, selected = NULL),
                                 selectInput(ns("yvar_comp"), label = "row (fluorophore)", 
                                             choices = NULL, selected = NULL),
                                 numericInput(ns("spill_value"), 
                                              label = "spillover (%)", 
                                              value = 0, 
                                              min = 0, 
                                              max = 100, 
                                              step = 1),
                                 numericInput(ns("step_size"), label = "step size", value = 1),
                                 actionButton(ns("set_spill_value"), "set value")
                                 ),
                             box(title = "Table", width = NULL, height = NULL, collapsible = TRUE, collapsed = TRUE,
                                 br(),
                                 div(style = 'overflow-x: scroll', DT::DTOutput(ns("spill_table"))),
                                 br() 
                                 ),
                             box(title = "Duplicate", width = NULL, height = NULL, collapsible = TRUE, collapsed = TRUE,
                                 textInput(ns("new_name"), "CompMat name", value = "new name"),
                                 actionButton(ns("duplicate"), "duplicate")
                             ),
                             downloadButton(ns("download_spill")),
                    ),
                    # tabPanel("Edit",
                    #          selectInput(ns("xvar_comp"), label = "column (channel)", 
                    #                      choices = NULL, selected = NULL),
                    #          selectInput(ns("yvar_comp"), label = "row (fluorophore)", 
                    #                      choices = NULL, selected = NULL),
                    #          numericInput(ns("spill_value"), 
                    #                       label = "spillover (%)", 
                    #                       value = 0, 
                    #                       min = 0, 
                    #                       max = 100, 
                    #                       step = 1),
                    #          numericInput(ns("step_size"), label = "step size", value = 1),
                    #          actionButton(ns("set_spill_value"), "set value")
                    # ),
                    # tabPanel(title = "Table",
                    #          actionButton(ns("reset_comp"), "reset"),
                    #          actionButton(ns("apply_comp"), "apply"),
                    #          downloadButton(ns("download_spill")),
                    #          br(),
                    #          br(),
                    #          div(style = 'overflow-x: scroll', DT::DTOutput(ns("spill_table"))),
                    #          br()    
                    # ),
                    tabPanel(title = "Compensation",
                             box(title = "Table", width = NULL, height = NULL, collapsible = TRUE, collapsed = TRUE,
                                 br(),
                                 div(style = 'overflow-x: scroll', DT::DTOutput(ns("spill_per_sample_table"))),
                                 br() 
                             ),
                             box(title = "Edit", width = NULL, height = NULL, collapsible = TRUE, collapsed = TRUE,
                                 selectizeInput(ns("selected_samples"), "Select samples", 
                                                choices = NULL, selected = NULL, multiple = TRUE),
                                 selectInput(ns("selected_matrix"), "Select matrix", choices = NULL, selected = NULL),
                                 actionButton(ns("apply_matrix"), "Apply to selected samples")
                             )
                    )
             ),
             tabBox(title = "",
                    width = NULL, height = NULL,   
                    tabPanel("Import",
                             width = NULL, height = NULL,
                             actionButton(ns("import_from_gs"), "Import from GatingSet"),
                             fileInput(inputId = ns("spill_file"), 
                                       label = "Choose spillover matrix file", 
                                       multiple = FALSE),
                             selectInput(ns("sep_spill"), "column separator", 
                                         choices = c("comma", "semi-column", "tab", "space"), 
                                         selected = "tab"),
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
                             selectizeInput(ns("spill_params"), "Spillover parameters", 
                                            choices = NULL, selected = NULL, multiple = TRUE),
                             actionButton(ns("compute_spillover_matrix"), "Compute spillover matrix")
                    )
             )
    ),
    column(width = 6,
           tabBox(title = "",
                  width = NULL, height = NULL,
                  tabPanel(title = "Plot",
                           checkboxInput(ns("show_all_channels"), "Show all channels", FALSE),
                           simpleDisplayUI(ns("simple_display_module"))
                  ),
                  tabPanel(title = "Parameters",
                           plotGatingSetInput(id = ns("plot_module"))
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
#' @rdname CompensationUI
Compensation <- function(input, output, session, rval) {

  plot_params <- reactiveValues()
  rval_mod <- reactiveValues(spill = NULL, spill_list = list() )
  
  ### Call modules ##########################################################################
  
  res <- callModule(plotGatingSet, "plot_module", rval, plot_params, simple_plot = FALSE)
  callModule(simpleDisplay, "simple_display_module", res$plot, nrow = 2, size = 200 )
  
  ### Get parameters from GatingSet ##################################################################
  choices <- reactive({
    rval$update_gs
    validate(need(class(rval$gating_set) == "GatingSet", "No GatingSet available"))
    get_parameters_gs(rval$gating_set)
  })
  
  ### Set plot parameters ###################################################################

  observe({
    plot_params$use_all_cells <- FALSE
  })
  
  # observe({
  #   
  #   validate(need(rval_mod$spill, "No compensation matrix"))
  #   
  #   #update plot_params
  #   for(var in intersect(names(res$params), names(plot_params))){
  #     plot_params[[var]] <- res$params[[var]]
  #   }
  #   
  #   plot_params$xvar <- colnames(rval_mod$spill)[1]
  #   plot_params$yvar <- colnames(rval_mod$spill)[2]
  # 
  # })

  observeEvent(c(input$xvar_comp, input$yvar_comp, input$show_all_channels), {
    
    #update plot_params
    for(var in intersect(names(res$params), names(plot_params))){
      plot_params[[var]] <- res$params[[var]]
    }
    
    if(input$show_all_channels){
      plot_params$xvar <- setdiff(choices()$plot_var, input$yvar_comp)
    }else{
      plot_params$xvar <- input$xvar_comp
    }
    plot_params$yvar <- input$yvar_comp
  })
  
  observe({
    updateSelectInput(session, "xvar_comp", 
                      choices = choices()$plot_var, selected = choices()$plot_var[1])
    updateSelectInput(session, "yvar_comp", 
                      choices = choices()$plot_var, selected = choices()$plot_var[2])
  })
  
  ### Set GatingSet compensation #####################################################################
  
  # observe({
  #   if(length(choices()$compensation)>0){
  #     for(name in names(choices()$compensation)){
  #       rval_mod$spill_list[[name]] <- choices()$compensation[[name]]
  #     }
  #   }else{
  #   
  # })
  
  ### Update GatingSet compensation ####################################################################
  
  observe({

    compensation <- choices()$compensation
    comp_samples <- setdiff(choices()$sample, names(compensation))

    if(length(comp_samples) > 0){
      for(sample in comp_samples){

        desc <- flowCore::description(rval$gating_set@data[[sample]])
        if("SPILL" %in% names(desc)){
          comp_mat <- desc[["SPILL"]]
          row.names(comp_mat) <- colnames(comp_mat)
        }else{
          m <- diag( length(choices()$params$name) )
          colnames(m) <- choices()$params$name
          row.names(m) <- colnames(m)
          comp_mat <- m
        }

        compensation[[sample]] <- comp_mat
      }
      print("update_gs")
      rval$gating_set@compensation <- compensation
      rval$update_gs <- rval$update_gs + 1
    }
  })

  ### Get list of compensation matrices from GatingSet ################################################
  
  observeEvent(choices()$compensation, {
    
    rval_mod$spill_per_sample <- list()
    
    for( sample in choices()$sample ){
      comp_mat <- choices()$compensation[[sample]]
      is_matched <- FALSE
      if(length(rval_mod$spill_list)>0){
        is_matched <- unlist(lapply(rval_mod$spill_list,
                                    function(x){matrix_equal(comp_mat, x)}))
      }
      idx_match <- which(is_matched)
      if(length(idx_match)==0){
        idx <- length(rval_mod$spill_list) + 1
        comp_name <- paste0("CompMat", idx)
        rval_mod$spill_list[[comp_name]] <- comp_mat
        rval_mod$spill_per_sample[[sample]] <- comp_name
      }else{
        rval_mod$spill_per_sample[[sample]] <- names(rval_mod$spill_list)[idx_match[1]]
      }
    }
    
    rval_mod$spill_per_sample <- unlist(rval_mod$spill_per_sample)
    print(rval_mod$comp_mat_name)
    
  })
  
  ### Define GatingSet compensation  #################################################################
  
  output$spill_per_sample_table <- DT::renderDT({
    validate(need(rval_mod$spill_per_sample, "No compensation defined in GatingSet"))
    df <- data.frame(name = names(rval_mod$spill_per_sample), 
                     CompMat = rval_mod$spill_per_sample)
    DT::datatable(df, rownames = FALSE)
  })
  
  
  observe({
    updateSelectInput(session, "selected_samples", choices = choices()$sample, selected = NULL)
  })
  
  observeEvent(input$spill_per_sample_table_rows_selected, {
    samples <- names(rval_mod$spill_per_sample)[input$spill_per_sample_table_rows_selected]
    updateSelectInput(session, "selected_samples", selected = samples)
  })
    
    
  observe({
    if(length(rval_mod$spill_list) > 0 ){
      updateSelectInput(session, "comp_mat", 
                        choices = names(rval_mod$spill_list), 
                        selected = names(rval_mod$spill_list))
      
      updateSelectInput(session, "selected_matrix", 
                        choices = names(rval_mod$spill_list), 
                        selected = names(rval_mod$spill_list))
    }
  })
  
  observeEvent(input$apply_matrix, {
    #rval_mod$spill_per_sample[input$selected_samples] <- selected_matrix
    compensation <- rval$gating_set@compensation
    compensation[input$selected_samples] <- rval_mod$spill_list[[selected_matrix]]
    rval$gating_set@compensation <- compensation
    rval$update_gs <- rval$update_gs + 1
  })
  
  observeEvent(input$comp_mat, {
    rval_mod$spill <- rval_mod$spill_list[[input$comp_mat]]
  })
      
  
  observeEvent(input$duplicate, {
    if(! input$new_name %in% names(rval_mod$spill_list)){
      rval_mod$spill_list[[input$new_name]] <- rval_mod$spill
    }
  })
  
  
  
  
  
  #     if(length(new_par)>0){
  #   rval_mod$spill <- spill
  #   
  #   if(is.null(rval$df_spill)){
  #     fs <- rval$gating_set
  #     params <- flowCore::parameters(fs[[1]])
  #     m <- diag( length(params$name) )
  #     colnames(m) <- params$name
  #     rval$df_spill <- as.data.frame(m)
  #     row.names(rval$df_spill) <- colnames(rval$df_spill)
  #     
  #     #rval$df_spill <- NULL
  #     
  #     for(i in 1:length(fs)){
  #       desc <- flowCore::description(fs[[i]])
  #       if("SPILL" %in% names(desc)){
  #         df <- as.data.frame(desc[["SPILL"]])
  #         is_identity <- sum(apply(df, 
  #                                  MARGIN = 1, 
  #                                  FUN = function(x){
  #                                    sum(x==0) == (length(x)-1)
  #                                    })
  #                            ) == dim(df)[1]
  #         if(!is_identity){
  #           rval$df_spill <- df
  #           row.names(rval$df_spill) <- colnames(rval$df_spill)
  #           break
  #         }
  #       }
  #     }
  #     
  #     rval$df_spill_original <- rval$df_spill
  #   }
  #   
  # })
  
  ### Compute compensation matrix ##################################################################

  # output$ui_compute_spill <- renderUI({
  #   
  #   ns <- session$ns
  #   
  #   validate(
  #     need(rval$gating_set, "No flow set available")
  #   )
  #   
  #   tagList(
  #     selectInput(ns("fluo"), 
  #                 "fluorophore",
  #                 choices = rval$gating_set@colnames,
  #                 selected = rval$gating_set@colnames[1]),
  #     
  #     selectInput(ns("sample_pos"),
  #                 "sample pos",
  #                 choices = pData(rval$gating_set)$name,
  #                 selected = pData(rval$gating_set)$name[1]),
  #     
  #     selectInput(ns("gate_pos"),
  #                 "gate pos",
  #                 choices = names(rval$gates_flowCore),
  #                 selected = "root"),
  #     
  #     selectInput(ns("sample_neg"), 
  #                 "sample neg",
  #                 choices = pData(rval$gating_set)$name,
  #                 selected = pData(rval$gating_set)$name[1]),
  #     
  #     selectInput(ns("gate_neg"),
  #                 "gate neg",
  #                 choices = names(rval$gates_flowCore),
  #                 selected = "root")
  #   )
  #   
  # })
  # 
  # observeEvent(input$add_spill_param, {
  #   
  #   validate(
  #     need(rval$gating_set, "No gating set available")
  #   )
  #   
  #   
  #   df_pos <- get_data_gs(gs = rval$gating_set,
  #                         sample = input$sample_pos,
  #                         subset = input$gate_pos,
  #                         spill = NULL)
  #   df_pos <- df_pos[names(df_pos) %in% rval$flow_set@colnames]
  #   pos_values <- apply(df_pos, MARGIN = 2, FUN = stats::median,  na.rm = TRUE)
  #   pos_values <- pos_values[rval$flow_set@colnames]
  #   names(pos_values) <- rval$flow_set@colnames
  #   rval$pos_values[[input$fluo]] <- pos_values
  #   
  #   print(rval$pos_values)
  #   
  #   df_neg <- get_data_gs(gs = rval$gating_set,
  #                         sample = input$sample_neg,
  #                         subset = input$gate_neg,
  #                         spill = NULL)
  #   
  #   df_neg <- df_neg[names(df_neg) %in% rval$flow_set@colnames]
  #   neg_values <- apply(df_neg, MARGIN = 2, FUN = stats::median, na.rm = TRUE)
  #   neg_values <- neg_values[rval$flow_set@colnames]
  #   names(neg_values) <- rval$flow_set@colnames
  #   rval$neg_values[[input$fluo]] <- neg_values
  #   
  #   print(rval$pos_values)
  #   
  # })
  # 
  # observe({
  #   updateSelectInput(session, "spill_params", 
  #                     choices = names(rval$pos_values), 
  #                     selected = names(rval$pos_values))
  # })
  # 
  # observeEvent(input$compute_spillover_matrix, {
  #   validate(
  #     need(length(input$spill_params)>0, "Not enough parameters selected")
  #   )
  #   
  #   df_pos_tot <- data.frame(do.call(rbind, rval$pos_values[input$spill_params]), 
  #                            check.names = FALSE)
  #   row.names(df_pos_tot) <- input$spill_params
  #   df_pos_tot <- df_pos_tot[input$spill_params]
  #   #print(df_pos_tot)
  #   
  #   df_neg_tot <- data.frame(do.call(rbind, rval$neg_values[input$spill_params]), 
  #                            check.names = FALSE)
  #   row.names(df_neg_tot) <- input$spill_params
  #   df_neg_tot <- df_neg_tot[input$spill_params]
  #   #print(df_neg_tot)
  #   
  #   df_spill <- df_pos_tot
  #   for(i in 1:length(input$spill_params)){
  #     neg <- rval$neg_values[[input$spill_params[i]]][[input$spill_params[i]]]
  #     pos <- rval$pos_values[[input$spill_params[i]]][[input$spill_params[i]]]
  #     df_spill[input$spill_params[i]] <- (df_pos_tot[input$spill_params[i]] - neg)/(pos - neg)
  #   }
  #   
  #   #print(df_spill)
  #   rval$df_spill <- df_spill
  #   
  # })
  

  ### Import compensation matrix ###################################################################
  
  # observe({
  #   validate(
  #     need(input$spill_file$datapath, "Please select a file to import")
  #   )
  #   
  #   sep <- switch(input$sep_spill,
  #                 "comma" = ",",
  #                 "semi-column" = ";",
  #                 "tab" = "\t",
  #                 "space" = " ")
  #   
  #   rval$df_spill_imported <- utils::read.table(file = input$spill_file$datapath, 
  #                                        sep = sep,
  #                                        fill = TRUE,
  #                                        quote = "\"",
  #                                        header = TRUE,
  #                                        check.names = FALSE)
  #   
  # })
  # 
  # output$spill_imported <- DT::renderDT({
  #   validate(
  #     need(rval$df_spill_imported, "No spillover data imported")
  #   )
  #   as.data.frame(rval$df_spill_imported)
  # })
  # 
  # observeEvent(input$set_spillover_matrix,{
  #   df <- as.data.frame(rval$df_spill_imported)
  #   row.names(df) <- colnames(df)
  #   df <- df[row.names(df) %in% colnames(rval$flow_set), colnames(df) %in% colnames(rval$flow_set)]
  #   rval$df_spill <- df
  #   
  # })
  # 
  # 
  # observe({
  #   
  #   validate(
  #     need(length(input$file_comp)>0, "Please select a file to load")
  #   )
  #   
  #   sep <- switch(input$sep_comp,
  #                 "comma" = ",",
  #                 "semi-column" = ";",
  #                 "tab" = "\t")
  #   
  #   rval$df_comp <- read.csv(input$file_comp$datapath, sep = sep, header = TRUE, quote = "\"", fill = TRUE)
  #   names(rval$df_comp)[1] <- "name"
  # })
  # 
  # observeEvent(input$import_comp, {
  #   idx_match_row <- match(rval$pdata$name, rval$df_comp[,1])
  #   idx_match_col <- match(rval$pdata$name, names(rval$df_comp))
  #   rval$df_spill <- rval$df_comp[idx_match_row, idx_match_col]
  # })
  
  ### Edit spill parameter ###########################################################################
  
  observeEvent(input$reset_comp, {
    rval_mod$spill <- rval_mod$spill_list[[input$comp_mat]]
  })
  
  observeEvent(input$set_spill_value, {
    df <- rval_mod$spill
    idx_x <- match(input$xvar_comp, colnames(df))
    idx_y <- match(input$yvar_comp, row.names(df))
    rval_mod$spill[idx_y, idx_x] <- input$spill_value/100
    rval_mod$spill_list[[input$comp_mat]] <- rval_mod$spill
    
    if(input$comp_mat %in% rval_mod$spill_per_sample){
      
      compensation <- rval$gating_set@compensation
      
      idx <- which(rval_mod$spill_per_sample == input$comp_mat)
      samples_to_update <- names(rval_mod$spill_per_sample)[idx]
      for(sample in samples_to_update){
        compensation[[sample]] <- rval_mod$spill
      }
      rval$gating_set@compensation <- compensation
      rval$update_gs <- rval$update_gs + 1
      print("update")
    }
    
  })
  
  observe({
    df <- rval_mod$spill
    idx_x <- match(input$xvar_comp, colnames(df))
    idx_y <- match(input$yvar_comp, row.names(df))
    updateNumericInput(session, "spill_value", value = df[idx_y, idx_x]*100)
  })
  
  observe({
    updateNumericInput(session, "spill_value", step = input$step_size)
  })
  
  ### Render compensation matrix #####################################################################
  
  output$spill_table <- DT::renderDT({
    
    validate(need(rval_mod$spill, "No spillover matrix"))
    
    df <- as.data.frame(100*rval_mod$spill)
    signif(df, digits = 2)
  })
  
  ### Render compensation matrix as an interactive heatmap ############################################
  
  output$heatmap_spill <- plotly::renderPlotly({
    
    validate(need(rval_mod$spill, "No spillover matrix"))
    
    df <- as.data.frame(100*rval_mod$spill)
    df[df == 0] <- NA
    df_log <- log10(df)
    
    #print(df)
    
    p <- heatmaply::heatmaply(df_log,
                   #colors = c(rgb(1,1,1), rgb(1,0,0)),
                   colors= viridis,
                   plot_method="plotly",
                   #scale_fill_gradient_fun = scale_fill_viridis(trans = scales::log10_trans(), 
                   #name = "spillover"),
                   Rowv = NULL,
                   Colv = NULL,
                   column_text_angle = 90,
                   xlab = "detection channel",
                   ylab = "emitting fluorophore",
                   fontsize_row = 10,
                   fontsize_col = 10,
                   cellnote_size = 6,
                   hide_colorbar = TRUE,
                   main = "spillover (%)",
                   custom_hovertext = df,
                   margins = c(50, 50, 50, 0)
    )
    p$x$source <- "select_heatmap"
    
    p
    
  })
  
  ### React to heatmap events #########################################################################
  
  observe({
    df <- rval_mod$spill
    event.data <- plotly::event_data("plotly_click", source = "select_heatmap")
    idx_y <- dim(df)[1] - event.data$pointNumber[[1]][1]
    idx_x <- event.data$pointNumber[[1]][2] + 1
    
    if(length(idx_x)>0){
      updateSelectInput(session, "xvar_comp", 
                        selected = colnames(df)[idx_x])
    }
    if(length(idx_y)>0){
      updateSelectInput(session, "yvar_comp", 
                        selected = row.names(df)[idx_y])
      
    }
    
  })
  
  ### Download matrix ###############################################################################
  
  output$download_spill <- downloadHandler(
    filename = "spillover_matrix.txt",
    content = function(file) {
      write.table(rval_mod$spill, file = file, 
                  row.names = FALSE, quote = FALSE, sep = "\t")
    }
  )
  
  
  
  return(rval)
  
  
  }

  matrix_equal <- function(x, y){
    is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
  }
    
  ### Tests ##############################################################################################
  # library(shiny)
  # library(shinydashboard)
  # 
  if (interactive()){

    ui <- dashboardPage(
      dashboardHeader(title = "Compensation"),
      sidebar = dashboardSidebar(disable = TRUE),
      body = dashboardBody(
        CompensationUI("module")
      )
    )

    server <- function(input, output, session) {
      rval <- reactiveValues()
      observe({
        #utils::data("GvHD", package = "flowCore")
        #rval$gating_set <- GatingSet(GvHD)
        load("../flowR_utils/demo-data/Rafa2Gui/analysis/cluster.rda")
        gs <- GatingSet(res$cluster$flow_set)
        gs@transformation <- res$cluster$transformation
        spill <- as.matrix(res$cluster$spill)
        spill_list <- lapply(pData(gs)$name, function(x){return(spill)})
        names(spill_list) <- pData(gs)$name
        gs@compensation <- spill_list
        rval$gating_set <- gs
      })

      res <- callModule(Compensation, "module", rval = rval)
    }

    shinyApp(ui, server)

  }