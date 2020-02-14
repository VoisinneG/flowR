#' Visualize, import, export and edit compensation matrices associated with a GatingSet
#' @param id shiny id
#' @import shiny
#' @importFrom shinydashboard box tabBox
#' @importFrom plotly plotlyOutput
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
#'     dashboardHeader(title = "Compensation"),
#'     sidebar = dashboardSidebar(disable = TRUE),
#'     body = dashboardBody(
#'       CompensationUI("module")
#'     )
#'   )
#' 
#'   server <- function(input, output, session) {
#'     rval <- reactiveValues()
#'     observe({
#'       utils::data("GvHD", package = "flowCore")
#'       rval$gating_set <- GatingSet(GvHD)
#'     })
#' 
#'     res <- callModule(Compensation, "module", rval = rval)
#'   }
#' 
#'   shinyApp(ui, server)
#' 
#' }
#' }
CompensationUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
    fluidRow(
      
      column(width = 4,
             tabBox(title = "", width = NULL, height = NULL,
                    tabPanel(title = "Compensation",
                             box(title = "Table", width = NULL, height = NULL, collapsible = TRUE, collapsed = FALSE,
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
                    ),
                    tabPanel(title = "Matrix",
                        selectInput(ns("comp_mat"), "Select matrix",
                                    choices = NULL, selected = NULL),
                        box(title = "Edit", width = NULL, height = NULL, collapsible = TRUE, collapsed = TRUE,
                            selectInput(ns("xvar_comp"), label = "column (channel)", 
                                        choices = NULL, selected = NULL),
                            selectInput(ns("yvar_comp"), label = "row (fluorophore)", 
                                        choices = NULL, selected = NULL),
                            sliderInput(ns("slider_input"), "spillover (%)", min = -150, max = 150, value = 0),
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
                            DT::DTOutput(ns("spill_table"))
                        ),
                        box(title = "Rename/Duplicate", width = NULL, height = NULL, collapsible = TRUE, collapsed = TRUE,
                            textInput(ns("new_name"), "CompMat name", value = "newCompMat"),
                            actionButton(ns("rename"), "rename"),
                            actionButton(ns("duplicate"), "duplicate")
                        ),
                        actionButton(ns("delete"), "delete"),
                        downloadButton(ns("download_spill"))
                    )
             ),
             box(title = "Import/Compute", width = NULL, height = NULL, collapsible = TRUE, collapsed = FALSE,
                 tabBox(title = "", width = NULL,
                        tabPanel("Import",
                                 width = NULL, height = NULL,
                                 ImportSpillUI(ns("import_spill_module"))
                        ),
                        tabPanel("Compute",
                                 width = NULL, height = NULL,
                                 ComputeSpillUI(ns("compute_spill_module"))
                        )
                 )
             )
    ),
    column(width = 8,
           box(title = "Spillover Matrix", width = NULL, collapsible = TRUE, collapsed = FALSE,
               #div(style = 'overflow-x: scroll', DT::DTOutput(ns("spill_table")))
               plotly::plotlyOutput(ns("heatmap_spill"))
           ),
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
#' @export
#' @rdname CompensationUI
Compensation <- function(input, output, session, rval) {

  plot_params <- reactiveValues()
  rval_mod <- reactiveValues(spill = NULL, spill_list = list() )
  
  ### Call modules ##########################################################################
  
  res <- callModule(plotGatingSet, "plot_module", rval, plot_params, simple_plot = FALSE)
  callModule(simpleDisplay, "simple_display_module", 
             plot_list = res$plot, 
             params = reactiveValues(nrow = 2,  width = 200, height = 200) )
  spill_computed <- callModule(ComputeSpill, "compute_spill_module", rval = rval)
  spill_imported <- callModule(ImportSpill, "import_spill_module", rval = rval)
    
  ### Get parameters from GatingSet #########################################################
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
      plot_params$xvar <- choices()$plot_var[1]
    }
    
    if(input$comp_mat %in% rval_mod$spill_per_sample[res$params$sample]){
      if(input$show_all_channels){
        plot_params$xvar <- setdiff(choices()$plot_var, input$yvar_comp)
      }else{
        plot_params$xvar <- input$xvar_comp
      }
      plot_params$yvar <- input$yvar_comp
    }
    
  })
  
  observeEvent(c(input$xvar_comp, input$yvar_comp, input$show_all_channels), {
    
    
  })
  
  observe({
    choices <- colnames(rval_mod$spill)

    if(!all(names(choices) %in% choices()$plot_var)){
      choices <- unname(choices)
    }
    updateSelectInput(session, "xvar_comp",
                      choices = choices, selected = choices[1])
    updateSelectInput(session, "yvar_comp", 
                      choices = choices, selected = choices[2])
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
  
  ### Set GatingSet compensation ####################################################################
  
  observe({
    print(choices()$sample)
    compensation <- choices()$compensation
    print(names(compensation))
    comp_samples <- setdiff(choices()$sample, names(compensation))

    if(length(comp_samples) > 0){
      for(sample in comp_samples){

        desc <- flowCore::description(rval$gating_set@data[[sample]])
        spill_retrieved <- FALSE
        if("SPILL" %in% names(desc) ){
          comp_mat <- desc[["SPILL"]]
          if(dim(comp_mat)[1] == dim(comp_mat)[2] & dim(comp_mat)[1]>0){
            row.names(comp_mat) <- colnames(comp_mat)
            spill_retrieved <- TRUE
          }
          
        }
        if(!spill_retrieved){
          m <- diag( length(choices()$params$name) )
          colnames(m) <- choices()$params$name
          row.names(m) <- colnames(m)
          comp_mat <- m
        }

        compensation[[sample]] <- comp_mat
      }
      rval$gating_set@compensation <- compensation
      # update rval$gating_set_list
      if("gating_set_selected" %in% names(rval)){
        rval$gating_set_list[[rval$gating_set_selected]]$gating_set@compensation <- compensation
      }
      rval$update_gs <- rval$update_gs + 1
    }
  })

  ### Match GatingSet compensation with loaded matrices ################################################
  
  observeEvent(c(choices()$compensation, rval_mod$spill_list), {
    
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
        
        idx <- 0
        comp_name <- paste0("CompMat", idx)
        while(comp_name %in% names(rval_mod$spill_list)){
          idx <- idx + 1
          comp_name <- paste0("CompMat", idx)
        }
        
        rval_mod$spill_list[[comp_name]] <- comp_mat
        rval_mod$spill_per_sample[[sample]] <- comp_name
      }else{
        rval_mod$spill_per_sample[[sample]] <- names(rval_mod$spill_list)[idx_match[1]]
      }
    }
    
    rval_mod$spill_per_sample <- unlist(rval_mod$spill_per_sample)
    
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
    
    if(length(input$spill_per_sample_table_rows_selected)>0){
      CompMat <- rval_mod$spill_per_sample[input$spill_per_sample_table_rows_selected[1]]
      updateSelectInput(session, "comp_mat", selected = CompMat)
    }
    
    #update plot_params
    for(var in intersect(names(res$params), names(plot_params))){
      plot_params[[var]] <- res$params[[var]]
    }
    plot_params$sample <- samples
    plot_params$facet_var <- "name"
    
  })
    
    
  observe({
    #if(length(rval_mod$spill_list) > 0 ){
      updateSelectInput(session, "comp_mat", 
                        choices = names(rval_mod$spill_list), 
                        selected = names(rval_mod$spill_list))
      
      updateSelectInput(session, "selected_matrix", 
                        choices = names(rval_mod$spill_list), 
                        selected = names(rval_mod$spill_list))
    #}
  })
  
  observeEvent(input$apply_matrix, {
    #rval_mod$spill_per_sample[input$selected_samples] <- input$selected_matrix
    compensation <- rval$gating_set@compensation
    df <- rval_mod$spill_list[[input$selected_matrix]]

    idx_match_row <- row.names(df)[row.names(df) %in% choices()$params$name]
    idx_match_col <- colnames(df)[colnames(df) %in% choices()$params$name]
    
    df <- df[idx_match_row, idx_match_col]

    if(dim(df)[1]!=dim(df)[2] || dim(df)[1]==0 || dim(df)[2]==0){
      showModal(modalDialog(
        title = "Error",
        paste("Incorrect matrix dimensions", sep=""),
        easyClose = TRUE,
        footer = NULL
      ))
    }else{
      
      for(sample in input$selected_samples){
        compensation[[sample]] <- df
      }
      
      rval$gating_set@compensation <- compensation
      # update rval$gating_set_list
      if("gating_set_selected" %in% names(rval)){
        rval$gating_set_list[[rval$gating_set_selected]]$gating_set@compensation <- compensation
      }
      
      rval$update_gs <- rval$update_gs + 1
    }
    
    
  })
  
  observe({
    if(!is.null(input$comp_mat)){
      if(input$comp_mat %in% names(rval_mod$spill_list)){
        rval_mod$spill <- rval_mod$spill_list[[input$comp_mat]]
      }else{
        rval_mod$spill <- NULL
      }
    }

  })
      
  
  observeEvent(input$duplicate, {
    if(! input$new_name %in% names(rval_mod$spill_list)){
      rval_mod$spill_list[[input$new_name]] <- rval_mod$spill
    }
  })
  
  observeEvent(input$rename, {
    idx <- which(names(rval_mod$spill_list) == input$comp_mat)
    if(length(idx)==1){
      names(rval_mod$spill_list)[idx] <- input$new_name
    }
  })
  
  observeEvent(input$delete, {
    idx <- which(names(rval_mod$spill_list) == input$comp_mat)
    if(length(idx)==1){
      rval_mod$spill_list <- rval_mod$spill_list[-idx]
    }
    
  })
  ### Compute compensation matrix ##################################################################

  observeEvent(spill_computed(), {
    
    if(length(spill_computed())>0){
      if(!names(spill_computed()) %in% names(rval_mod$spill_list)){
        rval_mod$spill_list[[names(spill_computed())]] <- as.matrix(spill_computed()[[1]])
      }else{
        showModal(modalDialog(
          title = "Matrix name already exists",
          paste("Please choose another name", sep=""),
          easyClose = TRUE,
          footer = NULL
        ))
      }
    }

  })
  
  ### Import compensation matrix ##################################################################
  
  observeEvent(spill_imported(), {
    
    if(length(spill_imported())>0){
      if(!names(spill_imported()) %in% names(rval_mod$spill_list)){
        rval_mod$spill_list[[names(spill_imported())]] <- as.matrix(spill_imported()[[1]])
      }else{
        showModal(modalDialog(
          title = "Matrix name already exists",
          paste("Please delete or rename the imported matrix first", sep=""),
          easyClose = TRUE,
          footer = NULL
        ))
      }
    }
    
  })
  
  ### Edit spill parameter ###########################################################################
  
  # observeEvent(input$reset_comp, {
  #   rval_mod$spill <- rval_mod$spill_list[[input$comp_mat]]
  # })
  
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
      # update rval$gating_set_list
      if("gating_set_selected" %in% names(rval)){
        rval$gating_set_list[[rval$gating_set_selected]]$gating_set@compensation <- compensation
      }
      rval$update_gs <- rval$update_gs + 1
    }
    
  })
  
  observe({
    df <- rval_mod$spill
    idx_x <- match(input$xvar_comp, colnames(df))
    idx_y <- match(input$yvar_comp, row.names(df))
    updateNumericInput(session, "spill_value", value = df[idx_y, idx_x]*100)
  })
  
  observeEvent(input$slider_input, {
    validate(need(input$spill_value, "missing input value"))
    validate(need(input$slider_input, "missing input value"))
    if(input$spill_value != input$slider_input){
      updateNumericInput(session, "spill_value", value = input$slider_input)
    }
  })
  
  observeEvent(input$spill_value, {
    validate(need(input$slider_input, "missing input value"))
    validate(need(input$spill_value, "missing input value"))
    if(input$spill_value != input$slider_input){
      updateSliderInput(session, "slider_input", value = input$spill_value)
    }
  })
  
  observe({
    updateNumericInput(session, "spill_value", step = input$step_size)
    updateSliderInput(session, "slider_input", step = input$step_size)
  })
  
  ### Render compensation matrix #####################################################################
  
  output$spill_table <- DT::renderDT({
    validate(need(rval_mod$spill, "No spillover matrix"))
    df <- format_style_comp_matrix(rval_mod$spill, editable = 'none')
    return(df)
  })
  
  ### React to DT selection events #########################################################################
  
  observeEvent(input$spill_table_cells_selected, {
    df <- rval_mod$spill
    idx <- input$spill_table_cells_selected
    
    if(dim(idx)[1]>0 & dim(idx)[2]==2){
      
        updateSelectInput(session, "xvar_comp",
                          selected = colnames(df)[idx[,2]])
        updateSelectInput(session, "yvar_comp",
                          selected = row.names(df)[idx[,1]])
      
    }
  })
  
  ### Render compensation matrix as an interactive heatmap ############################################
  
  output$heatmap_spill <- plotly::renderPlotly({
    
    validate(need(rval_mod$spill, "No spillover matrix"))
    p <- plot_comp_as_heatmap(rval_mod$spill, name  = input$comp_mat)
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

  
    
  ### Tests ##############################################################################################
  #  library(shiny)
  #  library(shinydashboard)
  #  
  # if (interactive()){
  # 
  #   ui <- dashboardPage(
  #     dashboardHeader(title = "Compensation"),
  #     sidebar = dashboardSidebar(disable = TRUE),
  #     body = dashboardBody(
  #       CompensationUI("module")
  #     )
  #   )
  # 
  #   server <- function(input, output, session) {
  #     rval <- reactiveValues()
  #     observe({
  #       #fs <- read.ncdfFlowSet(files = c("../flowR_utils/demo-data/JL04BMVLG-Valentin/Tumor_T_001_012.fcs",
  #       #                                 "../flowR_utils/demo-data/JL04BMVLG-Valentin/Tumor_T_002_013.fcs"))
  #       #rval$gating_set <- GatingSet(fs)
  #       utils::data("GvHD", package = "flowCore")
  #       rval$gating_set <- GatingSet(GvHD)
  #       # load("../flowR_utils/demo-data/Rafa2Gui/analysis/cluster.rda")
  #       # gs <- GatingSet(res$cluster$flow_set)
  #       # gs@transformation <- res$cluster$transformation
  #       # spill <- as.matrix(res$cluster$spill)
  #       # spill_list <- lapply(pData(gs)$name, function(x){return(spill)})
  #       # names(spill_list) <- pData(gs)$name
  #       # gs@compensation <- spill_list
  #       # rval$gating_set <- gs
  #     })
  # 
  #     res <- callModule(Compensation, "module", rval = rval)
  #   }
  # 
  #   shinyApp(ui, server)
  # 
  # }