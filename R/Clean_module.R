#' Identify and remove anomalies
#' @param id shiny id
#' @import shiny
#' @importFrom DT dataTableOutput
#' @importFrom shinyjs useShinyjs
#' @export
#' @examples 
#' \dontrun{
#' if (interactive()){
#' 
#'   ui <- dashboardPage(
#'     dashboardHeader(title = "flowAI"),
#'     sidebar = dashboardSidebar(disable = TRUE),
#'     body = dashboardBody(
#'       CleanUI("module")
#'     )
#'   )
#' 
#'   server <- function(input, output, session) {
#'     rval <- reactiveValues()
#'     observe({
#'       utils::data("Bcells", package = "flowAI")
#'       rval$gating_set <- flowWorkspace::GatingSet(Bcells)
#'     })
#'     res <- callModule(Clean, "module", rval = rval)
#'   }
#'   
#'   shinyApp(ui, server)
#' 
#' }}
CleanUI<-function(id){
  
  ns <- NS(id)
  
  fluidRow(
    br(),
    shinyjs::useShinyjs(),
    column(4,
           box(title = "Cleaning selection", width = NULL,
               checkboxGroupInput(ns("groupButton"), label = "Choose type of cleaning",
                                  choices = c("Flow Rate" = 1,
                                              "Dynamic range" = 2,
                                              "Signal acquisition" = 3),
                                  selected = c(1, 2, 3)
               )
               
           ),
           
           box(title = "Parameters", width = NULL,
               selectInput(inputId = ns("choice_sample_input"),
                           label = "Sample",
                           multiple = T,
                           choices = NULL,
                           selected = NULL),
               selectInput(inputId = ns("choice_channel_input"), 
                           label = "Time channel",
                           choices = NULL),
               numericInput(ns("timestep"),
                            label = "Time-step",
                            value = 0.01),
               numericInput(ns("second_fraction"),
                            label = "Time averaging window (s) [flow rate only]",
                            value = 0.1),
               numericInput(ns("binSize"),
                            label = "Number of events per bin [signal acquisition only]",
                            value = 500),
               actionButton(ns("clean_selected_sample_input"),
                            label = "Analyze selected sample(s)")
               
           ),
           box(width = NULL,
               title = "GatingSet",
               collapsible = F,
               collapsed = F,
               actionButton(inputId = ns("update_preview_badCells"), label = "Update preview of badCells"),
               checkboxInput(inputId = ns("gating_set_tag_or_not"), 
                             label = "Actual gatingset with gatingset tag",
                             value = T
                            ),
               textInput(ns("GatingSet_tagged_name"), label = "Create GatingSet"),
               actionButton(ns("action_create_gatingset"), label = "Create GatingSet")
               ),
           box(width = NULL,
               title = "Advanced options",
               collapsible = T, collapsed = T,
               box(title = "Flow rate",
                   width = NULL,
                   collapsible = T,
                   collapsed = T,
                   numericInput(ns("alpha"),
                                label = "Anomalies acceptation (ESD Method)",
                                min = 0,
                                max = 1,
                                value = 0.01,
                                step = 0.01),
                   radioButtons(ns("direction"), label = "Anomalies direction",
                                choices = list("Both tails" = "both",
                                               "lower-tail only (negative going anomalies)" = "neg",
                                               "upper-tail only (positive going anomalies)" = "pos"),
                                selected = "both"
                   ),
                   checkboxInput(ns("use_decomp"), 
                                 label = "Without usage of flow rate cytometry", 
                                 value = FALSE)
               ),
               box(title = "Dynamic range",
                   width = NULL,
                   collapsible = T,
                   collapsed = T,
                   selectInput(ns("options_chExclude"), 
                               label = "Channel to exclude",
                               multiple = T, choices = NULL, selected = NULL),
                   radioButtons(ns("neg_values"), label = "Negative values", 
                                choices = list("Remove negative outliers" = 1,
                                               "Truncate the negative values to the cut-off" = 2)),
                   radioButtons(ns("side"), label = "Select limits", 
                                choices = list("both"= "both",
                                               "upper" = "upper",
                                               "lower" = "lower"),
                                selected = "both")
               ),
               box(title = "Signal acquisition",
                   width = NULL,
                   collapsed = T,
                   collapsible = T,
                   
                   selectInput(ns("options_chExclude2"), 
                               label = "Channel to exclude", 
                               multiple = T, choices = NULL),
                   checkboxInput(ns("remove_outlier"),
                                 label = "Remove outlier",
                                 value = F)
               )
           )
    ),
    
    column(8,
           box(title = "Quality check plots",
               width = NULL, collapsed = FALSE, collapsible = T,
               selectInput(inputId = ns("select_one_sample"),
                           label = "Select sample for visualization",
                           choices = NULL),
               tabsetPanel(type ="pills",
                           tabPanel("Flow rate",
                                    br(),
                                    br(),
                                    textOutput(ns("fr_message")),
                                    br(),
                                    br(),
                                    plotOutput(ns("flow_rate_plot_output")),
                                    fluidRow(
                                      column(4,
                                             br(),
                                             checkboxInput(inputId = ns("useCutInput"), 
                                                           label = "Use interractive selection", 
                                                           value = F
                                             ),
                                             uiOutput(ns("actionRenderUI"))
                                      ),
                                      column(4,
                                             uiOutput(ns("sliderRate"))
                                      ),
                                      column(4,
                                             uiOutput(ns("sliderTime"))
                                      )
                                    ),
                                    hr()
                           ),
                           tabPanel("Dynamic range",
                                    br(),
                                    br(),
                                    textOutput(ns("dynamic_message")),
                                    br(),
                                    br(),
                                    plotOutput(ns("dynamic_plot_output"))
                           ),
                           tabPanel("Signal acquisition",
                                    br(),
                                    br(),
                                    textOutput(ns("signal_message")),
                                    br(),
                                    br(),
                                    simpleDisplayUI(ns("simple_display_module2"))
                           )
                           
               )
               
               
           ),
           box(title = "Results",
               width = NULL, collapsed = F, collapsible = T,
               tabsetPanel(type = "pills",
                           tabPanel("Heatmap",
                                    br(),
                                    br(),
                                    simpleDisplayUI(ns("simple_display_module")),
                                    box(title = "Plot options",
                                        width = 12,
                                        collapsible = T,
                                        collapsed = T,
                                        
                                        selectInput(ns("colorpalette_select"), label = "Color option", choices = list("Viridis" = "D",
                                                                                                                      "Magma" = "A",
                                                                                                                      "Inferno" = "B",
                                                                                                                      "Plasma" = "C"))
                                        
                                        
                                    )
                           ),
                           tabPanel("Table",
                                    br(),
                                    br(),
                                    DT::DTOutput(ns("result_output"))
                           ),
                           tabPanel("Preview badCells plot",
                                    simpleDisplayUI(ns("simple_display_badcells_module")),
                                    plotGatingSetInput(ns("plot_preview_badcells"))
                                    )
               )
           )
    )
    
  )
  
}

#' @importFrom flowCore exprs keyword
#' @importFrom flowWorkspace pData gslist_to_gs GatingSetList
#' @importFrom scales pretty_breaks
#' @importFrom data.table setDT
#' @importFrom plotly renderPlotly
#' @importFrom heatmaply heatmaply
#' @importFrom shinyjs enable disable
#' @import flowAI
#' @export
Clean <- function(input, output, session, rval) {
  
  ### Call modules ###################################################################
  # temp_rval <- reactiveValues(test = NULL)
  temp_gs <- reactiveValues(gating_set = NULL)
  preview_val_but <- reactiveValues(input_prev = NULL)
  
  callModule(simpleDisplay, "simple_display_module", 
             plot_list = heatmap_plot, 
             params = reactiveValues(use_plotly = TRUE, width = 500, height = 500, min_size = 300),
             save = FALSE)
  
  callModule(simpleDisplay, "simple_display_module2", 
             plot_list = signal_plot, 
             params = reactiveValues(width = 500, height = 40, max_height=500, min_size = 20))
  
  plot_params <- reactiveValues()
  
  ### setup preview badCells plottings via callModules ########################################################### 
  observeEvent(input$update_preview_badCells, {
    preview_val_but$input_prev <- 1
    
    #temp rval for preview of badcells
    temp_gs$gating_set <- rval$gating_set 
      gs <- create_futur_gs()[[1]]
      temp_gs$gating_set <- gs
  })
  
  observe({
    validate(need(flowCore::colnames(temp_gs$gating_set) == "badCells", "need to make cleaning to see plot"))
    
    # set parameter of plots by defaults
    plot_params$plot_type <- "dots"
    plot_params$xvar <- "FSC-H"
    plot_params$yvar <- "SSC-H"
    plot_params$color_var <- "badCells"

    res_badcells_plot <- callModule(plotGatingSet, "plot_preview_badcells",
                                    temp_gs,
                                    plot_params = plot_params,
                                    show_gates = F)
    
    callModule(simpleDisplay, "simple_display_badcells_module", res_badcells_plot$plot)
  })
  
  ### Setup sliderInput for flowRate #################################################
  
  sliders <- reactive({
    return(
      c(
        min(res()$flowRateQCList[[input$select_one_sample]]$frequencies[,3]) - 0.1,
        max(res()$flowRateQCList[[input$select_one_sample]]$frequencies[,3]) + 0.1,
        min(res()$flowRateQCList[[input$select_one_sample]]$frequencies[,4]) - 10,
        max(res()$flowRateQCList[[input$select_one_sample]]$frequencies[,4]) + 10
      )
    )
  })
  
  output$sliderRate <- renderUI({
    if(input$useCutInput == T){
      ns <- session$ns
      sliderInput(inputId = ns("rateSliderInput"), 
                  label = "Rate",
                  min = sliders()[3], max = sliders()[4],
                  value = c(sliders()[3], sliders()[4]), step = 0.1)
    }
  })
  
  output$sliderTime <- renderUI({
    if(input$useCutInput == T){
      ns <- session$ns
      sliderInput(inputId = ns("timeSliderInput"),
                  label = "Time cut",
                  min = sliders()[1], max = sliders()[2],
                  value = c(sliders()[1], sliders()[2]), step = 0.1)
    }
  })
  
  rateSlider <- reactive({
    return(
      c(input$rateSliderInput[1], input$rateSliderInput[2])
    )
  })
  
  timeSlider <- reactive({
    return(
      c(input$timeSliderInput[1], input$timeSliderInput[2])
    )
  })
  
  ### renderUI actionButton ##########################################################
  
  output$actionRenderUI <- renderUI({
    if(input$useCutInput == T){
      ns <- session$ns
      actionButton(inputId = ns("applyInput"),
                   label = "Apply for the current sample")
    }

  
  })
  ### Check parameter to provide an error ############################################
  # this is a reactiveVerification for the showModal (precleaning dataset) 
  reactiveVerification <- reactiveValues(alphaError = 0, secondFractionError = 0, timeStepError = 0, binSizeError = 0)
  
  #ESD parameter error
  observe({ 
    validate(
      need(input$alpha != "", "choose an ESD between 0-1"),
      need(is.numeric(input$alpha), "Numeric value only")
    )

    if(!is.null(input$alpha) && input$alpha>=0 && input$alpha<=1 && is.numeric(input$alpha)){
      input$alpha
      shinyjs::enable(id = "clean_selected_sample_input")
      
      reactiveVerification$alphaError <- 0
    } else {
      reactiveVerification$alphaError <- 1
      showModal(
        modalDialog(title = "Error from anomalies ESD values parameters",
                    "Choose an anomalies ESB between 0-1",
                    footer = modalButton("Quit")
        )
      )
      shinyjs::disable(id = "clean_selected_sample_input")
    }
  })
  
  #show message to provide parameter error on second fraction
  
  observe({
    validate(
      need(input$second_fraction != "", "choose timestep smoothness between 0-1"),
      need(is.numeric(input$second_fraction), "Numeric values only")
    )
    
    if(input$second_fraction>=0 && input$second_fraction<=1){
      input$second_fraction
      reactiveVerification$secondFractionError <- 0

      shinyjs::enable(id = "clean_selected_sample_input")

    } else {
      reactiveVerification$secondFractionError <- 1
      
      showModal(
        modalDialog(title = "Error with timestep smoothness entry",
                    "Choose an timestep between 0-1",
                    easyClose = T,
                    footer = modalButton("Quit")
        )
      )
      shinyjs::disable(id = "clean_selected_sample_input")
      
    }
  })
  
  ## show message to provide timestep error before the cleaning
  
  observe({
    validate(
      need(input$timestep != "", "Choose a timestep value > 0"),
      need(is.numeric(input$timestep), "Need numeric value only")
    )
    if(input$timestep>=0){
      input$timestep
      reactiveVerification$timeStepError <- 0
      shinyjs::enable(id = "clean_selected_sample_input")

    } else {
      reactiveVerification$timeStepError <- 1
      
      showModal(
        modalDialog(title = "Error with timestep values entry",
                    "Choose an timestep > 0",
                    easyClose = T,
                    footer = modalButton("Quit")
        )
      )
      shinyjs::disable(id = "clean_selected_sample_input")
      
    }
    
  })
  
  ## check bin before analysis #######################################################
  
  observe({
    ### Verification of bin size if the bin size is higher than bin_max then show popup error and disable button cleaning
    validate(
      need(input$binSize != "", "Enter a number"),
      need(is.numeric(input$binSize), "Only numerics numbers are authorized")
      )
    
    # Get dimension of sample 
    dim_sample <- list()
    sample_all_length <- lapply(choices()$sample, function(i){dim_sample[[i]] <- dim(set()[[i]])
    dim_sample[[i]][1]
    })
    
    # get the min cells lengths samples (to apply the verification on it)  
    val <- min(unlist(sample_all_length))
    bin <- round(val/3)
    bin_max <- round(bin - 2)
    
    # disable and show modal dialog or button is usable
    if(input$binSize<= 0 || input$binSize > bin_max){
      reactiveVerification$binSizeError <- 1
      showModal(
        modalDialog(
          title = "Bin verification to provide possible error in cleaning",
          paste("Use a bin under this values : ", bin_max),
          easyClose = T,
          footer = modalButton("Quit")
        )
      )
      shinyjs::disable(id = "clean_selected_sample_input")
      
    } else {
      shinyjs::enable(id = "clean_selected_sample_input")
      reactiveVerification$binSizeError <- 0
      
    }
    
    
  })
  ### get parameters from GatingSet ##################################################
  
  choices <- reactive({
    rval$update_gs
    validate(need(class(rval$gating_set) == "GatingSet", 
                  "input is not a GatingSet"))
    get_parameters_gs(rval$gating_set)
  })
  
  set <- reactive({
    rval$update_gs
    validate(need(class(rval$gating_set) == "GatingSet", 
                  "input is not a GatingSet"))
    rval$gating_set@data
  })
  
  ### Update UI #####################################################################
  
  ### Set default excluded channels #################################################
  
  observe({
    chNames <- choices()$plot_var
    pattern <- "^FSC|^SSC"
    excludeCh<- grep(pattern, chNames, value = TRUE)
    # print(excludeCh)
    updateSelectInput(session = session, inputId = "options_chExclude", 
                      choices = chNames,
                      select = excludeCh)
    
    updateSelectInput(session = session, inputId = "options_chExclude2", 
                      choices = chNames,
                      select = excludeCh)
  })
  
  ### Set time channel ###############################################################
  observe({
    chNames <- choices()$plot_var
    # print(chNames)
    pattern <- "^Time|^time"
    timeCh<- grep(pattern, chNames, value = TRUE)
    print(timeCh)
    updateSelectInput(session, "choice_channel_input", 
                      label = "Select time channel",
                      choices = chNames,
                      selected = timeCh)
  })
  
  ### Update selected samples ################################################################
  
  observe({
    updateSelectInput(session, "choice_sample_input",
                      choices = choices()$sample, 
                      selected = choices()$sample)
  })
  
  observeEvent(res_table(), {
    updateSelectInput(session, "select_one_sample",
                      label = paste("Select sample for visualization"),
                      choices = row.names(res_table()), 
                      selected = row.names(res_table())[1]
    )
  })
  
  ### Update time-step ################################################################
  
  observe({
    if("$TIMESTEP" %in% names(description(set()[[1]]))){
      updateNumericInput(session, "timestep",
                         value = as.numeric(description(set()[[1]])[["$TIMESTEP"]])
      )
    }
  })
  
 
  
  #### Process the cleaning ###############################################################
  
  run_clean <- reactive({ 
    show_error <- 0
    preview_val_but$input_prev <- 0
    
    if(is.null(input$groupButton)){
      showModal(
        modalDialog(title = "Warning cleaning type selection",
                    "Need a minimum of one type of cleaning to continue and make the cleaning",
                    footer = modalButton("Dismiss")
        )
      )
    }
    
    validate(need(input$groupButton, "Select at least one type of cleaning"))
    validate(need(input$choice_sample_input, "No sample selected"))
    validate(need(all(input$choice_sample_input %in% choices()$sample),
                  "Please select samples"))
  
    samples <- input$choice_sample_input
    
    timeCh <- input$choice_channel_input
    direction <- input$direction
    side <- input$side
    ChannelExclude <- input$options_chExclude
    timestep <- input$timestep
    second_fraction <- input$second_fraction # time interval used for averaging data
    binSize <- input$binSize
    pen_valueFS <- 500
    
    alpha <- input$alpha

    outlier_remove <- input$remove_outlier
    
    
    ## set Flow rate parameter (Quality content) & bin arg
    FR_QC_arg <- list(alpha = alpha, 
                      use_decomp = TRUE,
                      direction= direction)
    
    FR_bin_arg <- list(second_fraction = second_fraction, 
                       timeCh = timeCh,
                       timestep = timestep)
    
    ## set signal acquisition parameters 
    FS_bin_arg <- list(channels = NULL,
                       binSize = binSize, 
                       timeCh = timeCh, 
                       timestep = timestep, 
                       TimeChCheck = NULL)
    
    FS_QC_arg <- list(ChannelExclude = ChannelExclude, 
                      pen_valueFS = pen_valueFS, 
                      maxSegmentFS = 3, 
                      outlier_remove = outlier_remove)
    
    ## get the cleaning for dynamic range / flow rate / signal acquisition in list
    flowRateQCList <- list()
    FlowSignalQCList <- list()
    dynamic_range <- list()
    flowRateData <- list()
    
    withProgress(message = 'The data cleaning is running..', value = 0,{
      for(i in  1:length(samples)){
        sample <- samples[i]
        
        ordFCS <- flowAI:::ord_fcs_time(set()[[sample]], timeCh = timeCh)
        
        ### modification ici #### 
        if(2 %in% input$groupButton){
          dynamic_range[[sample]] <-   tryCatch(expr = {
            flowAI:::flow_margin_check(x = ordFCS,
                              ChannelExclude = ChannelExclude,
                              side = side,
                              neg_values = 1)
          }, error = function(e){
            message("Flow rate cleaning error, check your parameters.")
            return(NA)
          })
        }

        if(1 %in% input$groupButton){
          # provide a possible error
          flowRateQCList[[sample]] <- tryCatch({
            flowRateData[[sample]] <- flowAI:::flow_rate_bin(x = ordFCS, timeCh = timeCh, second_fraction = second_fraction, timestep = timestep)
            flowAI:::flow_rate_check(x= ordFCS, FlowRateData = flowRateData[[sample]], alpha = alpha, use_decomp = T)
          } ,
          error = function(e) {
            message("Selected channel is not appropriate here, please select choose other channel.")
            return(NA)
          })

        }
        
        if(is.na(flowRateQCList[[sample]]) && !is.null(flowRateQCList[[sample]])){break} # break loop if we have an error
        # signal acquisition process
        
        if(3 %in% input$groupButton){
          FlowSignalQCList[[sample]] <-   tryCatch(expr = {
            FlowSignalData <- do.call(flowAI:::flow_signal_bin, c(list(ordFCS), 
                                                         FS_bin_arg))
            do.call(flowAI:::flow_signal_check, 
                    c(list(x = ordFCS, FlowSignalData = FlowSignalData), 
                      FS_QC_arg))
          }, 
          error = function(e){
            message("Error in cleaning signal acquisition use the correct parameters.")
            return(NA)
          })
          
        }

        incProgress(1/i, detail = paste("(current sample : ", i, ")"))
      }
    })
    
    # if we have na in the list then we can get the alert popup
    if(length(which(is.na(flowRateQCList))) > 0){
      showModal(
        modalDialog(title = "Error with time channel",
                    "Select preferentially time channel, because another channel can't be use.",
                    footer = modalButton("Quit"),
                    easyClose = T
        )
      )
      return(NULL)
    } else if(show_error == 1){
      showModal(
        modalDialog(title = "Error with time channel",
                    "Select preferentially time channel, because another channel can't be use.",
                    footer = modalButton("Quit")
        )
      )
    }
    
    
    return(
      list(
        flowRateQCList = flowRateQCList,
        FlowSignalQCList = FlowSignalQCList,
        dynamic_range = dynamic_range,
        flowRateData = flowRateData
      )
    )
    
  })
  
  
  ### setup the pop up for precleaning or not ###################################################
  
  # update the value in the observeEvent for precleaning & cleaning to 0 or 1 (if cleaning is used update cleaning to 1 and precleaning to 0)
  status_clean_or_precleaning <- reactiveValues(precleaning = 0, cleaning = 0)
  
  # Create a clean result for the precleaning and cleaning button 
  clean_res <- reactiveValues(res = NULL)
  
  # run precleaning when the button is clicked and update value in reactiveValues and the result of cleaning in the reactiveValues
  observeEvent(input$pre_cleaning,{
    removeModal()
    status_clean_or_precleaning$precleaning <- 1
    status_clean_or_precleaning$cleaning <- 0
    clean_res$res <- run_clean()
  })
  
  # run cleaning and update the reactiveValues
  observeEvent(input$clean_selected_sample_input, {
    status_clean_or_precleaning$precleaning <- 0
    status_clean_or_precleaning$cleaning <- 1
    clean_res$res <- run_clean()
  })
  
  #use the reactiveValues to update the res (for the result of cleaning)
  res <- reactive({
    
    if(status_clean_or_precleaning$precleaning == 1 ){
      clean_res$res
    } else if(status_clean_or_precleaning$cleaning == 1){
      clean_res$res
    }
  })
  
  ### setup the gatingSet tagged (badCells) ##############################################################

  create_futur_gs <- reactive({
    progress <- shiny::Progress$new()
    
    on.exit(progress$close())
    progress$set(message = "Make the clean gatingSet", value = 0)
    
    df_temp <- NULL
    df_ending <- NULL
    
    df_temp2 <- list()
    
    subset_df_clean <- NULL 
    
    # Search value not in list
    `%!in%` = Negate(`%in%`)
    
    validate(need(input$choice_sample_input, "No sample selected"))
    validate(need(all(input$choice_sample_input %in% choices()$sample),
                  "Please select samples"))
    
    samples <- choices()$sample
    parameter <- get_parameters_gs(rval$gating_set)
    
    #get data from the actual 
    df <- get_data_gs(gs = rval$gating_set,
                      sample = choices()$sample,
                      subset = "root",
                      spill = parameter$compensation,
                      return_comp_data = TRUE,
                      Ncells = NULL
    )
    
    df$badCells <- 0
    
    for(i in 1:length(samples)){
      sample <- samples[i]
      
      df_temp2[[sample]] <- df[df$name == sample,]

      # search bad cells
      
      ## FlowRate search badCells
      
      if(input$groupButton == 1){
        if(sample %in% names(ok_after_verify$flowRateSelected)){
          # keep cell from flowRate
          df_temp2[[sample]] <- df_temp2[[sample]][ok_after_verify$flowRateSelected[[sample]],]
          
        } else {
          if(df_temp2[[sample]][,"badCells"] %!in% df_temp2[[sample]][res()$flowRateQCList[[sample]]$goodCellIDs,"badCells"]) {
            pos <- which(df_temp2[[sample]][,"badCells"] %!in% df_temp2[[sample]][res()$flowRateQCList[[sample]]$goodCellIDs, "badCells"])
            df_temp2[[sample]][pos, "badCells"] <- 1
            
          }
          
          if(is.null(res()$flowRateQCList[[sample]]$badCellIDs)){
            message("NULL VALUE ICI")
          } else {
            if(is.na(res()$flowRateQCList[[sample]]$badCellIDs) && length(res()$flowRateQCList[[sample]]$badCellIDs) == 0){
              message("badcells ID is not present in flowRate")
            } else {
              df_temp2[[sample]][res()$flowRateQCList[[sample]]$badCellIDs, "badCells"] <- 1
            }
            
          }
        }
      }
      
      ## Dynamic range search badCells 
      if(input$groupButton == 2){
        
        if(df_temp2[[sample]][, "badCells"] %!in% df_temp2[[sample]][res()$dynamic_range[[sample]]$goodCellIDs, "badCells"]){
          pos <- which(df_temp2[[sample]][,"badCells"]  %!in% df_temp2[[sample]][res()$dynamic_range[[sample]]$goodCellIDs, "badCells"])
          df_temp2[[sample]][pos, "badCells"] <- 1
          
        } 
        
        # search badCells if id is found in bad lower_ids or upper_ids
        if(is.null(res()$dynamic_range[[sample]]$bad_lowerIDs)) {
          message("NULL VALUE PRESENT HERE")
        } else {
          if(is.na(res()$dynamic_range[[sample]]$bad_lowerIDs) || length(res()$dynamic_range[[sample]]$bad_lowerIDs) == 0){
            message("bad lower ids not found in dynamic range")
          } else {
            df_temp2[[sample]][res()$dynamic_range[[sample]]$bad_lowerIDs, "badCells"] <- 1
          }
        }
        
        if(is.null(res()$dynamic_range[sample]$bad_upperIDs)){
          message("NULL VALUE PRESENT HERE ICI")
        } else{
          if(is.na(res()$dynamic_range[[sample]]$bad_upperIDs) && length(res()$dynamic_range[[sample]]$bad_upperIDs) == 0){
            message("badupper ID cells is not present in dynamic range")
            
          } else {
            df_temp2[[sample]][res()$dynamic_range[[sample]]$bad_upperIDs, "badCells"] <- 1
          }
        }
      }
 
      ## Signal acquisition search badCells
      if(input$groupButton == 3){
        
        if(df_temp2[[sample]][,"badCells"] %!in% df_temp2[[sample]][res()$FlowSignalQCList[[sample]]$goodCellIDs,"badCells"]){
          pos <- which(df_temp2[[sample]][,"badCells"] %!in% df_temp2[[sample]][res()$FlowSignalQCList[[sample]]$goodCellIDs, "badCells"])
          df_temp2[[sample]][pos, "badCells"] <- 1
        }
      }
      
      # get cleaning gating set
      df_ending <- rbind(df_ending, df_temp2[[sample]])
      
      subset_df_clean <- subset(df_ending, df_ending[,"badCells"] == 0)
      
      progress$inc(1/i, detail = paste("Doing part", sample))
    }
    
    subset_df_clean[,"badCells"] <- as.integer(subset_df_clean[,"badCells"])
    df_ending[,"badCells"] <- as.integer(df_ending[,"badCells"]) 

    gs_old_tagged <- build_gatingset_from_df(df = df_ending, gs_origin = rval$gating_set)
    gs_good_cells <- build_gatingset_from_df(df = subset_df_clean, gs_origin = rval$gating_set)

    return(
      list(
        gs_old_tagged,
        gs_good_cells
      )
    )

  })
  
  ### Build the new gating_set & old gating_set tagged #################################################
  
  observe({
    updateTextInput(session, "GatingSet_tagged_name", value = paste0(rval$gating_set_selected, "_clean"))
  })
  

  observeEvent(input$action_create_gatingset,{
    preview_val_but$input_prev <- 0
    gs_list <- create_futur_gs() # list of frame for futur gs
    
    if(is.null(gs_list)){
      # show modal
    } else {
      # reset of ok_after_verify 
      if(input$gating_set_tag_or_not == T){
        ok_after_verify$flowRateSelected <- NULL
        
        old_gs <- gs_list[[1]]

        params <- colnames(old_gs)[colnames(old_gs) %in% names(rval$trans_parameters)]
        
        
        rval$gating_set_list[[paste(rval$gating_set_selected, "_tag")]] <- list(gating_set = old_gs,
                                                                                parent = rval$gating_set_selected,
                                                                                trans_parameters = rval$trans_parameters[params]
        )
        
        rval$gating_set <- old_gs
        rval$update_gs <- rval$update_gs + 1
      } 
      
      
      gs <- gs_list[[2]]
      
      params <- colnames(gs)[colnames(gs) %in% names(rval$trans_parameters)]
      rval$gating_set_list[[input$GatingSet_tagged_name]] <- list(gating_set = gs,
                                                                  parent = rval$gating_set_selected,
                                                                  trans_parameters = rval$trans_parameters[params]
      )
      
      rval$gating_set_selected <- input$GatingSet_tagged_name
      
      rval$gating_set <- gs
      rval$update_gs <- rval$update_gs + 1
    }
    
    
  })
  
  ### Build result table ###############################################################################
  
  res_table <- eventReactive(res(), {
    if(!is.null(res())){

    df <- NULL
    
    samples <- input$choice_sample_input
    
    for(i in  1:length(samples)){
      sample <- samples[i]

      Signal_acquisition <- res()$FlowSignalQCList[[sample]]$Perc_bad_cells$badPerc_cp*100
      print(Signal_acquisition)
      Number_sig_acq_good_cells <- length(res()$FlowSignalQCList[[sample]]$goodCellIDs)
      
      Flow_rate <- res()$flowRateQCList[[sample]]$res_fr_QC$badPerc*100
      Number_flowRate_good_cells <- length(res()$Flow_rate[[sample]]$goodCellIDs)
      
      Dynamic_range <- res()$dynamic_range[[sample]]$badPerc*100
      Number_margin_good_cells <- length(res()$dynamic_range[[sample]]$goodCellIDs)
      tot_bad_cells_margin <- length(res()$dynamic_range[[sample]]$bad_lowerIDs) + length(res()$dynamic_range[[sample]]$bad_upperIDs)
      Number_margin_bad_cells <- tot_bad_cells_margin
      
      # if the user have selected Signal acquisition or FlowRate or Dynamic range (make different kind of dataframe)
      if(length(Dynamic_range) == 0 && length(Flow_rate) > 0 && length(Signal_acquisition) > 0){
        df <- rbind(df, data.frame(Flow_rate,
                                   Signal_acquisition,
                                   Number_flowRate_good_cells,
                                   Number_sig_acq_good_cells))
      } else if(length(Flow_rate) == 0 && length(Dynamic_range) > 0 && length(Signal_acquisition)> 0){
              df <- rbind(df, data.frame(Dynamic_range,
                                         Signal_acquisition,
                                         Number_sig_acq_good_cells,
                                         Number_margin_good_cells,
                                         Number_margin_bad_cells))
      } else if(length(Signal_acquisition) == 0 && length(Dynamic_range) > 0 && length(Flow_rate > 0)){
              df <- rbind(df, data.frame(Flow_rate,
                                         Dynamic_range,
                                         Number_flowRate_good_cells,
                                         Number_margin_good_cells,
                                         Number_margin_bad_cells))
      } else if(length(Signal_acquisition) == 0 && length(Dynamic_range) == 0) {
              df <- rbind(df, data.frame(Flow_rate,
                                         Number_flowRate_good_cells))
      } else if(length(Flow_rate) == 0 && length(Dynamic_range) == 0){
        df <- rbind(df, data.frame(Signal_acquisition,
                                   Number_sig_acq_good_cells
                                   ))
      } else if(length(Flow_rate) == 0 && length(Signal_acquisition) == 0){
        df <- rbind(df, data.frame(Dynamic_range,
                                   Number_margin_good_cells,
                                   Number_margin_bad_cells))
      }
      else {
              df <- rbind(df, data.frame(Flow_rate,
                                         Dynamic_range,
                                         Signal_acquisition,
                                         Number_flowRate_good_cells,
                                         Number_sig_acq_good_cells,
                                         Number_margin_good_cells,
                                         Number_margin_bad_cells))
      }
    }
    
    colnames(df)[which(names(df) == "Sample")] <- "Samples names"
    colnames(df)[which(names(df) == "Signal_acquisition")] <- "Signal acquisition bad cells(%)"
    colnames(df)[which(names(df) == "Flow_rate")] <- "Flow rate bad cells(%)"
    colnames(df)[which(names(df) == "Dynamic_range")] <- "Dynamic range bad cells(%)"
    colnames(df)[which(names(df) == "Number_flowRate_good_cells")] <- "Goods cells flowRate"
    colnames(df)[which(names(df) == "Number_sig_acq_good_cells")] <- "Signal Acquisition goods cells"
    colnames(df)[which(names(df) == "Number_margin_good_cells")] <- "Dynamic range goods cells"
    colnames(df)[which(names(df) == "Number_margin_bad_cells")] <- "Dynamic range bad cells"
    
    rownames(df) <- input$choice_sample_input

    return(df)
    }
  })
  ### get gatingSet name ##################################################################################
  
  # for reinitialize the plot
  name_gs_analysed <- eventReactive(c(input$clean_selected_sample_input, input$pre_cleaning), {
    value_name <- rval$gating_set_selected
    return(value_name)
  })
  
  # check condition 
  
  condition_reini <- reactive({
    if(!is.null(rval$gating_set_selected)){
      validate(need(name_gs_analysed() == rval$gating_set_selected, ""))
    }
  })
  
  ### Display option for heatmap ##########################################################################
  
  color_selection <- reactive({
    if(input$colorpalette_select == "A"){
      pal_fill <- scale_fill_viridis_c(option = "A")
    }
    else if(input$colorpalette_select == "B"){
      scale_fill_viridis_c(option = "B")
    }
    else if(input$colorpalette_select == "C"){
      scale_fill_viridis_c(option = "C")
    }
    else{
      scale_fill_viridis_c(option = "D")
    }
  })
  
  
  ### Build heatmap #######################################################################################

  heatmap_plot <- reactive({
    options(warn = -1) 
    validate(
      need(!is.null(res()), "Please clean your data with the correct parameters")
      )
    condition_reini()
    if(length(input$groupButton) > 1){
      heatmaply(res_table()[,1:length(input$groupButton)],scale_fill_gradient_fun = color_selection(), limits = c(0,100), dendrogram = F,
                Rowv = FALSE, Colv = FALSE)
    } else {
      
      heatmaply(res_table()[1],scale_fill_gradient_fun = color_selection(), limits = c(0,100), dendrogram = F,
                Rowv = FALSE, Colv = FALSE)
    }
  })

  ### Display messages on top of QC plots ##################################################################
  
  output$fr_message <- renderText({
    validate(
      need(length(res()$flowRateQCList) != 0, "")
    )
    condition_reini()
    perc <- res()$flowRateQCList[[input$select_one_sample]]$res_fr_QC$badPerc*100
    paste(perc,"% of anomalous cells detected in the flow rate check")
  })
  
  output$dynamic_message <- renderText({
    validate(
      need(length(res()$dynamic_range) != 0, "")
    )
    condition_reini()
    
    perc <- res()$dynamic_range[[input$select_one_sample]]$badPerc*100
    paste(perc,"% of anomalous cells detected in the dynamic range check")
  })
  
  output$signal_message <- renderText({
    validate(
      need(length(res()$FlowSignalQCList) != 0, "")
    )
    condition_reini()
    
    perc <- res()$FlowSignalQCList[[input$select_one_sample]]$Perc_bad_cells$badPerc_cp*100
    paste(perc,"% of anomalous cells detected in signal acquisition check")
  })
  
  ### Build QC plots #######################################################################################
  
  output$flow_rate_plot_output <- renderPlot({
    validate(
      # need(!is.null(res()), "Need to clean the data with the correct option"),
      need(length(res()$flowRateQCList) != 0, "Need to select flow rate cleaning to visualize plot")
    )
    condition_reini()
    
    plot_rate <- flowAI:::flow_rate_plot(res()$flowRateQCList[[input$select_one_sample]])
    plot_rate <- plot_rate + ggplot2:::geom_hline(yintercept = c(rateSlider()[1], rateSlider()[2]), color="blue",
                                                  linetype = "longdash", size = 1.2, show_guide = TRUE)
    plot_rate <- plot_rate + ggplot2:::geom_vline(xintercept = c(timeSlider()[1], timeSlider()[2]), color="blue",
                                                  linetype = "longdash", size = 1.2, show_guide = TRUE)
    plot_rate
  })
  
  # interractive selection for flow rate and kept it 
  selection_Cells_interactive <- reactive({
    frequencies <- as.data.frame(res()$flowRateQCList[[input$select_one_sample]]$frequencies)
    
    cellBinID <- res()$flowRateData[[input$select_one_sample]]$cellBinID

    goodcell_x <- which(frequencies$secbin < timeSlider()[2] & frequencies$secbin > timeSlider()[1])
    goodcell_y <- which(frequencies$tbCounts < rateSlider()[2] & frequencies$tbCounts > rateSlider()[1])

    flowRateQC <- cellBinID$cellID[cellBinID$binID %in% intersect(goodcell_x, goodcell_y)]

    return(flowRateQC)

  })
  
  ### Verify if the users use Ok button when apply the current selected sample ################################
  ok_after_verify <- reactiveValues(flowRateSelected = NULL)
  
  observeEvent(input$applyInput, {
    verify_selection()
  })
  
  # showmodal button (if ok is pressed then the user assume to get specific cell) or close and stop the process
  verify_selection <- function(failed = FALSE) {
    ns <- session$ns
    showModal(
      if (failed == F)
        modalDialog(title = "Verify selection cutting",
                    "Are you sure to apply selection of cells for the current sample",
                    footer = tagList(
                      actionButton(ns("ok"), "OK"),
                      modalButton("Cancel")
                      
                    )
        )
    )
  }
  
  # if ok is pressed then close the modal and add the current selected sample in list (reactive object)
  observeEvent(input$ok, {
    ok_after_verify$flowRateSelected <- list()
    ok_after_verify$flowRateSelected[[input$select_one_sample]] <- selection_Cells_interactive()
    removeModal()
  })
 
  output$dynamic_plot_output <- renderPlot({
    validate(
      need(length(res()$dynamic_range) != 0, "Need to select dynamic range cleaning to visualize plot")
    )
    condition_reini()
    
    flowAI:::flow_margin_plot(res()$dynamic_range[[input$select_one_sample]], binSize = input$binSize)
  })

  signal_plot <- reactive({
    validate(
      need(length(res()$FlowSignalQCList) != 0, "Need to select signal acquisition cleanin to visualize plot")
    )
    condition_reini()

    flowAI:::flow_signal_plot(res()$FlowSignalQCList[[input$select_one_sample]])
  })
  
  output$result_output <- DT::renderDataTable({
    condition_reini()

    datatable(res_table(), options = list(scrollX = T, scrollCollapse=TRUE, lengthMenu = c(100,50,20,10)))
  })
  
  ### Popup when the data is not clean #####################################################################################
  
  observe({
    validate(need(!is.null(rval$gating_set), ""))
    validate(need(!is.null(rval$active_menu), ""))
      
    if(rval$active_menu == "Clean_tab"){
      if(!"badCells" %in% colnames(rval$gating_set)){
        print("single cond")
        
        print(reactiveVerification$alphaError == 1)
        print(reactiveVerification$secondFractionError == 1)
        print(reactiveVerification$timeStepError == 1)
        print(reactiveVerification$binSizeError == 1)
        
        print("multiple condition")
        
        print(reactiveVerification$alphaError == 1 || reactiveVerification$secondFractionError == 1)
        print(reactiveVerification$alphaError == 1 || reactiveVerification$secondFractionError == 1 || reactiveVerification$timeStepError == 1 || reactiveVerification$binSizeError == 1)
        if(reactiveVerification$alphaError == 1  || reactiveVerification$secondFractionError == 1 || reactiveVerification$timeStepError == 1 || reactiveVerification$binSizeError == 1){
          ns <- session$ns
          
          showModal(
            modalDialog(title = "Would you like to make a first cleaning",
                        "You cannot perform a precleaning because one or multiple parameters provide an error",
                        footer = tagList(actionButton(ns("pre_cleaning"), "Run cleaning"),
                                                                 modalButton("Quit")
                        )
            )
          )
          shinyjs::disable("pre_cleaning")
        } else { 
          ns <- session$ns
          
          showModal(
            modalDialog(title = "Would you like to make a first cleaning",
                        tagList(actionButton(ns("pre_cleaning"), "Run cleaning"),
                                modalButton("Quit")
                        ),
                        footer = NULL
            )
          )
          
          shinyjs::enable("pre_cleaning")
        }

      }
    }
  })

  
  return(rval)
}

#### Tests ####
# 
# library(mFilter)
# library(plyr)
# library(changepoint)
# library(shiny)
# library(shinydashboard)
# library(flowWorkspace)
# library(flowCore)
# library(plotly)
# library(heatmaply)
# library(flowAI) 
# 
# if (interactive()){
# 
#   ui <- dashboardPage(
#     dashboardHeader(title = "flowAI"),
#     sidebar = dashboardSidebar(disable = TRUE),
#     body = dashboardBody(
#       CleanUI("module")
#     )
#   )
# 
#   server <- function(input, output, session) {
#     rval <- reactiveValues()
#     observe({
#       #utils::data("GvHD", package = "flowCore")
#       #rval$gating_set <- GatingSet(GvHD)
#        utils::data("Bcells", package = "flowAI")
#        rval$gating_set <- flowWorkspace::GatingSet(Bcells)
#     })
#     res <- callModule(Clean, "module", rval = rval)
#   }
# 
#   shinyApp(ui, server)
# 
# }
