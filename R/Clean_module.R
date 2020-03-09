#' @param id shiny id
#' @import shiny
#' @import DT
#' @import plotly
#' @import shinyalert
#' @import shinyjs

CleanUI<-function(id){
  
  ns <- NS(id)

  fluidRow(
    br(),
    shinyjs::useShinyjs(),
    shinyalert::useShinyalert(),
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
                                    plotOutput(ns("flow_rate_plot_output"))
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
                           
               ), 
               textInput(ns("GatingSet_tagged_name"), label = "Create GatingSet"),
               actionButton(ns("action_create_gatingset"), label = "Create GatingSet")
               
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
                                    DT::dataTableOutput(ns("result_output"))
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
#' @import flowAI
Clean <- function(input, output, session, rval) {
  
  ### Call modules ###################################################################
  
  callModule(simpleDisplay, "simple_display_module", 
             plot_list = heatmap_plot, 
             params = reactiveValues(use_plotly = TRUE, width = 500, height = 500, min_size = 300),
             save = FALSE)
  
  callModule(simpleDisplay, "simple_display_module2", 
             plot_list = signal_plot, 
             params = reactiveValues(width = 500, height = 50, max_height=500))
  
  ### Check parameter to provide an error ############################################
  
  #ESD parameter error
  observe({ 
    validate(
      need(input$alpha != "", "choose an ESD between 0-1"),
      need(is.numeric(input$alpha), "Numeric value only")
             )
    print(is.na(input$alpha))
    if(!is.null(input$alpha) && input$alpha>=0 && input$alpha<=1 && is.numeric(input$alpha)){
      input$alpha
      shinyjs::enable(id = "clean_selected_sample_input")
    } else {
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
      shinyjs::enable(id = "clean_selected_sample_input")
    } else {
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
      shinyjs::enable(id = "clean_selected_sample_input")
    } else {
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
    bin_max <- round(bin - 2 )
    
    # disable and show modal dialog or button is usable
    if(input$binSize<= 0 || input$binSize > bin_max){
    
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
    print(excludeCh)
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
    print(chNames)
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
  
  ### Analyze samples ###################################################
  
  res <- eventReactive(input$clean_selected_sample_input, {
    show_error <- 0
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
    
    FR_bin_arg <- list( second_fraction = second_fraction, 
                        timeCh = timeCh,
                        timestep = timestep)
    
    ## set signal acquisition parameters 
    
    FS_bin_arg <- list( channels = NULL,
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
    
    
    withProgress(message = 'The data cleaning is running..', value = 0,{
      for(i in  1:length(samples)){
        sample <- samples[i]
        
        ordFCS <- ord_fcs_time(set()[[sample]], timeCh = timeCh)
        ### modification ici #### 
        if(2 %in% input$groupButton){
          dynamic_range[[sample]] <-   tryCatch(expr = {
            flow_margin_check(x = ordFCS,
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
            flowRateData <- do.call(flow_rate_bin, c(list(ordFCS), 
                                                     FR_bin_arg))
            do.call(flow_rate_check_auto,
                    c(list(ordFCS, flowRateData),
                      FR_QC_arg))
          } ,
          error = function(e) {
            message("Selected channel is not appropriate here, please select choose other channel.")
            return(NA)
          }
          )
          
        }
        
        if(is.na(flowRateQCList[[sample]])){break} # break loop if we have an error
        # signal acquisition process
        
        if(3 %in% input$groupButton){
          FlowSignalQCList[[sample]] <-   tryCatch(expr = {
            FlowSignalData <- do.call(flow_signal_bin, c(list(ordFCS), 
                                                         FS_bin_arg))
            do.call(flow_signal_check_auto, 
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
                    footer = modalButton("Quit")
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
    # print(names(flowRateQCList))
    
    return(
      list(
        flowRateQCList = flowRateQCList,
        FlowSignalQCList = FlowSignalQCList,
        dynamic_range = dynamic_range
      )
    )
    
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
          if(df_temp2[[sample]][, "badCells"] %!in% df_temp2[[sample]][res()$dynamic_range[[sample]]$goodCellIDs, "badCells"]){
            pos <- which(df_temp2[[sample]][,"badCells"]  %!in% df_temp2[[sample]][res()$dynamic_range[[sample]]$goodCellIDs, "badCells"])
            df_temp2[[sample]][pos, "badCells"] <- 1

          } else if(df_temp2[[sample]][,"badCells"] %!in% df_temp2[[sample]][res()$flowRateQCList[[sample]]$goodCellIDs,"badCells"]) {
            pos <- which(df_temp2[[sample]][,"badCells"] %!in% df_temp2[[sample]][res()$flowRateQCList[[sample]]$goodCellIDs, "badCells"])
            df_temp2[[sample]][pos, "badCells"] <- 1


          } else if(df_temp2[[sample]][,"badCells"] %!in% df_temp2[[sample]][res()$FlowSignalQCList[[sample]]$goodCellIDs,"badCells"]){
            pos <- which(df_temp2[[sample]][,"badCells"] %!in% df_temp2[[sample]][res()$FlowSignalQCList[[sample]]$goodCellIDs, "badCells"])
            df_temp2[[sample]][pos, "badCells"] <- 1
          }
          
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
            if(is.na(res$dynamic_range[[sample]]$bad_upperIDs) && length(res$dynamic_range[[sample]]$bad_upperIDs) == 0){
              message("badupper ID cells is not present in dynamic range")

            } else {
              df_temp2[[sample]][res()$dynamic_range[[sample]]$bad_upperIDs, "badCells"] <- 1
            }
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
        
        # get cleaning gating set
        df_ending <- rbind(df_ending, df_temp2[[sample]])

        subset_df_clean <- subset(df_ending, df_ending[,"badCells"] == 0)
       
        progress$inc(1/i, detail = paste("Doing part", sample))
        }
      
      subset_df_clean[,"badCells"] <- as.factor(subset_df_clean[,"badCells"])
      df_ending[,"badCells"] <- as.factor(df_ending[,"badCells"]) 
      
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
    gs_list <- create_futur_gs()
    if(is.null(gs_list)){
      # show modal
    } else {
      old_gs <- gs_list[[1]]
      print(rval$gating_set_selected)
      params <- colnames(old_gs)[colnames(old_gs) %in% names(rval$trans_parameters)]


      rval$gating_set_list[[paste(rval$gating_set_selected, "_tag")]] <- list(gating_set = old_gs,
                                                                  parent = rval$gating_set_selected,
                                                                  trans_parameters = rval$trans_parameters[params]
      )

      rval$gating_set <- old_gs
      rval$update_gs <- rval$update_gs + 1
      
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
    
    # Signal_acquisition <- lapply(1:length(samples), function(sample){res()$FlowSignalQCList[[sample]]$Perc_bad_cells$badPerc_cp*100})
    # Flow_rate <- lapply(1:length(samples), function(sample){res()$flowRateQCList[[sample]]$res_fr_QC$badPerc*100})
    # 
    # 
    # Number_flowRate_good_cells <- lapply(1:length(samples), function(sample){length(res()$Flow_rate[[sample]]$goodCellIDs)})
    # Number_sig_acq_good_cells <- lapply(1:length(samples), function(sample){length(res()$FlowSignalQCList[[sample]]$goodCellIDs)})
    # 
    # Dynamic_range <- lapply(1:length(samples), function(sample){res()$dynamic_range[[sample]]$badPerc*100})
    # Number_margin_good_cells <- lapply(1:length(samples), function(sample){length(res()$dynamic_range[[sample]]$goodCellIDs)})
    # tot_bad_cells_margin <-  lapply(1:length(samples), function(sample){length(res()$dynamic_range[[sample]]$bad_lowerIDs) + length(res()$dynamic_range[[sample]]$bad_upperIDs)})
    # Number_margin_bad_cells <- tot_bad_cells_margin
    
    # print(input$groupButton)
    
    for(i in  1:length(samples)){
      sample <- samples[i]
      print(res()$FlowSignalQCList[[sample]]$Perc_bad_cells)
      Signal_acquisition <- res()$FlowSignalQCList[[sample]]$Perc_bad_cells$badPerc_cp*100
      Number_sig_acq_good_cells <- length(res()$FlowSignalQCList[[sample]]$goodCellIDs)
      
      Flow_rate <- res()$flowRateQCList[[sample]]$res_fr_QC$badPerc*100
      Number_flowRate_good_cells <- length(res()$Flow_rate[[sample]]$goodCellIDs)
      
      Dynamic_range <- res()$dynamic_range[[sample]]$badPerc*100
      Number_margin_good_cells <- length(res()$dynamic_range[[sample]]$goodCellIDs)
      tot_bad_cells_margin <- length(res()$dynamic_range[[sample]]$bad_lowerIDs) + length(res()$dynamic_range[[sample]]$bad_upperIDs)
      Number_margin_bad_cells <- tot_bad_cells_margin

      df <- rbind(df, data.frame(Flow_rate, 
                                 Dynamic_range,
                                 Signal_acquisition,
                                 Number_flowRate_good_cells,
                                 Number_sig_acq_good_cells,
                                 Number_margin_good_cells,
                                 Number_margin_bad_cells))
    
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
    validate(need(!is.null(res()), "Please clean your data with the correct parameters"))
      heatmaply(res_table()[,1:3],scale_fill_gradient_fun = color_selection(), limits = c(0,100), dendrogram = F,
                Rowv = FALSE, Colv = FALSE)
  })

  ### Display messages on top of QC plots ##################################################################
  
  output$fr_message <- renderText({
    validate(need(
      !is.null(res()), ""
    ))
    perc <- res()$flowRateQCList[[input$select_one_sample]]$res_fr_QC$badPerc*100
    paste(perc,"% of anomalous cells detected in the flow rate check")
  })
  
  output$dynamic_message <- renderText({
    validate(need(
      !is.null(res()), ""
    ))
    perc <- res()$dynamic_range[[input$select_one_sample]]$badPerc*100
    paste(perc,"% of anomalous cells detected in the dynamic range check")
  })
  
  output$signal_message <- renderText({
    validate(need(
      !is.null(res()), ""
    ))
    perc <- res()$FlowSignalQCList[[input$select_one_sample]]$Perc_bad_cells$badPerc_cp*100
    paste(perc,"% of anomalous cells detected in signal acquisition check")
  })
  
  ### Build QC plots #######################################################################################
  
  output$flow_rate_plot_output <- renderPlot({
    validate(need(
      !is.null(res()), "Need to clean the data with the correct option"
    ))
      flow_rate_plot_auto(res()$flowRateQCList[[input$select_one_sample]])
  })

  output$dynamic_plot_output <-renderPlot({
    validate(need(
      !is.null(res()), "Need to clean the data with the correct option"
    ))
    flow_margin_plot(res()$dynamic_range[[input$select_one_sample]], binSize = input$binSize)
  })
  
  signal_plot <- reactive({
    validate(need(
      !is.null(res()), "Need to clean the data with the correct option"
    ))
    flow_signal_plot_auto(res()$FlowSignalQCList[[input$select_one_sample]])
  })

  output$result_output <- DT::renderDataTable({
    validate(need(
      !is.null(res()), "Need to clean the data with the correct option"
    ))
    datatable(res_table(), options = list(scrollX = T, scrollCollapse=TRUE, lengthMenu = c(100,50,20,10)))
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
#       # utils::data("GvHD", package = "flowCore")
#       # rval$gating_set <- GatingSet(GvHD)
#       utils::data("Bcells", package = "flowAI")
#       rval$gating_set <- flowWorkspace::GatingSet(Bcells)
#     })
#     res <- callModule(Clean, "module", rval = rval)
#   }
# 
#   shinyApp(ui, server)
# 
# }
