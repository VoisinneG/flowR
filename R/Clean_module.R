## Directly adapted from the flowAI shiny app

library(plyr)
library(ggplot2)
library(flowCore)
library(reshape2)
### UI ########################################################################################

#' @param id shiny id
#' @import shiny
CleanUI<-function(id){
  
  ns <- NS(id)
  
  
  fluidPage(
    # titlePanel("Interactive Quality Control for Flow Cytometry Data"),
    
    tabsetPanel(type ="pills", tabPanel("Manual", fluidRow(
      column(3,
             
             h4("Input:"),
             selectInput(ns("sample"), "Select sample", choices = NULL, selected = NULL, multiple = FALSE),
             
             hr(),
             h4("Summary:"),
             textOutput(ns("summaryText1")),
             textOutput(ns("summaryText2")),
             
             hr(),
             h4("Parameters:"),
             numericInput(ns("timeLenth"), label = h5("Time step (sec)"), value = 0.1, step = 0.1),
             uiOutput(ns("signalBinSize")),
             
             hr(),
             h4("Download Output:"),
             downloadButton(ns('downloadQCFCS'),   'FCS with QC'),
             br(),
             downloadButton(ns('downloadGoodFCS'), 'High Q FCS'),
             br(),
             downloadButton(ns('downloadBadFCS'),  'Low Q FCS'),
             
             hr(),
             div(style = "margin-top: 30px; width: 200px; ", HTML("Citation")),
             div(style = "margin-top: 10px; ",
                 HTML("Monaco,G. et al. (2016) flowAI: automatic and interactive anomaly discerning tools for flow cytometry data.
                      Bioinformatics. 2016 Aug 15;32(16):2473-80."))
                 ),
      column(9,
             tabsetPanel(type = "pills",
                         
                         tabPanel("Flow Rate", fluidPage(
                           hr(),
                           textOutput(ns("flowRateSummary")),
                           hr(),
                           plotOutput(ns("flowRatePlot")),
                           hr(),
                           fluidRow(
                             column(4, offset = 1,
                                    uiOutput(ns("timeSlider"))
                             ),
                             column(4, offset = 2,
                                    uiOutput(ns("rateSlider"))
                             )
                           )
                         )),
                         
                         tabPanel("Signal Acquisition", fluidPage(
                           hr(),
                           textOutput(ns("flowSignalSummary")),
                           hr(),
                           uiOutput(ns("signalBinSlider")),
                           hr(),
                           plotOutput(ns("flowSignalPlot"), height = "800px")
                         )),
                         
                         tabPanel("Dynamic Range", fluidPage(
                           hr(),
                           fluidRow(
                             column(5,
                                    textOutput(ns("flowMarginSummary"))
                             ),
                             column(3, offset = 2,
                                    checkboxInput(ns("checkbox"), label = "Apply Margins Check", value = TRUE)
                             )
                           ),
                           hr(),
                           plotOutput(ns("flowMarginPlot"))
                         ))
             )
      )
      )
    ),
    tabPanel("Automatique", 
             
               fluidRow(
                 br(),
                 column(6, 
                        # fileInput(ns("file"), label = "Fichier FCS", multiple = T, accept = ".fcs"),
                        box(width = 12,
                            
                          textOutput(ns("load_text")),
                        
                        radioButtons(ns("select_mod_parameter_input"), 
                                     label = "Choose type of parameters",
                                     choices = list("Default" = "default",
                                                    "For advanced user" = "advanced")),
                        br(),
                        
                        
                       
                        actionButton(ns("auto_qc_button_input"), 
                                     label = "Clean data"),
                        br(),
                        br()),
                        # uiOutput(ns("show_information_render"))
                        conditionalPanel(condition = "input.auto_qc_button_input%2==1", ns = ns, 
                                         box(title = "Cleaning quality",
                                             width = 12,
                                             # div(style="display: inline-block;vertical-align:top; width: 500px;", cat("The current data selected is ", textOutput(ns("current_data_input")))),
                                             verbatimTextOutput(ns("info")),
                                             plotlyOutput(ns("plot_info_qc"))
                                             )
                                         )

                 ),
               
               ### Options panels ####
               column(6,
                      conditionalPanel(condition = "input.select_mod_parameter_input != 'default'", ns = ns,
                      box(title = "Options",
                          width = 12,
                        ### remove from (Anomalies) select choices ####
                        selectInput(ns("Remove_anomalies"), 
                                    label = "Remove anomalies",
                                    choices = list("all",
                                                   "FR_FS",
                                                   "FR_FM",
                                                   "FS_FM",
                                                   "FR",
                                                   "FS",
                                                   "FM")),
                        helpText("remove anomalies only on a subset of the steps where FR stands for the flow rate, FS for the signal acquisition and FM for dynamic range ('all' by default)"),
                        
                        ### button output choices #### 
                        radioButtons(ns("button_output"), label = "Output choices",
                                     choices= list("high quality event" = 1,
                                                   "low quality with value higher than 10 000" = 2,
                                                   "Id list of low quality" = 3)),
                        helpText("By default you have the highest quality event"),
                        
                        ### second fraction choice ####
                        numericInput(ns("sd_frac_c"),
                                     label = "Choice timestep",
                                     min = 0.01,
                                     max = 1,
                                     step = 0.01,
                                     value = 0.1),
                        helpText("by default the timestep is 0.1"),
                        
                        ### alphaFR statistical signifiant value ####
                        numericInput(ns("sg_input_value"),
                                     label = "statistical value to detect anomalies by method ESD",
                                     value = 0.01,
                                     min = 0,
                                     max = 1,
                                     step = 0.001),
                        
                        ### decomp the flow rate in the trend and the cyclical componants
                        checkboxInput(ns("decomp_input"),
                                      label = "Decomposate the flow rate",
                                      value = T),
                        checkboxInput(ns("outlier_bin"),
                                      label = "Remove outlier bins",
                                      value = F),
                        numericInput(ns("pen_valueFS_input"),
                                     label = "penality value for the changepoint detection algorithm",
                                     min = 0,
                                     value = 500
                        ),
                        sliderInput(ns("max_cptsFS_input"),
                                    label = "Maximum number of changepoints can be detected for each channel",
                                    min = 0,
                                    max = 25,
                                    step = 1,
                                    value = 3),
                        selectInput(ns("sideFM_input"),
                                    label = "select dynamic range",
                                    choices = list("Both" = "both",
                                                   "Upper" = "upper",
                                                   "Lower" = "lower")),
                        radioButtons(ns("neg_valuesFM_input"),
                                     label = "Scalar method",
                                     choices = list("remove outliers" = 1,
                                                    "truncate the negative value" = 2),
                                     inline = T),
                        box(title = "Report options",
                            collapsible = T,
                            collapsed = T,
                            width = 12,
                            
                            checkboxInput(ns("html_report_input"),
                                          label = "Make an html report",
                                          value = T),
                            checkboxInput(ns("mini_report_input"),
                                          label = "Make a TXT mini report",
                                          value = T),
                            checkboxInput(ns("fcs_QC_input"),
                                          label = "Add to the new fcs the quality event",
                                          value = T),
                            checkboxInput(ns("fcs_highQ_input"),
                                          label = "Add only the event that passed the quality control",
                                          value = F),
                            checkboxInput(ns("fcs_lowQ_input"),
                                          label = "Add the event that didn't pass the quality control",
                                          value = F),
                            checkboxInput(ns("folder_result_input"),
                                          label = "Create a file resultQC",
                                          value = T)),
                        
                        
                        
                        uiOutput(ns("ChExcludeFS"))
                      )
                      )
               )
               )
             )
             
    )
    )
}

### server ####################################################################################
#' @importFrom flowCore exprs keyword
#' @importFrom flowWorkspace pData
#' @importFrom scales pretty_breaks
#' @importFrom data.table setDT
#' @importFrom plotly renderPlotly
#' @import flowAI
Clean <- function(input, output, session, rval) {
  
  observe({
    rval$update_gs
    validate(need(class(rval$gating_set) == "GatingSet", "No GatingSet available"))
    choices <- flowWorkspace::pData(rval$gating_set)$name
    updateSelectInput(session, "sample", choices = choices, selected = choices[1])
  })
  
  ## load flowset data
  set <- reactive({
    
    validate(need(class(rval$gating_set) == "GatingSet", "No GatingSet available"))
    validate(need(input$sample, "No sample selected"))
    set <- rval$gating_set@data[[input$sample]]
    
    # if (input$goButton == 0)
    #   return()
    # isolate({fcsFiles <- input$fcsFiles
    # if (is.null(fcsFiles))
    #   return(NULL)
    # set <- read.FCS(fcsFiles$datapath)
    # set@description$FILENAME <- fcsFiles$name})
    return(set)
  })
  
  ## time channel name
  timeChannel <- reactive({
    if(is.null(set()))
      return(NULL)
    x <- set()
    time <- findTimeChannel(x)
    return(time)
  })
  
  ## time step
  timeStep <- reactive({
    if(is.null(set()))
      return(NULL)
    word <- which(grepl("TIMESTEP", names(set()@description),
                        ignore.case = TRUE))
    timestep <- as.numeric(set()@description[[word[1]]])
    if( !length(timestep) ){
      warning("The timestep keyword was not found in the FCS file and it was set to 0.01. Graphs labels indicating time might not be correct", call. =FALSE)
      timestep <- 0.01
    }
    return(timestep)
  })
  
  
  TimeChCheck <- reactive({
    if (!is.null(timeChannel())) {
      if (length(unique(flowCore::exprs(set())[, timeChannel()])) == 1){
        TimeChCheck <- "single_value"
      }else{
        TimeChCheck <- NULL 
      }
    }else{
      TimeChCheck <- "NoTime"
    }
    return(TimeChCheck)
  })
  
  
  ## order fcs expression according acquisition time
  ordFCS <- reactive({
    if(is.null(set()))
      return(NULL)
    if(is.null(TimeChCheck())){
      ordFCS <- ord_fcs_time(set(), timeChannel())
    }else{
      ordFCS <- set()
    }
    return(ordFCS)
  })
  
  
  ## signal bin size UI
  output$signalBinSize <- renderUI({
    
    ns <- session$ns
    
    if(is.null(set())){
      optSize <- NULL
      maxSize <- Inf
    }else{
      maxSize <- nrow(ordFCS())
      optSize <- min(max(1, floor(maxSize/100)), 500)
    }
    numericInput(ns("signalBinSize"), label = h5("Number of events per bin:"),
                 value = optSize, min = 1, max = maxSize)
  })
  
  
  ## cell quality check
  cellCheck <- reactive({
    if(is.null(ordFCS()))
      return(NULL)
    if(is.null(TimeChCheck())){
      flowRateData <- flow_rate_bin(ordFCS(), second_fraction = input$timeLenth,
                                    timeCh = timeChannel(), timestep = timeStep())
    }else{
      flowRateData <- list()
    }
    flowSignalData <- flow_signal_bin(ordFCS(), channels = NULL, 
                                      binSize = input$signalBinSize, timeCh = timeChannel(), 
                                      timestep = timeStep(), TimeChCheck = TimeChCheck() )
    
    flowMarginData <- flow_margin_check(ordFCS())
    
    res <- list(flowRateData, flowSignalData, flowMarginData)
    return(res)
  })
  
  
  ## flow rate time slider UI and check sliders. if they are null, a default value is returned for the QC
  sliders <- reactive({
    flowRateData <- cellCheck()[[1]]
    flowSignalData <- cellCheck()[[2]]
    return(c(
      min(flowRateData$frequencies[,3]) - 0.1,
      max(flowRateData$frequencies[,3]) + 0.1,
      min(flowRateData$frequencies[,4]) - 10,
      max(flowRateData$frequencies[,4]) + 10,
      0,
      nrow(flowSignalData$exprsBin) + 1)
    )
  })
  
  output$timeSlider <- renderUI({
    ns <- session$ns
    
    if(is.null(set()) || is.null(cellCheck()) || !is.null(TimeChCheck()))
      return(NULL)
    sliderInput(ns("timeSlider"), strong("Time cut:"),
                min = sliders()[1], max = sliders()[2], 
                value = c(sliders()[1], sliders()[2]), step = 0.1)
  })
  timeSlider <- reactive({
    if(is.null(input$timeSlider)){
      return(c(sliders()[1], sliders()[2]))
    }else{
      return(c(input$timeSlider[1],  input$timeSlider[2]))
    }
    
  })
  
  output$rateSlider <- renderUI({
    ns <- session$ns
    if(is.null(set()) || is.null(cellCheck()) || !is.null(TimeChCheck()))
      return(NULL)
    
    sliderInput(ns("rateSlider"), strong("Flow rate cut:"),
                min = sliders()[3], max = sliders()[4], 
                value = c(sliders()[3], sliders()[4]), step = 0.1)
  })
  rateSlider <- reactive({
    if(is.null(input$rateSlider)){
      flowRateData <- cellCheck()[[1]]
      return(c(sliders()[3], sliders()[4]))
    }else{
      return(c(input$rateSlider[1],  input$rateSlider[2]))
    }
    
  })
  
  output$signalBinSlider <- renderUI({
    ns <- session$ns
    if(is.null(set()) || is.null(cellCheck()))
      return(NULL)
    sliderInput(ns("signalBinSlider"), strong("Signal acquisition cut:"), width = "90%",
                min = sliders()[5], max = sliders()[6], 
                value = c(sliders()[5], sliders()[6]), step = 1)
  })
  signalSlider <- reactive({
    if(is.null(input$signalBinSlider)){
      return(c(sliders()[5], sliders()[6]))
    }else{
      return(c(input$signalBinSlider[1],  input$signalBinSlider[2]))
    }
  }) 
  
  
  ## plot
  output$flowRatePlot <- renderPlot({
    if(is.null(ordFCS()) || is.null(cellCheck()) || !is.null(TimeChCheck()))
      return(NULL)
    flowRateData <- cellCheck()[[1]]
    frp <- flow_rate_plot(flowRateData, input$rateSlider[1], input$rateSlider[2],
                          input$timeSlider[1], input$timeSlider[2])
    print(frp)
  })
  
  output$flowSignalPlot <- renderPlot({
    if(is.null(set()) || is.null(cellCheck()))
      return(NULL)
    flowSignalData <- cellCheck()[[2]]
    fsp <- flow_signal_plot(flowSignalData, input$signalBinSlider[1], input$signalBinSlider[2])
    print(fsp)
  })
  
  output$flowMarginPlot <- renderPlot({
    if(is.null(set()) || is.null(cellCheck()))
      return(NULL)
    flowMarginData <- cellCheck()[[3]]
    fmp <- flow_margin_plot(flowMarginData, input$signalBinSize)
    print(fmp)
  })
  
  
  
  ## check results
  checkRes <- reactive({
    if(is.null(set()) || is.null(cellCheck()))
      return(NULL)
    
    ordFCS <- ordFCS()
    totalCellNum <- nrow(ordFCS)
    origin_cellIDs <- 1:totalCellNum
    if(is.null(TimeChCheck())){
      FlowRateQC <- flow_rate_check(cellCheck()[[1]], rateSlider()[1], rateSlider()[2],
                                    timeSlider()[1], timeSlider()[2])
    }else{
      FlowRateQC <- origin_cellIDs
    }
    FlowSignalQC <- flow_signal_check(cellCheck()[[2]], signalSlider()[1], signalSlider()[2])
    
    if(input$checkbox[1] == TRUE){
      FlowMarginQC <- cellCheck()[[3]]$goodCellIDs
    }else{
      FlowMarginQC <- origin_cellIDs
    }
    
    goodCellIDs <- intersect(FlowRateQC, intersect(FlowSignalQC, FlowMarginQC))
    badCellIDs <- setdiff(origin_cellIDs, goodCellIDs)
    
    flowRatePerc <- 1 - length(FlowRateQC)/length(origin_cellIDs)
    flowSignalPerc <- 1 - length(FlowSignalQC)/length(origin_cellIDs)
    flowMarginPerc <- 1 - length(FlowMarginQC)/length(origin_cellIDs)
    totalBadPerc <- length(badCellIDs)/length(origin_cellIDs)
    
    params <- parameters(ordFCS)
    keyval <- flowCore::keyword(ordFCS)
    sub_exprs <- flowCore::exprs(ordFCS)
    
    good_sub_exprs <- sub_exprs[goodCellIDs, ]
    goodfcs <- flowFrame(exprs = good_sub_exprs, parameters = params, description = keyval)
    
    bad_sub_exprs <- sub_exprs[badCellIDs, ]
    badfcs <- flowFrame(exprs = bad_sub_exprs, parameters = params,description = keyval)
    
    tempQCvector <- cellCheck()[[2]]
    QCvector <- tempQCvector$cellBinID[,"binID"]
    QCvector[badCellIDs] <- runif(length(badCellIDs), min=10000, max=20000) 
    QCfcs <- addQC(QCvector, sub_exprs, params, keyval)
    
    return(list(totalCellNum, totalBadPerc, goodfcs, badfcs,
                flowRatePerc, flowSignalPerc, flowMarginPerc, QCfcs))
  })
  
  ## summary text
  output$summaryText1 <- renderText({
    if(is.null(checkRes()))
      return(NULL)
    paste0("Total number of events: ", checkRes()[[1]])
  })
  
  output$summaryText2 <- renderText({
    if(is.null(checkRes()))
      return(NULL)
    paste0("Percentage of low-Q events: ", round(checkRes()[[2]]*100,2), "%")
  })
  
  output$flowRateSummary <- renderText({
    if(is.null(checkRes()))
      return(NULL)
    if(is.null(TimeChCheck())){
      paste0("Percentage of low-Q events in flow rate check: ", round(checkRes()[[5]]*100,2), "%")
    }else if(!is.null(TimeChCheck()) && TimeChCheck() == "NoTime"){
      "It is not possible to recreate the flow rate because the time channel is missing."
    }else if(!is.null(TimeChCheck()) && TimeChCheck() == "single_value"){
      "It is not possible to recreate the flow rate because the time channel contains a single value."
    }
  })
  
  output$flowSignalSummary <- renderText({
    if(is.null(checkRes()))
      return(NULL)
    paste0("Percentage of low-Q events in signal acquisition check: ", round(checkRes()[[6]]*100,2), "%")
  })
  
  output$flowMarginSummary <- renderText({
    if(is.null(checkRes()))
      return(NULL)
    paste0("Percentage of low-Q events in dynamic range check: ", round(checkRes()[[7]]*100,2), "%")
  })
  
  file_base <- reactive({
    file_ext <- flowCore::description(ordFCS())$FILENAME
    file_base <- sub("^([^.]*).*", "\\1", file_ext)
    return(file_base)
  })
  
  ## download processed FCS files
  output$downloadGoodFCS <- downloadHandler(
    filename = function(){
      paste0(file_base(), "_HighQ.fcs")
    },
    
    content = function(file){
      data <- checkRes()[[3]]
      if(is.null(data)){
        return(NULL)
      }
      write.FCS(data, file)
      #tar(tarfile = file, files = tempdir)
    }
  )
  
  output$downloadBadFCS <- downloadHandler(
    filename = function(){
      paste0(file_base(), "_LowQ.fcs")
    },
    
    content = function(file){
      data <- checkRes()[[4]]
      if(is.null(data)){
        return(NULL)
      }
      write.FCS(data, file)
      #tar(tarfile = file, files = tempdir)
    }
  )
  
  
  ## download processed FCS files
  output$downloadQCFCS <- downloadHandler(
    filename = function(){
      paste(file_base(), "_QC.fcs", sep='')
    },
    
    content = function(file){
      data <- checkRes()[[8]]
      if(is.null(data)){
        return(NULL)
      }
      write.FCS(data, file)
      #tar(tarfile = file, files = tempdir)
    }
  )
  ### automatic part ####
  # observeEvent(input$auto_qc_button_input, {
  #   output$show_information_render <- renderUI({
  #     box(id = "box_information", width = 12, 
  #         # div(style="display: inline-block;vertical-align:top; width: 500px;", cat("The current data selected is ", textOutput(ns("current_data_input")))),
  #         verbatimTextOutput("information"))
  #   })
  # })
  # 

  
  
  # output$show_information_render <- renderUI({
  #   if(input$auto_qc_button_input %%2){
  #     box(width = 12,
  #             # div(style="display: inline-block;vertical-align:top; width: 500px;", cat("The current data selected is ", textOutput(ns("current_data_input")))),
  #             verbatimTextOutput(ns("info")))
  #     
  #   }
  # })
  # Read the gatingSet object and make the automatic cleaning & show the result in a box
  
  
  observe({
    observeEvent(input$auto_qc_button_input, {
     
      # library(flowAI)
      # if(!is.null(rval$gating_set)){
      #   rval_data <- rval$gating_set@data
      #   
      #   ## Show actual dataset selected from import ####
      #   output$current_data_input <- renderText({
      #     rval$gating_set_selected
      #   })
      #   
      # } else {
      #     rval_data <- NULL}

      if(!is.null(rval$gating_set@data)){
        # output$load_text <- renderText({
        #   "fichier charger check console"
        #   })
      # output$ChExcludeFS <- renderUI({
      #   selectInput("chExclude",
      #               choices = colnames(#file))
      # })

        ### flow_auto_qc automatic analyse for all gating_set object data available #####
        # output$info <- renderPrint({
        #   N <- 10
          withProgress(message = 'The data cleaning is running..', {
            # for(i in 1:N){
            #   
            #   # Long Running Task
            #   Sys.sleep(0.1)
            #   
            #   # Update progress
            #   incProgress(1/N)
            
          
          ## get the sample name from the gating set
          names_sam <- flowCore::sampleNames(rval$gating_set)
          
          ## apply Flow rate parameter (Quality content) & bin arg
          FR_QC_arg <- list(alpha = 0.1, use_decomp = F)
          FR_bin_arg <- list( second_fraction = 1, timeCh = "Time",
                              timestep = 0.1)
          ## apply signal acquisition parameters 
          
          FS_bin_arg <- list( binSize = 50, timeCh = NULL, timestep = 0.1, TimeChCheck = 0.1)
          FS_QC_arg <- list(ChannelExclude = c("FSC", "SSC"), 500, 3, F)
          
          ## get the cleaning for dynamic range / flow rate / signal acquisition in list
          flow_margin_data_list <- list() 
          flowRateQCList <- list()
          FlowSignalQCList <- list()
          
          for(i in 1:length(rval$gating_set@data)){
            
            
            flow_margin_data_list$aa[names_sam[i]] <- list(flow_margin_check_a(rval$gating_set@data[[i]], side = "both"))
            # ordFCS_list[names_sam[i]] <- ord_fcs_time(GvHD[[i]], "Time")
            ordFCS <- ord_fcs_time(rval$gating_set@data[[i]], "Time")
            
            # flow rate process
            flowRateData <- do.call(flow_rate_bin_a, c(ordFCS, FR_bin_arg))
            flowRateQCList$sample[[names_sam[i]]]<- do.call(flow_rate_check_a, c(ordFCS,list(flowRateData), FR_QC_arg))
            
            # signal acquisition process
            FlowSignalData <- do.call(flow_signal_bin_a, c(ordFCS,FS_bin_arg))
            FlowSignalQCList$sample[[names_sam[i]]] <- do.call(flow_signal_check_a, c(ordFCS,list(FlowSignalData),FS_QC_arg))
            
            
          }
          
          ## Creating dataframe for the heatmap ("showing the quality content")
          df <- NULL
          for(i in 1:length(rval$gating_set@data)){
            
            Signal_acquisition <- FlowSignalQCList$sample[[i]]$Perc_bad_cells$badPerc_cp*100
            Flow_rate <- flowRateQCList$sample[[i]]$res_fr_QC$badPerc*100
            #Dynamic_range <- dynamic_range$aa[[i]]$badPerc*100
            df <- rbind(df, data.frame(Flow_rate, 
                                       #Dynamic_range, 
                                       Signal_acquisition )) 
            
          }
          rownames(df) <- names_sam
          melted_data <- melt(data.table::setDT(df, keep.rownames = T))
          print(melted_data)
          #library(plotly)
          #library(data.table)
          output$plot_info_qc <- plotly::renderPlotly({
            
            ggplot(data = melted_data, aes(y = rn, x = variable)) + geom_tile(aes(fill = value), colour = "black") +  
              scale_fill_gradient(low = "#77ff00", high = "red") +
              scale_x_discrete("", expand = c(0, 0)) + 
              scale_y_discrete("", expand = c(0, 0)) +
              
              theme(
                    # axis.ticks = element_blank(), 
                    axis.text.x = element_text(angle = 330, hjust = 0)
                    )
          })
          # for(i in 1:length(rval_data)){
          # resQC <- flow_auto_qc(rval_data[[i]],
          #                       remove_from = input$Remove_anomalies,
          #                       output = input$button_output,
          #                       second_fractionFR = input$sd_frac_c,
          #                       alphaFR = input$sg_input_value,
          #                       decompFR = input$decomp_input,
          #                       # ChExcludeFS = ,
          #                       outlier_binsFS = input$outlier_bin,
          #                       pen_valueFS = input$pen_valueFS_input,
          #                       max_cptFS = input$max_cptsFS_input,
          #                       # ChExcludeFM =
          #                       sideFM = input$sideFM_input,
          #                       neg_valuesFM = input$neg_valuesFM_input,
          #                       html_report = F,
          #                       mini_report = F,
          #                       fcs_QC = F,
          #                       fcs_highQ = input$fcs_highQ_input,
          #                       # fcs_lowQ = input$fcs_lowQ_input,
          #                       folder_results = F
          # )
          # flow_auto_clicked()
          # cat("\n")
          # print(flow_auto_clicked()[1]['name' == "FSC-H"])
         
           # resQC <- flow_auto_clicked()
          # print(rval$gating_set@data$s5a01)
          # print(resQC[[1]])
          # print(table(rval$gating_set@data$s5a01@exprs %in% resQC[[1]]@exprs ))
          
          # sample_names <- (rownames(rval$gating_set@data@phenoData))
          # print(sample_names) 
          # print(getElement(rval$gating_set@data@frames, sample_names[i]))

          # }
          # print(names(resQC@frames))
          # print(paste0("Value of the output : " , input$button_output))
          # print(paste0("Anomalies to remove choice : ", input$Remove_anomalies))
          # print(paste0("Second_fractionFR value : ", input$sd_frac_c))
          # print(paste0("alphaFR value : " , input$sg_input_value))
          # print(paste0("decompFR value : ", input$decomp_input))
          # print(paste0("outlier_binsFS value : " , input$outlier_bin))
          # print(paste0("pen_valueFS value : " , input$pen_valueFS_input))
          # print(paste0("max_cptFS value : ", input$max_cptsFS_input))
   
          # print("____________________________________________________")
          # print(resQC)
          # print("____________________________________________________")
          
          # print(names(rval))
          # print(rval$gating_set@data)
          # print(length(rval$gating_set@data))
          # print(resQC)
          
          })
        # })

        

    }
    else{
      rval_data <- NULL
      message("Aucun fichier")
      }
      })
  })
  
  # flow_auto_clicked <- reactive({
  #   # in a resQC list adding the cleaning data which the corresponding sample name
  #   resQC <- list()
  #   for(i in 1:length(rval$gating_set@data)){
  #     resQC[rownames(rval$gating_set@data@phenoData[i])] <- flow_auto_qc(rval$gating_set@data[[i]],
  #                           remove_from = input$Remove_anomalies,
  #                           output = input$button_output,
  #                           second_fractionFR = input$sd_frac_c,
  #                           alphaFR = input$sg_input_value,
  #                           decompFR = input$decomp_input,
  #                           # ChExcludeFS = ,
  #                           outlier_binsFS = input$outlier_bin,
  #                           pen_valueFS = input$pen_valueFS_input,
  #                           max_cptFS = input$max_cptsFS_input,
  #                           # ChExcludeFM =
  #                           sideFM = input$sideFM_input,
  #                           neg_valuesFM = input$neg_valuesFM_input,
  #                           html_report = F,
  #                           mini_report = F,
  #                           fcs_QC = F,
  #                           fcs_highQ = input$fcs_highQ_input,
  #                           # fcs_lowQ = input$fcs_lowQ_input,
  #                           folder_results = F
  #     )
  #     cat("\n")
  #   }
  #     return(resQC)
    
  # })
  return(rval)
}

### functions ########################################################################################

# Guess which channel captures time in a exprs, flowFrame or flowset
findTimeChannel <- function(xx) {
  time <- grep("^Time$", colnames(xx), value = TRUE, ignore.case = TRUE)[1]
  if (is.na(time)) {
    if (is(xx, "flowSet") || is(xx, "ncdfFlowList"))
      xx <- flowCore::exprs(xx[[1]]) else if (is(xx, "flowFrame"))
        xx <- flowCore::exprs(xx)
      cont <- apply(xx, 2, function(y) all(sign(diff(y)) >= 0))
      time <- names(which(cont))
  }
  if (!length(time) || length(time) > 1)
    time <- NULL
  return(time)
}

# Check if the Fcs file is ordered according to time otherwise it order it.
ord_fcs_time <- function(x, timeCh= "Time"){
  xord <- order(flowCore::exprs(x)[, timeCh])
  
  if( !identical(xord, 1:nrow(x)) ){
    warning(paste0("Expression data in the file ", basename(flowCore::keyword(x)$FILENAME),
                   " were not originally ordered by time."))
    params <- parameters(x)
    keyval <- flowCore::keyword(x)
    sub_exprs <- flowCore::exprs(x)[xord, ]
    newx <- flowFrame(exprs = sub_exprs, parameters = params,
                      description = keyval)
    return(newx)
  }else{
    return(x)
  }
}


flow_rate_bin <- function(x, second_fraction = 0.1, timeCh = "Time", timestep = 0.1){
  
  xx <- flowCore::exprs(x)[, timeCh]
  idx <- c(1:nrow(x))
  lenx <- length(xx)                                                  # num of time ticks
  
  endsec <- ceiling(timestep * max(xx))                 # total seconds of the experiment
  tbins <- seq(0, endsec/timestep, by = second_fraction/timestep)             # time bins
  secbin <- seq(0, endsec, by = second_fraction)               # bin expressed in seconds
  minbin <- round(secbin/60, 3)                                # bin expressed in minutes
  tbCounts <- c(0, hist(xx, tbins, plot = FALSE)$counts)  # number of events per time bin
  
  nrBins <- length(tbins) - 1
  expEv <- lenx/(nrBins)           # median(tbCounts) # expected number of events per bin
  binID <- do.call(c, mapply(rep, x = 1:length(tbCounts), times = tbCounts,
                             SIMPLIFY = FALSE))
  
  if (length(idx) != length(binID))
    stop("length of cell ID not equal length of bin ID")
  
  flowRateData <- list(frequencies = cbind(tbins, minbin, secbin, tbCounts),
                       cellBinID = data.frame(cellID = idx, binID = binID),
                       info = data.frame(second_fraction = second_fraction,
                                         expFrequency = expEv, bins = nrBins))
  return(flowRateData)
}


flow_rate_plot <- function(flowRateData, lowerRateThres, upperRateThres,
                           lowerTimeCut, UpperTimeCut) {
  
  frequencies <- as.data.frame(flowRateData$frequencies)
  second_fraction <- flowRateData$info$second_fraction
  short_period <- quantile(frequencies$secbin, seq(0,1, length.out = 4))
  long_period <- quantile(frequencies$secbin, seq(0,1, length.out = 3*10 + 1))
  
  ## flow rate graph(frg)
  frg <- ggplot(frequencies, aes_string(x="secbin", y="tbCounts")) + geom_line(colour="red") +
    theme_bw() + theme(panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       text=element_text(size = 14))
  frg <- frg + geom_vline(xintercept=short_period, color="gray60") +
    geom_vline(xintercept=long_period, linetype = 2, color="gray60")
  
  frg <- frg + geom_hline(yintercept=c(lowerRateThres, upperRateThres), color="blue",
                          linetype = "longdash", size = 1.2, show_guide = TRUE)
  frg <- frg + geom_vline(xintercept=c(lowerTimeCut, UpperTimeCut), color="blue",
                          linetype = "longdash", size = 1.2, show_guide = TRUE)
  
  frg <- frg + labs(x= "Seconds", y= paste0("Number of cells per 1/", 1/second_fraction, " second"),
                    title= "Flow Rate Plot")
  
  return(frg)
}

flow_rate_check <- function(flowRateData, lowerRateThres, upperRateThres,
                            lowerTimeCut, UpperTimeCut) {
  
  frequencies <- as.data.frame(flowRateData$frequencies)
  cellBinID <- flowRateData$cellBinID
  
  goodcell_x <- which(frequencies$secbin < UpperTimeCut & frequencies$secbin > lowerTimeCut)
  goodcell_y <- which(frequencies$tbCounts < upperRateThres & frequencies$tbCounts > lowerRateThres)
  
  flowRateQC <- cellBinID$cellID[ cellBinID$binID %in% intersect(goodcell_x, goodcell_y) ]
  cat("flow rate high Q events: ", length(flowRateQC), "\n")
  return(flowRateQC)
}


#' @importFrom plyr ddply
flow_signal_bin <- function(x, channels = NULL, binSize=500, timeCh="Time", 
                            timestep=0.1, TimeChCheck = NULL) {
  
  if (is.null(channels) || missing(channels) || is.na(channels)) {
    parms <- setdiff(colnames(x), timeCh)
  } else {
    if (!all(channels %in% colnames(x)))
      stop("Invalid channel(s)")
    parms <- channels
  }
  
  if (missing(binSize) || is.null(binSize) || is.na(binSize))
    binSize <- 500
  
  ### Retriving time and expression info
  exp <- flowCore::exprs(x)
  if (!is.null(TimeChCheck)) {
    timex <- seq(from = 0, length.out = nrow(x), by = 0.1)
  }else{
    timex <- exp[, timeCh]
  }
  yy <- exp[, parms]  # channels data
  idx <- c(1:nrow(x))
  seconds <- timex * timestep
  lenSec <- length(seconds)  # num of time ticks
  uniSeconds <- unique(seconds)  # num of unique time tick
  nrBins <- floor(lenSec/binSize)  # num of bins
  
  if (length(uniSeconds) < nrBins || lenSec < binSize)
    stop("Improper bin size")
  
  cf <- c(rep(1:nrBins, each = binSize), rep(nrBins + 1, lenSec - nrBins * binSize))  # id bins
  stopifnot(length(cf) == lenSec)
  tmpx <- split(seconds, cf)
  xx2 <- sapply(tmpx, mean)       # mean of each time bin  (x axis)
  yy2 <- as.matrix(plyr::ddply(as.data.frame(yy), .(cf), colwise(median)))[, -1]
  
  return(list(exprsBin = cbind(timeSec = xx2, yy2), cellBinID = data.frame(cellID = idx, binID = cf),
              bins = length(unique(cf)), binSize = binSize))
}


flow_signal_plot <- function(flowSignalData, lowerBinThres, upperBinThres) {
  
  exprsBin <- flowSignalData$exprsBin
  
  binID <- 1:nrow(exprsBin)
  teCh <- grep("Time|time|Event|event", colnames(exprsBin), value = T)
  parms <- setdiff(colnames(exprsBin), teCh)
  dataORIG <- exprsBin[, parms]     # first channel is time
  data <- as.data.frame(dataORIG)
  data$binID <- binID
  
  longdata <- melt(data, id.vars = "binID", variable.name = "marker", value.name = "value")
  FS_graph <- ggplot(longdata, aes(x = binID, y = value, col = marker), environment = environment()) +
    geom_line() + facet_grid(marker ~ ., scales = "free") +
    labs(x = "Bin ID", y = "Median Intensity value") + theme_bw() +
    theme(strip.text.y = element_text(angle = 0, hjust = 1), axis.text = element_text(size = 10),
          axis.title = element_text(size = 15), legend.position = "none") +
    scale_x_continuous(breaks= scales::pretty_breaks(n = 10)) +
    scale_y_continuous(breaks= scales::pretty_breaks(n = 3)) +
    geom_rect(aes(xmin = lowerBinThres, xmax = upperBinThres, ymin = -Inf, ymax = Inf), fill = "orange", linetype = 0, alpha = 0.005)
  
  return(FS_graph)
}


flow_signal_check <- function(flowSignalData, lowerBinThres, upperBinThres) {
  
  exprsBin <- flowSignalData$exprsBin
  cellBinID <- flowSignalData$cellBinID
  
  goodBins <- cellBinID$binID < upperBinThres & cellBinID$binID > lowerBinThres
  FlowSignalQC <- cellBinID$cellID[goodBins]
  
  cat("flow signal check: ", length(FlowSignalQC), "\n")
  return(FlowSignalQC)
}


flow_margin_check <- function(x,  margin_channels = NULL, side = "both") {
  
  if (is.null(margin_channels)) {
    teCh <- grep("Time|time|Event|event", colnames(x), value = T)
    parms <- setdiff(colnames(x), teCh)
  } else {
    if (!all(margin_channels %in% colnames(x)))
      stop("Invalid channel(s)")
    parms <- margin_channels
  }
  scatter_parms <- grep("FSC|SSC", parms, value = T)
  
  xx <- c(1:nrow(x))
  yy <- x@exprs[, parms]
  range <- range(x)
  lenx <- length(xx)
  
  ## lower check
  if (side == "lower" || side == "both") {
    
    out_neg_range <- apply(yy, 2, function(x) {
      neg <- which(x < 0)
      # Zscores <- (0.6745*(x[neg] + median(x[neg])))/mad(x[neg]) ## it
      # calculates the Zscore outneg <- neg[which(Zscores < -3.5)]
      min_value <- (-3.5 * mad(x[neg]) + (0.6745 * median(x[neg])))/0.6745  # -3.5 is the default threshold
      if (is.na(min_value)) {
        min(x) - 1
      } else {
        min_value
      }
    })
  }
  
  # n. bad cells for each channel
  if (side == "lower" || side == "both") {
    neg_bad_len <- sapply(parms, function(x) length(xx[yy[, x] <= out_neg_range[x]]))
  }
  if (side == "upper" || side == "both") {
    pos_bad_len <- sapply(parms, function(x) length(xx[yy[, x] >= range[2,
                                                                        x]]))
  }
  
  # badcellIDs
  if (side == "lower" || side == "both") {
    lowID <- do.call(c, lapply(parms, function(ch) {
      xx[yy[, ch] <= out_neg_range[ch]]
    }))
    if(length(scatter_parms) != 0){   ### check for values less than 0 in scatter parameters
      minSc <- apply(yy[,scatter_parms], 1, function(x){
        min(x)
      })
      low_scatter_ID <- which(minSc < 0)
      lowID <- unique(c(lowID, low_scatter_ID))
    }
  }
  if (side == "upper" || side == "both") {
    upID <- do.call(c, lapply(parms, function(ch) {
      xx[yy[, ch] >= range[2, ch]]
    }))
  }
  
  if (side == "lower") {
    summary_bad_cells <- data.frame(lower_range = c(neg_bad_len,
                                                    total_SUM = length(lowID), total_UNIQUE = length(unique(lowID))))
    bad_lowerIDs <- unique(lowID)
    bad_upperIDs <- NULL
    badCellIDs <- unique(lowID)
  } else if (side == "upper") {
    summary_bad_cells <- data.frame(upper_range = c(pos_bad_len,
                                                    total_SUM = length(upID), total_UNIQUE = length(unique(upID))))
    bad_lowerIDs <- NULL
    bad_upperIDs <- unique(upID)
    badCellIDs <- unique(upID)
  } else {
    summary_bad_cells <- data.frame(lower_range = c(neg_bad_len,
                                                    total_SUM = length(lowID), total_UNIQUE = length(unique(lowID))),
                                    upper_range = c(pos_bad_len,
                                                    total_SUM = length(upID), total_UNIQUE = length(unique(upID))))
    bad_lowerIDs <- unique(lowID)
    bad_upperIDs <- unique(upID)
    badCellIDs <- unique(c(lowID,upID))
  }
  
  goodCellIDs <- setdiff(xx, badCellIDs)
  
  cat("margin check:", length(goodCellIDs), "\n")
  
  return(list(goodCellIDs = goodCellIDs, bad_lowerIDs = bad_lowerIDs,
              bad_upperIDs = bad_upperIDs, events = lenx))
}


###  graph showing where the anomalies mostly happened
flow_margin_plot <- function(FlowMarginData, binSize = 500) {
  
  tot_events <- FlowMarginData$events
  bad_lowerIDs <- FlowMarginData$bad_lowerIDs
  bad_upperIDs <- FlowMarginData$bad_upperIDs
  
  if (missing(binSize) || is.null(binSize) || is.na(binSize))
    binSize <- 500
  nrBins <- floor(tot_events/binSize)
  
  cf <- c(rep(1:nrBins, each = binSize), rep(nrBins + 1, tot_events - nrBins * binSize))
  tmpx <- split(1:tot_events, cf)
  
  if(length(bad_lowerIDs) != 0 && length(bad_upperIDs) != 0){
    lowline <- sapply(tmpx, function(x){
      length(which(bad_lowerIDs %in% x))
    })
    upline <- sapply(tmpx, function(x){
      length(which(bad_upperIDs %in% x))
    })
    ymax <- max(lowline, upline)
    plot(lowline, type ="l", col = "blue", bty ="n",
         ylim = c(0, ymax), xlab = "segment ID",
         ylab = "Number of cells removed" )
    lines(upline, col = "red")
    legend("top", c("Negative Outliers", "Upper Margine Events"), lty = 1,bty = "n", cex = 0.7,
           col = c("blue", "red"))
  }else if( length(bad_lowerIDs) != 0 && length(bad_upperIDs) == 0){
    lowline <- sapply(tmpx, function(x){
      length(which(bad_lowerIDs %in% x))
    })
    plot(lowline, type ="l", col = "blue", bty ="n", xlab = "segment ID",
         ylab = "Number of cells removed" )
    legend("top", c("Negative Outliers"), lty = 1,bty = "n", cex = 0.7,
           col = "blue")
  }else if( length(bad_lowerIDs) == 0 && length(bad_upperIDs) != 0){
    upline <- sapply(tmpx, function(x){
      length(which(bad_upperIDs %in% x))
    })
    plot(upline, type ="l", col = "red", bty ="n", xlab = "segment ID",
         ylab = "Number of cells removed" )
    legend("top", c("Upper Margine Events"), lty = 1,bty = "n", cex = 0.7,
           col = "red")
  }
}


## create new flowFrame with the parameter indicating good and bad cells
addQC <- function(QCvector, sub_exprs, params, keyval){
  
  rs <- attr(sub_exprs, "ranges")
  rs <- c(rs, rs[1])
  sub_exprs <- cbind(sub_exprs, QCvector)
  attr(sub_exprs, "ranges") <- rs
  NN <- as.numeric(keyval["$PAR"]) + 1
  names(dimnames(sub_exprs)[[2]]) <- sprintf("$P%sN", 1:NN)
  pnr <- paste0("$P", NN, "R")
  pnb <- paste0("$P", NN, "B")
  pne <- paste0("$P", NN, "E")
  pnn <- paste0("$P", NN, "N")
  pns <- paste0("$P", NN, "S")
  flowCorePnRmax <- paste0("flowCore_$P", NN, "Rmax")
  flowCorePnRmin <- paste0("flowCore_$P", NN, "Rmin")
  o <- params@data
  o[length(o[,1]) + 1,] <- c("QC", "bad > 10,000", as.numeric(keyval$`$P1R`), 0, 20000)
  rownames(o)[length(o[,1])] <- paste("$P", NN, sep = "")
  
  outFCS <- new("flowFrame", exprs=sub_exprs, parameters=new("AnnotatedDataFrame",o), description=keyval)
  flowCore::description(outFCS)[pnr] <- max(20000, flowCore::description(outFCS)$`$P1R`)
  flowCore::description(outFCS)[pnb] <- flowCore::description(outFCS)$`$P1B`
  flowCore::description(outFCS)[pne] <- "0,0"
  flowCore::description(outFCS)[pnn] <- "QC"
  flowCore::description(outFCS)[pns] <- "bad > 10,000"
  flowCore::description(outFCS)$`$PAR` <- NN
  flowCore::description(outFCS)[flowCorePnRmax] <- 20000
  flowCore::description(outFCS)[flowCorePnRmin] <- 0
  outFCS
}

# ### Add function for automatic cleaning #####################################

#' @importFrom mFilter cffilter
anomaly_detection = function(x, max_anoms=0.49, direction='both', alpha=0.01, use_decomp = TRUE, period=1, verbose = FALSE){

  idNOzero <- which(x != 0)
  x <- x[idNOzero]

  # Check for supported inputs types
  if(is.vector(x) && is.numeric(x)) {
    x <- ts(x, frequency = period)
  } else if(is.ts(x)) {
  } else {
    stop("data must be a time series object or a vector that holds numeric values.")
  }

  # Handle NAs
  if (length(rle(is.na(c(NA,x,NA)))$values)>3){
    stop("Data contains non-leading NAs. We suggest replacing NAs with interpolated values (see na.approx in Zoo package).")
  } else {
    x <- na.omit(x)
  }

  # Sanity check all input parameterss
  if(max_anoms > .49){
    stop(paste("max_anoms must be less than 50% of the data points (max_anoms =", round(max_anoms*length(x), 0), " data_points =", length(x),")."))
  }
  if(!direction %in% c('pos', 'neg', 'both')){
    stop("direction options are: pos | neg | both.")
  }
  if(!(0.01 <= alpha || alpha <= 0.1)){
    print("Warning: alpha is the statistical significance level, and is usually between 0.01 and 0.1")
  }
  if(is.null(period)){
    stop("Period must be set to the number of data points in a single period")
  }

  ############## -- Main analysis: Perform C-H-ESD -- #################
  # -- Step 1: Decompose data. This will return two more components: trend and cycle
  if(use_decomp){
    x_cf <- mFilter::cffilter(x)
    #med_t <- trunc(median(x_cf$trend))
    med_t <- trunc(median(x))
    sign_n <- sign(x_cf$trend - med_t)
    sign_n[which(sign_n == 0)] <-1
    # add the absolute values of the cycle component to the absolute values of the centered trend component. The signs are then added again
    x_2 <- as.vector(trunc(abs(x - med_t) + abs(x_cf$cycle)) * sign_n)
  } else {
    x_2 <- as.vector(x - median(x))
  }

  anomaly_direction = switch(direction,
                             "pos" = data.frame(one_tail=TRUE, upper_tail=TRUE), # upper-tail only (positive going anomalies)
                             "neg" = data.frame(one_tail=TRUE, upper_tail=FALSE), # lower-tail only (negative going anomalies)
                             "both" = data.frame(one_tail=FALSE, upper_tail=TRUE)) # Both tails. Tail direction is not actually used.


  n <- length(x_2)
  data_det <- data.frame(index = idNOzero, values = x_2, or_values = x)
  # Maximum number of outliers that C-H-ESD can detect (e.g. 49% of data)
  max_outliers <- trunc(n*max_anoms)
  func_ma <- match.fun(median)
  func_sigma <- match.fun(mad)
  R_idx <- 1L:max_outliers
  num_anoms <- 0L
  one_tail <- anomaly_direction$one_tail
  upper_tail <- anomaly_direction$upper_tail
  # Compute test statistic until r=max_outliers values have been
  # removed from the sample.
  for (i in 1L:max_outliers){
    if(verbose) message(paste(i,"/", max_outliers,"completed"))

    if(one_tail){
      if(upper_tail){
        ares <- data_det[[2L]] - func_ma(data_det[[2L]])
      } else {
        ares <- func_ma(data_det[[2L]]) - data_det[[2L]]
      }
    } else {
      ares = abs(data_det[[2L]] - func_ma(data_det[[2L]]))
    }

    # protect against constant time series
    data_sigma <- func_sigma(data_det[[3L]])
    # the standard deviation has to be calculated from the orginal
    # distribution because otherwise it would be affected too much
    # by the cycle component
    if(data_sigma == 0)
      break

    ares <- ares/data_sigma
    R <- max(ares)

    temp_max_idx <- which(ares == R)[1L]

    R_idx[i] <- data_det[[1L]][temp_max_idx]

    data_det <- data_det[-which(data_det[[1L]] == R_idx[i]), ]

    ## Compute critical value.
    if(one_tail){
      p <- 1 - alpha/(n-i+1)
    } else {
      p <- 1 - alpha/(2*(n-i+1))
    }

    t <- qt(p,(n-i-1L))
    lam <- t*(n-i) / sqrt((n-i-1+t**2)*(n-i+1))

    if(R > lam)
      num_anoms <- i
  }

  if(num_anoms > 0) {
    R_idx <- R_idx[1L:num_anoms]
    all_data <- data.frame(index = idNOzero, anoms = x)
    anoms_data <- subset(all_data, (all_data[[1]] %in% R_idx))
  } else {
    anoms_data <- NULL
  }
  return (list(anoms = anoms_data, num_obs = n))
}


flow_rate_bin_a <- function(x, second_fraction = 0.1, timeCh = timeCh,
                            timestep = timestep){

  xx <- exprs(x)[, timeCh]
  idx <- c(1:nrow(x))

  endsec <- ceiling(timestep * max(xx))  # total seconds of the experiment

  lenx <- length(xx)  # num of time ticks

  tbins <- seq(0, endsec/timestep, by = as.numeric(second_fraction)/timestep)  # time bins
  secbin <- seq(0, endsec, by = as.numeric(second_fraction))  # bin expressed in seconds
  minbin <- round(secbin/60, 3)  # bin expressed in minutes
  nrBins <- length(tbins) - 1
  tbCounts <- c(0, hist(xx, tbins, plot = FALSE)$counts)  # number of events per time bin
  expEv <- lenx/(nrBins)  ##median(tbCounts) # expected number of events per bin
  binID <- do.call(c, mapply(rep, x = 1:length(tbCounts), times = tbCounts,
                             SIMPLIFY = FALSE))

  if (length(idx) != length(binID))
    stop("length of cell ID not equal length of bin ID")

  timeFlowData <- list(frequencies = cbind(tbins, minbin, secbin, tbCounts),
                       cellBinID = data.frame(cellID = idx, binID = binID),
                       info = data.frame(second_fraction = second_fraction,
                                         expFrequency = expEv, bins = nrBins))
  return(timeFlowData)
}


# # Detection of anomalies in the flow rate using the algorithm
# # implemented in the package AnomalyDetection.
flow_rate_check_a <- function(x, FlowRateData, ...) {

  fr_frequences <- FlowRateData$frequencies
  fr_cellBinID <- FlowRateData$cellBinID
  second_fraction <- FlowRateData$info["second_fraction"]


  if (length(unique(fr_frequences[, 2])) == 1) {
    fr_autoqc <- NULL
  } else {
    fr_autoqc <- anomaly_detection(fr_frequences[, "tbCounts"], ...)
  }

  if (is.null(fr_autoqc) || is.null(fr_autoqc$anoms)) {
    badPerc <- 0
    newx <- x
    goodCellIDs <- fr_cellBinID$cellID
    badCellIDs <- NULL
  } else {
    goodCellIDs <- fr_cellBinID$cellID[!(fr_cellBinID$binID %in% fr_autoqc$anoms$index)]
    badCellIDs <- setdiff(fr_cellBinID$cellID, goodCellIDs)
    badPerc <- round(1 - (length(goodCellIDs)/nrow(fr_cellBinID)), 4)
    params <- parameters(x)
    keyval <- keyword(x)
    sub_exprs <- exprs(x)
    sub_exprs <- sub_exprs[goodCellIDs, ]
    newx <- flowFrame(exprs = sub_exprs, parameters = params, description = keyval)
  }
  cat(paste0(100 * badPerc, "% of anomalous cells detected in the flow rate check. \n"))
  return(list(anoms = fr_autoqc$anoms, frequencies = fr_frequences,
              FRnewFCS = newx,
              goodCellIDs = goodCellIDs, badCellIDs = badCellIDs,
              res_fr_QC = data.frame(second_fraction = second_fraction,
                                     num_obs = fr_autoqc$num_obs, badPerc = badPerc)))
}


# # Plot frequency values for a list y, containing the outputs from
# # the function flow_rate_check_a
flow_rate_plot_a <- function(FlowRateQC) {

  second_fraction <- FlowRateQC$res_fr_QC$second_fraction
  num_obs = FlowRateQC$res_fr_QC$num_obs
  frequencies = as.data.frame(FlowRateQC$frequencies)
  anoms = as.data.frame(FlowRateQC$anoms)
  anoms_points = as.data.frame(cbind(sec_anom = frequencies$secbin[anoms$index], count_anom = anoms$anoms))


  xgraph <- ggplot(frequencies, aes_string(x="secbin", y="tbCounts")) +
    theme_bw() + theme(panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       text=element_text(size = 14)) + geom_line(colour = "red" )
  xgraph <- xgraph + labs(x= "Seconds", y= paste0("Number of events per 1/",
                                                  1  /second_fraction, " of a second"), title= "Flow Rate")

  # Add anoms to the plot as circles.
  if(!is.null(anoms_points)){
    xgraph <- xgraph + geom_point(data=anoms_points, aes_string(x= "sec_anom", y= "count_anom"), color = "green4", size = 3, shape = 1)
  }

  return(xgraph)
}

# # Retrieves the number of events for each FCS file and creates
# # a bar plot.
# #
flow_set_qc_a <- function(set){
  if (!is(set, "flowSet"))
    stop("'set' needs to be of class 'flowSet'")

  cells <- as.numeric(fsApply(set, nrow))
  cells[cells == 1] <- NA
  samples <- factor(sampleNames(set), levels = sampleNames(set))
  cnframe <- data.frame(sampleName = samples , cellNumber = cells)
  return(cnframe)
}

# produce a bar plot
flow_set_plot_a <- function(N_cell_set, area){

  ggplot(N_cell_set, aes_string(x="sampleName", y = "cellNumber")) +
    geom_bar(stat = "identity",fill= area) + theme_classic() +
    theme( legend.position="none", axis.title.x = element_blank(),
           axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

# # The events on the upper margin and the outlier in the negative
# # range of values are detected and removed.
# #
flow_margin_check_a <- function(x,  ChannelExclude = NULL,
                                side = "both", neg_values = 1) {

  teCh <- grep("Time|time|TIME|Event|event|EVENT", colnames(x), value = TRUE)
  parms <- setdiff(colnames(x), teCh)

  if (!is.null(ChannelExclude)) {
    ChannelExclude_COMP <- grep(paste(ChannelExclude, collapse="|"),
                                colnames(x), value = TRUE)
    parms <- setdiff(parms, ChannelExclude_COMP)
  }

  scatter_parms <- grep("FSC|SSC", parms, value = TRUE)

  xx <- c(1:nrow(x))
  yy <- x@exprs[, parms, drop=F]
  range <- range(x)
  lenx <- length(xx)

  ## lower check
  if ((side == "lower" || side == "both") && neg_values == 1) {
    out_neg_range <- apply(yy, 2, function(x) {
      neg <- which(x < 0)
      # Zscores <- (0.6745*(x[neg] + median(x[neg])))/mad(x[neg]) ## it
      # calculates the Zscore outneg <- neg[which(Zscores < -3.5)]
      min_value <- -3.5 * mad(x[neg]) / 0.6745 + median(x[neg])  # -3.5 is the default threshold
      if (is.na(min_value)) {
        min(x) - 1
      } else {
        min_value
      }
    })
  }

  # n. bad cells for each channel
  if ((side == "lower" || side == "both") && neg_values == 1) {
    neg_bad_len <- sapply(parms, function(x) length(xx[yy[, x] <= out_neg_range[x]]))
  }
  if ((side == "lower" || side == "both") && neg_values == 2) {
    neg_bad_len <- sapply(parms, function(x) length(xx[yy[, x] <= range[1, x]]))
  }
  if (side == "upper" || side == "both") {
    pos_bad_len <- sapply(parms, function(x) length(xx[yy[, x] >= range[2, x]]))
  }

  # badcellIDs
  if ((side == "lower" || side == "both") && neg_values == 1) {
    lowID <- do.call(c, lapply(parms, function(ch) {
      xx[yy[, ch] <= out_neg_range[ch]]
    }))
    if(length(scatter_parms) != 0){   ### check for values less than 0 in scatter parameters
      minSc <- apply(yy[,scatter_parms], 1, function(x){
        min(x)
      })
      low_scatter_ID <- which(minSc < 0)
      lowID <- c(lowID, low_scatter_ID)
    }
  }
  if ((side == "lower" || side == "both") && neg_values == 2) {
    lowID <- do.call(c, lapply(parms, function(ch) {
      xx[yy[, ch] <= range[1, ch]]
    }))
  }
  if (side == "upper" || side == "both") {
    upID <- do.call(c, lapply(parms, function(ch) {
      xx[yy[, ch] >= range[2, ch]]
    }))
  }

  if (side == "lower") {
    summary_bad_cells <- data.frame(lower_range = c(neg_bad_len,
                                                    total_SUM = length(lowID), total_UNIQUE = length(unique(lowID))))
    bad_lowerIDs <- unique(lowID)
    bad_upperIDs <- NULL
    badCellIDs <- unique(lowID)
  } else if (side == "upper") {
    summary_bad_cells <- data.frame(upper_range = c(pos_bad_len,
                                                    total_SUM = length(upID), total_UNIQUE = length(unique(upID))))
    bad_lowerIDs <- NULL
    bad_upperIDs <- unique(upID)
    badCellIDs <- unique(upID)
  } else {
    summary_bad_cells <- data.frame(lower_range = c(neg_bad_len,
                                                    total_SUM = length(lowID), total_UNIQUE = length(unique(lowID))),
                                    upper_range = c(pos_bad_len,
                                                    total_SUM = length(upID), total_UNIQUE = length(unique(upID))))
    bad_lowerIDs <- unique(lowID)
    bad_upperIDs <- unique(upID)
    badCellIDs <- unique(c(lowID,upID))
  }

  goodCellIDs <- setdiff(xx, badCellIDs)
  badPerc <- round(length(badCellIDs)/lenx, 4)

  cat(paste0(100 * badPerc, "% of anomalous cells detected in the dynamic range check. \n"))

  params <- parameters(x)
  keyval <- keyword(x)
  sub_exprs <- exprs(x)
  sub_exprs <- sub_exprs[goodCellIDs, ]
  newx <- flowFrame(exprs = sub_exprs, parameters = params,
                    description = keyval)

  return(list(FMnewFCS = newx, goodCellIDs = goodCellIDs,
              bad_lowerIDs = bad_lowerIDs, bad_upperIDs = bad_upperIDs,
              margin_events = summary_bad_cells, badPerc = badPerc,
              events = lenx))
}


###  graph showing where the anomalies mostly happened
flow_margin_plot_a <- function(FlowMarginData, binSize = 500) {

  tot_events <- FlowMarginData$events
  bad_lowerIDs <- FlowMarginData$bad_lowerIDs
  bad_upperIDs <- FlowMarginData$bad_upperIDs

  if (missing(binSize) || is.null(binSize) || is.na(binSize))
    binSize <- 500
  nrBins <- floor(tot_events/binSize)

  cf <- c(rep(1:nrBins, each = binSize), rep(nrBins + 1, tot_events - nrBins * binSize))
  tmpx <- split(1:tot_events, cf)

  if(length(bad_lowerIDs) != 0 && length(bad_upperIDs) != 0){
    lowline <- sapply(tmpx, function(x){
      length(which(bad_lowerIDs %in% x))
    })
    upline <- sapply(tmpx, function(x){
      length(which(bad_upperIDs %in% x))
    })
    ymax <- max(lowline, upline)
    plot(lowline, type ="l", col = "blue", bty ="n",
         ylim = c(0, ymax), xlab = "Bin ID",
         ylab = "Number of events removed", cex.lab=1 )
    lines(upline, col = "red")
    legend("top", c("Negative Outliers", "Upper Margin Events"), lty = 1,bty = "n", cex = 1,
           col = c("blue", "red"))
  }else if( length(bad_lowerIDs) != 0 && length(bad_upperIDs) == 0){
    lowline <- sapply(tmpx, function(x){
      length(which(bad_lowerIDs %in% x))
    })
    plot(lowline, type ="l", col = "blue", bty ="n", xlab = "Bin ID",
         ylab = "Number of events removed", cex.lab=1 )
    legend("top", c("Negative Outliers"), lty = 1,bty = "n", cex = 1,
           col = "blue")
  }else if( length(bad_lowerIDs) == 0 && length(bad_upperIDs) != 0){
    upline <- sapply(tmpx, function(x){
      length(which(bad_upperIDs %in% x))
    })
    plot(upline, type ="l", col = "red", bty ="n", xlab = "Bin ID",
         ylab = "Number of events removed", cex.lab=1 )
    legend("top", c("Upper Margin Events"), lty = 1,bty = "n", cex = 1,
           col = "red")
  }
}

flow_signal_bin_a <- function(x, channels = NULL, binSize = 500,
                              timeCh = timeCh, timestep = timestep, TimeChCheck = TimeChCheck) {

  ## some sanity checking
  if (!is(x, "flowFrame"))
    stop("'x' needs to be of class 'flowFrame'")

  if (is.null(channels) || missing(channels) || is.na(channels)) {
    parms <- setdiff(colnames(x), timeCh)
  } else {
    if (!all(channels %in% colnames(x)))
      stop("Invalid channel(s)")
    parms <- channels
  }

  ### Retriving time and expression info
  exp <- exprs(x)
  if (!is.null(TimeChCheck)) {
    timex <- seq(from = 0, length.out = nrow(x), by = 0.1)
  }else{
    timex <- exp[, timeCh]
  }
  yy <- exp[, parms]  # channels data
  idx <- c(1:nrow(x))
  seconds <- timex * timestep
  lenSec <- length(seconds)  # num of time ticks
  uniSeconds <- unique(seconds)  # num of unique time tick
  nrBins <- floor(lenSec/binSize)  # num of bins

  cf <- c(rep(1:nrBins, each = binSize), rep(nrBins + 1, lenSec - nrBins * binSize))  # id bins
  stopifnot(length(cf) == lenSec)
  tmpx <- split(seconds, cf)
  xx2 <- sapply(tmpx, mean)  # mean of each time bin  (x axis)
  yy2 <- as.matrix(ddply(as.data.frame(yy), .(cf), colwise(median)))[, -1]

  return(list(exprsBin = cbind(timeSec = xx2, yy2), cellBinID = data.frame(cellID = idx, binID = cf),
              bins = length(unique(cf)), binSize = binSize))
}

# Detection of shifts in the median intensity signal detected
# by the laser of the flow cytometry over time
#' @importFrom changepoint cpt.meanvar
flow_signal_check_a <- function(x, FlowSignalData, ChannelExclude = NULL,
                                pen_valueFS = pen_valueFS, maxSegmentFS = maxSegmentFS, outlier_remove = FALSE) {

  fs_cellBinID <- FlowSignalData$cellBinID
  fs_res <- FlowSignalData$exprsBin
  ### log transformation.
  # fs_res[which(fs_res <= 1 & fs_res >= -1)] <- 0
  # fs_res[which(fs_res > 1)] <- log(fs_res[which(fs_res > 1)])
  # fs_res[which(fs_res < -1)] <- -log(abs(fs_res[which(fs_res < -1)]))

  teCh <- grep("Time|time|TIME|Event|event|EVENT", colnames(fs_res), value = TRUE)
  parms <- setdiff(colnames(fs_res), teCh)

  ##### scale and sum the value of each channel and find the outliers
  scale_sign <- apply(fs_res[, parms],2, scale)
  sum_sign <- apply(abs(scale_sign),1, sum)
  outup_tr <- (+3.5 * mad(sum_sign) + (0.6745 * median(sum_sign)))/0.6745
  FS_out <- which(sum_sign >outup_tr)

  #### Remove channel from the changepoint analysis
  if (!is.null(ChannelExclude)) {
    ChannelExclude_COMP <- grep(paste(ChannelExclude, collapse="|"),
                                colnames(fs_res), value = TRUE)
    #  cat(paste0("The channels whose signal acquisition will not be checked are: ",
    #   paste(ChannelExclude_COMP, collapse = ", "), ". \n"))
    parms <- setdiff(parms, ChannelExclude_COMP)
  }

  if(outlier_remove){
    ##transform outliers to median values
    if( length(FS_out) != 0 ){
      fs_res_adj <- apply(fs_res[, parms], 2, function(x) {
        med <- median(x)
        x[FS_out] <- med
        return(x)
      })
      badPerc_out <- round((length(FS_out)/nrow(fs_res)),4)
      # cat(paste0(badPerc_out * 100, "% of outliers found in channels' signal. \n"))
    }else{
      fs_res_adj <- fs_res[,parms]
      #  cat("0% of outliers found in channels' signal. \n")
      badPerc_out <- 0
    }

    cpt_res <- suppressWarnings(changepoint::cpt.meanvar(t(fs_res_adj),
                                            pen.value = pen_valueFS, Q = maxSegmentFS,
                                            penalty =  "Manual" , test.stat = "Normal",
                                            method = "BinSeg", param.estimates = FALSE))
  }else{

    cpt_res <- suppressWarnings(changepoint::cpt.meanvar(t(fs_res[, parms]),
                                            pen.value = pen_valueFS, Q = maxSegmentFS,
                                            penalty =  "Manual" , test.stat = "Normal",
                                            method = "BinSeg", param.estimates = FALSE))
    badPerc_out <- 0
  }

  list_seg <- lapply(1:length(cpt_res), function(x) {
    # it retrieves the changepoints that has been detected for each channel
    cpt_res[[x]]@cpts
  })
  names(list_seg) <- parms
  list_seg <- as.list(list_seg)

  list_cpt <- union(1, sort(unique(unlist(list_seg[parms]))))  # retrieve and order all the changepoints
  diff_cpt <- sapply(2:length(list_cpt), function(n) {
    # calculate the difference between each segment
    list_cpt[n] - list_cpt[n - 1]
  })
  max_diff <- which(diff_cpt == max(diff_cpt))
  max_seg <- c(list_cpt[max_diff], list_cpt[max_diff + 1] )  # selecting the biggest segment

  list_seg <- lapply(list_seg, function(x) setdiff(x, nrow(fs_res)))

  len_cpt <- sapply(list_seg, length)
  nam_cpt <- gsub("<|>","",names(len_cpt))
  nozero_cpt <- as.numeric(which(len_cpt != 0))
  zero_cpt <- as.numeric(which(len_cpt == 0))
  if(length(nozero_cpt) == 0){
    ch_no_cpt <- nam_cpt[zero_cpt]
    tab_cpt <- NULL
  }else{
    # cat(paste("Changepoint(s) detected in the channels: ",
    # paste(names(len_cpt[nozero_cpt]), collapse = ", "), sep = ""), fill = TRUE)
    ch_cpt <- list_seg[nozero_cpt]
    ch_no_cpt <- nam_cpt[zero_cpt]

    max_n_cpt <- max(sapply(ch_cpt, length))
    tab_cpt <- ldply(ch_cpt, function(x) c(x, rep(NA, max_n_cpt - length(x))),
                     .id = NULL)
    rownames(tab_cpt) <- nam_cpt[nozero_cpt]
    tab_cpt <- as.matrix(tab_cpt)
    tab_cpt[which(is.na(tab_cpt))] <- ""
    colnames(tab_cpt) <- 1:length(tab_cpt[1, ])
  }
  # percentage bad cell detected with the changepoint method
  badPerc_cp <- round(1 - ((max_seg[2] - max_seg[1])/(length(fs_res[, 1]) - 1)),4)

  cat(paste0(100 * badPerc_cp, "% of anomalous cells detected in signal acquisition check. \n"))

  # retrieve ID of good cells
  if(outlier_remove){
    fs_cellBinID <- fs_cellBinID[which(!fs_cellBinID[, 2] %in% FS_out),]
  }
  goodCellIDs <- fs_cellBinID[which(fs_cellBinID[, 2] >= max_seg[1] &
                                      fs_cellBinID[, 2] <= max_seg[2]), 1]

  badPerc_tot <- round(1 - length(goodCellIDs)/nrow(fs_cellBinID),4)

  params <- parameters(x)
  keyval <- keyword(x)
  sub_exprs <- exprs(x)
  sub_exprs <- sub_exprs[goodCellIDs, ]  ## check if the Id Correspond!
  newx <- flowFrame(exprs = sub_exprs, parameters = params, description = keyval)

  return(list(FSnewFCS = newx, exprsBin = FlowSignalData$exprsBin, Perc_bad_cells = data.frame(badPerc_tot,badPerc_cp, badPerc_out),
              goodCellIDs = goodCellIDs, tab_cpt = tab_cpt, ch_no_cpt =ch_no_cpt,
              segm = max_seg, FS_out = FS_out, outlier_remove = outlier_remove))
}


# Plot the flourescence intensity for each channel of a flowFrame
# over time, highlighting the wider segment that do not show shifts
# of the median intensity
# @param exprsBin give the exprsBin object from FlowSignalData
# @param segm give the segm object from FlowSignalQC
# @param FS_out give the FS_out object from FlowSignalQC
flow_signal_plot_a <- function(FlowSignalQC) {

  exprsBin <- FlowSignalQC$exprsBin
  segm <- FlowSignalQC$segm
  FS_out <- FlowSignalQC$FS_out
  outlier_remove <- FlowSignalQC$outlier_remove

  binID <- 1:nrow(exprsBin)
  teCh <- grep("Time|time|Event|event", colnames(exprsBin), value = TRUE)
  parms <- setdiff(colnames(exprsBin), teCh)
  dataORIG <- exprsBin[, parms]  # first channel is time

  if(length(FS_out) != 0){
    data <- apply(dataORIG, 2, function(x){
      overMAX <- FS_out[x[FS_out] > max(x[-FS_out])]
      overMIN <- FS_out[x[FS_out] < min(x[-FS_out])]
      x[overMAX] <- max(x[-FS_out])
      x[overMIN] <- min(x[-FS_out])
      return(x)
    })
    data <- as.data.frame(data)
  }else{
    data <- as.data.frame(dataORIG)
  }
  data$binID <- binID
  longdata <- melt(data, id.vars = "binID", variable.name = "marker",
                   value.name = "value")
  FS_graph <- ggplot(longdata, aes_string(x = "binID", y = "value", col = "marker"),
                     environment = environment()) + labs(x = "Bin ID",
                                                         y = "Median Intensity value") +
    facet_grid(marker ~ ., scales = "free") + theme_bw() +
    theme(strip.text.y = element_text(angle = 0,
                                      hjust = 1), axis.text.y = element_text(size = 6),
          legend.position = "none") +
    scale_x_continuous(breaks= pretty_breaks(n =10)) +
    scale_y_continuous(breaks= pretty_breaks(n =3)) +
    geom_rect(aes(xmin = segm[1], xmax = segm[2], ymin = -Inf,
                  ymax = Inf), fill = "khaki1", linetype = 0) + geom_line()
  # Add anoms to the plot as circles.
  if(outlier_remove){
    longdata_out <- melt(data[FS_out,], id.vars = "binID",
                         variable.name = "marker",value.name = "value")
    FS_graph <- FS_graph + geom_point(data=longdata_out, aes_string(x= "binID",
                                                                    y= "value"), color = "green4", size = 2, shape = 1)
  }
  return(FS_graph)
}


#### Tests ####

library(shiny)
library(shinydashboard)
library(flowWorkspace)

if (interactive()){

  ui <- dashboardPage(
    dashboardHeader(title = "flowAI"),
    sidebar = dashboardSidebar(disable = TRUE),
    body = dashboardBody(
      CleanUI("module")
    )
  )

  server <- function(input, output, session) {
    rval <- reactiveValues()
    observe({
      utils::data("GvHD", package = "flowCore")
      rval$gating_set <- GatingSet(GvHD)
    })
    res <- callModule(Clean, "module", rval = rval)
  }

  shinyApp(ui, server)

}


#### script ####

library(flowAI)
data("Bcells")


time_chnl <- "Time"
idx <- 1
side <- "both"
ChannelExclude <- time_chnl
time_step <- 0.01
second_fraction <- 0.1 # time interval used for averaging data
binSize <- 500

ordFCS <- ord_fcs_time(Bcells[[idx]], time_chnl)
print(colnames(ordFCS))

### margin ####
res_margin <- flow_margin_check_a(ordFCS, 
                                  ChannelExclude = ChannelExclude, 
                                  side = side, 
                                  neg_values = 1)
flow_margin_plot_a(res_margin, binSize = binSize)

### flow-rate ####

flow_rate_data <- flow_rate_bin_a(x = ordFCS, 
                                  second_fraction = second_fraction, 
                                  timeCh = time_chnl, 
                                  timestep = time_step)
res_flow_rate_auto <- flow_rate_check_a(x = ordFCS, FlowRateData = flow_rate_data, 
                                   alpha = 0.01, 
                                   use_decomp = TRUE,
                                   direction='neg')
flow_rate_plot_a(res_flow_rate_auto)

lowerTimeCut <- min(flow_rate_data$frequencies[,3]) - 0.1
UpperTimeCut <- max(flow_rate_data$frequencies[,3]) + 0.1
lowerRateThres <- min(flow_rate_data$frequencies[,4]) - 10
upperRateThres <- max(flow_rate_data$frequencies[,4]) + 10

res_flow_rate_qc <- flow_rate_check(flowRateData = flow_rate_data, 
                                        lowerRateThres = lowerRateThres, 
                                        upperRateThres = upperRateThres,
                                        lowerTimeCut = lowerTimeCut, 
                                        UpperTimeCut = UpperTimeCut)
flow_rate_plot(flowRateData = flow_rate_data, 
               lowerRateThres = lowerRateThres, 
               upperRateThres = upperRateThres,
               lowerTimeCut = lowerTimeCut, 
               UpperTimeCut = UpperTimeCut)                              

### signal ####

flow_signal_data <- flow_signal_bin(ordFCS, channels = c("FSC-A", "SSC-A"), 
                                    binSize=binSize, 
                                    timeCh = time_chnl, 
                                    timestep = time_step, 
                                    TimeChCheck = NULL)
