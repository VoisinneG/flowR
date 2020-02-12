## Directly adapted from the flowAI shiny app

library(plyr)

### UI ########################################################################################

#' @param id shiny id
#' @import shiny
CleanUI<-function(id){
  
  ns <- NS(id)
  
  fluidPage(
    titlePanel("Interactive Quality Control for Flow Cytometry Data"),

    fluidRow(
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
  )
}

### server ####################################################################################
#' @importFrom flowCore exprs keyword
#' @importFrom flowWorkspace pData
#' @importFrom scales pretty_breaks
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
    fs <- rval$gating_set@data
    idx <- which(pData(fs)$name == input$sample)
    set <- rval$gating_set@data[[idx]]
    
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

#### Tests ####
#
# library(shiny)
# library(shinydashboard)
# library(flowWorkspace)
# 
if (interactive()){

  ui <- dashboardPage(
    dashboardHeader(title = "Clean"),
    sidebar = dashboardSidebar(disable = TRUE),
    body = dashboardBody(
      CleanUI("module")
    )
  )

  server <- function(input, output, session) {
    rval <- reactiveValues()
    observe({
      #utils::data("GvHD", package = "flowCore")
      #rval$gating_set <- GatingSet(GvHD)
      utils::data("Bcells", package = "flowAI")
      gs <- GatingSet(Bcells)
      rval$gating_set <- gs
    })
    res <- callModule(Clean, "module", rval = rval)
  }

  shinyApp(ui, server)

}
