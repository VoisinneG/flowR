### Helper functions for Clean module######################################################

# Guess which channel captures time in a exprs, flowFrame or flowset
# findTimeChannel <- function(xx) {
#   time <- grep("^Time$", colnames(xx), value = TRUE, ignore.case = TRUE)[1]
#   if (is.na(time)) {
#     if (is(xx, "flowSet") || is(xx, "ncdfFlowList"))
#       xx <- flowCore::exprs(xx[[1]]) else if (is(xx, "flowFrame"))
#         xx <- flowCore::exprs(xx)
#       cont <- apply(xx, 2, function(y) all(sign(diff(y)) >= 0))
#       time <- names(which(cont))
#   }
#   if (!length(time) || length(time) > 1)
#     time <- NULL
#   return(time)
# }

# # Check if the Fcs file is ordered according to time otherwise it order it.
# ord_fcs_time <- function(x, timeCh= "Time"){
#   xord <- order(flowCore::exprs(x)[, timeCh])
# 
#   if( !identical(xord, 1:nrow(x)) ){
#     warning(paste0("Expression data in the file ", basename(flowCore::keyword(x)$FILENAME),
#                    " were not originally ordered by time."))
#     params <- parameters(x)
#     keyval <- flowCore::keyword(x)
#     sub_exprs <- flowCore::exprs(x)[xord, ]
#     newx <- flowFrame(exprs = sub_exprs, parameters = params,
#                       description = keyval)
#     return(newx)
#   }else{
#     return(x)
#   }
# }


# flow_rate_bin <- function(x, second_fraction = 0.1, timeCh = "Time", timestep = 0.1){
# 
#   xx <- flowCore::exprs(x)[, timeCh]
#   idx <- c(1:nrow(x))
#   lenx <- length(xx)                                                  # num of time ticks
# 
#   endsec <- ceiling(timestep * max(xx))                 # total seconds of the experiment
#   tbins <- seq(0, endsec/timestep, by = second_fraction/timestep)             # time bins
#   secbin <- seq(0, endsec, by = second_fraction)               # bin expressed in seconds
#   minbin <- round(secbin/60, 3)                                # bin expressed in minutes
#   tbCounts <- c(0, hist(xx, tbins, plot = FALSE)$counts)  # number of events per time bin
# 
#   nrBins <- length(tbins) - 1
#   expEv <- lenx/(nrBins)           # median(tbCounts) # expected number of events per bin
#   binID <- do.call(c, mapply(rep, x = 1:length(tbCounts), times = tbCounts,
#                              SIMPLIFY = FALSE))
# 
#   if (length(idx) != length(binID))
#     stop("length of cell ID not equal length of bin ID")
# 
#   flowRateData <- list(frequencies = cbind(tbins, minbin, secbin, tbCounts),
#                        cellBinID = data.frame(cellID = idx, binID = binID),
#                        info = data.frame(second_fraction = second_fraction,
#                                          expFrequency = expEv, bins = nrBins))
#   return(flowRateData)
# }

#' @import ggplot2
# flow_rate_plot <- function(flowRateData, lowerRateThres, upperRateThres,
#                            lowerTimeCut, UpperTimeCut) {
#   
#   frequencies <- as.data.frame(flowRateData$frequencies)
#   second_fraction <- flowRateData$info$second_fraction
#   short_period <- quantile(frequencies$secbin, seq(0,1, length.out = 4))
#   long_period <- quantile(frequencies$secbin, seq(0,1, length.out = 3*10 + 1))
#   
#   ## flow rate graph(frg)
#   frg <- ggplot(frequencies, aes_string(x="secbin", y="tbCounts")) + geom_line(colour="red") +
#     theme_bw() + theme(panel.grid.major = element_blank(),
#                        panel.grid.minor = element_blank(),
#                        text=element_text(size = 14))
#   frg <- frg + geom_vline(xintercept=short_period, color="gray60") +
#     geom_vline(xintercept=long_period, linetype = 2, color="gray60")
#   
#   frg <- frg + geom_hline(yintercept=c(lowerRateThres, upperRateThres), color="blue",
#                           linetype = "longdash", size = 1.2, show.legend = TRUE)
#   frg <- frg + geom_vline(xintercept=c(lowerTimeCut, UpperTimeCut), color="blue",
#                           linetype = "longdash", size = 1.2, show.legend = TRUE)
#   
#   frg <- frg + labs(x= "Seconds", y= paste0("Number of cells per 1/", 1/second_fraction, " second"),
#                     title= "Flow Rate Plot")
#   
#   return(frg)
# }

# flow_rate_check <- function(flowRateData, lowerRateThres, upperRateThres,
#                             lowerTimeCut, UpperTimeCut) {
#   
#   frequencies <- as.data.frame(flowRateData$frequencies)
#   cellBinID <- flowRateData$cellBinID
#   
#   goodcell_x <- which(frequencies$secbin < UpperTimeCut & frequencies$secbin > lowerTimeCut)
#   goodcell_y <- which(frequencies$tbCounts < upperRateThres & frequencies$tbCounts > lowerRateThres)
#   
#   flowRateQC <- cellBinID$cellID[ cellBinID$binID %in% intersect(goodcell_x, goodcell_y) ]
#   cat("flow rate high Q events: ", length(flowRateQC), "\n")
#   return(flowRateQC)
# }
# 

#' @import plyr ddply .
#' @import flowAI
# A flowFrame object is splitted in bins with equal number of events
# and for each bin the median is calculated.
#
# @param x: a flowFrame object
# @param channels: channel names for the selected markers
# @param binSize: the size of bin
# flow_signal_bin <- function(x, channels = NULL, binSize = 500,
#                             timeCh = timeCh, timestep = timestep, TimeChCheck = TimeChCheck) {
#   
#   ## some sanity checking
#   if (!is(x, "flowFrame"))
#     stop("'x' needs to be of class 'flowFrame'")
#   
#   if (is.null(channels) || missing(channels) || is.na(channels)) {
#     parms <- setdiff(colnames(x), timeCh)
#   } else {
#     if (!all(channels %in% colnames(x)))
#       stop("Invalid channel(s)")
#     parms <- channels
#   }
  
  ### Retriving time and expression info
#   exp <- exprs(x)
#   if (!is.null(TimeChCheck)) {
#     timex <- seq(from = 0, length.out = nrow(x), by = 0.1)
#   }else{
#     timex <- exp[, timeCh]
#   }
#   yy <- exp[, parms]  # channels data
#   idx <- c(1:nrow(x))
#   seconds <- timex * timestep
#   lenSec <- length(seconds)  # num of time ticks
#   uniSeconds <- unique(seconds)  # num of unique time tick
#   nrBins <- floor(lenSec/binSize)  # num of bins
#   
#   cf <- c(rep(1:nrBins, each = binSize), rep(nrBins + 1, lenSec - nrBins * binSize))  # id bins
#   stopifnot(length(cf) == lenSec)
#   tmpx <- split(seconds, cf)
#   xx2 <- sapply(tmpx, mean)  # mean of each time bin  (x axis)
#   yy2 <- as.matrix(plyr::ddply(as.data.frame(yy), plyr::.(cf), colwise(median)))[, -1]
#   
#   return(list(exprsBin = cbind(timeSec = xx2, yy2), cellBinID = data.frame(cellID = idx, binID = cf),
#               bins = length(unique(cf)), binSize = binSize))
# }



#'@import ggplot2
# flow_signal_plot <- function(flowSignalData, lowerBinThres, upperBinThres) {
#   
#   exprsBin <- flowSignalData$exprsBin
#   
#   binID <- 1:nrow(exprsBin)
#   teCh <- grep("Time|time|Event|event", colnames(exprsBin), value = T)
#   parms <- setdiff(colnames(exprsBin), teCh)
#   dataORIG <- exprsBin[, parms]     # first channel is time
#   data <- as.data.frame(dataORIG)
#   data$binID <- binID
#   
#   longdata <- melt(data, id.vars = "binID", variable.name = "marker", value.name = "value")
#   FS_graph <- ggplot(longdata, aes(x = binID, y = value, col = marker), environment = environment()) +
#     geom_line() + facet_grid(marker ~ ., scales = "free") +
#     labs(x = "Bin ID", y = "Median Intensity value") + theme_bw() +
#     theme(strip.text.y = element_text(angle = 0, hjust = 1), axis.text = element_text(size = 10),
#           axis.title = element_text(size = 15), legend.position = "none") +
#     scale_x_continuous(breaks= scales::pretty_breaks(n = 10)) +
#     scale_y_continuous(breaks= scales::pretty_breaks(n = 3)) +
#     geom_rect(aes(xmin = lowerBinThres, xmax = upperBinThres, ymin = -Inf, ymax = Inf), fill = "orange", linetype = 0, alpha = 0.005)
#   
#   return(FS_graph)
# }


# flow_signal_check <- function(flowSignalData, lowerBinThres, upperBinThres) {
#   
#   exprsBin <- flowSignalData$exprsBin
#   cellBinID <- flowSignalData$cellBinID
#   
#   goodBins <- cellBinID$binID < upperBinThres & cellBinID$binID > lowerBinThres
#   FlowSignalQC <- cellBinID$cellID[goodBins]
#   
#   cat("flow signal check: ", length(FlowSignalQC), "\n")
#   return(FlowSignalQC)
# }

# # The events on the upper margin and the outlier in the negative
# # range of values are detected and removed.
# #
# flow_margin_check <- function(x,  ChannelExclude = NULL,
#                               side = "both", neg_values = 1) {
# 
#   teCh <- grep("Time|time|TIME|Event|event|EVENT", colnames(x), value = TRUE)
#   parms <- setdiff(colnames(x), teCh)
# 
#   if (!is.null(ChannelExclude)) {
#     ChannelExclude_COMP <- grep(paste(ChannelExclude, collapse="|"),
#                                 colnames(x), value = TRUE)
#     parms <- setdiff(parms, ChannelExclude_COMP)
#   }
# 
#   scatter_parms <- grep("FSC|SSC", parms, value = TRUE)
# 
#   xx <- c(1:nrow(x))
#   yy <- x@exprs[, parms, drop=F]
#   range <- range(x)
#   lenx <- length(xx)
# 
# ## lower check
# if ((side == "lower" || side == "both") && neg_values == 1) {
#   out_neg_range <- apply(yy, 2, function(x) {
#     neg <- which(x < 0)
#     # Zscores <- (0.6745*(x[neg] + median(x[neg])))/mad(x[neg]) ## it
#     # calculates the Zscore outneg <- neg[which(Zscores < -3.5)]
#     min_value <- -3.5 * mad(x[neg]) / 0.6745 + median(x[neg])  # -3.5 is the default threshold
#     if (is.na(min_value)) {
#       min(x) - 1
#     } else {
#       min_value
#     }
#   })
# }
# 
# # n. bad cells for each channel
# if ((side == "lower" || side == "both") && neg_values == 1) {
#   neg_bad_len <- sapply(parms, function(x) length(xx[yy[, x] <= out_neg_range[x]]))
# }
# if ((side == "lower" || side == "both") && neg_values == 2) {
#   neg_bad_len <- sapply(parms, function(x) length(xx[yy[, x] <= range[1, x]]))
# }
# if (side == "upper" || side == "both") {
#   pos_bad_len <- sapply(parms, function(x) length(xx[yy[, x] >= range[2, x]]))
# }
# 
# # badcellIDs
# if ((side == "lower" || side == "both") && neg_values == 1) {
#   lowID <- do.call(c, lapply(parms, function(ch) {
#     xx[yy[, ch] <= out_neg_range[ch]]
#   }))
#   if(length(scatter_parms) != 0){   ### check for values less than 0 in scatter parameters
#     minSc <- apply(yy[,scatter_parms], 1, function(x){
#       min(x)
#     })
#     low_scatter_ID <- which(minSc < 0)
#     lowID <- c(lowID, low_scatter_ID)
#   }
# }
# if ((side == "lower" || side == "both") && neg_values == 2) {
#   lowID <- do.call(c, lapply(parms, function(ch) {
#     xx[yy[, ch] <= range[1, ch]]
#   }))
# }
# if (side == "upper" || side == "both") {
#   upID <- do.call(c, lapply(parms, function(ch) {
#     xx[yy[, ch] >= range[2, ch]]
#   }))
# }
# 
# if (side == "lower") {
#   summary_bad_cells <- data.frame(lower_range = c(neg_bad_len,
#                                                   total_SUM = length(lowID), total_UNIQUE = length(unique(lowID))))
#   bad_lowerIDs <- unique(lowID)
#   bad_upperIDs <- NULL
#   badCellIDs <- unique(lowID)
# } else if (side == "upper") {
#   summary_bad_cells <- data.frame(upper_range = c(pos_bad_len,
#                                                   total_SUM = length(upID), total_UNIQUE = length(unique(upID))))
#   bad_lowerIDs <- NULL
#   bad_upperIDs <- unique(upID)
#   badCellIDs <- unique(upID)
# } else {
#   summary_bad_cells <- data.frame(lower_range = c(neg_bad_len,
#                                                   total_SUM = length(lowID), total_UNIQUE = length(unique(lowID))),
#                                   upper_range = c(pos_bad_len,
#                                                   total_SUM = length(upID), total_UNIQUE = length(unique(upID))))
#   bad_lowerIDs <- unique(lowID)
#   bad_upperIDs <- unique(upID)
#   badCellIDs <- unique(c(lowID,upID))
# }
# 
# goodCellIDs <- setdiff(xx, badCellIDs)
# badPerc <- round(length(badCellIDs)/lenx, 4)
# 
# cat(paste0(100 * badPerc, "% of anomalous cells detected in the dynamic range check. \n"))
# 
# params <- parameters(x)
# keyval <- keyword(x)
# sub_exprs <- exprs(x)
# sub_exprs <- sub_exprs[goodCellIDs, ]
# newx <- flowFrame(exprs = sub_exprs, parameters = params,
#                   description = keyval)
# 
# return(list(FMnewFCS = newx, goodCellIDs = goodCellIDs,
#             bad_lowerIDs = bad_lowerIDs, bad_upperIDs = bad_upperIDs,
#             margin_events = summary_bad_cells, badPerc = badPerc,
#             events = lenx))
# }
# 

###  graph showing where the anomalies mostly happened
# flow_margin_plot <- function(FlowMarginData, binSize = 500) {
#   
#   tot_events <- FlowMarginData$events
#   bad_lowerIDs <- FlowMarginData$bad_lowerIDs
#   bad_upperIDs <- FlowMarginData$bad_upperIDs
#   
#   if (missing(binSize) || is.null(binSize) || is.na(binSize))
#     binSize <- 500
#   nrBins <- floor(tot_events/binSize)
#   
#   cf <- c(rep(1:nrBins, each = binSize), rep(nrBins + 1, tot_events - nrBins * binSize))
#   tmpx <- split(1:tot_events, cf)
#   
#   if(length(bad_lowerIDs) != 0 && length(bad_upperIDs) != 0){
#     lowline <- sapply(tmpx, function(x){
#       length(which(bad_lowerIDs %in% x))
#     })
#     upline <- sapply(tmpx, function(x){
#       length(which(bad_upperIDs %in% x))
#     })
#     ymax <- max(lowline, upline)
#     plot(lowline, type ="l", col = "blue", bty ="n",
#          ylim = c(0, ymax), xlab = "segment ID",
#          ylab = "Number of cells removed" )
#     lines(upline, col = "red")
#     legend("top", c("Negative Outliers", "Upper Margine Events"), lty = 1,bty = "n", cex = 0.7,
#            col = c("blue", "red"))
#   }else if( length(bad_lowerIDs) != 0 && length(bad_upperIDs) == 0){
#     lowline <- sapply(tmpx, function(x){
#       length(which(bad_lowerIDs %in% x))
#     })
#     plot(lowline, type ="l", col = "blue", bty ="n", xlab = "segment ID",
#          ylab = "Number of cells removed" )
#     legend("top", c("Negative Outliers"), lty = 1,bty = "n", cex = 0.7,
#            col = "blue")
#   }else if( length(bad_lowerIDs) == 0 && length(bad_upperIDs) != 0){
#     upline <- sapply(tmpx, function(x){
#       length(which(bad_upperIDs %in% x))
#     })
#     plot(upline, type ="l", col = "red", bty ="n", xlab = "segment ID",
#          ylab = "Number of cells removed" )
#     legend("top", c("Upper Margine Events"), lty = 1,bty = "n", cex = 0.7,
#            col = "red")
#   }
# }


## create new flowFrame with the parameter indicating good and bad cells
# addQC <- function(QCvector, sub_exprs, params, keyval){
#   
#   rs <- attr(sub_exprs, "ranges")
#   rs <- c(rs, rs[1])
#   sub_exprs <- cbind(sub_exprs, QCvector)
#   attr(sub_exprs, "ranges") <- rs
#   NN <- as.numeric(keyval["$PAR"]) + 1
#   names(dimnames(sub_exprs)[[2]]) <- sprintf("$P%sN", 1:NN)
#   pnr <- paste0("$P", NN, "R")
#   pnb <- paste0("$P", NN, "B")
#   pne <- paste0("$P", NN, "E")
#   pnn <- paste0("$P", NN, "N")
#   pns <- paste0("$P", NN, "S")
#   flowCorePnRmax <- paste0("flowCore_$P", NN, "Rmax")
#   flowCorePnRmin <- paste0("flowCore_$P", NN, "Rmin")
#   o <- params@data
#   o[length(o[,1]) + 1,] <- c("QC", "bad > 10,000", as.numeric(keyval$`$P1R`), 0, 20000)
#   rownames(o)[length(o[,1])] <- paste("$P", NN, sep = "")
#   
#   outFCS <- new("flowFrame", exprs=sub_exprs, parameters=new("AnnotatedDataFrame",o), description=keyval)
#   flowCore::description(outFCS)[pnr] <- max(20000, flowCore::description(outFCS)$`$P1R`)
#   flowCore::description(outFCS)[pnb] <- flowCore::description(outFCS)$`$P1B`
#   flowCore::description(outFCS)[pne] <- "0,0"
#   flowCore::description(outFCS)[pnn] <- "QC"
#   flowCore::description(outFCS)[pns] <- "bad > 10,000"
#   flowCore::description(outFCS)$`$PAR` <- NN
#   flowCore::description(outFCS)[flowCorePnRmax] <- 20000
#   flowCore::description(outFCS)[flowCorePnRmin] <- 0
#   outFCS
# }

# ### Add function for automatic cleaning #####################################

#' @importFrom mFilter cffilter
# anomaly_detection = function(x, max_anoms=0.49, direction='both', alpha=0.01, use_decomp = TRUE, period=1, verbose = FALSE){
#   
#   idNOzero <- which(x != 0)
#   x <- x[idNOzero]
#   
#   # Check for supported inputs types
#   if(is.vector(x) && is.numeric(x)) {
#     x <- ts(x, frequency = period)
#   } else if(is.ts(x)) {
#   } else {
#     stop("data must be a time series object or a vector that holds numeric values.")
#   }
#   
#   # Handle NAs
#   if (length(rle(is.na(c(NA,x,NA)))$values)>3){
#     stop("Data contains non-leading NAs. We suggest replacing NAs with interpolated values (see na.approx in Zoo package).")
#   } else {
#     x <- na.omit(x)
#   }
#   
#   # Sanity check all input parameterss
#   if(max_anoms > .49){
#     stop(paste("max_anoms must be less than 50% of the data points (max_anoms =", round(max_anoms*length(x), 0), " data_points =", length(x),")."))
#   }
#   if(!direction %in% c('pos', 'neg', 'both')){
#     stop("direction options are: pos | neg | both.")
#   }
#   if(!(0.01 <= alpha || alpha <= 0.1)){
#     print("Warning: alpha is the statistical significance level, and is usually between 0.01 and 0.1")
#   }
#   if(is.null(period)){
#     stop("Period must be set to the number of data points in a single period")
#   }
#   
#   ############## -- Main analysis: Perform C-H-ESD -- #################
#   # -- Step 1: Decompose data. This will return two more components: trend and cycle
#   if(use_decomp){
#     x_cf <- mFilter::cffilter(x)
#     #med_t <- trunc(median(x_cf$trend))
#     med_t <- trunc(median(x))
#     sign_n <- sign(x_cf$trend - med_t)
#     sign_n[which(sign_n == 0)] <-1
#     # add the absolute values of the cycle component to the absolute values of the centered trend component. The signs are then added again
#     x_2 <- as.vector(trunc(abs(x - med_t) + abs(x_cf$cycle)) * sign_n)
#   } else {
#     x_2 <- as.vector(x - median(x))
#   }
#   
#   anomaly_direction = switch(direction,
#                              "pos" = data.frame(one_tail=TRUE, upper_tail=TRUE), # upper-tail only (positive going anomalies)
#                              "neg" = data.frame(one_tail=TRUE, upper_tail=FALSE), # lower-tail only (negative going anomalies)
#                              "both" = data.frame(one_tail=FALSE, upper_tail=TRUE)) # Both tails. Tail direction is not actually used.
#   
#   
#   n <- length(x_2)
#   data_det <- data.frame(index = idNOzero, values = x_2, or_values = x)
#   # Maximum number of outliers that C-H-ESD can detect (e.g. 49% of data)
#   max_outliers <- trunc(n*max_anoms)
#   func_ma <- match.fun(median)
#   func_sigma <- match.fun(mad)
#   R_idx <- 1L:max_outliers
#   num_anoms <- 0L
#   one_tail <- anomaly_direction$one_tail
#   upper_tail <- anomaly_direction$upper_tail
#   # Compute test statistic until r=max_outliers values have been
#   # removed from the sample.
#   for (i in 1L:max_outliers){
#     if(verbose) message(paste(i,"/", max_outliers,"completed"))
#     
#     if(one_tail){
#       if(upper_tail){
#         ares <- data_det[[2L]] - func_ma(data_det[[2L]])
#       } else {
#         ares <- func_ma(data_det[[2L]]) - data_det[[2L]]
#       }
#     } else {
#       ares = abs(data_det[[2L]] - func_ma(data_det[[2L]]))
#     }
#     
#     # protect against constant time series
#     data_sigma <- func_sigma(data_det[[3L]])
#     # the standard deviation has to be calculated from the orginal
#     # distribution because otherwise it would be affected too much
#     # by the cycle component
#     if(data_sigma == 0)
#       break
#     
#     ares <- ares/data_sigma
#     R <- max(ares)
#     
#     temp_max_idx <- which(ares == R)[1L]
#     
#     R_idx[i] <- data_det[[1L]][temp_max_idx]
#     
#     data_det <- data_det[-which(data_det[[1L]] == R_idx[i]), ]
#     
#     ## Compute critical value.
#     if(one_tail){
#       p <- 1 - alpha/(n-i+1)
#     } else {
#       p <- 1 - alpha/(2*(n-i+1))
#     }
#     
#     t <- qt(p,(n-i-1L))
#     lam <- t*(n-i) / sqrt((n-i-1+t**2)*(n-i+1))
#     
#     if(R > lam)
#       num_anoms <- i
#   }
#   
#   if(num_anoms > 0) {
#     R_idx <- R_idx[1L:num_anoms]
#     all_data <- data.frame(index = idNOzero, anoms = x)
#     anoms_data <- subset(all_data, (all_data[[1]] %in% R_idx))
#   } else {
#     anoms_data <- NULL
#   }
#   return (list(anoms = anoms_data, num_obs = n))
# }


# # Detection of anomalies in the flow rate using the algorithm
# # implemented in the package AnomalyDetection.
# flow_rate_check_auto <- function(x, FlowRateData, ...) {
# 
#   fr_frequences <- FlowRateData$frequencies
#   fr_cellBinID <- FlowRateData$cellBinID
#   second_fraction <- FlowRateData$info["second_fraction"]
# 
# 
#   if (length(unique(fr_frequences[, 2])) == 1) {
#     fr_autoqc <- NULL
#   } else {
#     fr_autoqc <- anomaly_detection(fr_frequences[, "tbCounts"], ...)
#   }
# 
#   if (is.null(fr_autoqc) || is.null(fr_autoqc$anoms)) {
#     badPerc <- 0
#     newx <- x
#     goodCellIDs <- fr_cellBinID$cellID
#     badCellIDs <- NULL
#   } else {
#     goodCellIDs <- fr_cellBinID$cellID[!(fr_cellBinID$binID %in% fr_autoqc$anoms$index)]
#     badCellIDs <- setdiff(fr_cellBinID$cellID, goodCellIDs)
#     badPerc <- round(1 - (length(goodCellIDs)/nrow(fr_cellBinID)), 4)
#     params <- parameters(x)
#     keyval <- keyword(x)
#     sub_exprs <- exprs(x)
#     sub_exprs <- sub_exprs[goodCellIDs, ]
#     newx <- flowFrame(exprs = sub_exprs, parameters = params, description = keyval)
#   }
#   cat(paste0(100 * badPerc, "% of anomalous cells detected in the flow rate check. \n"))
#   return(list(anoms = fr_autoqc$anoms, frequencies = fr_frequences,
#               FRnewFCS = newx,
#               goodCellIDs = goodCellIDs, badCellIDs = badCellIDs,
#               res_fr_QC = data.frame(second_fraction = second_fraction,
#                                      num_obs = fr_autoqc$num_obs, badPerc = badPerc)))
# }
# 

# # # Plot frequency values for a list y, containing the outputs from
# # # the function flow_rate_check_auto
# flow_rate_plot_auto <- function(FlowRateQC) {
#   
#   second_fraction <- FlowRateQC$res_fr_QC$second_fraction
#   num_obs = FlowRateQC$res_fr_QC$num_obs
#   frequencies = as.data.frame(FlowRateQC$frequencies)
#   anoms = as.data.frame(FlowRateQC$anoms)
#   anoms_points = as.data.frame(cbind(sec_anom = frequencies$secbin[anoms$index], count_anom = anoms$anoms))
#   
#   
#   xgraph <- ggplot(frequencies, aes_string(x="secbin", y="tbCounts")) +
#     theme_bw() + theme(panel.grid.major = element_blank(),
#                        panel.grid.minor = element_blank(),
#                        text=element_text(size = 14)) + geom_line(colour = "red" )
#   xgraph <- xgraph + labs(x= "Seconds", y= paste0("Number of events per 1/",
#                                                   1  /second_fraction, " of a second"), title= "Flow Rate")
#   
#   # Add anoms to the plot as circles.
#   if(!is.null(anoms_points)){
#     xgraph <- xgraph + geom_point(data=anoms_points, aes_string(x= "sec_anom", y= "count_anom"), color = "green4", size = 3, shape = 1)
#   }
#   
#   return(xgraph)
# }

# # Retrieves the number of events for each FCS file and creates
# # a bar plot.
# # #
# flow_set_qc <- function(set){
#   if (!is(set, "flowSet"))
#     stop("'set' needs to be of class 'flowSet'")
#   
#   cells <- as.numeric(fsApply(set, nrow))
#   cells[cells == 1] <- NA
#   samples <- factor(sampleNames(set), levels = sampleNames(set))
#   cnframe <- data.frame(sampleName = samples , cellNumber = cells)
#   return(cnframe)
# }
#' 
#' # produce a bar plot
#' #' @import ggplot2
#' flow_set_plot <- function(N_cell_set, area){
#'   
#'   ggplot(N_cell_set, aes_string(x="sampleName", y = "cellNumber")) +
#'     geom_bar(stat = "identity",fill= area) + theme_classic() +
#'     theme( legend.position="none", axis.title.x = element_blank(),
#'            axis.text.x = element_text(angle = 45, hjust = 1)
#'     )
#' }

#' @importFrom changepoint cpt.meanvar
# Detection of shifts in the median intensity signal detected
# by the laser of the flow cytometry over time
# flow_signal_check_auto <- function(x, FlowSignalData, ChannelExclude = NULL,
#                               pen_valueFS = pen_valueFS, maxSegmentFS = maxSegmentFS, outlier_remove = FALSE) {
# 
#   fs_cellBinID <- FlowSignalData$cellBinID
#   fs_res <- FlowSignalData$exprsBin
#   ### log transformation.
#   # fs_res[which(fs_res <= 1 & fs_res >= -1)] <- 0
#   # fs_res[which(fs_res > 1)] <- log(fs_res[which(fs_res > 1)])
#   # fs_res[which(fs_res < -1)] <- -log(abs(fs_res[which(fs_res < -1)]))
# 
#   teCh <- grep("Time|time|TIME|Event|event|EVENT", colnames(fs_res), value = TRUE)
#   parms <- setdiff(colnames(fs_res), teCh)
# 
#   ##### scale and sum the value of each channel and find the outliers
#   scale_sign <- apply(fs_res[, parms],2, scale)
#   sum_sign <- apply(abs(scale_sign),1, sum)
#   outup_tr <- (+3.5 * mad(sum_sign) + (0.6745 * median(sum_sign)))/0.6745
#   FS_out <- which(sum_sign >outup_tr)
# 
#   #### Remove channel from the changepoint analysis
#   if (!is.null(ChannelExclude)) {
#     ChannelExclude_COMP <- grep(paste(ChannelExclude, collapse="|"),
#                                 colnames(fs_res), value = TRUE)
#     #  cat(paste0("The channels whose signal acquisition will not be checked are: ",
#     #   paste(ChannelExclude_COMP, collapse = ", "), ". \n"))
#     parms <- setdiff(parms, ChannelExclude_COMP)
#   }
# 
#   if(outlier_remove){
#     ##transform outliers to median values
#     if( length(FS_out) != 0 ){
#       fs_res_adj <- apply(fs_res[, parms], 2, function(x) {
#         med <- median(x)
#         x[FS_out] <- med
#         return(x)
#       })
#       badPerc_out <- round((length(FS_out)/nrow(fs_res)),4)
#       # cat(paste0(badPerc_out * 100, "% of outliers found in channels' signal. \n"))
#     }else{
#       fs_res_adj <- fs_res[,parms]
#       #  cat("0% of outliers found in channels' signal. \n")
#       badPerc_out <- 0
#     }
# 
#     cpt_res <- suppressWarnings(changepoint::cpt.meanvar(t(fs_res_adj),
#                                             pen.value = pen_valueFS, Q = maxSegmentFS,
#                                             penalty =  "Manual" , test.stat = "Normal",
#                                             method = "BinSeg", param.estimates = FALSE))
#   }else{
# 
#     cpt_res <- suppressWarnings(changepoint::cpt.meanvar(t(fs_res[, parms]),
#                                             pen.value = pen_valueFS, Q = maxSegmentFS,
#                                             penalty =  "Manual" , test.stat = "Normal",
#                                             method = "BinSeg", param.estimates = FALSE))
#     badPerc_out <- 0
#   }
# 
#   list_seg <- lapply(1:length(cpt_res), function(x) {
#     # it retrieves the changepoints that has been detected for each channel
#     cpt_res[[x]]@cpts
#   })
#   names(list_seg) <- parms
#   list_seg <- as.list(list_seg)
# 
#   list_cpt <- union(1, sort(unique(unlist(list_seg[parms]))))  # retrieve and order all the changepoints
#   diff_cpt <- sapply(2:length(list_cpt), function(n) {
#     # calculate the difference between each segment
#     list_cpt[n] - list_cpt[n - 1]
#   })
#   max_diff <- which(diff_cpt == max(diff_cpt))
#   max_seg <- c(list_cpt[max_diff], list_cpt[max_diff + 1] )  # selecting the biggest segment
# 
#   list_seg <- lapply(list_seg, function(x) setdiff(x, nrow(fs_res)))
# 
#   len_cpt <- sapply(list_seg, length)
#   nam_cpt <- gsub("<|>","",names(len_cpt))
#   nozero_cpt <- as.numeric(which(len_cpt != 0))
#   zero_cpt <- as.numeric(which(len_cpt == 0))
#   if(length(nozero_cpt) == 0){
#     ch_no_cpt <- nam_cpt[zero_cpt]
#     tab_cpt <- NULL
#   }else{
#     # cat(paste("Changepoint(s) detected in the channels: ",
#     # paste(names(len_cpt[nozero_cpt]), collapse = ", "), sep = ""), fill = TRUE)
#     ch_cpt <- list_seg[nozero_cpt]
#     ch_no_cpt <- nam_cpt[zero_cpt]
# 
#     max_n_cpt <- max(sapply(ch_cpt, length))
#     tab_cpt <- ldply(ch_cpt, function(x) c(x, rep(NA, max_n_cpt - length(x))),
#                      .id = NULL)
#     rownames(tab_cpt) <- nam_cpt[nozero_cpt]
#     tab_cpt <- as.matrix(tab_cpt)
#     tab_cpt[which(is.na(tab_cpt))] <- ""
#     colnames(tab_cpt) <- 1:length(tab_cpt[1, ])
#   }
#   # percentage bad cell detected with the changepoint method
#   badPerc_cp <- round(1 - ((max_seg[2] - max_seg[1])/(length(fs_res[, 1]) - 1)),4)
#   
#   cat(paste0(100 * badPerc_cp, "% of anomalous cells detected in signal acquisition check. \n"))
#   
#   # retrieve ID of good cells
#   if(outlier_remove){
#     fs_cellBinID <- fs_cellBinID[which(!fs_cellBinID[, 2] %in% FS_out),]
#   }
#   goodCellIDs <- fs_cellBinID[which(fs_cellBinID[, 2] >= max_seg[1] &
#                                       fs_cellBinID[, 2] <= max_seg[2]), 1]
#   
#   badPerc_tot <- round(1 - length(goodCellIDs)/nrow(fs_cellBinID),4)
#   
#   params <- parameters(x)
#   keyval <- keyword(x)
#   sub_exprs <- exprs(x)
#   sub_exprs <- sub_exprs[goodCellIDs, ]  ## check if the Id Correspond!
#   newx <- flowFrame(exprs = sub_exprs, parameters = params, description = keyval)
#   
#   return(list(FSnewFCS = newx, exprsBin = FlowSignalData$exprsBin, Perc_bad_cells = data.frame(badPerc_tot,badPerc_cp, badPerc_out),
#               goodCellIDs = goodCellIDs, tab_cpt = tab_cpt, ch_no_cpt =ch_no_cpt,
#               segm = max_seg, FS_out = FS_out, outlier_remove = outlier_remove))
# }



# Plot the flourescence intensity for each channel of a flowFrame
# over time, highlighting the wider segment that do not show shifts
# of the median intensity
#' @param exprsBin give the exprsBin object from FlowSignalData
#' @param segm give the segm object from FlowSignalQC
#' @param FS_out give the FS_out object from FlowSignalQC
#' @import ggplot2
# flow_signal_plot_auto <- function(FlowSignalQC) {
#   
#   exprsBin <- FlowSignalQC$exprsBin
#   segm <- FlowSignalQC$segm
#   FS_out <- FlowSignalQC$FS_out
#   outlier_remove <- FlowSignalQC$outlier_remove
#   
#   binID <- 1:nrow(exprsBin)
#   teCh <- grep("Time|time|Event|event", colnames(exprsBin), value = TRUE)
#   parms <- setdiff(colnames(exprsBin), teCh)
#   dataORIG <- exprsBin[, parms]  # first channel is time
#   
#   if(length(FS_out) != 0){
#     data <- apply(dataORIG, 2, function(x){
#       overMAX <- FS_out[x[FS_out] > max(x[-FS_out])]
#       overMIN <- FS_out[x[FS_out] < min(x[-FS_out])]
#       x[overMAX] <- max(x[-FS_out])
#       x[overMIN] <- min(x[-FS_out])
#       return(x)
#     })
#     data <- as.data.frame(data)
#   }else{
#     data <- as.data.frame(dataORIG)
#   }
#   data$binID <- binID
#   longdata <- melt(data, id.vars = "binID", variable.name = "marker",
#                    value.name = "value")
#   FS_graph <- ggplot(longdata, aes_string(x = "binID", y = "value", col = "marker"),
#                      environment = environment()) + labs(x = "Bin ID",
#                                                          y = "Median Intensity value") +
#     facet_grid(marker ~ ., scales = "free") + theme_bw() +
#     theme(strip.text.y = element_text(angle = 0,
#                                       hjust = 1), axis.text.y = element_text(size = 6),
#           legend.position = "none") +
#     scale_x_continuous(breaks= scales::pretty_breaks(n =10)) +
#     scale_y_continuous(breaks= scales::pretty_breaks(n =3)) +
#     geom_rect(aes(xmin = segm[1], xmax = segm[2], ymin = -Inf,
#                   ymax = Inf), fill = "khaki1", linetype = 0) + geom_line()
#   # Add anoms to the plot as circles.
#   if(outlier_remove){
#     longdata_out <- melt(data[FS_out,], id.vars = "binID",
#                          variable.name = "marker",value.name = "value")
#     FS_graph <- FS_graph + geom_point(data=longdata_out, aes_string(x= "binID",
#                                                                     y= "value"), color = "green4", size = 2, shape = 1)
#   }
#   return(FS_graph)
# }