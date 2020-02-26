# ### script ####
library(mFilter)
library(plyr)
library(changepoint)
library(flowWorkspace)
library(flowCore)
library(flowAI)

data("Bcells")
data("GvHD")

fs <- GvHD
fs <- Bcells

time_chnl <- "Time"
idx <- 1
side <- "both"
ChannelExclude <- time_chnl
time_step <- 0.01
second_fraction <- 0.11 # time interval used for averaging data
binSize <- 500



ordFCS <- ord_fcs_time(fs[[idx]], time_chnl)
# print(colnames(ordFCS))

### margin ####
res_margin <- flow_margin_check(ordFCS,
                                  ChannelExclude = ChannelExclude,
                                  side = "both",
                                  neg_values = 1)
flow_margin_plot(res_margin, binSize = binSize)


### flow-rate ####

flow_rate_data <- flow_rate_bin(x = ordFCS,
                                  second_fraction = second_fraction,
                                  timeCh = time_chnl,
                                  timestep = time_step)

res_flow_rate_auto <- flow_rate_check_auto(x = ordFCS, FlowRateData = flow_rate_data,
                                   alpha = 0.01,
                                   use_decomp = TRUE,
                                   direction= "neg")

flow_rate_plot_auto(res_flow_rate_auto)

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
pen_valueFS <- 500

flow_signal_data <- flow_signal_bin(ordFCS, 
                                    channels = NULL,
                                    binSize=binSize,
                                    timeCh = time_chnl,
                                    timestep = time_step,
                                    TimeChCheck = NULL)

res_signal_qc <-flow_signal_check_auto(ordFCS, flow_signal_data, ChannelExclude = NULL,
                       pen_valueFS = pen_valueFS, maxSegmentFS = 3, outlier_remove = FALSE )

# install.packages("mFilter") installer