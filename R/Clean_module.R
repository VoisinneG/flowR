#' @param id shiny id
#' @import shiny
#' @import DT
#' @import plotly
CleanUI<-function(id){
  
  ns <- NS(id)

  fluidRow(
    br(),
    column(4, 
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
                            label = "Analyze selected sample(s)"),
               
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
                   numericInput(ns("binSize"),
                                label = "Number of events per bin",
                                min = 0,
                                step = 100,
                                value = 500),
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
                                    plotOutput(ns("signal_acquisition_plot_output"))
                           )
                           
               )
           ),
           box(title = "Results",
               width = NULL, collapsed = F, collapsible = T,
               tabsetPanel(type = "pills",
                           tabPanel("Heatmap",
                                    br(),
                                    br(),
                                    simpleDisplayUI(ns("simple_display_module"))
                                    #plotlyOutput(ns("heatmap"), height = "auto")
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
#' @importFrom flowWorkspace pData
#' @importFrom scales pretty_breaks
#' @importFrom data.table setDT
#' @importFrom plotly renderPlotly
#' @importFrom heatmaply heatmaply
#' @import flowAI
Clean <- function(input, output, session, rval) {
  
  ### Call modules ###################################################################
  
  callModule(simpleDisplay, "simple_display_module", 
             plot_list = heatmap_plot, 
             params = reactiveValues(use_plotly = TRUE, width = 500, height = 500),
             save = FALSE)
  
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
  
  ## Set time channel ###############################################################
  observe({
    chNames <- choices()$params$name
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
    
    withProgress(message = 'The data cleaning is running..', {
      for(i in  1:length(samples)){
        sample <- samples[i]
        
        ordFCS <- ord_fcs_time(set()[[sample]], timeCh = timeCh)
        
        dynamic_range[[sample]] <- flow_margin_check(x = ordFCS,
                                                     ChannelExclude = ChannelExclude,
                                                     side = side,
                                                     neg_values = 1)
        
        flowRateData <- do.call(flow_rate_bin, c(list(ordFCS), 
                                                 FR_bin_arg))
        
        flowRateQCList[[sample]] <- do.call(flow_rate_check_auto, 
                                           c(list(ordFCS, flowRateData), 
                                             FR_QC_arg))
        # signal acquisition process
        FlowSignalData <- do.call(flow_signal_bin, c(list(ordFCS), 
                                                     FS_bin_arg))
        FlowSignalQCList[[sample]] <- do.call(flow_signal_check_auto, 
                                              c(list(x = ordFCS, FlowSignalData = FlowSignalData), 
                                                FS_QC_arg))

        incProgress(1/i, detail = paste("(current sample : ", i, ")"))
      }
    })
    
    print(names(flowRateQCList))
    return(
      list(
        flowRateQCList = flowRateQCList,
        FlowSignalQCList = FlowSignalQCList,
        dynamic_range = dynamic_range
      )
    )
    
  })
  
  ### Build result table ###############################################################################
  
  res_table <- eventReactive(res(), {
    
    df <- NULL
    samples <- input$choice_sample_input
    for(i in  1:length(samples)){
      sample <- samples[i]
      Signal_acquisition <- res()$FlowSignalQCList[[sample]]$Perc_bad_cells$badPerc_cp*100
      Flow_rate <- res()$flowRateQCList[[sample]]$res_fr_QC$badPerc*100
      Dynamic_range <- res()$dynamic_range[[sample]]$badPerc*100
      df <- rbind(df, data.frame(Flow_rate, 
                                 Dynamic_range,
                                 Signal_acquisition )) 
      
    }
    rownames(df) <- input$choice_sample_input
    return(df)
  })
  
  ### Build heatmap #######################################################################################
  
  heatmap_plot <- reactive({
    heatmaply(res_table(),colors = viridis::magma(10), limits = c(0,100),
              Rowv = FALSE, Colv = FALSE)
  })
  
  # output$heatmap <- renderPlotly({
  #   heatmap_plot()
  # })
  
  ### Display messages on top of QC plots ##################################################################
  
  output$fr_message <- renderText({
    perc <- res()$flowRateQCList[[input$select_one_sample]]$res_fr_QC$badPerc*100
    paste(perc,"% of anomalous cells detected in the flow rate check")
  })
  
  output$dynamic_message <- renderText({
    perc <- res()$dynamic_range[[input$select_one_sample]]$badPerc*100
    paste(perc,"% of anomalous cells detected in the dynamic range check")
  })
  
  output$signal_message <- renderText({
    perc <- res()$FlowSignalQCList[[input$select_one_sample]]$Perc_bad_cells$badPerc_cp*100
    paste(perc,"% of anomalous cells detected in signal acquisition check")
  })
  
  ### Build QC plots #######################################################################################
  
  output$flow_rate_plot_output <- renderPlot({
    # print(flowRateQCList$sample[[flowCore::sampleNames(rval$gating_set)[1]]])
    flow_rate_plot_auto(res()$flowRateQCList[[input$select_one_sample]])
  })
  
  output$dynamic_plot_output <-renderPlot({
    flow_margin_plot(res()$dynamic_range[[input$select_one_sample]], binSize = input$binSize)
  })
  
  output$signal_acquisition_plot_output <- renderPlot({
    # print(FlowSignalQCList$sample[[1]])
    flow_signal_plot_auto(res()$FlowSignalQCList[[input$select_one_sample]])
    
  })
  
  output$result_output <- DT::renderDataTable({
    res_table()
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
#       utils::data("Bcells", package = "flowAI")
#       rval$gating_set <- flowWorkspace::GatingSet(Bcells)
#     })
#     res <- callModule(Clean, "module", rval = rval)
#   }
# 
#   shinyApp(ui, server)
# 
# }


