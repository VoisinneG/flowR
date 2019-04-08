#BiocManager::install("flowCore")
#BiocManager::install("flowViz")
library(openCyto)
library(ggcyto)
library(ggridges)

library(shiny)
library(shinyBS)
library(shinydashboard)
library(DT)
library(tools)
#library(ggplot2)
#library(ggrepel)
#library(data.table)
#library(plotly)
#library(heatmaply)
#library(RColorBrewer)
#library(viridis)
##########################################################################################################
#User interface ----

options(shiny.maxRequestSize = 100*1024^2)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "Import_tab",
            fluidRow(
              #dataTableOutput("files_table"),
              column(width = 6,
                     box(title = "Import",
                         width = NULL, height = NULL,
                         fileInput(inputId = "files", 
                                   label = "Choose files", 
                                   multiple = TRUE)
                     ),
                     box(title = "Input files",
                         width = NULL, height = NULL,
                         dataTableOutput("files_table"),
                         br(),
                         br(),
                         numericInput("N_lines", label = "Number of cells to import", value = 1000),
                         actionButton("load", label = "Load selected files")
                     )
              ),
              column(width = 6,
                     tabBox(title = "Info",
                            width = NULL, height = NULL,
                            tabPanel(title = "summary",
                                     verbatimTextOutput("message")
                            ),
                            tabPanel(title = "parameters",
                                     dataTableOutput("parameters_table")
                            ),
                            tabPanel(title = "metadata",
                                     dataTableOutput("pData")
                            )
                     )
              )
            )
      
    ),
    tabItem(tabName = "Plot_tab",
            fluidRow(
              column(width = 4,
                     box(title = "Plot options",
                         width = NULL, height = NULL,
                         selectInput("plot_type", 
                                     label = "Select plot type", 
                                     choices = c("Histogram", "2D plot"), 
                                     selected = "2D plot"),
                         selectInput("xvar", label = "x variable", choices = NULL, selected = NULL),
                         selectInput("yvar", label = "y variable", choices = NULL, selected = NULL),
                         numericInput("bin_number", label = "number of bins", value = 30)
                     ),
                     box(title = "Select samples",
                         width = NULL, height = NULL,
                         dataTableOutput("files_selection_table")
                     ),
                     box(title = "Add Gate",
                         width = NULL, height = NULL,
                         selectInput("gate_type", label = "Type of gate", choices = c("rectangular", "polygonal"), selected = "rectangular"),
                         dataTableOutput("files_selection_table")
                     ),
                     box(title = "Message",
                         width = NULL, height = NULL,
                         verbatimTextOutput("message")
                         )
              ),
              column(width = 8,
                     box(title = "Plot",
                         width = NULL, height = NULL,
                         plotOutput("plotGate", 
                                    brush = brushOpts(id = "plot_brush"),
                                    click = clickOpts(id = "plot_click"),
                                    dblclick = "plot_dblclick")
                     )
              )
              
            )
    )
    
    
  )
)



sidebar <- dashboardSidebar(
  sidebarMenu(id = "menu",
              menuItem("Import",
                       tabName = "Import_tab", 
                       startExpanded = FALSE,
                       icon = icon("check-circle")
                       
              ),
              menuItem("Plot",
                       tabName = "Plot_tab", 
                       startExpanded = FALSE,
                       icon = icon("check-circle")
                       
              )
              
  )
  
  
)

ui <- dashboardPage(
  dashboardHeader(title = "flowShine"),
  sidebar,
  body
)

##########################################################################################################
# Server logic ----
server <- function(session, input, output) {
  
  rval <- reactiveValues(df_files = NULL, 
                         flow_set = NULL,
                         gating_set = NULL)
  
  # load raw data
  observe({
    
    validate(
      need(input$files$datapath, "Please select a file to import")
    )
    
    rval$df_files <- input$files
    
  })
  
  # observe({
  #   file_type <- file_ext(rval$df_files$datapath[1])
  #   
  #   cat(file_type)
  #   
  #   validate(
  #     need(file_type %in% c("fcs", "FCS"), "Please select fcs files")
  #   )
  # })

  observeEvent(input$load, {
    validate(
      need(length(input$files_table_rows_selected)>0, "Please select a file to load")
    )
    
    cat(rval$df_files$datapath[1])
    cat(dirname(rval$df_files$datapath[1]))
    
    if(length(rval$df_files$datapath)>0){
      
      rval$flow_set <- read.flowSet( rval$df_files$datapath[input$files_table_rows_selected], 
                                     which.lines = input$N_lines)
      
      phenoData(rval$flow_set)$name <- rval$df_files$name[input$files_table_rows_selected]
      
      rval$gating_set <- GatingSet(rval$flow_set)
    }
    
  })
  
  
  
  
  observe({
    if(!is.null(rval$flow_set)){
      
      plot_var <- parameters(rval$flow_set[[1]])$desc
      plot_var <- plot_var[!is.na(plot_var)]
      plot_var_x <- parameters(rval$flow_set[[1]])$name
      names(plot_var) <- NULL
      names(plot_var_x) <- NULL

      #cat(plot_var)
      if(length(plot_var)>1){
        updateSelectInput(session, "xvar", choices = plot_var, selected = plot_var[1])
        updateSelectInput(session, "yvar", choices = plot_var, selected = plot_var[2])
      }
      
    }
    
  })
  
  
  ##########################################################################################################
  # Output
  output$files_table <- renderDataTable({
    validate(
      need(rval$df_files, "Please select a file to import")
    )
    rval$df_files[ ,c("name", "size")]
  })
  
  output$message <- renderText({
    paste("You have loaded", length(rval$flow_set), "items")
  })
  
  output$parameters_table <- renderDataTable({
    if(!is.null(rval$flow_set)){
      
      data.frame(name = parameters(rval$flow_set[[1]])$name, 
                 desc = parameters(rval$flow_set[[1]])$desc
                 )
                 
      
      
    }
  })
  
  output$pData <- renderDataTable({
    if(!is.null(rval$flow_set)){
      data.frame(name = pData(rval$flow_set)$name,
                 events = fsApply(rval$flow_set, function(x){description(x)[["$TOT"]]}))
    }
  })
  
  output$files_selection_table <- renderDataTable({
    if(!is.null(rval$flow_set)){
      data.frame(name = pData(rval$flow_set)$name )
    }
  })
  
  output$plotGate <- renderPlot({
    
    validate(
      need(input$files_selection_table_rows_selected, "Please select samples")
    )
    
    if(!is.null(rval$flow_set)){
      
      if(input$plot_type == "2D plot"){
        p <- ggcyto(rval$flow_set[input$files_selection_table_rows_selected], 
                    aes_string(x = input$xvar, y = input$yvar))
        p <- p + geom_hex(bins = input$bin_number)  +
          scale_x_log10() +
          scale_y_log10()
          #facet_wrap(~name)
            
        plot(p)
      }
      
      if(input$plot_type == "Histogram"){
        p <- ggcyto(rval$flow_set[input$files_selection_table_rows_selected], 
                    aes_string(x = input$xvar))
        p <- p + 
          geom_density_ridges(mapping= aes_string(y = "name", fill = "name"), alpha = 0.2) +
          #geom_density(mapping= aes_string(fill = "name"), alpha = 0.2,) +
          #geom_histogram(bins = input$bin_number)  +
          scale_x_log10() +
          facet_null()
          #guides(fill=guide_legend())
          #facet_wrap(~name)
        
        plot(p)
      }

    }
  })
  
}
  
shinyApp(ui, server)