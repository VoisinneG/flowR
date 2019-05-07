#BiocManager::install("flowCore")
#BiocManager::install("flowViz")
library(openCyto)
#library(flowWorkspace)
library(ggcyto)
library(ggridges)
library(gridExtra)

library(shiny)
#library(shinyBS)
library(shinydashboard)
library(DT)
library(tools)
library(utils)
library(sp)

library(viridis)
library(igraph)
library(scales)
#library(ggplot2)
#library(ggrepel)
#library(data.table)
library(plotly)
library(heatmaply)
library(ggsignif)
#library(RColorBrewer)
#library(viridis)


options(repos = BiocManager::repositories(), shiny.maxRequestSize = 300*1024^2)
getOption("repos")

source("./R/flowShine_plot.R")

flowJo_biexp_inverse_trans <- function (..., n = 6, equal.space = FALSE){
  trans <- flowJoTrans(..., inverse = TRUE)
  inv <- flowJoTrans(...)
  flow_trans(name = "flowJo_biexp_inverse", trans.fun = trans, inverse.fun = inv, 
             n = n, equal.space = equal.space)
}

asinh_transform <- function(b=5, inverse = FALSE){ 
  if(inverse){
    function(x){b*sinh(x)} 
  }else{
    function(x){asinh(x/b)} 
  }
}

asinh_trans <- function (..., n = 6, equal.space = FALSE){
  trans <- asinh_transform(...)
  inv <- asinh_transform(..., inverse = TRUE)
  flow_trans(name = "asinh", trans.fun = trans, inverse.fun = inv, 
             n = n, equal.space = equal.space)
}


transform_values <- function(x, scale, ...){
  xtrans <- switch(scale,
                   "identity" = identity_trans()$inverse(x),
                   "log10" = log10_trans()$inverse(x),
                   "flowJo_asinh" = flowJo_fasinh_trans()$inverse(x),
                   "asinh" = asinh_trans()$inverse(x),
                   "logicle" = logicle_trans()$inverse(x))

}



##########################################################################################################
#User interface ----

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
                         div(style = 'overflow-x: scroll', DT::dataTableOutput("files_table")),
                         br(),
                         selectizeInput("groups", "select groups", 
                                        choices = NULL, 
                                        selected = NULL,
                                        multiple = TRUE),
                         numericInput("N_lines", label = "Number of cells to import", value = 3000),
                         actionButton("load", label = "Load selected files")
                     ),
                     box(title = "summary",
                         width = NULL, height = NULL,
                         verbatimTextOutput("message")
                     )
              ),
              column(width = 6,
                     tabBox(title = "Meta",
                            width = NULL, height = NULL,
                            tabPanel(title = "Table",
                                     div(style = 'overflow-x: scroll', DT::dataTableOutput("pData"))
                                     
                                     
                            ),
                            tabPanel(title = "Keywords",
                                     selectizeInput("keyword", "select keywords", 
                                                    choices = NULL, 
                                                    selected = NULL,
                                                    multiple = TRUE),
                                     actionButton("append_keywords", label = "Add keywords"),
                                     br()
                                     
                                     
                                     
                            ),
                            tabPanel(title = "Import",
                                     fileInput("file_meta", "load metadata file", multiple = FALSE),
                                     selectInput("sep_meta", "column separator", choices = c("comma", "semi-column", "tab"), selected = "tab"),
                                     #checkboxInput("sep_meta", "Choose `;` as column separator", value = FALSE),
                                     div(style = 'overflow-x: scroll', DT::dataTableOutput("meta")),
                                     br(),
                                     actionButton("append_meta", label = "Add metadata"),
                                     actionButton("reset_meta", label = "Reset"),
                                     br()
                                     
                            )
                     )
              )
            )
      
    ),
    tabItem(tabName = "Trans_tab",
            fluidRow(
              
              column(width = 5,
                     tabBox(title = "Chanels",
                            width = NULL, height = NULL,
                            tabPanel(title = "Table",
                                     div(style = 'overflow-x: scroll', DT::dataTableOutput("parameters_table"))
                                     
                            ),
                            tabPanel(title = "transform",
                                     selectInput("trans", "transformation", 
                                                 choices = c("identity", "logicle", "asinh", "flowJo_asinh", "log"), 
                                                 selected = "identity"),
                                     conditionalPanel(condition = "input.trans == 'asinh'",
                                                      h5("Parameters"),
                                                      numericInput("base_asinh", label = "base", value = 1)
                                     ),
                                     conditionalPanel(condition = "input.trans == 'flowJo_asinh'",
                                                      h5("Parameters"),
                                                      numericInput("m", label = "m", value = 5),
                                                      numericInput("t", label = "t", value = 12000),
                                                      numericInput("a", label = "a", value = 0.7),
                                                      numericInput("length", label = "length", value = 256)
                                     ),
                                     conditionalPanel(condition = "input.trans == 'logicle'",
                                                      h5("Parameters"),
                                                      numericInput("w_logicle", label = "w", value = 0.5),
                                                      numericInput("t_logicle", label = "t", value = 262144),
                                                      numericInput("m_logicle", label = "m", value = 4.5),
                                                      numericInput("a_logicle", label = "a", value = 0)
                                     ),
                                     conditionalPanel(condition = "input.trans == 'log'",
                                                      h5("Parameters"),
                                                      numericInput("base_log", label = "base", value = 10)
                                     ),
                                     br(),
                                     actionButton("apply_transformation", label = "apply to selected variables"),
                                     br()
                            )#,
                            # tabPanel(title = "show",
                            #          selectInput("xvar_show", label = "variable", choices = NULL, selected = NULL),
                            #          verbatimTextOutput("message_transform")
                            # )
                     )
              ),
              column(width = 7,
                     tabBox(title = "Plot",
                            width = NULL, height = NULL,
                            tabPanel("Plot",
                                     plotOutput("plot_trans")
                                     
                            ),
                            tabPanel("Select",
                                     selectInput("sample_selected_trans", label = "Sample", choices = NULL, selected = NULL),
                                     actionButton("previous_frame_trans", "previous"),
                                     actionButton("next_frame_trans", "next"),
                                     br(),
                                     br(),
                                     selectizeInput("gate_trans", 
                                                    label = "subset", 
                                                    choices = "root", 
                                                    selected = "root",
                                                    multiple = FALSE),
                                     selectInput("xvar_trans", label = "x variable", choices = NULL, selected = NULL),
                                     selectInput("yvar_trans", label = "y variable", choices = NULL, selected = NULL)
                            ),
                            # tabPanel("Scale axis",
                            #          selectInput("x_scale", 
                            #                      label = "x scale", 
                            #                      choices = c("identity", "log10", "asinh", "logicle"), 
                            #                      selected = "identity"),
                            #          selectInput("y_scale", 
                            #                      label = "y scale", 
                            #                      choices = c("identity", "log10", "asinh", "logicle"), 
                            #                      selected = "identity")
                            #          #checkboxInput("freeze_limits", label = "freeze plot limits", value = TRUE)
                            #          
                            # ),
                            tabPanel("Options",
                                     selectInput("plot_type_trans", label = "plot type",
                                                 choices = c("hexagonal", "histogram", "dots", "contour"),
                                                 selected = "histogram"),
                                     #checkboxInput("apply_trans_trans", "apply tansformation", value = TRUE),
                                     checkboxInput("legend_trans", "show legend", value = FALSE),
                                     checkboxInput("norm_trans", "normalize (set max to 1)", value = TRUE),
                                     checkboxInput("smooth_trans", "smooth", value = FALSE),
                                     #checkboxInput("ridges", "ridges", value = FALSE),
                                     #checkboxInput("facet", "faceting", value = TRUE),
                                     # selectizeInput("facet_var", 
                                     #                multiple =TRUE,
                                     #                label = "facet variables", 
                                     #                choices = "name", 
                                     #                selected = NULL),
                                     # selectizeInput("group_var", 
                                     #                multiple =FALSE,
                                     #                label = "group variable", 
                                     #                choices = c("name","subset"), 
                                     #                selected = "subset"),
                                     # selectizeInput("yridges_var", 
                                     #                multiple =FALSE,
                                     #                label = "y ridges variable", 
                                     #                choices = c("name","subset"), 
                                     #                selected = "subset"),
                                     selectInput("color_var_trans", "color variable",
                                                 choices = "none",
                                                 selected = "none"),
                                     numericInput("bin_number_trans", label = "number of bins", value = 50),
                                     numericInput("alpha_trans", label = "alpha", value = 0.5),
                                     numericInput("size_trans", label = "size", value = 1)
                                     
                            )
                     )
              )
            )
            
    ),
    tabItem(tabName = "Comp_tab",
            fluidRow(
              
              column(width = 6,
                     tabBox(title = "Comp",
                            width = NULL, height = NULL,
                            tabPanel(title = "Table",
                                     div(style = 'overflow-x: scroll', DT::dataTableOutput("spill_table")),
                                     br(),
                                     actionButton("reset_comp", "reset")
                                     
                            ),
                            tabPanel(title = "Heatmap",
                                     plotlyOutput("heatmap_spill")
                                     
                            ),
                            tabPanel("Set",
                                     selectInput("xvar_comp", label = "column (chanel)", choices = NULL, selected = NULL),
                                     selectInput("yvar_comp", label = "row (fluorophore)", choices = NULL, selected = NULL),
                                     numericInput("spill_value", 
                                                  label = "spillover value", 
                                                  value = 0, 
                                                  min = 0, 
                                                  max = 2, 
                                                  step = 0.01),
                                     numericInput("step_size", label = "step size", value = 0.01),
                                     # sliderInput("spill_value", "spillover value (log10):",
                                     #             min = -5, max = 1,
                                     #             value = 0.5, step = 0.01),
                                     actionButton("set_spill_value", "set value")
                                     
                            )
                     )
              ),
              column(width = 6,
                     tabBox(title = "Plot",
                            width = NULL, height = NULL,
                            tabPanel("Plot",
                                     plotOutput("plot_comp")
                                     
                            ),
                            tabPanel("Select",
                                     selectInput("sample_selected_comp", label = "Sample", choices = NULL, selected = NULL),
                                     actionButton("previous_frame_comp", "previous"),
                                     actionButton("next_frame_comp", "next"),
                                     br(),
                                     br(),
                                     selectizeInput("gate_comp", 
                                                    label = "subset", 
                                                    choices = "root", 
                                                    selected = "root",
                                                    multiple = FALSE)
                                     
                            ),
                            tabPanel("Options",
                                     selectInput("plot_type_comp", label = "plot type",
                                                 choices = c("hexagonal", "histogram", "dots", "contour"),
                                                 selected = "hexagonal"),
                                     #checkboxInput("apply_trans_comp", "apply tansformation", value = TRUE),
                                     checkboxInput("legend_comp", "show legend", value = FALSE),
                                     checkboxInput("norm_comp", "normalize (set max to 1)", value = TRUE),
                                     checkboxInput("smooth_comp", "smooth", value = FALSE),
                                     #checkboxInput("ridges", "ridges", value = FALSE),
                                     #checkboxInput("facet", "faceting", value = TRUE),
                                     # selectizeInput("facet_var", 
                                     #                multiple =TRUE,
                                     #                label = "facet variables", 
                                     #                choices = "name", 
                                     #                selected = NULL),
                                     # selectizeInput("group_var", 
                                     #                multiple =FALSE,
                                     #                label = "group variable", 
                                     #                choices = c("name","subset"), 
                                     #                selected = "subset"),
                                     # selectizeInput("yridges_var", 
                                     #                multiple =FALSE,
                                     #                label = "y ridges variable", 
                                     #                choices = c("name","subset"), 
                                     #                selected = "subset"),
                                     selectInput("color_var_comp", "color variable",
                                                 choices = "none",
                                                 selected = "none"),
                                     numericInput("bin_number_comp", label = "number of bins", value = 50),
                                     numericInput("alpha_comp", label = "alpha", value = 0.5),
                                     numericInput("size_comp", label = "size", value = 1)
                                     
                            )
                     )
              )
              
            )
            
    ),
    tabItem(tabName = "Gates_tab",
            fluidRow(
              column(width = 4,
                     box(title = "Select",
                         width = NULL, height = NULL,
                         selectInput("sample_selected", label = "Sample", choices = NULL, selected = NULL),
                         actionButton("previous_frame", "previous"),
                         actionButton("next_frame", "next"),
                         br(),
                         br(),
                         selectInput("xvar_gate", label = "x variable", choices = NULL, selected = NULL),
                         selectInput("yvar_gate", label = "y variable", choices = NULL, selected = NULL),
                         selectInput("gate_selected", 
                                     label = "Subset (gate)", 
                                     choices = "root", 
                                     selected = "root"),
                         actionButton("show_gate", "show gate")
                                 
                                 
                     ),
                     tabBox(title = "Gates",
                         width = NULL, height = NULL,
                         # tabPanel("Select",
                         #          selectInput("gate_selected", 
                         #                      label = "gate", 
                         #                      choices = "root", 
                         #                      selected = "root"),
                         #          actionButton("show_gate", "show gate")
                         #          ),
                         tabPanel("Add",
                                  #selectInput("gate_type",
                                  #            label = "Type of gate",
                                  #            choices = c("rectangular", "polygonal"),
                                  #            selected = "rectangular"),
                                  #selectInput("parent_gate", label = "Select parent gate", choices = "root", selected = "root"),
                                  textInput("gate_name", label = "Enter gate name", value = ""),
                                  actionButton("create_gate", "create gate"),
                                  actionButton("reset_gate", "reset gate")
                                  ),
                         tabPanel("Delete",
                                  selectInput("gate_to_delete", 
                                              label = "Delete gate", 
                                              choices = NULL, 
                                              selected = NULL),
                                  actionButton("delete_gate", "Delete")
                                  )
                         
                         
                     ),
                     box(title = "Get data",
                         width = NULL, height = NULL,
                         helpText( "Get data for all files and subsets"),
                         actionButton("compute_data", "Get data"),
                         actionButton("reset_data", "reset")
                     ),
                     box(title = "Message_gate",
                         width = NULL, height = NULL,
                         verbatimTextOutput("message_gate")
                     )
              ),
              column(width = 8,
                     tabBox(title = "Plot",
                         width = NULL, height = NULL,
                         tabPanel("Plot",
                                  plotOutput("plotGate2",
                                             brush = "plot_brush",
                                             click = "plot_click",
                                             dblclick = "plot_dblclick")
                         
                         ),
                         # tabPanel("Scale axis",
                         #          selectInput("x_scale_gate", 
                         #                      label = "x scale", 
                         #                      choices = c("identity", "log10", "asinh", "logicle"), 
                         #                      selected = "identity"),
                         #          selectInput("y_scale_gate", 
                         #                      label = "y scale", 
                         #                      choices = c("identity", "log10", "asinh", "logicle"), 
                         #                      selected = "identity"),
                         #          checkboxInput("freeze_limits", label = "freeze plot limits", value = TRUE)
                         #          
                         # ),
                         tabPanel("Options",
                                  selectInput("plot_type_gate", label = "plot type",
                                              choices = c("hexagonal", "histogram", "dots","contour"),
                                              selected = "hexagonal"),
                                  #checkboxInput("apply_trans_gate", "apply tansformation", value = TRUE),
                                  checkboxInput("legend_gate", "show legend", value = TRUE),
                                  checkboxInput("norm_gate", "normalize (set max to 1)", value = TRUE),
                                  checkboxInput("smooth_gate", "smooth", value = FALSE),
                                  checkboxInput("freeze_limits", label = "freeze plot limits", value = TRUE),
                                  selectInput("color_var_gate", "color variable",
                                              choices = "none",
                                              selected = "none"),
                                  numericInput("bin_number_gate", label = "number of bins", value = 50),
                                  numericInput("alpha_gate", label = "alpha", value = 0.5),
                                  numericInput("size_gate", label = "size", value = 1)
                                  )
                     ),
                     # box("Plot gate",
                     #     width = NULL, height = NULL,
                     #     plotOutput("plotGate_flowCore",
                     #                brush = "plot_brush_1",
                     #                click = "plot_click_1",
                     #                dblclick = "plot_dblclick_1")
                     # ),
                     tabBox(title = "Gating hierarchy",
                         width = NULL, height = NULL,
                         tabPanel("Tree",
                                  plotOutput("tree")
                                  ),
                         tabPanel("Plot gates",
                                  plotOutput("plot_gh"))
                         
                     )
              )
              
            )
    ),
    tabItem(tabName = "Plot_tab",
            fluidRow(
              column(width = 4,
                     tabBox(title = "Select",
                            width = NULL, height = NULL,
                            tabPanel("Samples",
                                     div(style = 'overflow-x: scroll', DT::dataTableOutput("files_selection_table"))
                            ),
                            tabPanel("Subset",
                                     selectizeInput("gate", 
                                                    label = "subset", 
                                                    choices = "root", 
                                                    selected = "root",
                                                    multiple = TRUE)
                            ),
                            tabPanel("Variables",
                                     selectInput("xvar", label = "x variable", choices = NULL, selected = NULL),
                                     selectInput("yvar", label = "y variable", choices = NULL, selected = NULL)
                            )
                     ) 
              ),
              # column(width = 4,
              #        tabBox(title = "Sample",
              #               width = NULL, height = NULL,
              #               tabPanel("Select",
              #                        selectizeInput("gate", 
              #                                       label = "subset", 
              #                                       choices = "root", 
              #                                       selected = "root",
              #                                       multiple = TRUE),
              #                        selectInput("xvar", label = "x variable", choices = NULL, selected = NULL),
              #                        selectInput("yvar", label = "y variable", choices = NULL, selected = NULL)
              #               )
              #        ),
              #        box(title = "Select samples",
              #            width = NULL, height = NULL,
              #            div(style = 'overflow-x: scroll', DT::dataTableOutput("files_selection_table"))
              #            
              #        )
              # ),
              column(width = 8,
                     tabBox(title = "Plot",
                            width = NULL, height = NULL,
                            tabPanel("Plot",
                                     plotOutput("plot_focus")
                                     
                            ),
                            # tabPanel("Scale axis",
                            #          selectInput("x_scale", 
                            #                      label = "x scale", 
                            #                      choices = c("identity", "log10", "asinh", "logicle"), 
                            #                      selected = "identity"),
                            #          selectInput("y_scale", 
                            #                      label = "y scale", 
                            #                      choices = c("identity", "log10", "asinh", "logicle"), 
                            #                      selected = "identity")
                            #          #checkboxInput("freeze_limits", label = "freeze plot limits", value = TRUE)
                            #          
                            # ),
                            tabPanel("Options",
                                     selectInput("plot_type", label = "plot type",
                                                 choices = c("hexagonal", "histogram", "dots", "contour"),
                                                 selected = "histogram"),
                                     #checkboxInput("apply_trans_plot", "apply tansformation", value = TRUE),
                                     checkboxInput("legend", "show legend", value = TRUE),
                                     checkboxInput("norm", "normalize (set max to 1)", value = TRUE),
                                     checkboxInput("smooth", "smooth", value = FALSE),
                                     checkboxInput("ridges", "ridges", value = FALSE),
                                     #checkboxInput("facet", "faceting", value = TRUE),
                                     selectizeInput("facet_var", 
                                                    multiple =TRUE,
                                                    label = "facet variables", 
                                                    choices = "name", 
                                                    selected = NULL),
                                     selectizeInput("group_var", 
                                                    multiple =FALSE,
                                                    label = "group variable", 
                                                    choices = c("name","subset"), 
                                                    selected = "subset"),
                                     selectizeInput("yridges_var", 
                                                    multiple =FALSE,
                                                    label = "y ridges variable", 
                                                    choices = c("name","subset"), 
                                                    selected = "subset"),
                                     selectInput("color_var", "color variable",
                                                 choices = "none",
                                                 selected = "none"),
                                     numericInput("bin_number", label = "number of bins", value = 50),
                                     numericInput("alpha", label = "alpha", value = 0.5),
                                     numericInput("size", label = "size", value = 1)
                                     
                            )
                     )
              )
              
              
          )
    ),
    tabItem(tabName = "Stat_tab",
            fluidRow(
              column(width = 4,
                     tabBox(title = "Select",
                            width = NULL, height = NULL,
                            tabPanel("Samples",
                                     div(style = 'overflow-x: scroll', DT::dataTableOutput("samples_stat"))
                                     ),
                            tabPanel("Subset",
                                     selectizeInput("gate_stat", 
                                                    label = "subset", 
                                                    choices = "root", 
                                                    selected = "root",
                                                    multiple = TRUE)
                            ),
                            tabPanel("Variables",
                                     selectizeInput("yvar_stat", 
                                                    label = "y variable", 
                                                    choices = NULL, 
                                                    selected = NULL, 
                                                    multiple = TRUE)
                            )
                     ) 
              ),
              column(width = 8,
                     tabBox(title = "Plot",
                            width = NULL, height = NULL,
                            tabPanel("Plot",
                                     plotOutput("plot_stat")
                                     
                            ),
                            tabPanel("Options",
                                     selectInput("plot_type_stat", label = "plot type",
                                                 choices = c("bar", "tile"),
                                                 selected = "bar"),
                                     checkboxInput("legend_stat", "show legend", value = TRUE),
                                     #checkboxInput("apply_trans", "apply tansformation", value = TRUE),
                                     checkboxInput("free_y_scale", "free y scale", value = FALSE),
                                     checkboxInput("scale_values", "scale values by row", value = FALSE),
                                     numericInput("max_scale", label = "scale limit", value = 2),
                                     selectizeInput("facet_var_stat", 
                                                    multiple =TRUE,
                                                    label = "facet variables", 
                                                    choices = "name", 
                                                    selected = NULL),
                                     selectizeInput("group_var_stat", 
                                                    multiple =FALSE,
                                                    label = "group variable", 
                                                    choices = c("name","subset"), 
                                                    selected = "subset"),
                                     selectInput("color_var_stat", "color variable",
                                                 choices = "none",
                                                 selected = "none")
                                     
                            )
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
              menuItem("Transform",
                       tabName = "Trans_tab", 
                       startExpanded = FALSE,
                       icon = icon("check-circle")   
              ),
              menuItem("Compensate",
                       tabName = "Comp_tab", 
                       startExpanded = FALSE,
                       icon = icon("check-circle")
              ),
              menuItem("Gates",
                       tabName = "Gates_tab", 
                       startExpanded = FALSE,
                       icon = icon("check-circle")
                       
              ),
              menuItem("Plot",
                       tabName = "Plot_tab", 
                       startExpanded = FALSE,
                       icon = icon("check-circle")
              ),
              menuItem("Stats",
                       tabName = "Stat_tab", 
                       startExpanded = FALSE,
                       icon = icon("check-circle")
              ),
              menuItem("Controls",
                       tabName = "Control_tab", 
                       startExpanded = FALSE,
                       icon = icon("check-circle"),
                       checkboxInput("apply_comp", "apply compensation", TRUE),
                       checkboxInput("apply_trans", "apply transformation", TRUE),
                       br()
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
                         gating_set = NULL,
                         idx_ff_gate = NULL,
                         parameters = NULL,
                         gates = list(),
                         gate_focus = NULL,
                         df_gate_focus = NULL,
                         gates_flowCore = list(),
                         min_val = NULL,
                         max_val = NULL,
                         transformation = list(),
                         keywords = NULL,
                         plot_var = NULL,
                         gate = NULL,
                         data_range = NULL,
                         df_tot = NULL,
                         pdata = NULL,
                         pdata_original = NULL,
                         df_meta = NULL,
                         df_meta_mapped = NULL,
                         df_keywords = NULL,
                         spill = NULL,
                         df_spill_original = NULL,
                         df_spill = NULL
                         )
  
  gate <- reactiveValues(x = NULL, y = NULL)
  
  # Select files
  observe({
    
    validate(
      need(input$files$datapath, "Please select a file to import")
    )
    rval$df_files <- input$files
    print(names(input$files))
  })
  
  
  observe({
    
    validate(
      need(length(input$files_table_rows_selected)>0, "Please select a file to load")
    )
    
    if(file_ext(rval$df_files$datapath[input$files_table_rows_selected[1]]) == "xml"){
      ws <- openWorkspace(rval$df_files$datapath[input$files_table_rows_selected[1]])
      groups <- unique(getSampleGroups(ws)$groupName)
      updateSelectInput(session, "groups", choices = groups, selected = groups[1])
    }
    
  })
  
  ##########################################################################################################
  # load data and initialize values
  
  observeEvent(input$load, {
    
    validate(
      need(length(input$files_table_rows_selected)>0, "Please select a file to load")
    )
    
    
    if(length(rval$df_files$datapath)>0){
      
      if(file_ext(rval$df_files$datapath[input$files_table_rows_selected[1]]) == "xml"){
        ws <- openWorkspace(rval$df_files$datapath[input$files_table_rows_selected[1]])
        #print(getSamples(ws))
        #show(ws)
        #cat(dirname(rval$df_files$datapath)[1])
        gs <- parseWorkspace(ws,
                                              name = input$groups,
                                              execute = TRUE, 
                                              isNcdf = TRUE,
                                              sampNloc = "sampleNode",
                                              path = dirname(rval$df_files$datapath)[1])
        
        
        #samples <- pData(rval$gating_set)$name
        #samples <- paste(unlist(strsplit(s$name, split = "_[0-9]{3}\\.fcs$")), ".fcs", sep = "")
        #print(samples)
        # idx_match <- sapply(samples, function(x){as.numeric(strsplit(x, split= ".", fixed = TRUE)[[1]][1])})
        # samples <- rval$df_files$name[idx_match+1]
        # print(samples)
          
        nodes <- getNodes(gs)
        
        for(node in setdiff(nodes, "root")){
          g <- getGate(gs[[1]], node)
          parent <- getParent(gs[[1]], node)
          rval$gates_flowCore[[basename(node)]] <- list(gate = g, parent = basename(parent))
          if(basename(node) == "CD4+ Tcells"){
            print(g@boundaries)
          }
        }   
        
        
        # idx_to_import <- which(rval$df_files$name %in% samples)
        # 
        # print(rval$df_files$name %in% samples)
        # 
        # 
        # rval$flow_set <- read.ncdfFlowSet(rval$df_files$datapath[ file_ext(rval$df_files$datapath) == ".fcs" ], 
        #                                   which.lines = input$N_lines,
        #                                   which.lines = 10)
        # 
        # names_imported <- fsApply(rval$flow_set, function(x){description(x)[["FILENAME"]]})
        # print(names_imported)
        # 
        # phenoData(rval$flow_set)$name <- rval$df_files$name[idx_to_import]
        
        fs <- getData(gs)
        
        
        # ff <- rval$flow_set[[1]]
        # 
        # rval$df_spill <- as.data.frame(description(ff)[["SPILL"]])
        # colnames(rval$df_spill) <- paste("<",colnames(rval$df_spill),">", sep = "")
        # row.names(rval$df_spill) <- colnames(rval$df_spill)
        # rval$df_spill_original <- rval$df_spill
        # 
        # sp <- as.matrix(rval$df_spill)
        # sp_inverse <- solve(sp)
        # colnames(sp_inverse) <- colnames(sp)
        # rval$flow_set <- compensate(rval$flow_set, sp_inverse)


        names_imported <- fsApply(fs, function(x){description(x)[["FILENAME"]]})
        names_imported <- basename(names_imported)
        idx_match <- sapply(names_imported, function(x){as.numeric(strsplit(x, split= ".", fixed = TRUE)[[1]][1])})
        
        
        
        #pData(rval$flow_set)$name <- rval$df_files$name[idx_match+1]
        #pData(rval$gating_set) <- pData(rval$flow_set)
        
        rval$flow_set <- read.ncdfFlowSet( rval$df_files$datapath[idx_match+1],
                                           which.lines = input$N_lines)
        
        phenoData(rval$flow_set)$name <- rval$df_files$name[idx_match+1]
        

      }else{
        rval$flow_set <- read.ncdfFlowSet( rval$df_files$datapath[input$files_table_rows_selected], 
                                       which.lines = input$N_lines)
        
        phenoData(rval$flow_set)$name <- rval$df_files$name[input$files_table_rows_selected]
        
      }
      
      ff <- rval$flow_set[[1]]
      
      rval$df_spill <- as.data.frame(description(ff)[["SPILL"]])
      row.names(rval$df_spill) <- colnames(rval$df_spill)
      rval$df_spill_original <- rval$df_spill
      
      desc <- parameters(ff)$desc
      name <- parameters(ff)$name
      name_long <-  name
      name_long[!is.na(desc)] <- paste(name[!is.na(desc)], " (", desc[!is.na(desc)], ")", sep = "")
      
      #cat(rownames(parameters(ff)@data))
      
      display <- unlist(sapply(rownames(parameters(ff)@data), FUN = function(x){
        kw <- substr(x, start = 2, stop = nchar(x))
        kw <- paste(kw, "DISPLAY", sep = "")
        disp <- ff@description[[kw]]
        if(is.null(disp)){
          disp <- "NA"
        }
        return(disp)
        }))
      names(display) <- NULL
      #print(display)
      
      rval$parameters <- data.frame(name = name,
                                    desc = desc,
                                    name_long = name_long,
                                    display = display,
                                    range = parameters(ff)@data$range,
                                    minRange = parameters(ff)@data$minRange,
                                    maxRange = parameters(ff)@data$maxRange,
                                    stringsAsFactors = FALSE)
      
      print(rval$parameters)
      #########################################################################################################
      #initialization of transformation
      
      rval$parameters$transform <- "identity"
      rval$parameters$transform[rval$parameters$display == "LIN"] <- "identity"
      rval$parameters$transform[rval$parameters$display == "LOG"] <- "logicle"
     
      rval$parameters$transform_parameters <- sapply(rval$parameters$transform, function(x){
        l <- switch(x,
                    "identity" = list(),
                    "asinh" = list(base = input$base_asinh),
                    "log" = list(base = input$base_log),
                    "flowJo_asinh" = list(m=input$m, 
                                          t = input$t, 
                                          a = input$a, 
                                          length = input$length),
                    "logicle" = list(w=input$w_logicle, 
                                     m=input$m_logicle, 
                                     t = input$t_logicle, 
                                     a = input$a_logicle))
        return(paste( paste(names(l), as.character(l), sep = ": "), collapse="; "))
      })
      
      rval$transformation <- lapply(rval$parameters$transform, function(x){
        switch(x,
               "identity" = identity_trans(),
               "log" = log10_trans(b = input$base_log),
               "asinh" = asinh_trans(b = input$base_asinh),
               "flowJo_asinh" = flowJo_fasinh_trans(m=input$m, 
                                                    t = input$t, 
                                                    a = input$a, 
                                                    length = input$length),
               "logicle" = logicle_trans(w=input$w_logicle, 
                                         m=input$m_logicle, 
                                         t = input$t_logicle, 
                                         a = input$a_logicle))})
      
      names(rval$transformation) <- rval$parameters$name
      
      #########################################################################################################
      
      # if(file_ext(rval$df_files$datapath[input$files_table_rows_selected[1]]) == "xml"){
      #   myTrans <- lapply(rval$parameters$display, function(x){
      #     switch(x,
      #            "LOG" = flowJo_biexp_inverse_trans(),
      #            identity_trans())
      #   })
      #   myTrans <- transformerList(rval$parameters$name, myTrans)
      #   rval$gating_set <- transform(rval$gating_set, myTrans)
      #   recompute(rval$gating_set)
      #   rval$flow_set <- getData(rval$gating_set)
      # }
      
      if(! setequal(rval$plot_var, rval$parameters$name_long)){
        rval$plot_var <- rval$parameters$name_long
        names(rval$plot_var) <- NULL
        
        if(length(rval$plot_var)>1){
          
          updateSelectInput(session, "xvar_show", choices = rval$plot_var, selected = rval$plot_var[1])
          updateSelectInput(session, "xvar_trans", choices = rval$plot_var, selected = rval$plot_var[1])
          updateSelectInput(session, "yvar_trans", choices = rval$plot_var, selected = rval$plot_var[2])
          
          comp_params <- rval$parameters$name_long[match(colnames(rval$df_spill), rval$parameters$name)]
          names(comp_params) <- NULL
          updateSelectInput(session, "xvar_comp", choices = comp_params, selected = comp_params[1])
          updateSelectInput(session, "yvar_comp", choices = comp_params, selected = comp_params[2])
          
          updateSelectInput(session, "xvar_gate", choices = rval$plot_var, selected = rval$plot_var[1])
          updateSelectInput(session, "yvar_gate", choices = rval$plot_var, selected = rval$plot_var[2])
          updateSelectInput(session, "yvar_stat", choices = rval$plot_var, selected = rval$plot_var[1])
          updateSelectInput(session, "xvar", choices = rval$plot_var, selected = rval$plot_var[1])
          updateSelectInput(session, "yvar", choices = rval$plot_var, selected = rval$plot_var[2])
          updateSelectInput(session, "color_var_gate", choices = c("none", rval$plot_var), selected = "none")
          updateSelectInput(session, "color_var", choices = c("none", rval$plot_var), selected = "none")
          updateSelectInput(session, "color_var_trans", choices = c("none", rval$plot_var), selected = "none")
          updateSelectInput(session, "color_var_comp", choices = c("none", rval$plot_var), selected = "none")
          
          
        }
      }
      
      #########################################################################################################
      # Compute max values for each parameter in flow set
      min_val <- as.data.frame(fsApply(rval$flow_set, each_col, min, na.rm = TRUE))
      rval$min_val <- apply(min_val, 2, min)
      max_val <- as.data.frame(fsApply(rval$flow_set, each_col, max,  na.rm = TRUE))
      rval$max_val <- apply(max_val, 2, max)
      
      rval$data_range <- lapply(rval$parameters$name, function(x){
        c(rval$min_val[[x]] , rval$max_val[[x]])
      })
      names(rval$data_range) <- rval$parameters$name
      
      #########################################################################################################
      # Create gating set
      #if(file_ext(rval$df_files$datapath[input$files_table_rows_selected[1]]) != "xml"){
        
      myTrans <- lapply(rval$parameters$display, function(x){
            switch(x,
                   "LOG" = flowJo_biexp_inverse_trans(),
                   identity_trans())
      })
        
      names(myTrans) <- rval$parameters$name
      
      # polygon <- g@boundaries
      # colnames(polygon) <- gsub("<", "", colnames(polygon))
      # colnames(polygon) <- gsub(">", "", colnames(polygon))
      # poly_gate <- polygonGate(.gate = polygon, filterId=basename(node))
      
      
        rval$gating_set <- GatingSet(rval$flow_set)
        
        # add existing gates
        ngates <- length(rval$gates_flowCore)
        
        if(ngates>0){
          
          #gs <- rval$gating_set
          idx <- 1:ngates
          while(length(idx)>0){
            
            i_added <- NULL
            for(i in 1:length(idx)){
              
              g <- rval$gates_flowCore[[idx[i]]]
              
              if(g$parent %in% union(basename(getNodes(gs)), "root") ){
                
                polygon <- g$gate@boundaries
                colnames(polygon) <- gsub("<", "", colnames(polygon))
                colnames(polygon) <- gsub(">", "", colnames(polygon))
                
                for(j in 1:length(colnames(polygon))){
                  polygon[,j] <- myTrans[[colnames(polygon)[j]]]$transform(polygon[,j])
                }
                
                poly_gate <- polygonGate(.gate = polygon, filterId=names(rval$gates_flowCore)[idx[i]])
                
                add(rval$gating_set, 
                    poly_gate, 
                    parent = g$parent, 
                    name = names(rval$gates_flowCore)[idx[i]])
                
                rval$gates_flowCore[[idx[i]]] <- list(gate = poly_gate, parent = basename(g$parent))
                
                if(basename(names(rval$gates_flowCore)[idx[i]]) == "CD4+ Tcells"){
                  print(poly_gate@boundaries)
                }
                
                i_added <- c(i_added, i)
              }
            }
            idx <- idx[-i_added]
          }
          #rval$gating_set <- gs
          
          recompute(rval$gating_set)
        }
        
        
      #}
      # else{
      #   
      #   #remove gates
      #   for(i in 1:length(rval$gates_flowCore)){
      #     gate_name <- names(rval$gates_flowCore)[i]
      #     if(gate_name %in% basename(getNodes(rval$gating_set))){
      #       Rm(gate_name, rval$gating_set)
      #     }
      #   }
      #   #recompute(rval$gating_set)
      #   getNodes(rval$gating_set)
      #   
      #   #add gates with rescaled gate coordinates
      #   for(i in 1:length(rval$gates_flowCore)){
      #     g <- rval$gates_flowCore[[i]]$gate
      #     if(.hasSlot(g, "boundaries")){
      #       bdr <- g@boundaries
      #       for(j in 1:dim(bdr)[2]){
      #         if(colnames(bdr)[j] %in% rval$parameters$name[rval$parameters$display == "LOG"]){
      #           g@boundaries[,j] <- flowJo_biexp_inverse_trans()$transform(bdr[,j])
      #         }
      #       }
      #     }
      #     rval$gates_flowCore[[i]]$gate <- g
      #     add(rval$gating_set, g, 
      #         parent = rval$gates_flowCore[[i]]$parent, 
      #         name = names(rval$gates_flowCore)[i])
      #   }
      #   
      #   recompute(rval$gating_set)
      # }
      
    
      #########################################################################################################
      # update gates
      
      gate_names <- c("root", names(rval$gates_flowCore))
      
      updateSelectInput(session, "gate_selected", choices = gate_names)
      updateSelectInput(session, "gate", choices = gate_names, selected = "root")
      updateSelectInput(session, "gate_stat", choices = gate_names, selected = "root")
      updateSelectInput(session, "gate_trans", choices = gate_names, selected = "root")
      updateSelectInput(session, "gate_comp", choices = gate_names, selected = "root")
      updateSelectInput(session, "gate_to_delete", choices = names(rval$gates_flowCore))
      
      
      #########################################################################################################
      # Metadata
      
      if(!setequal(phenoData(rval$flow_set)$name, rval$pdata$name)){
        
        rval$pdata <- as.data.frame(pData(rval$gating_set))
        rval$pdata_original <- rval$pdata
        
        updateSelectInput(session, "facet_var", 
                          choices = c("subset", names(rval$pdata)), 
                          selected = "name")
        updateSelectInput(session, "facet_var_stat", 
                          choices = c("subset", names(rval$pdata)), 
                          selected = "name")
        
        updateSelectInput(session, "group_var", 
                          choices = c("subset", names(rval$pdata)), 
                          selected = "subset")
        updateSelectInput(session, "group_var_stat", 
                          choices = c("subset", names(rval$pdata)), 
                          selected = "subset")
        
        updateSelectInput(session, "yridges_var", 
                          choices = c("subset", names(rval$pdata)), 
                          selected = "subset")
        
        updateSelectInput(session, "color_var_gate", 
                          choices = c("subset", names(rval$pdata), rval$plot_var), 
                          selected = "subset")
        
        updateSelectInput(session, "color_var", 
                          choices = c("subset", names(rval$pdata), rval$plot_var), 
                          selected = "subset")
        
        updateSelectInput(session, "color_var_stat", 
                          choices = c("subset", names(rval$pdata)), 
                          selected = "subset")
        
        updateSelectInput(session, "color_var_trans", 
                          choices = c("subset", names(rval$pdata)), 
                          selected = "subset")
        updateSelectInput(session, "color_var_comp", 
                          choices = c("subset", names(rval$pdata)), 
                          selected = "subset")
        
        updateSelectInput(session, "sample_selected",
                          choices = pData(rval$flow_set)$name,
                          selected = pData(rval$flow_set)$name[1])
        
        updateSelectInput(session, "sample_selected_trans",
                          choices = pData(rval$flow_set)$name,
                          selected = pData(rval$flow_set)$name[1])
        
        updateSelectInput(session, "sample_selected_comp",
                          choices = pData(rval$flow_set)$name,
                          selected = pData(rval$flow_set)$name[1])
        #cat("update\n")
      }
      
      
      ff <- rval$flow_set[[1]]
      rval$keywords <- names( ff@description )
      
      updateSelectInput(session, "keyword",
                        choices = rval$keywords,
                        selected = NULL)
      
    
    }
    
  })
  
  ##########################################################################################################
  # Observe functions for compensation
  
  observe({
    
    validate(
      need(length(input$file_comp)>0, "Please select a file to load")
    )
    
    sep <- switch(input$sep_comp,
                  "comma" = ",",
                  "semi-column" = ";",
                  "tab" = "\t")
    
    rval$df_comp <- read.csv(input$file_comp$datapath, sep = sep, header = TRUE, quote = "\"", fill = TRUE)
    names(rval$df_comp)[1] <- "name"
  })
  
  observeEvent(input$import_comp, {
    idx_match_row <- match(rval$pdata$name, rval$df_comp[,1])
    idx_match_col <- match(rval$pdata$name, names(rval$df_comp))
    rval$df_spill <- rval$df_comp[idx_match_row, idx_match_col]
  })
  
  observeEvent(input$reset_comp, {
    rval$df_spill <- rval$df_spill_original
  })
  
  observeEvent(input$set_spill_value, {
    
    xvar <- rval$parameters$name[match(input$xvar_comp, rval$parameters$name_long)]
    yvar <- rval$parameters$name[match(input$yvar_comp, rval$parameters$name_long)]
    idx_x <- match(xvar, names(rval$df_spill))
    idx_y <- match(yvar, names(rval$df_spill))
    rval$df_spill[idx_y, idx_x] <- input$spill_value

  })
  
  observe({
    if(input$apply_comp){
      rval$spill <- rval$df_spill
    }else{
      rval$spill <- NULL
    }
    
  })
  
  observe({
    df <- rval$df_spill
    event.data <- event_data("plotly_click", source = "select_heatmap")
    idx_y <- dim(df)[1] - event.data$pointNumber[[1]][1]
    idx_x <- event.data$pointNumber[[1]][2] + 1
    
    if(length(idx_x)>0){
      updateSelectInput(session, "xvar_comp", 
                        selected = rval$parameters$name_long[match(colnames(df)[idx_x], rval$parameters$name)])
    }
    if(length(idx_y)>0){
      updateSelectInput(session, "yvar_comp", 
                        selected = rval$parameters$name_long[match(row.names(df)[idx_y], rval$parameters$name)])
      
    }
    
  })
  
  observe({
    df <- rval$df_spill
    xvar <- rval$parameters$name[match(input$xvar_comp, rval$parameters$name_long)]
    yvar <- rval$parameters$name[match(input$yvar_comp, rval$parameters$name_long)]
    idx_x <- match(xvar, colnames(df))
    idx_y <- match(yvar, row.names(df))
    #if(length(idx_x)>0 & length(idx_y)>0){
      updateNumericInput(session, "spill_value", value = df[idx_y, idx_x])
      
    #}
    
  })
  
  observe({
    updateNumericInput(session, "spill_value", step = input$step_size)
  })
  
  ##########################################################################################################
  # Observe functions for metadata
  
  observe({
    
    validate(
      need(length(input$file_meta)>0, "Please select a file to load")
    )
    
    sep <- switch(input$sep_meta,
                  "comma" = ",",
                  "semi-column" = ";",
                  "tab" = "\t")
    
    rval$df_meta <- read.csv(input$file_meta$datapath, sep = sep, header = TRUE, quote = "\"", fill = TRUE)

  })
  
  observeEvent(input$append_meta, {
    idx_match <- match(rval$pdata$name, rval$df_meta[,1])
    rval$df_meta_mapped <- rval$df_meta[idx_match, -1]
  })
  
  observeEvent(input$reset_meta, {
    rval$df_meta <- NULL
    rval$df_meta_mapped <- NULL
  })
  
  
  
  observeEvent(input$append_keywords, {
    
    if(!is.null(rval$gating_set)){
      df <- NULL
      #new_keys <- setdiff(input$keyword, colnames(rval$pdata))
      
      #print(new_keys)
      #print(names(rval$pdata))
      keys <- input$keyword
      if(length(keys)>0){
        for(key in keys){
          df <- cbind(df, fsApply(rval$flow_set, function(x){description(x)[[key]]}))
        }
        keys <- gsub(pattern = " ", replacement = "_", keys, fixed = TRUE)
        keys <- gsub(pattern = "$", replacement = "", keys, fixed = TRUE)
        colnames(df) <- keys
        row.names(df) <- NULL
        
        
      }
      rval$df_keywords <- df
    }
  })

  
  observe({
    df <- rval$pdata_original
    if(!is.null(rval$df_meta_mapped)){
      df <- cbind(df, rval$df_meta_mapped)
    }
    if(!is.null(rval$df_keywords)){
      df <- cbind(df, rval$df_keywords)
    }
    if(!is.null(df)){
      rval$pdata <- df
      pData(rval$gating_set) <- df
    }
    
  })
  
  
  observe( {
    
    rpd <- rval$pdata
    
    if(!is.null(rval$pdata)){
      updateSelectInput(session, "facet_var", 
                        choices = c("subset", names(rval$pdata)), 
                        selected = "name")
      updateSelectInput(session, "facet_var_stat", 
                        choices = c("subset", names(rval$pdata)), 
                        selected = "name")
      
      updateSelectInput(session, "group_var", 
                        choices = c("subset", names(rval$pdata)), 
                        selected = "subset")
      updateSelectInput(session, "group_var_stat", 
                        choices = c("subset", names(rval$pdata)), 
                        selected = "subset")
      
      updateSelectInput(session, "yridges_var", 
                        choices = c("subset", names(rval$pdata)), 
                        selected = "subset")
      
      updateSelectInput(session, "color_var_gate", 
                        choices = c("subset", names(rval$pdata), rval$plot_var), 
                        selected = "subset")
      updateSelectInput(session, "color_var", 
                        choices = c("subset", names(rval$pdata), rval$plot_var), 
                        selected = "subset")
      updateSelectInput(session, "color_var_stat", 
                        choices = c("subset", names(rval$pdata)), 
                        selected = "subset")
      updateSelectInput(session, "color_var_trans", 
                        choices = c("subset", names(rval$pdata)), 
                        selected = "subset")
      updateSelectInput(session, "color_var_trans", 
                        choices = c("subset", names(rval$pdata)), 
                        selected = "subset")
      
      #cat("update\n")
    }
  })

    
  ##########################################################################################################
  # Observe functions for sample selection
  
  observeEvent(input$sample_selected, {
    if(!is.null(rval$flow_set)){
      rval$idx_ff_gate <- which(rval$pdata$name == input$sample_selected)
      
    }
  })
  
  observeEvent(input$next_frame, {
    if(!is.null(rval$flow_set)){
      idx <- which(rval$pdata$name == input$sample_selected)
      idx <- idx +1
      if(idx > length(rval$pdata$name)){
        idx <- 1
      }
      updateSelectInput(session, "sample_selected", selected = rval$pdata$name[idx])
      
    }
  })
  
  observeEvent(input$previous_frame, {
    if(!is.null(rval$flow_set)){
      idx <- which(rval$pdata$name == input$sample_selected)
      idx <- idx - 1
      if(idx < 1){
        idx <- length(rval$pdata$name)
      }
      updateSelectInput(session, "sample_selected", selected = rval$pdata$name[idx])
    }
  })
  
  observeEvent(input$next_frame_trans, {
    if(!is.null(rval$flow_set)){
      idx <- which(rval$pdata$name == input$sample_selected_trans)
      idx <- idx +1
      if(idx > length(rval$pdata$name)){
        idx <- 1
      }
      updateSelectInput(session, "sample_selected_trans", selected = rval$pdata$name[idx])
      
    }
  })
  
  observeEvent(input$previous_frame_trans, {
    if(!is.null(rval$flow_set)){
      idx <- which(rval$pdata$name == input$sample_selected_trans)
      idx <- idx - 1
      if(idx < 1){
        idx <- length(rval$pdata$name)
      }
      updateSelectInput(session, "sample_selected_trans", selected = rval$pdata$name[idx])
    }
  })
  
  observeEvent(input$next_frame_comp, {
    if(!is.null(rval$flow_set)){
      idx <- which(rval$pdata$name == input$sample_selected_comp)
      idx <- idx +1
      if(idx > length(rval$pdata$name)){
        idx <- 1
      }
      updateSelectInput(session, "sample_selected_comp", selected = rval$pdata$name[idx])
      
    }
  })
  
  observeEvent(input$previous_frame_comp, {
    if(!is.null(rval$flow_set)){
      idx <- which(rval$pdata$name == input$sample_selected_comp)
      idx <- idx - 1
      if(idx < 1){
        idx <- length(rval$pdata$name)
      }
      updateSelectInput(session, "sample_selected_comp", selected = rval$pdata$name[idx])
    }
  })
  
  ##########################################################################################################
  # Observe functions for gating
  
    observeEvent(input$plot_click, {
      
      #gate$x <- c(gate$x, transform_values(input$plot_click$x, input$x_scale_gate))
      #gate$y <- c(gate$y, transform_values(input$plot_click$y, input$y_scale_gate))
      
      xvar <- rval$parameters$name[match(input$xvar_gate, rval$parameters$name_long)]
      yvar <- rval$parameters$name[match(input$yvar_gate, rval$parameters$name_long)]
      
      gate$x <- c(gate$x, rval$transformation[[xvar]]$inverse(input$plot_click$x))
      gate$y <- c(gate$y, rval$transformation[[xvar]]$inverse(input$plot_click$y))
      
      #cat("click")
    })
    
    observeEvent(input$plot_brush, {
      brush <- input$plot_brush
      #cat("brush")
      if (!is.null(brush)) {
        
        xvar <- rval$parameters$name[match(input$xvar_gate, rval$parameters$name_long)]
        yvar <- rval$parameters$name[match(input$yvar_gate, rval$parameters$name_long)]
        
        # gate$x <- sapply(c(brush$xmin, brush$xmax, brush$xmax, brush$xmin), 
        #                  transform_values, 
        #                  scale = input$x_scale_gate)
        # gate$y <- sapply(c(brush$ymin, brush$ymin, brush$ymax, brush$ymax),
        #                  transform_values, 
        #                  scale = input$y_scale_gate)
        gate$x <- rval$transformation[[xvar]]$inverse(c(brush$xmin, brush$xmax, brush$xmax, brush$xmin))
        gate$y <- rval$transformation[[yvar]]$inverse(c(brush$ymin, brush$ymin, brush$ymax, brush$ymax))
        
        session$resetBrush("plot_brush")
        
      }
    })
    
    observeEvent(input$plot_dblclick, {
      gate$x <- NULL
      gate$y <- NULL
      #cat("dblclick")
      session$resetBrush("plot_brush")
    })
    
    observeEvent(input$reset_gate, {
      gate$x <- NULL
      gate$y <- NULL
      rval$gate <- NULL
      session$resetBrush("plot_brush")
    })
  
    observeEvent(input$create_gate, {
      
      if(input$gate_name %in% basename(getNodes(rval$gating_set))){
        showModal(modalDialog(
          title = "Error",
          "Gate name already exists! Please choose another name.",
          easyClose = TRUE,
          footer = NULL
        ))
      }else{
        if(!is.null(gate$x)){
          polygon <- data.frame(x =gate$x, y = gate$y)
          hpts <- chull(polygon)
          hpts <- c(hpts, hpts[1])
          polygon <- polygon[hpts, ]

          boundaries = list("x" = polygon$x, "y" = polygon$y )
          names(boundaries) <- c(rval$parameters$name[match(input$xvar_gate, rval$parameters$name_long)],
                                 rval$parameters$name[match(input$yvar_gate, rval$parameters$name_long)])
          
          poly_gate <- polygonGate(.gate = boundaries, filterId=input$gate_name)
          rval$gate <- poly_gate
          rval$gates_flowCore[[input$gate_name]] <- list(gate = poly_gate, parent = rval$gate_focus)
          add(rval$gating_set, poly_gate, parent = rval$gate_focus, name = input$gate_name)
          
          recompute(rval$gating_set)
          #cat("update3\n")
          updateSelectInput(session, "gate_selected", choices = basename(getNodes(rval$gating_set)), selected = input$gate_name)
          updateSelectInput(session, "gate_to_delete", choices = setdiff(basename(getNodes(rval$gating_set)), "root"))
          updateSelectInput(session, "gate", choices = basename(getNodes(rval$gating_set)), selected = "root")
          updateSelectInput(session, "gate_stat", choices = basename(getNodes(rval$gating_set)), selected = "root")
          updateSelectInput(session, "gate_trans", choices = basename(getNodes(rval$gating_set)), selected = "root")
          updateSelectInput(session, "gate_comp", choices = basename(getNodes(rval$gating_set)), selected = "root")
          
          
          gate$x <- NULL
          gate$y <- NULL
          
        }
      }
        
    })

    
    observeEvent(input$gate_selected, {
      rval$gate_focus <- input$gate_selected
    })
    
    observeEvent(input$delete_gate, {
      if(input$gate_to_delete != "root"){
        
        idx_gh <- which( basename(getNodes(rval$gating_set)) == input$gate_to_delete )
        target_gate <- basename(getNodes(rval$gating_set)[idx_gh])
        child_gates <- basename(getChildren(rval$gating_set[[1]], target_gate))
        idx_delete <- which( names(rval$gates_flowCore) %in% c(target_gate, child_gates) )
        rval$gates_flowCore <- rval$gates_flowCore[-idx_delete]
        
        Rm(target_gate, rval$gating_set)
        recompute(rval$gating_set)
        #cat("update4\n")
        updateSelectInput(session, "gate_selected", choices = basename(getNodes(rval$gating_set)), selected = "root")
        updateSelectInput(session, "gate_to_delete", choices = setdiff(basename(getNodes(rval$gating_set)), "root"))
        updateSelectInput(session, "gate", choices = basename(getNodes(rval$gating_set)), selected = "root")
        updateSelectInput(session, "gate_stat", choices = basename(getNodes(rval$gating_set)), selected = "root")
        updateSelectInput(session, "gate_trans", choices = basename(getNodes(rval$gating_set)), selected = "root")
        updateSelectInput(session, "gate_comp", choices = basename(getNodes(rval$gating_set)), selected = "root")
        
      }
      
    })
    
    observeEvent(input$show_gate, {
      if(input$gate_selected != "root"){
        
        rval$gate <- rval$gates_flowCore[[input$gate_selected]]$gate
        g <- rval$gates_flowCore[[input$gate_selected]]$gate@boundaries
        g <- as.data.frame(g)
        names(g) <- rval$parameters$name_long[match(names(g), rval$parameters$name)]
        
        updateSelectInput(session, "xvar_gate", selected = names(g)[1])
        updateSelectInput(session, "yvar_gate", selected = names(g)[2])
        
        gate$x <- g[[1]]
        gate$y <- g[[2]]
      }
      #cat("update5\n")
      updateSelectInput(session, "gate_selected",  
                        selected = rval$gates_flowCore[[input$gate_selected]]$parent)

    })
    
    

    ##########################################################################################################
    # Observe functions for data transformation
    
    observeEvent(input$apply_transformation, {
      
      if(length(input$parameters_table_rows_selected)>0){
        
        var_name <- rval$parameters$name[input$parameters_table_rows_selected]
        
        trans_params <- switch(input$trans,
                               "identity" = list(),
                               "asinh" = list(base = input$base_asinh),
                               "log" = list(base = input$base_log),
                               "flowJo_asinh" = list(m=input$m, 
                                              t = input$t, 
                                              a = input$a, 
                                              length = input$length),
                               "logicle" = list(w=input$w_logicle, 
                                                m=input$m_logicle, 
                                                t = input$t_logicle, 
                                                a = input$a_logicle))
        
        trans <- switch(input$trans,
                        "identity" = identity_trans(),
                        "log" = log10_trans(b = input$base_log),
                        "asinh" = asinh_trans(b = input$base_asinh),
                        "flowJo_asinh" = flowJo_fasinh_trans(m=input$m, 
                                                      t = input$t, 
                                                      a = input$a, 
                                                      length = input$length),
                        "logicle" = logicle_trans(w=input$w_logicle, 
                                                  m=input$m_logicle, 
                                                  t = input$t_logicle, 
                                                  a = input$a_logicle))
        
        rval$parameters$transform[input$parameters_table_rows_selected] <- input$trans
        rval$parameters$transform_parameters[input$parameters_table_rows_selected] <- paste( paste(names(trans_params), as.character(trans_params), sep = ": "), collapse="; ")
        
        for(i in 1:length(var_name)){
          rval$transformation[[var_name[i]]] <- trans
        }
       
      }
      
    })
      
    #   df <- rval$parameters$name[input$parameters_table_rows_selected]
    #   
    #   if(!is.null(rval$parameters)){
    #     #cat("OK\n")
    #     rval$transformation <- lapply(rval$parameters$transform, function(x){
    #       switch(x,
    #              "identity" = identity_trans(),
    #              "log10" = log10_trans(),
    #              "asinh" = flowJo_fasinh_trans(),
    #              "logicle" = logicle_trans())})
    #     
    #     names(rval$transformation) <- rval$parameters$name
    #     #cat("OK2\n")
    #   }
    #   
    # })
    
    
    # observeEvent(rval$parameters, {
    #   if(!is.null(rval$parameters)){
    #     #cat("OK\n")
    #     rval$transformation <- lapply(rval$parameters$transform, function(x){
    #       switch(x,
    #              "identity" = identity_trans(),
    #              "log10" = log10_trans(),
    #              "asinh" = flowJo_fasinh_trans(),
    #              "logicle" = logicle_trans())})
    #     
    #     names(rval$transformation) <- rval$parameters$name
    #     #cat("OK2\n")
    #   }
    #   
    # })
    
    # observe({
    #   updateSelectInput(session, "y_scale_gate", 
    #                     selected = rval$parameters$transform[match(input$yvar_gate, rval$parameters$name_long)])
    #   updateSelectInput(session, "x_scale_gate", 
    #                     selected = rval$parameters$transform[match(input$xvar_gate, rval$parameters$name_long)])
    #   updateSelectInput(session, "y_scale", 
    #                     selected = rval$parameters$transform[match(input$yvar, rval$parameters$name_long)])
    #   updateSelectInput(session, "x_scale", 
    #                     selected = rval$parameters$transform[match(input$xvar, rval$parameters$name_long)])
    #   
    # })
    # 
    # 
    # observeEvent(input$x_scale_gate, {
    #   rval$parameters$transform[match(input$xvar_gate, rval$parameters$name_long)] <- input$x_scale_gate
    # })
    # 
    # observeEvent(input$y_scale_gate, {
    #   rval$parameters$transform[match(input$yvar_gate, rval$parameters$name_long)] <- input$y_scale_gate
    # })
    # 
    # observeEvent(input$x_scale, {
    #   rval$parameters$transform[match(input$xvar, rval$parameters$name_long)] <- input$x_scale
    # })
    # 
    # observeEvent(input$y_scale, {
    #   rval$parameters$transform[match(input$yvar, rval$parameters$name_long)] <- input$y_scale
    # })
    # 
    # observeEvent(input$default_trans, {
    #   #cat(rval$parameters$display == "NA")
    #   rval$parameters$transform[rval$parameters$display == "NA"] <- input$default_trans
    # })
    
    
    ##########################################################################################################
    # Observe functions to store data
    
    # observe({
    #   if(input$plot_type_gate == "dots"){
    #     ff <- getData(rval$gating_set[[rval$idx_ff_gate]], y = rval$gate_focus)
    #     df <- as.data.frame(exprs(ff))
    #     df$identifier <- input$sample_selected
    #     rval$df_gate_focus <- df
    #   }
    #   
    # })
    
    observeEvent(input$compute_data, {
      
      rval$df_tot <- get_data_gs(gs = rval$gating_set,
                        idx = 1:length(rval$gating_set), 
                        subset = basename(getNodes(rval$gating_set)),
                        spill = rval$spill)
    })
    
    observeEvent(input$reset_data, {
      rval$df_tot <- NULL
    })
    
  ##########################################################################################################
  # Output
  
  output$meta <- renderDataTable({
    validate(
      need(rval$df_meta, "No meta data imported")
    )
    
    idx_match <- match(rval$df_meta[,1], rval$pdata$name)
    
    validate(
      need(length(!is.na(idx_match))>0, "Metadata names do not match gating set names")
    )
    
    as.data.frame(rval$df_meta[!is.na(idx_match), ])
    
    
  })
    
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
    
    validate(
      need(rval$flow_set, "No data imported")
    )
    
     df <- rval$parameters
     df$minRange <- format(df$minRange, digits = 2)
     df$maxRange <- format(df$maxRange, digits = 2)
     df
    
  })
  
  output$pData <- DT::renderDataTable({
    if(!is.null(rval$flow_set)){
      DT::datatable(rval$pdata, rownames = FALSE)
    }
  })
  
  output$files_selection_table <- DT::renderDataTable({
    if(!is.null(rval$flow_set)){
      data.frame("name" = rval$pdata$name, row.names = NULL)
    }
  })
  
  output$samples_stat <- DT::renderDataTable({
    if(!is.null(rval$flow_set)){
      data.frame("name" = rval$pdata$name, row.names = NULL)
    }
  })
  
  output$message_gate <- renderPrint({
        print(gate$x)
  })
  
  output$message_transform <- renderPrint({
    if(!is.null(rval$parameters)){
      var_show <- rval$parameters$name[ match(input$xvar_show, rval$parameters$name_long) ]
      print(rval$transformation[[var_show]])
    }
    
  })
   
  output$tree <- renderPlot({
    
    validate(
      need(length(rval$gates_flowCore)>0, "Empty gates set")
    )
    
    df <- lapply(names(rval$gates_flowCore), function(x){
      if(x != "root"){
        data.frame("source" = rval$gates_flowCore[[x]]$parent, "target" = x)}
      else{NULL}}
    )
    
    df <- do.call(rbind, df)
    net <- igraph::graph.data.frame(df, directed=TRUE)
    net$layout <- layout_as_tree(net)
    
    plot.igraph(net, 
                layout = layout_as_tree, 
                vertex.size = 50, 
                vertex.color = rgb(0,0,0,0.2), 
                vertex.frame.color = NA, 
                vertex.label.color = "black", 
                vertex.label.family = "Helvetica",
                edge.arrow.size = 0.3, 
                vertex.label.cex = 0.7)

  })
  
  output$plot_gh <- renderPlot({
    
    validate(
      need(rval$gating_set, "Empty gating set")
    )
    
    validate(
      need(length(getNodes(rval$gating_set))>1, "No gates to display")
    )
    
    validate(
      need(rval$idx_ff_gate, "Please select a sample")
    )
    
    cat("sample_selected : \n")
    print(rval$idx_ff_gate)
    
    axis_labels <- rval$parameters$name_long
    names(axis_labels) <- rval$parameters$name
    
    transformation <- NULL
    if(input$apply_trans){
      transformation <- rval$transformation
    }
    
    if(input$color_var_gate %in% rval$parameters$name_long){
      color_var <- rval$parameters$name[match(input$color_var_gate, rval$parameters$name_long)]
    }else{
      color_var <- input$color_var_gate
    }
    
    p <- plot_gh(df = rval$df_tot,
                 gs = rval$gating_set, 
                 idx = rval$idx_ff_gate,
                 spill = rval$spill,
                 transformation = transformation,
                 bins = input$bin_number_gate,
                 color_var = color_var,
                 facet_vars = NULL,
                 axis_labels = axis_labels,
                 data_range = rval$data_range,
                 type = input$plot_type_gate,
                 alpha = input$alpha_gate,
                 size = input$size_gate,
                 show.legend = FALSE)
    
    n <- length(p)
    
    if(n>2){
      g <- marrangeGrob(p, nrow = 2, ncol = n%/%2 + n%%2  , top = input$sample_selected)
    }else{
      g <- marrangeGrob(p, nrow = 1, ncol = n  , top = input$sample_selected)
    }
    
    g
    
  })
  
  output$plot_trans <- renderPlot({
    
    validate(
      need(rval$gating_set, "Empty gating set")
    )
    
    validate(
      need(input$sample_selected_trans, "Please select a sample")
    )
  
    
    idx_x <- match(input$xvar_trans, rval$parameters$name_long)
    idx_y <- match(input$yvar_trans, rval$parameters$name_long)
    xvar <- rval$parameters$name[idx_x]
    yvar <- rval$parameters$name[idx_y]
    
    
    if(input$color_var_trans %in% rval$parameters$name_long){
      color_var <- rval$parameters$name[match(input$color_var_trans, rval$parameters$name_long)]
    }else{
      color_var <- input$color_var
    }
    
    #color_var <- rval$parameters$name[match(input$color_var_gate, rval$parameters$name_long)]
    #color_var <- input$color_var
    
    axis_labels <- rval$parameters$name_long
    names(axis_labels) <- rval$parameters$name
    
    transformation <- NULL
    if(input$apply_trans){
      transformation <- rval$transformation
    }
    
    p <- plot_gs(df = rval$df_tot,
                 gs = rval$gating_set, 
                 idx = match(input$sample_selected_trans, rval$pdata$name),
                 subset = input$gate_trans, 
                 spill = rval$spill,
                 xvar = xvar, 
                 yvar = yvar, 
                 color_var = color_var, 
                 gate = NULL, 
                 type = input$plot_type_trans, 
                 bins = input$bin_number_trans,
                 alpha = input$alpha_trans,
                 size = input$size_trans,
                 norm_density = input$norm_trans,
                 smooth = input$smooth_trans,
                 transformation =  transformation,
                 show.legend = input$legend_trans,
                 axis_labels = axis_labels)
    
    p <- p + xlab(input$xvar_trans) + ylab(input$yvar_trans)
    
    p
    
  })
  
  
  output$plot_focus <- renderPlot({
    
    validate(
      need(rval$gating_set, "Empty gating set")
    )
    
    validate(
      need(input$files_selection_table_rows_selected, "Please select samples")
    )
    
    idx_x <- match(input$xvar, rval$parameters$name_long)
    idx_y <- match(input$yvar, rval$parameters$name_long)
    xvar <- rval$parameters$name[idx_x]
    yvar <- rval$parameters$name[idx_y]
    
    
    if(input$color_var %in% rval$parameters$name_long){
      color_var <- rval$parameters$name[match(input$color_var, rval$parameters$name_long)]
    }else{
      color_var <- input$color_var
    }
    
    #color_var <- rval$parameters$name[match(input$color_var_gate, rval$parameters$name_long)]
    #color_var <- input$color_var
    
    axis_labels <- rval$parameters$name_long
    names(axis_labels) <- rval$parameters$name
    
    transformation <- NULL
    if(input$apply_trans){
      transformation <- rval$transformation
    }
    
    p <- plot_gs(df = rval$df_tot,
                 gs = rval$gating_set, 
                 idx = input$files_selection_table_rows_selected,
                 subset = input$gate, 
                 spill = rval$spill,
                 xvar = xvar, 
                 yvar = yvar, 
                 color_var = color_var, 
                 gate = NULL, 
                 type = input$plot_type, 
                 bins = input$bin_number,
                 alpha = input$alpha,
                 size = input$size,
                 norm_density = input$norm,
                 smooth = input$smooth,
                 ridges = input$ridges,
                 transformation =  transformation,
                 facet_vars = input$facet_var,
                 group_var = input$group_var,
                 yridges_var = input$yridges_var,
                 show.legend = input$legend,
                 axis_labels = axis_labels)
    
    p <- p + xlab(input$xvar) + ylab(input$yvar)

    p
    
  })


  
  
  output$plotGate2 <- renderPlot({
    
    validate(
      need(rval$gating_set, "Empty gating set")
    )
    
    validate(
      need(rval$idx_ff_gate, "Please select a sample")
    )
    
    idx_x <- match(input$xvar_gate, rval$parameters$name_long)
    idx_y <- match(input$yvar_gate, rval$parameters$name_long)
    
    xvar <- rval$parameters$name[idx_x]
    yvar <- rval$parameters$name[idx_y]
    

    data_range <- NULL
    if(input$freeze_limits){
      data_range <- rval$data_range
    }
    
    #print(rval$data_range)
    if(input$color_var_gate %in% rval$parameters$name_long){
      color_var <- rval$parameters$name[match(input$color_var_gate, rval$parameters$name_long)]
    }else{
      color_var <- input$color_var_gate
    }
    
    #color_var <- input$color_var_gate
    
    axis_labels <- rval$parameters$name_long
    names(axis_labels) <- rval$parameters$name
    
    polygon_gate <- data.frame(x = gate$x, y=gate$y)
    
    transformation <- NULL
    if(input$apply_trans){
      transformation <- rval$transformation
    }
    
    p <- plot_gs(df = rval$df_tot,
                 gs = rval$gating_set, 
                 idx = rval$idx_ff_gate,
                 subset = rval$gate_focus, 
                 spill = rval$spill,
                 xvar = xvar, 
                 yvar = yvar, 
                 color_var = color_var, 
                 data_range = data_range,
                 axis_labels = axis_labels,
                 gate = rval$gate,
                 polygon_gate = polygon_gate,
                 type = input$plot_type_gate, 
                 bins = input$bin_number_gate,
                 alpha = input$alpha_gate,
                 size = input$size_gate,
                 norm_density = input$norm_gate,
                 smooth = input$smooth_gate,
                 transformation = transformation,
                 show.legend = input$legend_gate)
            
    
    p <- p + xlab(input$xvar_gate) + ylab(input$yvar_gate)
    
    p
    
  })
  
  
  output$plot_stat <- renderPlot({
    
    validate(
      need(rval$gating_set, "Empty gating set")
    )
    
    validate(
      need(input$samples_stat_rows_selected, "Please select a sample")
    )
    
    idx_y <- match(input$yvar_stat, rval$parameters$name_long)
    yvar <- rval$parameters$name[idx_y]
    
    transformation <- NULL
    if(input$apply_trans){
      transformation <- rval$transformation
    }
    
    axis_labels <- rval$parameters$name_long
    names(axis_labels) <- rval$parameters$name
    
    p <- plot_stat(df = rval$df_tot,
                   gs = rval$gating_set,
                   idx =  input$samples_stat_rows_selected,
                   subset = input$gate_stat,
                   spill = rval$spill,
                   yvar = yvar,
                   type = input$plot_type_stat,
                   transformation = transformation,
                   axis_labels = axis_labels,
                   default_trans = identity_trans(),
                   scale_values = input$scale_values,
                   max_scale = input$max_scale,
                   free_y_scale = input$free_y_scale,
                   color_var = input$color_var_stat, 
                   facet_vars = input$facet_var_stat,
                   group_var = input$group_var_stat,
                   show.legend = input$legend_stat)
    
    p                          
      
  })
  
  output$plot_comp <- renderPlot({
    
    validate(
      need(rval$gating_set, "Empty gating set")
    )
    
    validate(
      need(input$sample_selected_comp, "Please select a sample")
    )
    
    
    idx_x <- match(input$xvar_comp, rval$parameters$name_long)
    idx_y <- match(input$yvar_comp, rval$parameters$name_long)
    yvar <- rval$parameters$name[idx_x]
    xvar <- rval$parameters$name[idx_y]
    
    
    if(input$color_var_comp %in% rval$parameters$name_long){
      color_var <- rval$parameters$name[match(input$color_var_comp, rval$parameters$name_long)]
    }else{
      color_var <- input$color_var
    }
    
    #color_var <- rval$parameters$name[match(input$color_var_gate, rval$parameters$name_long)]
    #color_var <- input$color_var
    
    axis_labels <- rval$parameters$name_long
    names(axis_labels) <- rval$parameters$name
    axis_labels[[xvar]] <- paste(axis_labels[[xvar]], "(fluo)")
    axis_labels[[yvar]] <- paste(axis_labels[[yvar]], "(chanel)")
    
    transformation <- NULL
    if(input$apply_trans){
      transformation <- rval$transformation
    }
    
    p <- plot_gs(df = rval$df_tot,
                 gs = rval$gating_set, 
                 idx = match(input$sample_selected_comp, rval$pdata$name),
                 subset = input$gate_comp, 
                 spill = rval$spill,
                 xvar = xvar, 
                 yvar = yvar, 
                 color_var = color_var, 
                 gate = NULL, 
                 type = input$plot_type_comp, 
                 bins = input$bin_number_comp,
                 alpha = input$alpha_comp,
                 size = input$size_comp,
                 norm_density = input$norm_comp,
                 smooth = input$smooth_comp,
                 transformation =  transformation,
                 show.legend = input$legend_comp,
                 axis_labels = axis_labels)
    
    #p <- p + xlab(paste(input$yvar_comp, "(fluo)")) + ylab(paste(input$xvar_comp, "(chanel)"))
    
    p
    
  })
  
  output$spill_table <- renderDataTable({
    
    validate(
      need(rval$df_spill, "No spillover matrix")
    )
    
    df <- rval$df_spill
    format(df, digits =3)
  })
  
  output$heatmap_spill <- renderPlotly({
    
    validate(
      need(rval$df_spill, "No spillover matrix")
    )
    
    df <- rval$df_spill
    df[df == 0] <- NA
    df_log <- log10(df)
    p <- heatmaply(df,
                   #colors = c(rgb(1,1,1), rgb(1,0,0)),
                   #colors= viridis,
                   plot_method="ggplot",
                   scale_fill_gradient_fun = scale_fill_viridis(trans = log10_trans(), name = "spillover"),
                   Rowv = NULL,
                   Colv = NULL,
                   xlab = "detection chanel",
                   ylab = "emitting fluorophore"
                   )
    p$x$source <- "select_heatmap"
    p
  })
 
}
  
shinyApp(ui, server)