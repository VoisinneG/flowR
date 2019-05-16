#BiocManager::install("flowCore")
#BiocManager::install("flowViz")
library(openCyto)
library(CytoML)
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
library("Rgraphviz")
#library(RColorBrewer)
#library(viridis)


options(repos = BiocManager::repositories(), shiny.maxRequestSize = 300*1024^2)
getOption("repos")

source("./R/flowShine_plot.R")




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
  
  fluidRow(
    valueBoxOutput("progressBox", width = 3),
    valueBoxOutput("progressBox2", width = 3),
    valueBoxOutput("progressBox3", width = 3),
    valueBoxOutput("progressBox4", width = 3)
  ),
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
                         #numericInput("N_lines", label = "Number of cells to import", value = 3000),
                         actionButton("load", label = "Load selected files")
                     )
                     # ,
                     # box(title = "summary",
                     #     width = NULL, height = NULL,
                     #     verbatimTextOutput("message")
                     # )
              ),
              column(width = 6,
                     # fluidRow(
                     #   valueBoxOutput("progressBox", width = 6),
                     #   valueBoxOutput("progressBox2", width = 6)
                     # ),
                     # fluidRow(
                     #   valueBoxOutput("progressBox3", width = 12)
                     # ),
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
                                     "Select chanels",
                                     br(),
                                     br(),
                                     div(style = 'overflow-x: scroll', DT::dataTableOutput("parameters_table"))
                                     
                            ),
                            tabPanel(title = "Transform",
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
                                     actionButton("apply_transformation", label = "apply to selected chanels"),
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
                                     numericInput("expand_factor", label = "expand factor", value = 0.1),
                                     selectInput("stat_function", 
                                                 label = "statistics", 
                                                 choices = c("mean", "median", "sd"), 
                                                 selected = "mean"),
                                     selectInput("y_trans", 
                                                 label = "Transform variables:", 
                                                 choices = c("log10", "asinh", "identity", "default"), 
                                                 selected = "default"),
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
    ),
    tabItem(tabName = "Sub_tab",
            fluidRow(
              column(width = 6,
                     tabBox(title = "Sub-sample",
                            width = NULL, height = NULL,
                            tabPanel("Samples",
                                     div(style = 'overflow-x: scroll', DT::dataTableOutput("sub_sample_table"))
                            ),
                            tabPanel("Subset",
                                     selectizeInput("gate_sub_sample", 
                                                    label = "subset", 
                                                    choices = "root", 
                                                    selected = "root",
                                                    multiple = FALSE)
                            ),
                            tabPanel("Compute",
                                     numericInput("ncells_per_sample", "Number of cells / subset / sample", 1000),
                                     actionButton("compute_data", "sample"),
                                     #actionButton("reset_data", "reset"),
                                     br(),
                                     br(),
                                     "Summary",
                                     br(),
                                     verbatimTextOutput("summary_sub_sample")
                            )
                            
                     ),
                     fluidRow(
                       valueBoxOutput("progressBoxSub", width = 6),
                       valueBoxOutput("progressBoxSub2", width = 6)
                     )
              )
            )
    ),
    tabItem(tabName = "TSNE_tab",
            fluidRow(
              column(width = 6,
                     tabBox(title = "tSNE",
                            width = NULL, height = NULL,
                            tabPanel("Variables",
                                     div(style = 'overflow-x: scroll', DT::dataTableOutput("tSNE_variables_table"))
                            ),
                            tabPanel("Options",
                                     
                                     selectInput("y_trans_tsne", 
                                                 label = "Transform variables:", 
                                                 choices = c("log10", "asinh", "identity", "default"), 
                                                 selected = "log10"),
                                     numericInput("perplexity", "perplexity", 50)
                            ),
                            tabPanel("Compute",
                                     numericInput("ncells_tsne", "Number of cells", 1000),
                                     actionButton("compute_tsne", "Perform tSNE"),
                                     br(),
                                     br(),
                                     "Summary",
                                     br(),
                                     verbatimTextOutput("summary_tsne")
                            )
                     ),
                     fluidRow(
                       valueBoxOutput("progressBoxTSNE", width = 6),
                       valueBoxOutput("progressBoxTSNE2", width = 6)
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
              menuItem("Sub-sample",
                       tabName = "Sub_tab",
                       startExpanded = FALSE,
                       icon = icon("check-circle")
                       
              ),
              menuItem("t-SNE",
                       tabName = "TSNE_tab",
                       startExpanded = FALSE,
                       icon = icon("check-circle")
                       
              ),
              menuItem("General controls",
                      tabName = "General_tab",
                      startExpanded = TRUE,
                      icon = icon("check-circle"),
                      checkboxInput("apply_comp", "apply compensation", TRUE),
                      checkboxInput("apply_trans", "apply transformation", TRUE),
                      selectInput("flow_set", "Select flow set", choices = NULL, selected = NULL),
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
                         flow_set_imported = NULL,
                         flow_set_sample = NULL,
                         flow_set_tsne = NULL,
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
                         trans_parameters = list(),
                         keywords = NULL,
                         plot_var = NULL,
                         gate = NULL,
                         data_range = NULL,
                         pdata = NULL,
                         pdata_original = NULL,
                         df_tot = NULL,
                         df_meta = NULL,
                         df_meta_mapped = NULL,
                         df_keywords = NULL,
                         df_sample = NULL,
                         df_tsne = NULL,
                         df_spill_original = NULL,
                         df_spill = NULL,
                         spill = NULL,
                         Ncells_tot = 0,
                         flow_set_names = NULL
                         )
  
  gate <- reactiveValues(x = NULL, y = NULL)
  
  ##########################################################################################################
  # Select files
  
  observeEvent(input$files, {
    validate(
      need(input$files$datapath, "Please select a file to import")
    )
    rval$df_files <- input$files
    
    file.rename(from = rval$df_files$datapath, to = paste(dirname(rval$df_files$datapath[1]),"/", rval$df_files$name, sep =""))
    
    rval$df_files$datapath <- paste(dirname(rval$df_files$datapath[1]),"/", rval$df_files$name, sep ="")
    
    print( list.files( dirname(rval$df_files$datapath[1])) )
    
    #print(names(input$files))
  })
  
  ##########################################################################################################
  # Select group within a workspace
  observe({
    validate(
      need(length(input$files_table_rows_selected)>0, "Please select a file to load")
    )
    if(file_ext(rval$df_files$datapath[input$files_table_rows_selected[1]]) %in% c("xml", "wsp") ){
      ws <- openWorkspace(rval$df_files$datapath[input$files_table_rows_selected[1]])
      groups <- unique(getSampleGroups(ws)$groupName)
      updateSelectInput(session, "groups", choices = groups, selected = groups[1])
    }
    
  })
  
  ##########################################################################################################
  # import flow set
  
  observeEvent(input$load, {
    
    validate(
      need(length(input$files_table_rows_selected)>0, "Please select a file to load")
    )
    
    # Create a Progress object
    progress <- shiny::Progress$new(min = 0, max = 1)
    on.exit(progress$close())
    progress$set(message = "Loading data", value = 0.5)
    
    if(length(rval$df_files$datapath)>0){
      
      if(file_ext(rval$df_files$datapath[input$files_table_rows_selected[1]]) %in% c("xml", "wsp")){
        ws <- openWorkspace(rval$df_files$datapath[input$files_table_rows_selected[1]])
        #print(getSamples(ws))
        show(ws)
        print(dirname(rval$df_files$datapath[1]))
        print( list.files( dirname(rval$df_files$datapath[1])) )
        
        gs <- try(parseWorkspace(ws,
                             name = input$groups,
                             execute = TRUE, 
                             isNcdf = TRUE,
                             sampNloc = "sampleNode",
                             #sampNloc = "keyword",
                             #path = "/var/folders/lb"),
                             path = dirname(rval$df_files$datapath[1])),
                  silent = TRUE)
        
        if(class(gs) != "GatingSet"){
          showModal(modalDialog(
            title = "Error pasing xml workspace",
            paste("Please try importing FCS files directly", sep=""),
            easyClose = TRUE,
            footer = NULL
          ))
        }
        
        validate(
          need(class(gs) == "GatingSet", "No gating set imported")
        )
        
          fs <- getData(gs)
          
          
          
          ###################################################################################
          #get gates and transfrom gates
          
          gates <- get_gates_from_gs(gs)
          
          ff <- fs[[1]]
          
          # time_step is needed to transform gates containing the parameter "Time"
          time_step <- as.numeric(description(ff)[["$TIMESTEP"]])
          
          # Parameters with a DISPLAY = LOG have been transformed with flowJo_biexp_trans().
          # We need to apply the inverse transfrom for such parameters
          
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
          
          myTrans <- lapply(display, function(x){
            switch(x,
                   "LOG" = flowJo_biexp_inverse_trans(),
                   identity_trans())
          })
          
          params <- parameters(ff)$name
          
          pattern <- NULL
          if( length( grep("[\\<|\\>]", params) ) >0 ){
            pattern <- "[\\<|\\>]"
          }else if(length( grep("Comp-", params) ) >0){
            pattern <- "Comp-"
          }
          print(pattern)
          replacement <- ""
          if(!is.null(pattern)){
            params <- gsub(pattern = pattern, replacement = replacement, params)
          }
          
          names(myTrans) <- params
          
          rval$gates_flowCore <- transform_gates(gates = gates, 
                                                 pattern = pattern, 
                                                 replacement = replacement,
                                                 transformation = myTrans, 
                                                 time_step = time_step)
          
          ###################################################################################
          #match fcs file names and import non-compensated, non-transfromed data
          
          names_imported <- fsApply(fs, function(x){description(x)[["FILENAME"]]})
          names_imported <- basename(names_imported)
          print(names_imported)
          #idx_match <- sapply(names_imported, function(x){as.numeric(strsplit(x, split= ".", fixed = TRUE)[[1]][1])})
          idx_match <- match(names_imported, rval$df_files$name)
          
          rval$flow_set <- read.ncdfFlowSet( rval$df_files$datapath[idx_match] )
          phenoData(rval$flow_set)$name <- rval$df_files$name[idx_match]


      }else{
        rval$flow_set <- read.ncdfFlowSet( rval$df_files$datapath[input$files_table_rows_selected] )
        phenoData(rval$flow_set)$name <- rval$df_files$name[input$files_table_rows_selected]
      }
      
      #Initialization of some reactive variables
      fs <- rval$flow_set 
      rval$flow_set_imported <- fs
      
      if("SPILL" %in% names(description(fs[[1]]))){
        rval$df_spill <- as.data.frame(description(fs[[1]])[["SPILL"]])
      }else{
        m <- diag( length(parameters(fs[[1]])$name) )
        colnames(m) <- parameters(fs[[1]])$name
        rval$df_spill <- as.data.frame(m)
      }
      
      row.names(rval$df_spill) <- colnames(rval$df_spill)
      rval$df_spill_original <- rval$df_spill

      rval$flow_set_names <- unique(c(rval$flow_set_names, "imported"))
      updateSelectInput(session, "flow_set", choices = rval$flow_set_names, selected = "imported")
    }
  })
  
  
  # observe({
  #   validate(
  #     need(rval$flow_set_names, "No flow set available")
  #   )
  #   updateSelectInput(session, "flow_set", choices = rval$flow_set_names, selected = rval$flow_set_names[length(rval$flow_set_names)])
  # })
  
  ##########################################################################################################
  # Create gating set
  
  observe({
    validate(
      need(input$flow_set, "No flow set selected")
    )
    rval$flow_set <- switch(input$flow_set,
                            "imported" = rval$flow_set_imported,
                            "sub-sample" = rval$flow_set_sample,
                            "t-SNE" = rval$flow_set_tsne)
  })

  observe({
    validate(
      need(rval$flow_set, "No flow set available")
    )
    
    fs <- rval$flow_set
    rval$Ncells_tot <- sum( sapply(1:length(fs), function(x){dim(fs[[x]]@exprs)[1]}) )

  })
  
  
  observeEvent(input$flow_set, {

    validate(
      need(rval$flow_set, "No flow set available")
    )

    fs <- rval$flow_set
    params <- parameters(fs[[1]])$name
    
    min_val <- as.data.frame(fsApply(fs, each_col, min, na.rm = TRUE))
    min_val_all <- apply(min_val, 2, min)
    max_val <- as.data.frame(fsApply(fs, each_col, max,  na.rm = TRUE))
    max_val_all <- apply(max_val, 2, max)

    #print(min_val_all)

    
    rval$data_range <- lapply(params, function(x){
      c(min_val_all[[x]] , max_val_all[[x]])
    })
    names(rval$data_range) <- params
    
    #print(rval$data_range)
  })
  
  observe({
    
    validate(
      need(rval$flow_set, "No flow set available")
    )
    
    rval$gating_set <- GatingSet(rval$flow_set)
    
    # add gates
    print(names(rval$gates_flowCore))
    rval$gating_set <- add_gates_flowCore(rval$gating_set, rval$gates_flowCore)
    
    # update gates
    
    gate_names <- getNodes(rval$gating_set)
    
    updateSelectInput(session, "gate_selected", choices = gate_names)
    updateSelectInput(session, "gate", choices = gate_names, selected = "root")
    updateSelectInput(session, "gate_stat", choices = gate_names, selected = "root")
    updateSelectInput(session, "gate_trans", choices = gate_names, selected = "root")
    updateSelectInput(session, "gate_comp", choices = gate_names, selected = "root")
    updateSelectInput(session, "gate_to_delete", choices = setdiff(gate_names,"root"))
    updateSelectInput(session, "gate_sub_sample", choices = gate_names, selected = gate_names)
    
  })
  
  ##########################################################################################################
  # Observe functions for parameters
  
  
  #get parameters information from flow set
  observe({

    validate(
      need(rval$flow_set, "No flow set available")
    )
    
    ff <- rval$flow_set[[1]]
    
    if(!setequal(rval$parameters$name, parameters(ff)$name)){
      desc <- parameters(ff)$desc
      name <- parameters(ff)$name
      name_long <- name
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
      
      names(display) <- parameters(ff)@data$name
      #print(display)
      
      rval$parameters <- data.frame(name = name,
                                    desc = desc,
                                    name_long = name_long,
                                    display = display[match(name, names(display))],
                                    range = parameters(ff)@data$range,
                                    minRange = parameters(ff)@data$minRange,
                                    maxRange = parameters(ff)@data$maxRange,
                                    stringsAsFactors = FALSE)
      #print(rval$parameters)
      #print("OK")
    }
    
  })
  
  #Update available variables
  observe({
    
    validate(
      need(rval$parameters, "No parameters defined")
    )
    
    if(! setequal(rval$plot_var, rval$parameters$name_long)){
      
      #print("update plot vars")
      rval$plot_var <- rval$parameters$name_long
      names(rval$plot_var) <- NULL
      
      if(length(rval$plot_var)>1){
        
        updateSelectInput(session, "xvar_show", choices = rval$plot_var, selected = rval$plot_var[1])
        updateSelectInput(session, "xvar_trans", choices = rval$plot_var, selected = rval$plot_var[1])
        updateSelectInput(session, "yvar_trans", choices = rval$plot_var, selected = rval$plot_var[2])
        updateSelectInput(session, "xvar_gate", choices = rval$plot_var, selected = rval$plot_var[1])
        updateSelectInput(session, "yvar_gate", choices = rval$plot_var, selected = rval$plot_var[2])
        updateSelectInput(session, "yvar_stat", choices = rval$plot_var, selected = rval$plot_var[1])
        updateSelectInput(session, "xvar", choices = rval$plot_var, selected = rval$plot_var[1])
        updateSelectInput(session, "yvar", choices = rval$plot_var, selected = rval$plot_var[2])
        
        updateSelectInput(session, "color_var_gate", choices = c("none", rval$plot_var), selected = "none")
        updateSelectInput(session, "color_var", choices = c("none", rval$plot_var), selected = "none")
        updateSelectInput(session, "color_var_trans", choices = c("none", rval$plot_var), selected = "none")
        updateSelectInput(session, "color_var_comp", choices = c("none", rval$plot_var), selected = "none")
        
        comp_params <- rval$parameters$name_long[match(colnames(rval$df_spill), rval$parameters$name)]
        names(comp_params) <- NULL
        updateSelectInput(session, "xvar_comp", choices = comp_params, selected = comp_params[1])
        updateSelectInput(session, "yvar_comp", choices = comp_params, selected = comp_params[2])
        
        
      }
    }
  })
  
  ##########################################################################################################
  # Observe functions for data transformation
  
  # Initialization of transformation for new parameters
  observe({
    
    validate(
      need(rval$parameters, "No parameters defined")
    )
    
    new_par <- setdiff(rval$parameters$name, names(rval$transformation))
    #print(new_par)
    idx_new <- match(new_par, rval$parameters$name)
    
    if(length(new_par)>0){
      
      
      
      for(i in 1:length(new_par)){
        rval$transformation[[new_par[i]]] <- switch(rval$parameters$display[idx_new[i]],
                                                    "LOG" = logicle_trans(w=input$w_logicle, 
                                                                          m=input$m_logicle, 
                                                                          t = input$t_logicle, 
                                                                          a = input$a_logicle),
                                                    identity_trans())
        rval$trans_parameters[[new_par[i]]] <- switch(rval$parameters$display[idx_new[i]],
                                                      "LOG" = list(w=input$w_logicle, 
                                                                   m=input$m_logicle, 
                                                                   t = input$t_logicle, 
                                                                   a = input$a_logicle),
                                                      list())
      }
      
    }
    
  })
  
  
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
                      "log" = log_trans(base = input$base_log),
                      "asinh" = asinh_trans(b = input$base_asinh),
                      "flowJo_asinh" = flowJo_fasinh_trans(m=input$m,
                                                           t = input$t,
                                                           a = input$a,
                                                           length = input$length),
                      "logicle" = logicle_trans(w=input$w_logicle,
                                                m=input$m_logicle,
                                                t = input$t_logicle,
                                                a = input$a_logicle))
      
      
      
      for(i in 1:length(var_name)){
        rval$transformation[[var_name[i]]] <- trans
        rval$trans_parameters[[var_name[i]]] <- trans_params
      }
      
    }
    
  })
  
  observe({
    
    validate(
      need(rval$transformation, "No transformation defined")
    )
    
    validate(
      need(rval$parameters, "No parameters")
    )
    
    trans_name <- sapply(rval$transformation, function(x){x$name})
    trans_param <- sapply(rval$trans_parameters, function(x){
      paste( paste(names(x), as.character(x), sep = ": "), collapse="; ")})
    
    #print(trans_name)
    #print(trans_param)
    
    idx_match <- match(rval$parameters$name, names(rval$transformation))
    
    rval$parameters$transform <- trans_name[idx_match]
    rval$parameters[["transform parameters"]] <- trans_param[idx_match]
    
    #print(rval$parameters)
    
  })
  
  observe({
    updateSelectInput(session, "xvar_trans", 
                      selected = rval$parameters$name_long[input$parameters_table_rows_selected[1]])
    if(length(input$parameters_table_row_selected)>1){
      updateSelectInput(session, "yvar_trans", 
                        selected = rval$parameters$name_long[input$parameters_table_rows_selected[2]])
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
      need(rval$flow_set, "No flow set available")
    )
    
    if(setequal(pData(rval$flow_set)$name, rval$pdata$name)){
      pData(rval$flow_set) <- rval$pdata
    }
    
  })
  
  observe({
    
    validate(
      need(rval$flow_set, "No flow set available")
    )
    
    if(!setequal(pData(rval$flow_set)$name, rval$pdata$name)){
      rval$pdata_original <- as.data.frame(pData(rval$flow_set))
    }
    
  })
  
  observe({
    
    validate(
      need(rval$pdata, "No metadata available")
    )
    
    validate(
      need(rval$plot_var, "No plotting variables")
    )
    
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
    
      
  })
  

  #Update available keywords
  observe({
    
    validate(
      need(rval$flow_set, "No flow set available")
    )
    
    ff <- rval$flow_set[[1]]
    
    rval$keywords <- names( ff@description )
    
    updateSelectInput(session, "keyword",
                      choices = rval$keywords,
                      selected = NULL)  
  })
  
  
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
    idx_match <- match(rval$pdata_original$name, rval$df_meta[,1])
    rval$df_meta_mapped <- rval$df_meta[idx_match, -1]
  })
  
  observeEvent(input$reset_meta, {
    rval$df_meta <- NULL
    rval$df_meta_mapped <- NULL
  })
  
  
  
  observeEvent(input$append_keywords, {
    
    if(!is.null(rval$flow_set)){
      
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
      #pData(rval$flow_set) <- df
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
      gate$y <- c(gate$y, rval$transformation[[yvar]]$inverse(input$plot_click$y))
      
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
          #hpts <- c(hpts, hpts[1])
          polygon <- polygon[hpts, ]
          polygon <- as.matrix(polygon)
          
          var_names <- c(rval$parameters$name[match(input$xvar_gate, rval$parameters$name_long)],
                         rval$parameters$name[match(input$yvar_gate, rval$parameters$name_long)])
          names(var_names)<-NULL
          colnames(polygon) <- var_names
          
          poly_gate <- polygonGate(.gate = polygon, filterId=input$gate_name)
          rval$gate <- poly_gate
          rval$gates_flowCore[[input$gate_name]] <- list(gate = poly_gate, parent = rval$gate_focus)
          
          print(names(poly_gate@parameters))
          add(rval$gating_set, poly_gate, parent = rval$gate_focus)
          
          recompute(rval$gating_set)
          #cat("update3\n")
          updateSelectInput(session, "gate_selected", choices = getNodes(rval$gating_set), selected = input$gate_name)
          updateSelectInput(session, "gate_to_delete", choices = setdiff(getNodes(rval$gating_set), "root"))
          updateSelectInput(session, "gate", choices = getNodes(rval$gating_set), selected = "root")
          updateSelectInput(session, "gate_stat", choices = getNodes(rval$gating_set), selected = "root")
          updateSelectInput(session, "gate_trans", choices = getNodes(rval$gating_set), selected = "root")
          updateSelectInput(session, "gate_comp", choices = getNodes(rval$gating_set), selected = "root")
          
          
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
        
        idx_gh <- which( getNodes(rval$gating_set) == input$gate_to_delete )
        target_gate <- getNodes(rval$gating_set)[idx_gh]
        child_gates <- getChildren(rval$gating_set[[1]], target_gate)
        idx_delete <- which( names(rval$gates_flowCore) %in% c(target_gate, child_gates) )
        rval$gates_flowCore <- rval$gates_flowCore[-idx_delete]
        
        Rm(target_gate, rval$gating_set)
        recompute(rval$gating_set)
        #cat("update4\n")
        updateSelectInput(session, "gate_selected", choices = getNodes(rval$gating_set), selected = "root")
        updateSelectInput(session, "gate_to_delete", choices = setdiff(getNodes(rval$gating_set), "root"))
        updateSelectInput(session, "gate", choices = getNodes(rval$gating_set), selected = "root")
        updateSelectInput(session, "gate_stat", choices = getNodes(rval$gating_set), selected = "root")
        updateSelectInput(session, "gate_trans", choices = getNodes(rval$gating_set), selected = "root")
        updateSelectInput(session, "gate_comp", choices = getNodes(rval$gating_set), selected = "root")
        
      }
      
    })
    
    observeEvent(input$show_gate, {
      if(input$gate_selected != "root"){
        
        rval$gate <- rval$gates_flowCore[[input$gate_selected]]$gate
        gate_params <- names(rval$gates_flowCore[[input$gate_selected]]$gate@parameters)
        params <- gates_params[gates_params %in% rval$parameters$name]
        
        if(length(params) > 0){
          updateSelectInput(session, "xvar_gate", selected = params[1])
        }
        if(length(params) > 1){
          updateSelectInput(session, "yvar_gate", selected = params[2])
        }
        
        # g <- as.data.frame(g)
        # names(g) <- rval$parameters$name_long[match(names(g), rval$parameters$name)]
        # 
        # updateSelectInput(session, "xvar_gate", selected = names(g)[1])
        # updateSelectInput(session, "yvar_gate", selected = names(g)[2])
        # 
        # gate$x <- g[[1]]
        # gate$y <- g[[2]]
      }
      #cat("update5\n")
      updateSelectInput(session, "gate_selected",  
                        selected = rval$gates_flowCore[[input$gate_selected]]$parent)

    })
    
    
    ##########################################################################################################
    # Observe functions to store data

    
    observeEvent(input$compute_data, {
      
      # Create a Progress object
      progress <- shiny::Progress$new(min = 0, max = 100)
      on.exit(progress$close())
      progress$set(message = "Computing...", value = 0)
      updateProgress <- function(value = NULL, detail = NULL) {
        progress$set(value = value, detail = detail)
      }
      
      
      if( length(input$sub_sample_table_rows_selected)==0){
        showModal(modalDialog(
          title = "No sample selected",
          paste("Please select samples before proceeding", sep=""),
          easyClose = TRUE,
          footer = NULL
        ))
      }
      
      validate(
        need(length(input$sub_sample_table_rows_selected)>0, "No sample selected")
      )
      
      print(input$gate_sub_sample)
      
      if( nchar(input$gate_sub_sample) == 0 ){
        showModal(modalDialog(
          title = "No subset selected",
          paste("Please select a subset before proceeding", sep=""),
          easyClose = TRUE,
          footer = NULL
        ))
      }
      
      validate(
        need(input$gate_sub_sample, "No subset selected")
      )
      
      sample = rval$pdata$name[input$sub_sample_table_rows_selected]
      
      rval$df_sample <- get_data_gs(gs = rval$gating_set,
                                    sample = sample, 
                                    subset = input$gate_sub_sample,
                                    spill = NULL,
                                    Ncells = input$ncells_per_sample,
                                    updateProgress = updateProgress)
                        
      rval$flow_set_sample <- build_flowset_from_df(rval$df_sample, fs = rval$flow_set)
      rval$flow_set_names <- unique(c(rval$flow_set_names, "sub-sample"))
      updateSelectInput(session, "flow_set", choices = rval$flow_set_names, selected = "sub-sample")
    })
    
    # observeEvent(input$reset_data, {
    #   rval$df_tot <- NULL
    # })
    
    
    observeEvent(input$compute_tsne, {
      
      validate(
        need(rval$flow_set, "Empty flow set")
      )
      
      if( is.null(rval$flow_set_sample) ){
        showModal(modalDialog(
          title = "No sub-sample data available",
          paste("Please sub-sample data first", sep=""),
          easyClose = TRUE,
          footer = NULL
        ))
      }
      
      validate(
        need(rval$flow_set_sample, "Please sub-sample data first")
      )

      validate(
        need(rval$df_sample, "Please sub-sample data first")
      )
      
      if( length(input$tSNE_variables_table_rows_selected)==0){
        showModal(modalDialog(
          title = "No variable selected",
          paste("Please select variables before proceeding", sep=""),
          easyClose = TRUE,
          footer = NULL
        ))
      }
      
      validate(
        need(length(input$tSNE_variables_table_rows_selected) >0, "No variables selected")
      )
      
      # Create a Progress object
      progress <- shiny::Progress$new(min = 0, max = 100)
      on.exit(progress$close())
      progress$set(message = "Computing...", value = 0)
      updateProgress <- function(value = NULL, detail = NULL) {
        progress$set(value = value, detail = detail)
      }
      
      transformation <- NULL
      if(input$apply_trans){
        transformation <- rval$transformation
      }
      
      y_trans <- switch(input$y_trans_tsne,
                        "log10" = log10_trans(),
                        "asinh" = asinh_trans(),
                        "identity" = identity_trans(),
                        NULL)
      
      #rval$gating_set <- GatingSet(rval$flow_set)
      
      # add gates
      #rval$gating_set <- add_gates_flowCore(rval$gating_set, rval$gates_flowCore)
      
      gs <- GatingSet(rval$flow_set_sample)
      gs <- add_gates_flowCore(gs, rval$gates_flowCore)
      
      rval$df_tsne <- get_data_gs(gs = gs,
                                  sample = pData(gs)$name, 
                                  subset = "root",
                                  spill = rval$spill,
                                  Ncells = NULL,
                                  updateProgress = updateProgress)
                  
      #print(summary(rval$df_tsne))
      
      rval$df_tsne <- dim_reduction(df = rval$df_tsne,
                                    yvar = rval$parameters$name[input$tSNE_variables_table_rows_selected], 
                                    Ncells = input$ncells_tsne, 
                                    y_trans = y_trans,
                                    transformation = transformation,
                                    perplexity = input$perplexity)
      
      rval$flow_set_tsne <- build_flowset_from_df(rval$df_tsne, fs = rval$flow_set_sample)
      rval$flow_set_names <- unique(c(rval$flow_set_names, "t-SNE"))
      updateSelectInput(session, "flow_set", choices = rval$flow_set_names, selected = "t-SNE")
      
    })
    
    # observe({
    #   updateSelectInput(session, "xvar_show", choices = rval$plot_var)
    #   updateSelectInput(session, "xvar_trans", choices = rval$plot_var)
    #   updateSelectInput(session, "yvar_trans", choices = rval$plot_var)
    #   
    #   updateSelectInput(session, "xvar_gate", choices = rval$plot_var)
    #   updateSelectInput(session, "yvar_gate", choices = rval$plot_var)
    #   updateSelectInput(session, "yvar_stat", choices = rval$plot_var)
    #   updateSelectInput(session, "xvar", choices = rval$plot_var)
    #   updateSelectInput(session, "yvar", choices = rval$plot_var)
    #   updateSelectInput(session, "color_var_gate", choices = c("none", rval$plot_var))
    #   updateSelectInput(session, "color_var", choices = c("none", rval$plot_var))
    #   updateSelectInput(session, "color_var_trans", choices = c("none", rval$plot_var))
    #   updateSelectInput(session, "color_var_comp", choices = c("none", rval$plot_var))
    # })
    
    
    
  ##########################################################################################################
  # Output Tables
  
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
    df <- rval$df_files[ ,c("name", "size")]
    df$new_name <- basename(rval$df_files$datapath)
    df$dir_name <- dirname(rval$df_files$datapath)
    df
  })
  
  output$parameters_table <- renderDataTable({
    
    validate(
      need(rval$parameters, "No data imported")
    )
    df <- rval$parameters
    df$minRange <- format(df$minRange, digits = 2)
    df$maxRange <- format(df$maxRange, digits = 2)
    df[["chanel_name"]] <- df$name_long
    DT::datatable(
      df[, c("chanel_name", "transform", "transform parameters", "display", "range", "minRange", "maxRange")], 
      rownames = FALSE)

  })
  
  
  output$tSNE_variables_table <- renderDataTable({
    
    validate(
      need(rval$parameters, "No data imported")
    )
    
    df <- rval$parameters
    df[["chanel_name"]] <- df$name_long
    
    DT::datatable(
      df[, c("chanel_name", "transform", "transform parameters")], 
      rownames = FALSE)
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
  
  output$sub_sample_table <- DT::renderDataTable({
    if(!is.null(rval$flow_set)){
      data.frame("name" = rval$pdata$name, row.names = NULL)
    }
  })
  
  output$spill_table <- renderDataTable({
    
    validate(
      need(rval$df_spill, "No spillover matrix")
    )
    
    df <- rval$df_spill
    format(df, digits =3)
  })
  
  # output$message <- renderText({
  #   paste("You have loaded", length(rval$flow_set), "items")
  # })
  
  
  ##########################################################################################################
  # Output value boxes
  
  output$progressBox <- renderValueBox({
    valueBox(
      length(rval$flow_set), "samples",icon = icon("list"),
      color = "purple"
    )
  })
  

  output$progressBox2 <- renderValueBox({
    ngates <- 0
    if(!is.null(rval$gating_set)){
      ngates <- length(setdiff(getNodes(rval$gating_set), "root"))
    }
    
    valueBox(
      ngates, "gates", icon = icon("list"),
      color = "yellow"
    )
  })
  
  output$progressBox3 <- renderValueBox({
    
    valueBox(
      format(rval$Ncells_tot, digits = 2), "cells", icon = icon("list"),
      color = "green"
    )
  })
  
  output$progressBox4 <- renderValueBox({
    nparams <- 0
    if(!is.null(rval$flow_set)){
      nparams <- length(rval$flow_set@colnames)
    }
    
    valueBox(
      nparams, "parameters",icon = icon("list"),
      color = "red"
    )
  })
  
  output$progressBoxSub <- renderValueBox({
    valueBox(
      length(rval$flow_set_sample), "samples",icon = icon("list"),
      color = "purple"
    )
  })
  
  output$progressBoxSub2 <- renderValueBox({
    ncells <- 0
    if(!is.null(rval$flow_set_sample)){
      fs <- rval$flow_set_sample
      ncells <- sum( sapply(1:length(fs), function(x){dim(fs[[x]]@exprs)[1]}) )
    }
    
    valueBox(
      ncells, "cells", icon = icon("list"),
      color = "green"
    )
  })
  
  output$progressBoxTSNE <- renderValueBox({
    valueBox(
      length(rval$flow_set_tsne), "samples",icon = icon("list"),
      color = "purple"
    )
  })
  
  output$progressBoxTSNE2 <- renderValueBox({
    ncells <- 0
    if(!is.null(rval$flow_set_tsne)){
      fs <- rval$flow_set_tsne
      ncells <- sum( sapply(1:length(fs), function(x){dim(fs[[x]]@exprs)[1]}) )
    }
    
    valueBox(
      ncells, "cells", icon = icon("list"),
      color = "green"
    )
  })
    
    
  ##########################################################################################################
  # Output messages
  
  output$summary_sub_sample <- renderPrint({
    if(!is.null(rval$df_sample)){
      print(summary(rval$df_sample[, c("name", "subset")]))
    }else{
      "No sub-sampling performed yet"
    }
  })
  
  output$summary_tsne <- renderPrint({
    if(!is.null(rval$df_tsne)){
      print(summary(rval$df_tsne[, c("name", "subset")]))
    }else{
      "No t-SNE performed yet"
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
  
  
  ##########################################################################################################
  # Output plots
  
  output$tree <- renderPlot({
    
    validate(
      need(rval$gating_set, "Empty gating set")
    )
    
    validate(
      need(length(setdiff(getNodes(rval$gating_set), "root"))>0, "No gates in gating set")
    )
    
    p <- plot(rval$gating_set)
    
    renderGraph(p)

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
    
    #cat("sample_selected : \n")
    #print(rval$idx_ff_gate)
    
    axis_labels <- rval$parameters$name_long
    names(axis_labels) <- rval$parameters$name
    
    transformation <- NULL
    if(input$apply_trans){
      transformation <- rval$transformation
    }
    
    data_range <- NULL
    if(input$freeze_limits){
      data_range <- rval$data_range
    }
    
    if(input$color_var_gate %in% rval$parameters$name_long){
      color_var <- rval$parameters$name[match(input$color_var_gate, rval$parameters$name_long)]
    }else{
      color_var <- input$color_var_gate
    }
    
    if(input$plot_type_gate != "histogram"){
      type <- input$plot_type_gate
    }
    
    p <- plot_gh(df = rval$df_tot,
                 gs = rval$gating_set,
                 sample = rval$pdata$name[rval$idx_ff_gate],
                 spill = rval$spill,
                 transformation = transformation,
                 bins = input$bin_number_gate,
                 color_var = color_var,
                 facet_vars = NULL,
                 axis_labels = axis_labels,
                 data_range = data_range,
                 type = type,
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
                 sample = input$sample_selected_trans,
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
    
    if(!is.null(p)){
      p <- p + xlab(input$xvar_trans) 
      if(input$plot_type_trans != "histogram"){
        p <- p + ylab(input$yvar_trans)
      }
    }
    
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
                 sample = rval$pdata$name[input$files_selection_table_rows_selected],
                 subset = input$gate, 
                 spill = rval$spill,
                 xvar = xvar, 
                 yvar = yvar, 
                 color_var = color_var, 
                 #gate = NULL, 
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
    
    if(!is.null(p)){
      p <- p + xlab(input$xvar) 
      if(input$plot_type != "histogram"){
        p <- p + ylab(input$yvar)
      }
    }
    p
    
  })


  output$plotGate2 <- renderPlot({
    
    validate(
      need(rval$gating_set, "Empty gating set")
    )
    
    validate(
      need(rval$idx_ff_gate, "Please select a sample")
    )
    
    #print(input$xvar_gate)
    
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
                 sample = rval$pdata$name[rval$idx_ff_gate],
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
            
    if(!is.null(p)){
      p <- p + xlab(input$xvar_gate)
      if(input$plot_type_gate != "histogram"){
        p <- p + ylab(input$yvar_gate)
      }
    }
    
      
    
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
    
    y_trans <- switch(input$y_trans,
                      "log10" = log10_trans(),
                      "asinh" = asinh_trans(),
                      "identity" = identity_trans(),
                      NULL)
    
    p <- plot_stat(df = rval$df_tot,
                   gs = rval$gating_set,
                   sample =  rval$pdata$name[input$samples_stat_rows_selected],
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
                   expand_factor = input$expand_factor,
                   stat_function = input$stat_function,
                   show.legend = input$legend_stat,
                   y_trans = y_trans)
    
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
                 sample = input$sample_selected_comp,
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
                   column_text_angle = 90,
                   xlab = "detection chanel",
                   ylab = "emitting fluorophore",
                   fontsize_row = 6,
                   fontsize_col = 6,
                   cellnote_size = 6,
                   hide_colorbar = TRUE,
                   main = "spillover matrix",
                   margins = c(50, 50, 50, 0)
                   )
    p$x$source <- "select_heatmap"
    p
  })
 
}
  
shinyApp(ui, server)