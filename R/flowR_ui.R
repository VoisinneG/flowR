#' @import shiny
#' @import shinydashboard
#' @import DT
#' @import plotly
#' @export
flowR_ui <- function() {
  
  body <- dashboardBody(
    
    fluidRow(
      valueBoxOutput("progressBox", width = 3),
      valueBoxOutput("progressBox2", width = 3),
      valueBoxOutput("progressBox3", width = 3),
      valueBoxOutput("progressBox4", width = 3)
    ),
    tabItems(
      tabItem(tabName = "Import_tab",
              importUI(id = "import_module")
      ),
      tabItem(tabName = "Meta_tab",
              metadataUI(id = "metadata_module")       
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
                              )
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
                              tabPanel("Options",
                                       selectInput("plot_type_trans", label = "plot type",
                                                   choices = c("hexagonal", "histogram", "dots", "contour"),
                                                   selected = "histogram"),
                                       checkboxInput("legend_trans", "show legend", value = FALSE),
                                       checkboxInput("norm_trans", "normalize (set max to 1)", value = TRUE),
                                       checkboxInput("smooth_trans", "smooth", value = FALSE),
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
                       tabBox(title = "",
                              width = NULL, height = NULL,
                              tabPanel(title = "Table",
                                       actionButton("reset_comp", "reset"),
                                       downloadButton("download_spill"),
                                       br(),
                                       br(),
                                       div(style = 'overflow-x: scroll', DT::dataTableOutput("spill_table")),
                                       br()
                                       
                                       
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
                                       actionButton("set_spill_value", "set value")
                                       
                              )
                              # tabPanel("Save",
                              #          width = NULL, height = NULL,
                              #          "Save spillover matrix in .txt format :",
                              #          br(),
                              #          br(),
                              #          downloadButton("download_spill")
                              # ),
                              # tabPanel("Import",
                              #          width = NULL, height = NULL,
                              #          fileInput(inputId = "spill_file", 
                              #                    label = "Choose spillover matrix file", 
                              #                    multiple = FALSE),
                              #          selectInput("sep_spill", "column separator", choices = c("comma", "semi-column", "tab", "space"), selected = "tab"),
                              #          div(style = 'overflow-x: scroll', DT::dataTableOutput("spill_imported")),
                              #          br(),
                              #          actionButton("set_spillover_matrix", "Set spillover matrix")
                              # ),
                              # tabPanel("Compute",
                              #          width = NULL, height = NULL,
                              #          uiOutput("ui_compute_spill"),
                              #          actionButton("add_spill_param", "Add parameter"),
                              #          br(),
                              #          br(),
                              #          selectizeInput("spill_params", "Spillover parameters", choices = NULL, selected = NULL, multiple = TRUE),
                              #          actionButton("compute_spillover_matrix", "Compute spillover matrix")
                              #          
                              # )
                       ),
                       tabBox(title = "",
                              width = NULL, height = NULL,
                              tabPanel("Import",
                                       width = NULL, height = NULL,
                                       fileInput(inputId = "spill_file", 
                                                 label = "Choose spillover matrix file", 
                                                 multiple = FALSE),
                                       selectInput("sep_spill", "column separator", choices = c("comma", "semi-column", "tab", "space"), selected = "tab"),
                                       div(style = 'overflow-x: scroll', DT::dataTableOutput("spill_imported")),
                                       br(),
                                       actionButton("set_spillover_matrix", "Set spillover matrix")
                              ),
                              tabPanel("Compute",
                                       width = NULL, height = NULL,
                                       uiOutput("ui_compute_spill"),
                                       actionButton("add_spill_param", "Add parameter"),
                                       br(),
                                       br(),
                                       selectizeInput("spill_params", "Spillover parameters", choices = NULL, selected = NULL, multiple = TRUE),
                                       actionButton("compute_spillover_matrix", "Compute spillover matrix")
                                       
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
                                       checkboxInput("legend_comp", "show legend", value = FALSE),
                                       checkboxInput("norm_comp", "normalize (set max to 1)", value = TRUE),
                                       checkboxInput("smooth_comp", "smooth", value = FALSE),
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
                              tabPanel("Add",
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
                                       plotOutput("plotGate",
                                                  brush = "plot_brush",
                                                  click = "plot_click",
                                                  dblclick = "plot_dblclick")
                                       
                              ),
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
                       tabBox(title = "Gating hierarchy",
                              width = NULL, height = NULL,
                              tabPanel("Tree",
                                       plotOutput("tree")
                              ),
                              tabPanel("Plot gates",
                                       plotOutput("plot_gh")),
                              tabPanel("Options",
                                       numericInput("nrow", label = "Number of rows", value = 2)),
                              tabPanel("Save",
                                       numericInput("width_plot_gh", label = "width", value = 7),
                                       numericInput("height_plot_gh", label = "height", value = 7),
                                       downloadButton("download_plot_gh", "Save plot")     
                              )
                              
                       )
                )
                
              )
      ),
      tabItem(tabName = "Plot_tab",
              fluidRow(
                column(width = 4,
                       tabBox(title = "",
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
                                       selectizeInput("xvar", 
                                                      multiple = TRUE,
                                                      label = "x variable", 
                                                      choices = NULL, 
                                                      selected = NULL),
                                       selectizeInput("yvar", 
                                                      multiple = TRUE,
                                                      label = "y variable", 
                                                      choices = NULL, 
                                                      selected = NULL),
                                       selectizeInput("color_var", 
                                                      multiple = TRUE,
                                                      label = "color variable",
                                                      choices = "none",
                                                      selected = "none"),
                                       selectInput("split_variable",
                                                   label = "select variable used to split plots",
                                                   choices = c("x variable", "y variable", "color variable"),
                                                   selected = "x variable"
                                       ),
                                       numericInput("nrow_split", label = "Number of rows", value = 1)
                                       
                                       
                              )
                       ) 
                ),
                column(width = 8,
                       tabBox(title = "Plot",
                              width = NULL, height = NULL,
                              tabPanel("Plot",
                                       actionButton("update_plot", "update"),
                                       br(),
                                       uiOutput("ui_plot")
                                       #plotOutput("plot_focus", height = input$nrow_split * 300)
                                       
                              ),
                              tabPanel("Options",
                                       selectInput("plot_type", label = "plot type",
                                                   choices = c("hexagonal", "histogram", "dots", "contour"),
                                                   selected = "histogram"),
                                       checkboxInput("legend", "show legend", value = TRUE),
                                       checkboxInput("norm", "normalize (set max to 1)", value = TRUE),
                                       checkboxInput("smooth", "smooth", value = FALSE),
                                       checkboxInput("ridges", "ridges", value = FALSE),
                                       selectizeInput("facet_var", 
                                                      multiple =TRUE,
                                                      label = "facet variables", 
                                                      choices = "name", 
                                                      selected = "name"),
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
                                       selectInput("legend_pos", label = "legend position",
                                                   choices = c("right", "top", "left", "bottom"),
                                                   selected = "right"),
                                       numericInput("bin_number", label = "number of bins", value = 50),
                                       numericInput("alpha", label = "alpha", value = 0.5),
                                       numericInput("size", label = "size", value = 1),
                                       numericInput("row_size", label = "row size (px)", value = 400)
                              ),
                              tabPanel("Save",
                                       numericInput("width_plot", label = "width", value = 5),
                                       numericInput("height_plot", label = "height", value = 5),
                                       downloadButton("download_plot", "Save plot")
                              )
                              
                       )
                       
                )
                
              )
      ),
      tabItem(tabName = "Stat_tab",
              fluidRow(
                column(width = 4,
                       tabBox(title = "",
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
                                       actionButton("update_plot_stat", "update"),
                                       br(),
                                       uiOutput("ui_plot_stat")
                                       
                              ),
                              tabPanel("Options",
                                       selectInput("plot_type_stat", label = "plot type",
                                                   choices = c("bar", "tile"),
                                                   selected = "bar"),
                                       checkboxInput("legend_stat", "show legend", value = TRUE),
                                       checkboxInput("free_y_scale", "free y scale", value = TRUE),
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
                                                   selected = "none"),
                                       numericInput("row_size_stat", label = "row size (px)", value = 150),
                                       numericInput("strip_text_angle", label = "strip text angle", value = 0)
                              ),
                              tabPanel("Save",
                                       numericInput("width_plot_stat", label = "width", value = 5),
                                       numericInput("height_plot_stat", label = "height", value = 5),
                                       downloadButton("download_plot_stat", "Save plot")
                              )
                       )
                )
                
                
              )
      ),
      tabItem(tabName = "Sub_tab",
              fluidRow(
                column(width = 6,
                       tabBox(title = "",
                              width = NULL, height = NULL,
                              tabPanel("Samples",
                                       div(style = 'overflow-x: scroll', DT::dataTableOutput("sub_sample_table"))
                              ),
                              tabPanel("Subset",
                                       selectizeInput("gate_sub_sample", 
                                                      label = "subset", 
                                                      choices = "root", 
                                                      selected = "root",
                                                      multiple = TRUE)
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
                       tabBox(title = "",
                              width = NULL, height = NULL,
                              tabPanel("Variables",
                                       div(style = 'overflow-x: scroll', DT::dataTableOutput("tSNE_variables_table"))
                              ),
                              tabPanel("Options",
                                       
                                       selectInput("y_trans_tsne", 
                                                   label = "Transform variables:", 
                                                   choices = c("log10", "asinh", "identity", "default"), 
                                                   selected = "default"),
                                       selectInput("dim_red_method", label = "method", choices = c("tSNE" , "umap"), selected = "tSNE"),
                                       conditionalPanel(condition = "input.dim_red_method == 'tSNE'",
                                                        numericInput("perplexity", "perplexity", 50)
                                                        )
                                       
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
      ),
      tabItem(tabName = "Cluster_tab",
              fluidRow(
                column(width = 6,
                       tabBox(title = "",
                              width = NULL, height = NULL,
                              tabPanel("Variables",
                                       div(style = 'overflow-x: scroll', DT::dataTableOutput("clustering_variables_table"))
                              ),
                              tabPanel("Options",
                                       selectInput("y_trans_clustering",
                                                   label = "Transform variables:",
                                                   choices = c("log10", "asinh", "identity", "default"),
                                                   selected = "default"),
                                       numericInput("cluster_dc", "dc", 5),
                                       numericInput("cluster_alpha", "alpha", 0.0001)
                              ),
                              tabPanel("Cluster",
                                       actionButton("start_clustering", "Find clusters"),
                                       br(),
                                       br(),
                                       "Summary",
                                       br(),
                                       verbatimTextOutput("summary_cluster")
                              )
                       )
                )
              )
      ),
      tabItem(tabName = "Save_tab",
              fluidRow(
                column(width = 6,
                       tabBox(title = "",
                              width = NULL, height = NULL,
                              tabPanel("Gating set",
                                       selectInput("export_format",
                                                   label = "format",
                                                   choices = c("Cytobank", "FlowJo"),
                                                   selected = "FlowJo"),
                                       downloadButton("export_gating_set", "Export")
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
                menuItem("Metadata",
                         tabName = "Meta_tab", 
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
                menuItem("Clustering",
                         tabName = "Cluster_tab",
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
                menuItem("Save",
                         tabName = "Save_tab", 
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
    dashboardHeader(title = "flowR"),
    sidebar,
    body
  )
  
  return(ui)
}