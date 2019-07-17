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
              transformUI(id = "transform_module")
      ),
      tabItem(tabName = "Comp_tab",
              compensationUI(id = "compensation_module")
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
              displayUI("display_module")
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
              subsampleUI(id = "subsample_module")
      ),
      tabItem(tabName = "TSNE_tab",
              dimRedUI(id = "dim_reduction_module")
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