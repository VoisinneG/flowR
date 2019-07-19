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
              gatingUI(id = "gating_module")
      ),
      tabItem(tabName = "Plot_tab",
              displayUI("plot_module", module_ui_name = "plotGatingSetInput", simple_plot = FALSE)
      ),
      tabItem(tabName = "Stat_tab",
              displayUI("statistics_module", module_ui_name = "plotStatInput")
      ),
      tabItem(tabName = "Sub_tab",
              subsampleUI(id = "subsample_module")
      ),
      tabItem(tabName = "TSNE_tab",
              dimRedUI(id = "dim_reduction_module")
      ),
      tabItem(tabName = "Cluster_tab",
              clusterUI(id = "cluster_module")
      ),
      tabItem(tabName = "Save_tab",
              saveWorkspaceUI(id = "save_module")
      )
      #         fluidRow(
      #           column(width = 6,
      #                  tabBox(title = "",
      #                         width = NULL, height = NULL,
      #                         tabPanel("Gating set",
      #                                  selectInput("export_format",
      #                                              label = "format",
      #                                              choices = c("Cytobank", "FlowJo"),
      #                                              selected = "FlowJo"),
      #                                  downloadButton("export_gating_set", "Export")
      #                         )
      #                  )
      #           )
      #         )
      # )
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