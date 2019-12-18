#' FlowR UI function
#' @param user_module_name name of a user shiny module (ui function) to integrate to the app.
#' The name of the ui function should be made of the parameter 'user_module_name' with the suffix 'UI'.
#' @import shiny
#' @import shinydashboard
#' @export
flowR_ui <- function(user_module_name = NULL) {
  
  module_ui_name <- paste(user_module_name, "UI", sep = "")
  
  if(!is.null(user_module_name)){
    module_ui_function <- function(...){
      do.call(module_ui_name, list(...) )
    }
  }

  body <- {dashboardBody(
    textOutput("flow_set_name"),
    br(),
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
              plotting2UI(id = "plotting_module")
      ),
      tabItem(tabName = "Stat_tab",
              statsUI(id = "stats_module")
      ),
      tabItem(tabName = "Sub_tab",
              subsampleUI(id = "subsample_module")
      ),
      tabItem(tabName = "Dim_red_tab",
              dimRedUI(id = "dim_reduction_module")
      ),
      tabItem(tabName = "Cluster_tab",
              clusterUI(id = "cluster_module")
      ),
      if(!is.null(user_module_name)){
        tabItem(tabName = "user_module_tab",
                module_ui_function(id = "user_module") )
      }else{
        tabItem(tabName = "user_module_tab", br())
      },
      tabItem(tabName = "Flowsets_tab",
              flowsetsUI(id = "flowsets_module")
      )
      
    )
      
  )}

  sidebar <- {dashboardSidebar(
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
                menuItem("Dim. reduction",
                         tabName = "Dim_red_tab",
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
                if(!is.null(user_module_name)){
                  menuItem(user_module_name,
                           tabName = "user_module_tab", 
                           startExpanded = FALSE,
                           icon = icon("check-circle")
                  )
                },
                menuItem("Flow-sets",
                         tabName = "Flowsets_tab", 
                         startExpanded = FALSE,
                         icon = icon("check-circle")
                ),
                menuItem("General controls",
                         tabName = "General_tab",
                         startExpanded = FALSE,
                         icon = icon("check-circle"),
                         checkboxInput("apply_comp", "apply compensation", TRUE),
                         checkboxInput("apply_trans", "apply transformation", TRUE),
                         selectInput("flow_set", "Select flow set", choices = NULL, selected = NULL),
                         br()
                )
    )
    
    
  )}
  
  ui <- dashboardPage(
    dashboardHeader(title = "flowR"),
    sidebar,
    body
  )
  
  return(ui)
}