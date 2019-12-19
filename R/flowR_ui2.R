#' FlowR UI function
#' @param user_module_name name of a user shiny module (ui function) to integrate to the app.
#' The name of the ui function should be made of the parameter 'user_module_name' with the suffix 'UI'.
#' @import shiny
#' @import shinydashboard
#' @export
flowR_ui2 <- function(module_names = NULL) {
  
  module_names_ui <- paste(module_names, "UI", sep = "")
  tab_elements <- list()
  menu_elements <- list()
  
  for(i in 1:length(module_names)){
    
    mod_name <- module_names[i]
    mod_name_ui <- module_names_ui[i]
    
    tab_elements[[mod_name]] <- tabItem(tabName = paste(mod_name, "tab", sep="_"),
                                       do.call(mod_name_ui, list(id = paste(mod_name, "module", sep="_") )))
    
    menu_elements[[mod_name]] <- menuItem(mod_name,
                                     tabName = paste(mod_name, "tab", sep="_"), 
                                     startExpanded = FALSE,
                                     icon = icon("check-circle"))
  }
  
  tab_elements<-unname(tab_elements)

  body <- {
    dashboardBody(
      textOutput("flow_set_name"),
      br(),
      fluidRow(
        valueBoxOutput("progressBox", width = 3),
        valueBoxOutput("progressBox2", width = 3),
        valueBoxOutput("progressBox3", width = 3),
        valueBoxOutput("progressBox4", width = 3)
      ),
      do.call(tabItems, tab_elements)
  )}

  sidebar <- {dashboardSidebar(
    sidebarMenu(id = "menu",
                tagList(menu_elements),
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