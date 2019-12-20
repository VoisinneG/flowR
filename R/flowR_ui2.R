#' FlowR UI function
#' @param user_module_name name of a user shiny module (ui function) to integrate to the app.
#' The name of the ui function should be made of the parameter 'user_module_name' with the suffix 'UI'.
#' @import shiny
#' @import shinydashboard
#' @export
flowR_ui2 <- function() {

  body <- {
    dashboardBody(
      uiOutput("body")
  )}

  sidebar <- {
    dashboardSidebar(
      sidebarMenu(menuItemOutput("menu"))
  )}
  
  ui <- dashboardPage(
    dashboardHeader(title = "flowR"),
    sidebar,
    body
  )
  
  return(ui)
}