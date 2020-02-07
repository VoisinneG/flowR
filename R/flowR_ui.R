#' flowR app main ui function
#' @import shiny
#' @import shinydashboard
#' @export
flowR_ui <- function() {

  body <- {
    dashboardBody(
      uiOutput("body")
  )}

  sidebar <- {
    dashboardSidebar(
      sidebarMenu(id = "tabs", menuItemOutput("menu"))
  )}
  
  ui <- dashboardPage(header = dashboardHeader(title = "flowR"),
                      sidebar = sidebar,
                      body = body)
  
  return(ui)
}