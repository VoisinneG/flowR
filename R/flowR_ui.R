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
      sidebarMenu(menuItemOutput("menu"))
  )}
  
  ui <- dashboardPage(
    dashboardHeader(title = "flowR"),
    sidebar,
    body
  )
  
  return(ui)
}