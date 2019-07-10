#' @import shiny
#' @export
run_flowR <- function(){
  options(shiny.maxRequestSize = 1000*1024^2)
  shinyApp(ui = flowR_ui(), server = flowR_server)
}