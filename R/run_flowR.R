#' @import shiny
#' @export
run_flowR <- function(module_name = NULL){
  options(shiny.maxRequestSize = 1000*1024^2)
  #module_name.. <- "cluster"
  app <- shinyApp(ui = flowR_ui(user_module_name = module_name), 
           server = function(input, output, session){
             flowR_server(input = input,
                          output=output,
                          session = session,
                          user_module_name = module_name)}
           )
  runApp(app)
}