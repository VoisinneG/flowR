#' Run flowR GUI
#' @param module_name name of shiny module to include in the GUI
#' @param shiny.maxRequestSize file size limit (in MB)
#' @import shiny
#' @export
run_flowR <- function(module_name = NULL, shiny.maxRequestSize = 1000){
  
  options(shiny.maxRequestSize = shiny.maxRequestSize * 1024^2)
  
  app <- shinyApp(ui = flowR_ui(user_module_name = module_name), 
           server = function(input, output, session){
             flowR_server(input = input,
                          output=output,
                          session = session,
                          user_module_name = module_name)}
           )
  
  runApp(app)
  
}