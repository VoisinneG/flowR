#' Run flowR GUI
#' @param module_name name of shiny module to include in the GUI
#' @param shiny.maxRequestSize file size limit (in MB)
#' @import shiny
#' @export
run_flowR2 <- function(module_names = NULL, shiny.maxRequestSize = 1000){
  
  options(shiny.maxRequestSize = shiny.maxRequestSize * 1024^2)
  
  app <- shinyApp(ui = flowR_ui2(module_names = module_names), 
           server = function(input, output, session){
             flowR_server2(input = input,
                          output=output,
                          session = session,
                          module_names = module_names)}
           )
  
  runApp(app)
  
}