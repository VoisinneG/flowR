#' @title   ModulesUI and Modules
#' @description  A shiny module to select shiny modules
#' @param id shiny id
#' @importFrom shinydashboard box tabBox
#' @import shiny
#' @export
ModulesUI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(width = 4,
           box(width = NULL, height = NULL, title = "Selection",
               #plotGatingSet2Input(id = ns("plot_module"))
               selectizeInput(ns("packages"), 
                              "Packages", 
                              choices = NULL, 
                              selected = NULL, 
                              multiple = TRUE),
               selectizeInput(ns("mod_selection"), 
                              "Select modules", 
                              choices = NULL, 
                              selected = NULL, 
                              multiple = TRUE),
               actionButton(ns("apply"), "apply selection")
           )
    ),
    column(width = 8,
           box(title = "Module description",
                  width = NULL, height = NULL,
                  DTOutput(ns("mod_description"))
           )
    )
    
  )
  
}


#' Modules server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param rval reactivevalues object with the following elements :
#' \describe{
#'   \item{gating_set}{: a GatingSet object}
#' }
#' @return The updated reactiveValues object \code{rval}
#' @import shiny
#' @export
#' @rdname ModulesUI
Modules <- function(input, output, session, rval) {

  rval_mod <- reactiveValues(modules = NULL, df_module_info = NULL, packages = NULL)
  
  observe({
    packages <- names(sessionInfo()$otherPkgs)
    updateSelectizeInput(session, "packages", choices = packages, selected = NULL)
  })
  
  
  observe({
    
    df_info_list <- list()
    for(pack in c(input$packages, "local_env")){
      if(pack == "local_env"){
        mod_ui_name <- ls()[grep("UI$", ls())]
        df_info_list[[pack]] <- data.frame(package = rep(pack, length(mod_ui_name)), 
                                           module = mod_ui_name, 
                                           description = rep(NA, length(mod_ui_name)))
      }else{
        info <- library(help = pack, character.only = TRUE)
        pack_objs <- ls(paste0("package:", pack))
        idx_ui <- grep("UI$", pack_objs)
        if(length(idx_ui)>0){
          mod_ui_name <- pack_objs[idx_ui]
          module_info <- info$info[[2]]
          idx_mod <- sapply(mod_ui_name, function(x){
            idx <- grep(paste0("^", x), module_info)
            if(length(idx)>0){
              idx[1]
            }else{
              NA
            }
          })
          df_info_list[[pack]] <- data.frame(package = rep(pack, length(mod_ui_name)), 
                                             module = mod_ui_name, 
                                             description = module_info[idx_mod])
        }else{
          df_info_list[[pack]] <- NULL
        }
        
      }
    }
    
    rval_mod$df_module_info <- do.call(rbind, df_info_list)

  })
  
  output$mod_description <- renderDT({
    DT::datatable(rval_mod$df_module_info, rownames = FALSE)
  })
  
  return(rval)
}


##################################################################################
# Tests
##################################################################################


library(shiny)
library(shinydashboard)
library(flowR)

if (interactive()){

  ui <- dashboardPage(
    dashboardHeader(title = "Modules"),
    sidebar = dashboardSidebar(disable = TRUE),
    body = dashboardBody(
      ModulesUI("module")
    )
  )

  server <- function(input, output, session) {

    rval <- reactiveValues()

    # observe({
    #   utils::data("GvHD", package = "flowCore")
    #   rval$gating_set <- GatingSet(GvHD)
    #   #gs <- load_gs("./inst/ext/gs")
    #   #rval$gating_set <- gs
    # })

    res <- callModule(Modules, "module", rval = rval)

  }

  shinyApp(ui, server)

}
