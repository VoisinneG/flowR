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
    if(!is.null(rval_mod$df_module_info$module)){
      updateSelectizeInput(session, "mod_selection",
                           choices = rval_mod$df_module_info$module,
                           selected = rval_mod$df_module_info$module[1])
    }
  })
  
  observeEvent(input$apply, {
    if(!is.null(input$mod_selection)){
      rval$modules <- input$mod_selection
    }
  })
  
  observe({
    
    df_info_list <- list()
    for(pack in c(input$packages, "local_env")){
      if(pack == "local_env"){
        mod_ui_name <- ls()[grep("UI$", ls())]
        mod_name <- strsplit(mod_ui_name, split = "UI")
        df_info_list[[pack]] <- data.frame(package = rep(pack, length(mod_name)), 
                                           module = mod_name, 
                                           description = rep(NA, length(mod_name)))
      }else{
        info <- library(help = pack, character.only = TRUE)
        pack_objs <- ls(paste0("package:", pack))
        module_info <- info$info[[2]]
        idx_ui <- grep("UI$", pack_objs)
        if(length(idx_ui)>0){
          mod_ui_name <- pack_objs[idx_ui]
          mod_name <- unlist(strsplit(mod_ui_name, split = "UI"))
        }
        print(mod_ui_name)
        
        mod_is_valid <- sapply(1:length(mod_name), function(x){
          !is.na(match( mod_name[x], pack_objs ))})
        
        print(mod_name)
        print(mod_is_valid)
        print(sum(mod_is_valid)>0)
        
        if(sum(mod_is_valid)>0){
          mod_ui_name <- mod_ui_name[mod_is_valid]
          mod_name <- mod_name[mod_is_valid]
          
          info <- library(help = pack, character.only = TRUE)
          module_info <- info$info[[2]]
          if(length(module_info)>0){
            description <- sapply(1:length(mod_name), function(x){
              idx_server_fonction <- grep(paste0("^", mod_name[x], " "), module_info)
              idx_ui_fonction <- grep(paste0("^", mod_ui_name[x], " "), module_info)
              if(length(idx_server_fonction)>0 & length(idx_ui_fonction)>0){
                paste(module_info[idx_server_fonction[1]], 
                      module_info[idx_ui_fonction[1]], sep = "/")
              }else{
                NA
              }
            })
            
          }else{
            description <- rep(NA, length(mod_name))
          }
          df_info_list[[pack]] <- data.frame(package = rep(pack, length(mod_name)), 
                                             module = mod_name, 
                                             modules = paste(mod_name, mod_ui_name, sep = "/"),
                                             description = description)
          print(description)
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
# 
# 
# library(shiny)
# library(shinydashboard)
# library(flowR)
# 
# if (interactive()){
# 
#   ui <- dashboardPage(
#     dashboardHeader(title = "Modules"),
#     sidebar = dashboardSidebar(disable = TRUE),
#     body = dashboardBody(
#       ModulesUI("module")
#     )
#   )
# 
#   server <- function(input, output, session) {
# 
#     rval <- reactiveValues()
# 
#     # observe({
#     #   utils::data("GvHD", package = "flowCore")
#     #   rval$gating_set <- GatingSet(GvHD)
#     #   #gs <- load_gs("./inst/ext/gs")
#     #   #rval$gating_set <- gs
#     # })
# 
#     res <- callModule(Modules, "module", rval = rval)
# 
#   }
# 
#   shinyApp(ui, server)
# 
# }
