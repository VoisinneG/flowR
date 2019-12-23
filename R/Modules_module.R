#' Search, select and load other modules
#' @param id shiny id
#' @importFrom shinydashboard box tabBox
#' @import shiny
#' @export
ModulesUI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(width = 4,
           box(width = NULL, height = NULL, title = "Module selection",
               #plotGatingSet2Input(id = ns("plot_module"))
               selectizeInput(ns("mod_selection"), 
                              "Selected modules", 
                              choices = NULL, 
                              selected = NULL, 
                              multiple = TRUE),
               actionButton(ns("apply"), "apply selection")
           )
    ),
    column(width = 8,
           box(title = "Search for modules",
               width = NULL, height = NULL,
               selectizeInput(ns("packages"), 
                              "Select Packages", 
                              choices = NULL, 
                              selected = NULL, 
                              multiple = TRUE),
              DTOutput(ns("mod_description"))
           )
    )
    
  )
  
}


#' server function for the shiny module Modules
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param rval reactivevalues object with the following elements :
#' \describe{
#'   \item{gating_set}{: a GatingSet object}
#' }
#' @return The updated reactiveValues object \code{rval}
#' @import shiny
#' @rdname ModulesUI
#' @export
Modules <- function(input, output, session, rval) {

  rval_mod <- reactiveValues(modules = NULL, df_module_info = NULL, packages = NULL)
  
  observe({
    packages <- names(sessionInfo()$otherPkgs)
    updateSelectizeInput(session, "packages", choices = packages, selected = NULL)
  })
  
  
  
  observe({
    
      updateSelectizeInput(session, "mod_selection",
                           choices = union(rval_mod$df_module_info$module, 
                                           names(rval$menu_elements)),
                           selected = names(rval$menu_elements))
    
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
          
          mod_is_valid <- sapply(1:length(mod_name), function(x){
            !is.na(match( mod_name[x], pack_objs ))})
          
          if(sum(mod_is_valid)>0){
            
            mod_ui_name <- mod_ui_name[mod_is_valid]
            mod_name <- mod_name[mod_is_valid]

            module_info <- get_package_functions_info(package_name = pack)
           
            if(length(module_info)>0){
              description <- sapply(1:length(mod_name), function(x){
                idx_server_fonction <- grep(paste0("^", mod_name[x], " "), module_info)
                idx_ui_fonction <- grep(paste0("^", mod_ui_name[x], " "), module_info)
                if(length(idx_ui_fonction)>0){
                  module_info[idx_ui_fonction[1]]
                }else if(length(idx_server_fonction)>0){
                  module_info[idx_server_fonction[1]]
                }else{
                  NA
                }
              })
              
            }else{
              description <- rep(NA, length(mod_name))
            }
            df_info_list[[pack]] <- data.frame(package = rep(pack, length(mod_name)), 
                                               module = mod_name, 
                                               "functions" = paste(mod_name, mod_ui_name, sep = "/"),
                                               description = description)
            print(description)
          }else{
            df_info_list[[pack]] <- NULL
          }
          
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

#' get description of package's functions
#' @param package_name Name of the package
#' @return A vector with the description of the functions in the package 
get_package_functions_info <- function(package_name){
  info <- library(help = package_name, character.only = TRUE)
  pack_info <- info$info[[2]]
  idx_blank <- grep("^ ", pack_info)
  if(length(idx_blank)>0){
    pack_info[idx_blank] <- sapply(pack_info[idx_blank], function(x){
      substr(x, 
             start = regexpr("[^ ]", x), 
             stop = nchar(x))
    })
  }
  pack_info[idx_blank-1] <- paste(pack_info[idx_blank-1], pack_info[idx_blank])
  pack_info <- pack_info[-idx_blank]
  return(pack_info)
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
