#' Search, select and load modules
#' @param id shiny id
#' @importFrom shinydashboard box
#' @importFrom DT DTOutput
#' @import shiny
#' @export
#' @examples 
#' \dontrun{
#' library(shiny)
#' library(shinydashboard)
#' library(flowR)
#' library(DT)
#' 
#' if (interactive()){
#'   
#'   ui <- dashboardPage(
#'     dashboardHeader(title = "Modules"),
#'     sidebar = dashboardSidebar(disable = TRUE),
#'     body = dashboardBody(
#'       ModulesUI("module")
#'     )
#'   )
#'   
#'   server <- function(input, output, session) {
#'     rval <- reactiveValues(modules = c("Import"))
#'     rval <- callModule(Modules, "module", rval = rval)
#'   }
#'   
#'   shinyApp(ui, server)
#'   
#' }
#' }

ModulesUI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(width = 4,
           box(width = NULL, height = NULL, title = "Module selection",
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
              DTOutput(ns("mod_description")),
              actionButton(ns("add_modules"), "add selected modules")
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
#'   \item{modules}{: Names of pre-selected modules}
#' }
#' @return The input reactivevalues object 'rval' with updated elements :
#' \describe{
#'   \item{modules}{: names of selected modules}
#' }
#' @import shiny
#' @importFrom utils sessionInfo
#' @importFrom DT renderDT
#' @rdname ModulesUI
#' @export
Modules <- function(input, output, session, rval) {

  rval_mod <- reactiveValues(modules = NULL, df_module_info = NULL, packages = NULL, choices = NULL)
  
  
  observe({
    rval_mod$packages <- c(names(sessionInfo()$otherPkgs), ".GlobalEnv")
    updateSelectizeInput(session, "packages", choices = rval_mod$packages, selected = c("flowR", ".GlobalEnv"))
  })
  
  observe({
      updateSelectizeInput(session, "mod_selection",
                           choices = rval_mod$choices,
                           selected = rval$modules)
  })
  
  observeEvent(input$apply, {
    if(!is.null(input$mod_selection)){
      rval$modules <- union(input$mod_selection, "Modules")
    }
  })
  
  observe({
    
    df_info_list <- list()
    for(pack in input$packages){
      if(pack == ".GlobalEnv"){
        pack_objs <- ls(envir=.GlobalEnv)
        if(length(pack_objs)>0){
          mod_ui_name <- pack_objs[grep("UI$", pack_objs)]
          if(length(mod_ui_name)>0){
            mod_name <- unlist(strsplit(mod_ui_name, split = "UI"))
            mod_is_valid <- sapply(1:length(mod_name), function(x){
              !is.na(match( mod_name[x], pack_objs ))})
            if(sum(mod_is_valid)>0){
              mod_name <- mod_name[mod_is_valid]
              df_info_list[[pack]] <- data.frame(package = rep(pack, length(mod_name)), 
                                                 module = mod_name, 
                                                 description = rep(NA, length(mod_name)))
            }
          }
          
        }
        
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
                  format_info(module_info[idx_ui_fonction[1]])
                }else if(length(idx_server_fonction)>0){
                  format_info(module_info[idx_server_fonction[1]])
                }else{
                  NA
                }
              })
              
            }else{
              description <- rep(NA, length(mod_name))
            }
            df_info_list[[pack]] <- data.frame(package = rep(pack, length(mod_name)),
                                               module = mod_name,
                                               description = description)
          }else{
            df_info_list[[pack]] <- NULL
          }
          
        }
       
        
        
      }
    }
    
    rval_mod$df_module_info <- do.call(rbind, df_info_list)
    rval_mod$choices <- union(rval_mod$choices, rval_mod$df_module_info$module)
    
  })
  
  output$mod_description <- renderDT({
    DT::datatable(rval_mod$df_module_info, rownames = FALSE)
  })
  
  observeEvent(input$add_modules, {
    new_modules <- rval_mod$df_module_info$module[input$mod_description_rows_selected]
    updateSelectizeInput(session, "mod_selection",
                         selected = union(new_modules, input$mod_selection))
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

#' Suppresses the function name from its description as 
#' returned by 'get_package_functions_info()'
#' @param info a character string
#' @return a character string
format_info <- function(info){
  s <- regexpr(" +", info)
  s <- substr(info, start = s + attr(s, "match.length"), stop = nchar(info))
  return(s)
}

##################################################################################
# Tests
##################################################################################
# 
# library(shiny)
# library(shinydashboard)
# library(flowR)
# library(DT)
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
#     rval <- reactiveValues(modules = c("Import"))
#     rval <- callModule(Modules, "module", rval = rval)
#   }
# 
#   shinyApp(ui, server)
# 
# }
