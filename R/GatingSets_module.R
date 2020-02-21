#' Manage and visualize GatingSets and their hierarchy
#' @param id shiny id
#' @importFrom shinydashboard box
#' @import shiny
#' @export
#' @examples 
#' \dontrun{
#' library(shiny)
#' library(shinydashboard)
#' library(flowCore)
#' 
#' if (interactive()){
#'   
#'   ui <- dashboardPage(
#'     dashboardHeader(title = "GatingSets"),
#'     sidebar = dashboardSidebar(disable = TRUE),
#'     body = dashboardBody(
#'       fluidRow(
#'         column(8, box(width = NULL, GatingSetsUI("module")))
#'       )
#'     )
#'   )
#'   
#'   server <- function(input, output, session) {
#'     
#'     rval <- reactiveValues()
#'     
#'     observe({
#'       utils::data("GvHD", package = "flowCore")
#'       gs <- GatingSet(GvHD)
#'       rval$gating_set_list <- list( GvHD = list(gating_set = gs, parent = NULL),
#'                                     subset = list(gating_set = gs, parent = "GvHD"))
#'     })
#'     
#'     rval <- callModule(GatingSets, "module", rval = rval)
#'     
#'   }
#'   
#'   shinyApp(ui, server)
#'   
#' }}
GatingSetsUI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(width = 6,
           box(title = "Flow-set",
               width = NULL, height = NULL,
               selectInput(ns("gating_set"), "Select flow set", choices = NULL, selected = NULL),
               actionButton(ns("delete"), "delete"),
               downloadButton(ns("save_gating_set"), "Save"),
               downloadButton(ns("save_all"), "Save all")
           )
    ),
    column(width = 6,
           box(title = "Hierarchy",
               width = NULL, height = NULL,
               plotOutput(ns("gating_set_tree"))
           )  
    )
  )
  
}

#' GatingSets module server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param rval a reactivevalues object with elements :
#' \describe{
#'   \item{gating_set_list}{a named list with each element being itself a list with:}
#'   \describe{
#'     \item{gating_set}{: a GatingSet}
#'     \item{parent}{: the name of its parent GatingSet}
#'   }
#'}
#' @return The reactivevalues object \code{rval} with updated elements :
#' \describe{
#'   \item{gating_set_list}{a named list with each element being itself a list with:}
#'   \describe{
#'     \item{gating_set}{: a GatingSet}
#'     \item{parent}{: the name of its parent GatingSet}
#'   }
#'   \item{gating_set_selected}{Name of the selected GatingSet}
#' }
#' @import shiny
#' @importFrom flowWorkspace GatingSet pData gs_get_pop_paths sampleNames
#' @importFrom graph addEdge
#' @importFrom Rgraphviz renderGraph layoutGraph
#' @importFrom methods new
#' @export
#' @rdname GatingSetsUI
GatingSets <- function(input, output, session, rval) {
  
  observeEvent( c(names(rval$gating_set_list), rval$gating_set_selected), {
    updateSelectInput(session, "gating_set", 
                      choices = names(rval$gating_set_list), 
                      selected = rval$gating_set_selected)
  })
  
  observeEvent(input$gating_set, {
    rval$gating_set_selected <- input$gating_set
  })
  
  observeEvent(input$delete, {
    validate(need(input$gating_set, "no GatingSet selected"))
    children <- union(input$gating_set,
                      get_all_descendants(named_list = rval$gating_set_list, names = input$gating_set))
    idx <- which(names(rval$gating_set_list) %in%  children)
    rval$gating_set_list <- rval$gating_set_list[-idx]
  })
  
  ### Plot Hierarchy ###############################################################################
  
  output$gating_set_tree <- renderPlot({
    
    validate(need(length(rval$gating_set_list)>0, "no hierarchy"))
    
    gR = methods::new("graphNEL", nodes = names(rval$gating_set_list), edgemode = "directed")

    for(i in 1:length(rval$gating_set_list)){
      
      if(!is.null(rval$gating_set_list[[i]]$parent)){
        if(rval$gating_set_list[[i]]$parent %in% names(rval$gating_set_list) ){
          gR = graph::addEdge(rval$gating_set_list[[i]]$parent,  names(rval$gating_set_list)[i], gR)
        }else{
          gR = graph::addEdge(names(rval$gating_set_list)[i],  names(rval$gating_set_list)[i], gR)
        }
      }else{
        gR = graph::addEdge(names(rval$gating_set_list)[i],  names(rval$gating_set_list)[i], gR)
      }
    }
    
    Rgraphviz::renderGraph(Rgraphviz::layoutGraph(gR, 
                            attrs=list(node=list(fixedsize = TRUE,
                                                 fillcolor = "gray",
                                                 fontsize = 12,
                                                 shape = "ellipse")
                                       )
                            )
                )
    
  })
  
  ### Save GatingSet(s) ############################################################################
  
  output$save_gating_set <- downloadHandler(
    
    filename = function(){
      paste(input$gating_set,".rda", sep = "")
    },
    
    content = function(file) {
      
      res <- list()
      
      gs <- rval$gating_set_list[[input$gating_set]]$gating_set
      df <- get_data_gs(gs = gs,
                        sample = flowWorkspace::sampleNames(gs), 
                        subset = "root",
                        spill = NULL,
                        return_comp_data = FALSE)
      
      res[[input$gating_set]] <- list(data = df,
                                      flow_set = gs@data,
                                      metadata = pData(gs),
                                      compensation = gs@compensation,
                                      transformation = gs@transformation,
                                      trans_parameters = rval$trans_parameters,
                                      gates = get_gates_from_gs(gs),
                                      parent = NULL,
                                      name = input$gating_set)
                       
      
      save(res, file=file)
      
    }
  )
  
  output$save_all <- downloadHandler(
    
    filename = function(){
      "all_flowsets.rda"
    },
    
    content = function(file) {
      
      res <- list()
      
      for(i in 1:length(rval$gating_set_list)){
        gs <- rval$gating_set_list[[i]]$gating_set
        df <- get_data_gs(gs = gs,
                          sample = flowWorkspace::sampleNames(gs), 
                          subset = "root",
                          spill = NULL,
                          return_comp_data = FALSE)
        
        name <- names(rval$gating_set_list)[i]
        
        res[[name]] <- list(data = df,
                            flow_set = gs@data,
                            metadata = pData(gs),
                            compensation = gs@compensation,
                            transformation = gs@transformation,
                            trans_parameters = rval$trans_parameters,
                            gates = get_gates_from_gs(gs),
                            parent = rval$gating_set_list[[i]]$parent,
                            name = name)
        
                             
      }
      
      save(res, file=file)
      
    }
  )
  
  return(rval)
  
}


### Tests #########################################################################################
# 
# library(shiny)
# library(shinydashboard)
# library(flowCore)
# 
# if (interactive()){
# 
#   ui <- dashboardPage(
#     dashboardHeader(title = "GatingSets"),
#     sidebar = dashboardSidebar(disable = TRUE),
#     body = dashboardBody(
#       fluidRow(
#         column(8, box(width = NULL, GatingSetsUI("module")))
#       )
#     )
#   )
# 
#   server <- function(input, output, session) {
# 
#     rval <- reactiveValues()
# 
#     observe({
#       utils::data("GvHD", package = "flowCore")
#       gs <- GatingSet(GvHD)
#       rval$gating_set_list <- list( GvHD = list(gating_set = gs, parent = NULL),
#                                     subset = list(gating_set = gs, parent = "GvHD"))
#       rval$gating_set_list <- list( GvHD = list(gating_set = gs, parent = NULL) )
#     })
# 
#     rval <- callModule(GatingSets, "module", rval = rval)
# 
#   }
# 
#   shinyApp(ui, server)
# 
# }