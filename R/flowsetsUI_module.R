#' @title   flowsetsUI and flowsets
#' @description  A shiny Module that imports a flow set and gates from fcs files and a workspace 
#' @param id shiny id
#' @importFrom shinydashboard box
#' @import DT
#' @import shiny
flowsetsUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(
    column(width = 6,
           box(title = "Flow-set",
               width = NULL, height = NULL,
               selectInput(ns("flow_set"), "Select flow set", choices = NULL, selected = NULL),
               actionButton(ns("delete"), "delete"),
               downloadButton(ns("save_gating_set"), "Save"),
               downloadButton(ns("save_all"), "Save all")
           )
    ),
    column(width = 6,
           box(title = "Hierarchy",
               width = NULL, height = NULL,
               plotOutput(ns("flow_set_tree"))
           )  
    )
  )
  
}

#' import server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @return a reactivevalues object with values "df_files", "flow_set_imported" and "gates_flowCore"
#' @importFrom CytoML parseWorkspace openWorkspace
#' @import flowWorkspace
#' @import flowCore
#' @import ncdfFlow
#' @import shiny
#' @import DT
#' @export
#' @rdname flowsetsUI
flowsets <- function(input, output, session, rval) {
  
  observeEvent( c(names(rval$flow_set_list), rval$flow_set_selected), {
    updateSelectInput(session, "flow_set", choices = names(rval$flow_set_list), selected = rval$flow_set_selected)
  })
  
  observeEvent(input$flow_set, {
    rval$flow_set_selected <- input$flow_set
  })
  
  observeEvent(input$delete, {
    validate(need(input$flow_set, "no flow set selected"))
    idx <- which(names(rval$flow_set_list) == input$flow_set | sapply(rval$flow_set_list, function(x){x$parent}) ==  input$flow_set)
    rval$flow_set_list <- rval$flow_set_list[-idx]
  })
  
  output$flow_set_tree <- renderPlot({
    
    validate(need(length(rval$flow_set_list)>1, "no hierarchy"))
    
    gR = new("graphNEL", nodes = names(rval$flow_set_list), edgemode = "directed")
    
    for(i in 1:length(rval$flow_set_list)){
      if(!is.null(rval$flow_set_list[[i]]$parent)){
        
        gR = graph::addEdge(rval$flow_set_list[[i]]$parent,  rval$flow_set_list[[i]]$name, gR)
        
      }
    }
    
    renderGraph(layoutGraph(gR, 
                            attrs=list(node=list(fixedsize = FALSE,
                                                 fillcolor = "gray",
                                                 fontsize = 12,
                                                 shape = "ellipse")
                                       )
                            )
                )
    
  })
  
  output$save_gating_set <- downloadHandler(
    
    filename = function(){
      paste(input$flow_set,".rda", sep = "")
    },
    
    content = function(file) {
      res <- list()
      df <- get_data_gs(gs = rval$gating_set,
                          sample = pData(rval$gating_set)$name, 
                          subset = "root",
                          spill = NULL,
                          return_comp_data = FALSE)
      
      res[[1]] <- list(data = df,
                       flow_set = rval$flow_set,
                       metadata = pData(rval$gating_set),
                       spill = rval$df_spill,
                       transformation = rval$transformation,
                       trans_parameters = rval$trans_parameters,
                       gates = rval$gates_flowCore[setdiff(getNodes(rval$gating_set), "root")],
                       parent = NULL,
                       name = input$flow_set)
                  
      
      
      save(res, file=file)
      
    }
  )
  
  output$save_all <- downloadHandler(
    
    filename = function(){
      "all_flowsets.rda"
    },
    
    content = function(file) {
      
      res_all <- list()
      
      for(i in 1:length(rval$flow_set_list)){
        gs <- GatingSet(rval$flow_set_list[[i]]$flow_set)
        df <- get_data_gs(gs = gs,
                          sample = pData(gs)$name, 
                          subset = "root",
                          spill = NULL,
                          return_comp_data = FALSE)
        
        res_all[[i]] <- list(data = df,
                             flow_set = rval$flow_set_list[[i]]$flow_set,
                             metadata = pData(gs),
                             spill = rval$df_spill,
                             transformation = rval$transformation,
                             trans_parameters = rval$trans_parameters,
                             gates = rval$gates_flowCore,
                             parent = rval$flow_set_list[[i]]$parent,
                             name = rval$flow_set_list[[i]]$name)
                    
      }
      
      save(res_all, file=file)
      
    }
  )
  
  return(rval)
  
}