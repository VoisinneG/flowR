#' Manage and visualize flow-sets and their hierarchy
#' @param id shiny id
#' @importFrom shinydashboard box
#' @import shiny
#' @export
FlowsetsUI <- function(id) {
  
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

#' Flowsets module server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param rval A reactive values object
#' @return The updated reactive values object \code{rval}
#' @import shiny
#' @importFrom flowWorkspace GatingSet pData gs_get_pop_paths
#' @importFrom graph addEdge
#' @importFrom Rgraphviz renderGraph layoutGraph
#' @importFrom methods new
#' @export
#' @rdname FlowsetsUI
Flowsets <- function(input, output, session, rval) {
  
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
    
    gR = methods::new("graphNEL", nodes = names(rval$flow_set_list), edgemode = "directed")
    
    for(i in 1:length(rval$flow_set_list)){
      if(!is.null(rval$flow_set_list[[i]]$parent)){
        
        gR = graph::addEdge(rval$flow_set_list[[i]]$parent,  rval$flow_set_list[[i]]$name, gR)
        
      }
    }
    
    Rgraphviz::renderGraph(Rgraphviz::layoutGraph(gR, 
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
      res <- rval$flow_set_list[input$flow_set]
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
                       gates = rval$gates_flowCore[setdiff(flowWorkspace::gs_get_pop_paths(rval$gating_set), "root")],
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
      
      res_all <- rval$flow_set_list
      
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