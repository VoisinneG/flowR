#' @title saveWorkspaceUI and saveWorkspoce
#' @description  A shiny Module that deals with metadata
#' @param id shiny id
#' @importFrom shinydashboard tabBox
#' @import shiny
saveWorkspaceUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(
    column(width = 6,
           tabBox(title = "",
                  width = NULL, height = NULL,
                  tabPanel("Gating set",
                           selectInput(ns("export_format"),
                                       label = "format",
                                       choices = c("Cytobank", "FlowJo"),
                                       selected = "FlowJo"),
                           downloadButton(ns("export_gating_set"), "Export")
                  )
           )
    )
  )
  
}


#' saveWorkspace server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @return a reactivevalues object with values "df_files", "flow_set_imported" and "gates_flowCore"
#' @importFrom flowWorkspace GatingSet colnames gs_pop_get_gate
#' @importFrom flowCore asinhtGml2_trans transformList transform compensate
#' @importFrom CytoML GatingSet2flowJo GatingSet2cytobank
#' @importFrom scales trans_new
#' @import shiny
#' @export
#' @rdname saveWorkspaceUI
saveWorkspace <- function(input, output, session, rval) {
  
  output$export_gating_set <- downloadHandler(
    
    filename = function(){
      switch(input$export_format,
             "FlowJo" = "workspace_flowJo.wsp",
             "Cytobank" = "workspace_cytobank.xml")
    },
    
    content = function(file) {
      print(input$export_format)
      gs <- GatingSet(rval$flow_set)
      
      ####################################################
      #transform
      if(input$export_format == "FlowJo"){
        trans.def <- scales::trans_new("flowJo_linear", 
                               transform = function(x){x}, 
                               inverse = function(x){x})
      }else if(input$export_format == "Cytobank"){
        trans.def <- flowCore::asinhtGml2_trans()
      }
      
      
      trans_list <- rval$transformation
      
      for(i in 1:length(trans_list)){
        trans_list[[i]] <- trans.def
      }
      
      trans <- flowCore::transformList(flowWorkspace::colnames(gs), trans_list)
      
      gs <- flowCore::transform(gs, trans)
      print(gs@transformation)
      
      ####################################################
      #compensate
      
      if(input$apply_comp & !is.null(rval$df_spill)){
        comp <- rval$df_spill
      }else{
        comp <- diag( length(flowWorkspace::colnames(rval$flow_set)))
        colnames(comp) <- flowWorkspace::colnames(rval$flow_set)
        row.names(comp) <- colnames(comp)
      }
      comp <- compensation(comp)
      gs <- flowCore::compensate(gs, comp)
      print(gs@compensation)
      
      
      ####################################################
      #add gates
      
      # print(getNodes(gs))
      gates <- transform_gates(gates = rval$gates_flowCore, 
                               transformation = trans_list,
                               pattern = "", 
                               replacement = "",
                               time_step = 1/rval$time_step
      )
      
      gs <- add_gates_flowCore(gs, gates)
      
      g <- flowWorkspace::gs_pop_get_gate(gs, "/live")
      print(g[[1]]@boundaries)
      
      #gs <- rval$gating_set
      
      
      if(input$export_format == "FlowJo"){
        CytoML::GatingSet2flowJo(gs = gs, outFile = file)
      }else if(input$export_format == "Cytobank"){
        CytoML::GatingSet2cytobank(gs = gs, outFile = file, cytobank.default.scale = FALSE)
      }
      
    }
  )
  
  return( NULL )
  
}