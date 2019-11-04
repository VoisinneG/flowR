#' @title plotGatingHierarchyOutput and plotGatingHierarchy
#' @description  A shiny Module that deals with metadata
#' @param id shiny id
#' @importFrom shinydashboard box tabBox
#' @import shiny
#' @import DT
plotGatingHierarchyOutput <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  plotOutput(ns("plot_gh"))
  
}


#' plotGatingHierarchy server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @return a reactivevalues object with values "df_files", "flow_set_imported" and "gates_flowCore"
#' @import flowWorkspace
#' @import flowCore
#' @import shiny
#' @import DT
#' @export
#' @rdname plotGatingHierarchyOutput
plotGatingHierarchy <- function(input, output, session, rval, plot_params = reactiveValues() ){
  
  `%then%` <- shiny:::`%OR%`
  
  #ns <- session$ns
  
  output$plot_gh <- renderPlot({
    
    p <- plot_all_gates()
    
    n <- length(p)
    
    ncol <- ceiling(n/2)
    g <- marrangeGrob(p, nrow = 2, ncol = ncol, top = paste(plot_params$samples))
    
    g
    
  })
  
  plot_all_gates <- reactive({
    
    validate(
      need(rval$gating_set, "Empty gating set") %then%
      need(setdiff(names(rval$gates_flowCore), "root"), "No gates to display") %then%
      need(plot_params$samples, "Please select a sample")
    )

    if(plot_params$plot_type == "histogram") plot_params$plot_type <- "hexagonal"
    
    axis_labels <- rval$parameters$name_long
    names(axis_labels) <- rval$parameters$name
    
    transformation <- NULL
    if(rval$apply_trans){
      transformation <- rval$transformation
    }
    
    Ncells <- 30000
    if("use_all_cells" %in% names(plot_params)){
      if(plot_params$use_all_cells){
        Ncells <- NULL
      }
    }
    
    
    options <- list(theme = plot_params[["theme"]],
                    transformation = transformation,
                    axis_labels = axis_labels,
                    legend.position = plot_params[["legend.position"]])
    
  
    p <- plot_gh( gs = rval$gating_set,
                  df = NULL,
                  sample = plot_params$samples,
                  selected_subsets = plot_params$selected_subsets,
                  spill = rval$spill,
                  Ncells =  Ncells,
                  plot_type = plot_params$plot_type,
                  plot_args = reactiveValuesToList(plot_params),
                  options = options)
    
    p
    
  })
  
  return( plot_all_gates )
  
}