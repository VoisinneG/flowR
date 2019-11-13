#' @title plotGatingHierarchy
#' @description  A shiny Module to plot a gating hierarchy
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param rval A reactiveValues object
#' @param plot_params A reactiveValues object with plot parameters
#' @return A list of plots
#' @import shiny
plotGatingHierarchy <- function(input, output, session, rval, plot_params = reactiveValues() ){

  plot_all_gates <- reactive({
    
    validate(need(rval$gating_set, "Empty gating set"))
    validate(need(setdiff(names(rval$gates_flowCore), "root"), "No gates to display"))
    validate(need(plot_params$samples, "Please select a sample"))

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