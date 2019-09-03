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
      need(plot_params$samples, "Please select a sample") %then%
      need(plot_params$plot_type != "histogram", "Plot type not supported") 
    )

    
    axis_labels <- rval$parameters$name_long
    names(axis_labels) <- rval$parameters$name
    
    transformation <- NULL
    if(rval$apply_trans){
      transformation <- rval$transformation
    }
    
    #data_range <- NULL
    # if(plot_params$freeze_limits){
    #   data_range <- rval$data_range
    # }
    
    color_var <- NULL
    if(!is.null(plot_params$color_var)){
      if(plot_params$color_var %in% rval$parameters$name_long){
        color_var <- rval$parameters$name[match(plot_params$color_var, rval$parameters$name_long)]
      }else{
        color_var <- plot_params$color_var
      }
    }
    
    
    if(plot_params$plot_type != "histogram"){
      type <- plot_params$plot_type
    }
    
    p <- plot_gh(df = NULL,
                 gs = rval$gating_set,
                 sample = plot_params$samples,
                 spill = rval$spill,
                 transformation = transformation,
                 bins = plot_params$bin_number,
                 color_var = color_var,
                 facet_vars = NULL,
                 axis_labels = axis_labels,
                 #data_range = data_range,
                 type = type,
                 alpha = plot_params$alpha,
                 size = plot_params$size,
                 show.legend = FALSE,
                 theme_name = paste("theme_", plot_params$theme, sep = ""))
    
    p
    
  })
  
  return( plot_all_gates )
  
}