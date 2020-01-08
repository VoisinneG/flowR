#' @title plotGatingHierarchy
#' @description  A shiny Module to plot a gating hierarchy
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param rval a reactivevalues object with the following elements :
#' \describe{
#'   \item{gating_set}{: a GatingSet object}
#'   \item{apply_trans}{: logical; apply transformations defined in \code{rval$gating_set}}
#'   \item{apply_comp}{: logical; apply compensation defined in \code{rval$gating_set}}
#' }
#' @param plot_params reactivevalues object used to define plot parameters. 
#' Amongst others it can contain the following elements (not mandatory):
#' \describe{
#'   \item{plot_type}{: type of plot}
#'   \item{sample}{: names of samples}
#'   \item{color_var}{: color aesthetic}
#'   \item{group_var}{: group aesthetic}
#'   \item{facet_var}{: variable used to show different plot facets}
#'   \item{split_var}{: variable used to create multiple plots}
#'  }
#' @param plot_params A reactiveValues object with plot parameters
#' @return A list of plots
#' @import shiny
#' @examples 
#' \dontrun{
#' library(shiny)
#' library(shinydashboard)
#' library(flowWorkspace)
#' library(flowCore)
#' 
#' if (interactive()){
#' 
#'   ui <- dashboardPage(
#'     dashboardHeader(title = "plotGatingHierarchy"),
#'     sidebar = dashboardSidebar(disable = TRUE),
#'     body = dashboardBody(
#'       fluidRow(
#'         column(12, box(width = NULL, simpleDisplayUI("simple_display_module")))
#'       )
#'     )
#'   )
#' 
#'   server <- function(input, output, session) {
#' 
#'     rval <- reactiveValues()
#'     plot_params <- reactiveValues()
#'     display_params <- reactiveValues()
#'     
#'     observe({
#'       gs <- load_gs("./inst/ext/gs")
#'       rval$gating_set <- gs
#'       plot_params$sample <- pData(gs)$name
#'       plot_params$plot_type <- "hexagonal"
#'       display_params$top <- paste(plot_params$sample, collapse = " + ") 
#'     })
#' 
#'     plot_all_gates <- callModule(plotGatingHierarchy, "module",
#'                       rval = rval,
#'                       plot_params = plot_params)
#' 
#'     callModule(simpleDisplay, "simple_display_module", 
#'                plot_list = plot_all_gates, params = display_params)
#' 
#'   }
#' 
#'   shinyApp(ui, server)
#' 
#' }
#'}
plotGatingHierarchy <- function(input, output, session,
                                 rval,
                                 plot_params = reactiveValues() ){
  
  rval_plot <- reactiveValues()
  
  ######################################################################################
  # Set plot parameters
  
  observe({
    rval_plot$sample <- choices()$sample[1]
    rval_plot$plot_type <- "hexagonal"
  })
  
  observe({
    
    for(var in names(plot_params)){
        rval_plot[[var]] <- plot_params[[var]]
    }
    
    if("plot_type" %in% names(plot_params)){
      if(plot_params$plot_type == "histogram"){
        rval_plot$plot_type <- "hexagonal"
      }
    }
    
    
  })
                              
  ######################################################################################
  # get parameters from GatingSet
  choices <- reactive({
    validate(need(class(rval$gating_set) == "GatingSet", "Input is not a GatingSet"))
    
    plot_var <- parameters(rval$gating_set@data[[1]])$name
    
    validate(need(length(plot_var)>0, "No variables in GatingSet"))
    
    desc <- parameters(rval$gating_set@data[[1]])$desc
    labels <- sapply(1:length(plot_var), function(x){
      if(is.na(desc[x])){
        plot_var[x]
      }else{
        paste(plot_var[x], "(", desc[x], ")")
      }
    })
    names(plot_var) <- labels
    names(labels) <- plot_var 

    return( 
      list(sample = pData(rval$gating_set)$name,
           plot_var = plot_var,
           labels = labels,
           transformation = rval$gating_set@transformation,
           compensation = rval$gating_set@compensation,
           gates = get_gates_from_gs(rval$gating_set)
      )
    )
  })
  
  plot_all_gates <- reactive({
    
    validate(need(class(rval$gating_set) == "GatingSet", "Input is not a GatingSet"))
    validate(need(setdiff(gs_get_pop_paths(rval$gating_set), "root"), "No gates to display"))
    
    axis_labels <- choices()$labels

    transformation <- choices()$transformation
    if(!is.null(rval$apply_trans)){
      if(!rval$apply_trans){
        transformation <- NULL
      }
    }
    
    spill <- choices()$compensation
    if(!is.null(rval$apply_comp)){
      if(!rval$apply_comp){
        spill <- NULL
      }
    }
    
    Ncells <- 30000
    if("use_all_cells" %in% names(rval_plot)){
      if(rval_plot$use_all_cells){
        Ncells <- NULL
      }
    }
    
    options <- reactiveValuesToList(rval_plot)
    options$transformation <- transformation
    options$axis_labels <- axis_labels
    
    p <- plot_gh( gs = rval$gating_set,
                  df = NULL,
                  sample = rval_plot$sample,
                  selected_subsets = rval_plot$selected_subsets,
                  spill = spill,
                  Ncells =  Ncells,
                  plot_type = rval_plot$plot_type,
                  plot_args = reactiveValuesToList(rval_plot),
                  options = options)
    #print(reactiveValuesToList(rval_plot))
    #print(options)
    p
    
  })
  
  return( plot_all_gates )
  
}

##################################################################################
# Tests
##################################################################################
# 
# library(shiny)
# library(shinydashboard)
# library(flowWorkspace)
# library(flowCore)
# library(viridis)
# library(scales)
# library(ggplot2)
# library(ggrepel)
# library(plotly)
# library(ggridges)
# 
# if (interactive()){
# 
#   ui <- dashboardPage(
#     dashboardHeader(title = "plotGatingHierarchy"),
#     sidebar = dashboardSidebar(disable = TRUE),
#     body = dashboardBody(
#       fluidRow(
#         column(12, box(width = NULL, simpleDisplayUI("simple_display_module")))
#       )
#     )
#   )
# 
#   server <- function(input, output, session) {
# 
#     rval <- reactiveValues()
#     plot_params <- reactiveValues()
#     display_params <- reactiveValues()
# 
#     observe({
#       #gs <- load_gs("./inst/ext/gs")
#       load("../flowR_utils/demo-data/Rafa2Gui/analysis/cluster.rda")
#       fs <- build_flowset_from_df(df = res$cluster$data)
#       gs <- GatingSet(fs)
#       add_gates_flowCore(gs, res$cluster$gates)
#       rval$gating_set <- gs
#       plot_params$sample <- pData(gs)$name
#       print(gs_get_pop_paths(rval$gating_set))
#       plot_params$sample <- "Z_037.fcs"
#       plot_params$use_all_cells <- FALSE
#       #plot_params$plot_type <- "hexagonal"
#       plot_params$selected_subsets <- c("/HLA-DR_myeloid", "/HLA-DR_myeloid/Single_Cells", "/cluster1")
#       display_params$top <- paste(plot_params$sample, collapse = " + ")
#     })
# 
#     plot_all_gates <- callModule(plotGatingHierarchy, "module",
#                       rval = rval,
#                       plot_params = plot_params)
# 
#     callModule(simpleDisplay, "simple_display_module", plot_list = plot_all_gates, params = display_params)
# 
#   }
# 
#   shinyApp(ui, server)
# 
# }
