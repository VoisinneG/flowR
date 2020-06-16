utils::globalVariables("GvHD")

#' Import data and build a GatingSet
#' @param id shiny id
#' @importFrom shinydashboard box
#' @importFrom DT DTOutput
#' @import shiny
#' @export
#' @examples
#' \dontrun{
#' library(shiny)
#' library(shinydashboard)
#' 
#' if (interactive()){
#'   
#'   ui <- dashboardPage(
#'     dashboardHeader(title = "Import"),
#'     sidebar = dashboardSidebar(disable = TRUE),
#'     body = dashboardBody(
#'       fluidRow(
#'         column(4, box(width = NULL, verbatimTextOutput("info"))),
#'         column(8, box(width = NULL, ImportUI("module")))
#'       )
#'     )
#'   )
#'   
#'   server <- function(input, output, session) {
#'     rval <- reactiveValues()
#'     rval <- callModule(Import, "module", rval = rval)
#'     output$info <- renderPrint({
#'       print(rval$gating_set_list)
#'     })
#'   }
#'   
#'   shinyApp(ui, server)
#'   
#' }}
ImportUI <- function(id) {

  ns <- NS(id)
  
  fluidRow(
    column(width = 6,
           box(title = "Import",
               width = NULL, height = NULL,
               fileInput(inputId = ns("files"),
                         label = "Choose files",
                         multiple = TRUE)
           ),
           box(title = "Example dataset",
             width = NULL, height = NULL,
             selectInput(ns("example_selected"), "Select dataset", choices=c("GvHD", "Bcells"), selected = "GvHD"),
             actionButton(ns("import_example"), "Load dataset")
           )
    ),
    column(width = 6,
           box(title = "GatingSet",
               width = NULL, height = NULL,
               checkboxInput(ns("select_all"), label = "Select all", value = F),
               div(style = 'overflow-x: scroll', DT::DTOutput(ns("files_table"))),
               br(),
               # selectizeInput(ns("groups"), "select groups",
               #                choices = NULL,
               #                selected = NULL,
               #                multiple = FALSE),
               textInput(ns("gs_name"), "GatingSet name", "import"),
               actionButton(ns("load"), label = "Create GatingSet")
           )
    )
  )
  
}

#' Import module server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param rval a reactivevalues object (can be empty)
#' @return The input reactivevalues object 'rval' with updated elements :
#' \describe{
#'   \item{gating_set_list}{a named list with each element containing :}
#'   \describe{
#'     \item{gating_set}{: a GatingSet objects}
#'     \item{parent}{: the name of its parent GatingSet}
#'   }
#'   \item{gating_set}{selected GatingSet}
#'   \item{gating_set_selected}{Name of the selected GatingSet}
#' }
#' @import shiny
#' @importFrom flowWorkspace pData GatingSet
#' @importFrom flowCore fsApply
#' @importFrom ncdfFlow read.ncdfFlowSet
#' @importFrom DT renderDT
#' @importFrom tools file_ext
#' @importFrom utils read.table data
#' @import flowAI
#' @export
#' @rdname ImportUI
Import <- function(input, output, session, rval) {
  
  rval_mod <- reactiveValues(df_files = NULL)
  
  
  #### Import Files ####
  
  observeEvent(input$files, {
    validate(
      need(input$files$datapath, "Please select a file to import")
    )
    df <- input$files
    file.rename(from = df$datapath, to = paste(dirname(df$datapath[1]),"/", df$name, sep =""))
    df$datapath <- paste(dirname(df$datapath[1]),"/", df$name, sep ="")
    rval_mod$df_files <- df
  })
  
  #### Create GatingSet ####
  
  observeEvent(input$load, {
    
    validate(need(length(input$files_table_rows_selected)>0, "Please select a file to load"))
    
    ###Check GatingSet name
    if( input$gs_name %in% names(rval$gating_set_list) ){
      showModal(modalDialog(
        title = "Name already exists",
        paste("Please choose another name", sep=""),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    validate(need(! input$gs_name %in% names(rval$gating_set_list), "Name already exists" ))
    
    # Create a Progress object
    progress <- shiny::Progress$new(min = 0, max = 1)
    on.exit(progress$close())
    progress$set(message = "Loading data", value = 0.5)
    
    #### Build GatingSet ####
    
    if(file_ext(rval_mod$df_files$datapath[input$files_table_rows_selected[1]]) == "rda"){
        
        res_name <- load(rval_mod$df_files$datapath[input$files_table_rows_selected[1]])
        res <- get(res_name)

        for(name in names(res)){
          fs <- build_flowset_from_df(df = res[[name]]$data, origin = res[[name]]$flow_set)
          gs <- flowWorkspace::GatingSet(fs)
          add_gates_flowCore(gs = gs, gates = res[[name]]$gates)
          gs@compensation <- res[[name]]$compensation
          gs@transformation <- res[[name]]$transformation
          rval$trans_parameters <- res[[name]]$trans_parameters
          parent <- NULL
          if(!is.null(res[[name]]$parent)){
            if(res[[name]]$parent  %in% names(res)){
              parent <- res[[name]]$parent
            }
          }
          
          
          rval$gating_set_list[[name]] <- list(gating_set = gs, 
                                               parent = parent, 
                                               trans_parameters = res[[name]]$trans_parameters)
        }
        
        rval$gating_set_selected <- names(res)[1]
        
      }else if(file_ext(rval_mod$df_files$datapath[input$files_table_rows_selected[1]]) %in% c("csv", "txt")){
        
        df <- utils::read.table(rval_mod$df_files$datapath[input$files_table_rows_selected[1]], 
                                header = TRUE, 
                                sep = "\t", 
                                as.is = TRUE)
        
        df$name <- basename(rval_mod$df_files$datapath[input$files_table_rows_selected[1]])
        df$subset <- "root"
        fs <- build_flowset_from_df(df)
        gs <- flowWorkspace::GatingSet(fs)
        rval$gating_set_list[[input$gs_name]] <- list(gating_set = gs,
                                                      parent = NULL)
        rval$gating_set_selected <- input$gs_name
        
        
      }else{
        files <- rval_mod$df_files$datapath[input$files_table_rows_selected]
        idx_fcs <- which(file_ext(files) %in% c("fcs", "FCS"))
        if(length(idx_fcs)>0){
          fs <- ncdfFlow::read.ncdfFlowSet( files[idx_fcs], 
                                            emptyValue=FALSE, 
                                            truncate_max_range = TRUE )
          
          flowWorkspace::pData(fs)$name <- basename(files[idx_fcs])
          gs <- flowWorkspace::GatingSet(fs)
          rval$gating_set_list[[input$gs_name]] <- list(gating_set = gs,
                                                        parent = NULL)
          rval$gating_set_selected <- input$gs_name
        }
        
      }

  })

  #### Import example dataset ####
  
  observeEvent(input$import_example, {
    
    if(input$example_selected == "GvHD"){
      utils::data("GvHD", package = "flowCore")
      gs <- flowWorkspace::GatingSet(GvHD)
      rval$gating_set_list[["GvHD"]] <- list(gating_set = gs,
                                             parent = NULL)
      rval$gating_set_selected <- "GvHD"
    }else if(input$example_selected == "Bcells"){
      utils::data("Bcells", package = "flowAI")
      gs <- flowWorkspace::GatingSet(Bcells)
      rval$gating_set_list[["Bcells"]] <- list(gating_set = gs,
                                             parent = NULL)
      rval$gating_set_selected <- "Bcells"
    }
    
  
  })
  
  #### Display loaded files ####
  
  output$files_table <- DT::renderDT({
    validate(need(rval_mod$df_files, "Please select a file to import"))
    df <- rval_mod$df_files[ ,c("name", "size")]
    df$new_name <- basename(rval_mod$df_files$datapath)
    df$dir_name <- dirname(rval_mod$df_files$datapath)
    df
  })
  
  #### Select all element in datatable ####
  dt_proxy <- DT::dataTableProxy("files_table")
  observeEvent(input$select_all, {
    if (isTRUE(input$select_all)) {
      DT::selectRows(dt_proxy, input$files_table_rows_all)
    } else {
      DT::selectRows(dt_proxy, NULL)
    }
    
  })
  
  return(rval)
  
}

#### Tests ####
#
# library(shiny)
# library(shinydashboard)
# 
# if (interactive()){
# 
#   ui <- dashboardPage(
#     dashboardHeader(title = "Import"),
#     sidebar = dashboardSidebar(disable = TRUE),
#     body = dashboardBody(
#       fluidRow(
#         column(4, box(width = NULL, verbatimTextOutput("info"))),
#         column(8, box(width = NULL, ImportUI("module")))
#       )
#     )
#   )
# 
#   server <- function(input, output, session) {
# 
#     rval <- reactiveValues()
# 
#     rval <- callModule(Import, "module", rval = rval)
# 
#     output$info <- renderPrint({
#       print(rval$gating_set_list)
#     })
# 
#   }
# 
#   shinyApp(ui, server)
# 
# }
