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
             actionButton(ns("import_gvhd"), "import dataset (GvHD)")
           )
    ),
    column(width = 6,
           box(title = "GatingSet",
               width = NULL, height = NULL,
               div(style = 'overflow-x: scroll', DT::DTOutput(ns("files_table"))),
               br(),
               selectizeInput(ns("groups"), "select groups",
                              choices = NULL,
                              selected = NULL,
                              multiple = FALSE),
               textInput(ns("gs_name"), "GatingSet name", "import"),
               actionButton(ns("load"), label = "Load selected files")
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
#' @importFrom flowWorkspace pData
#' @importFrom flowCore fsApply
#' @importFrom CytoML open_flowjo_xml open_diva_xml flowjo_to_gatingset fj_ws_get_sample_groups diva_get_sample_groups
#' @importFrom ncdfFlow read.ncdfFlowSet
#' @importFrom DT renderDT
#' @importFrom tools file_ext
#' @importFrom utils read.table data
#' @export
#' @rdname ImportUI
Import <- function(input, output, session, rval) {
  
  rval_mod <- reactiveValues(df_files = NULL)
  
  observeEvent(input$files, {
    validate(
      need(input$files$datapath, "Please select a file to import")
    )
    df <- input$files
    file.rename(from = df$datapath, to = paste(dirname(df$datapath[1]),"/", df$name, sep =""))
    df$datapath <- paste(dirname(df$datapath[1]),"/", df$name, sep ="")
    rval_mod$df_files <- df
  })
  
  
  ##########################################################################################################
  # Select group within a workspace
  observe({
    validate(
      need(length(input$files_table_rows_selected)>0, "Please select a file to load")
    )
    if(file_ext(rval_mod$df_files$datapath[input$files_table_rows_selected[1]]) %in% c("xml", "wsp") ){
      ws <- try( CytoML::open_flowjo_xml(rval_mod$df_files$datapath[input$files_table_rows_selected[1]]),
                 silent = TRUE)
      if(class(ws) != "try-error"){
        groups <- unique(CytoML::fj_ws_get_sample_groups(ws)$groupName)
      }
      else{
        ws <- try( CytoML::open_diva_xml(rval_mod$df_files$datapath[input$files_table_rows_selected[1]]),
                   silent = TRUE)
        if(class(ws) != "try-error"){
          groups <- unique(CytoML::diva_get_sample_groups(ws)$specimen)
        }
      }
      
      if(class(ws) == "try-error"){
        showModal(modalDialog(
          title = "Error parsing xml workspace",
          print(ws),
          easyClose = TRUE,
          footer = NULL
        ))
      }
      
      updateSelectInput(session, "groups", choices = groups, selected = groups[1])
    }
    
  })
  
  observeEvent(input$load, {
    
    validate(
      need(length(input$files_table_rows_selected)>0, "Please select a file to load")
    )
    
    print("OK")
    print(input$gs_name)
    print(names(rval$gating_set_list))
    print("OK2")
    
    if( input$gs_name %in% names(rval$gating_set_list) ){
      showModal(modalDialog(
        title = "Name already exists",
        paste("Please choose another name", sep=""),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    validate(need(! input$gs_name %in% names(rval$gating_set_list), "Name already exists" ))
    print("OK3")
    
    # Create a Progress object
    progress <- shiny::Progress$new(min = 0, max = 1)
    on.exit(progress$close())
    progress$set(message = "Loading data", value = 0.5)
    
    
    
      
      if(tools::file_ext(rval_mod$df_files$datapath[input$files_table_rows_selected[1]]) %in% c("xml", "wsp")){
        ws <- try( open_flowjo_xml(rval_mod$df_files$datapath[input$files_table_rows_selected[1]]),
                   silent = TRUE)
        if(class(ws) == "try-error"){
          ws <- try( open_diva_xml(rval_mod$df_files$datapath[input$files_table_rows_selected[1]]),
                     silent = TRUE)
        }
        
        if(class(ws) == "try-error"){
          showModal(modalDialog(
            title = "Error parsing xml workspace",
            print(ws),
            easyClose = TRUE,
            footer = NULL
          ))
        }
  
        gs <- try(CytoML::flowjo_to_gatingset(ws,
                                 name = input$groups,
                                 execute = TRUE,
                                 isNcdf = TRUE,
                                 sampNloc = "sampleNode",
                                 path = dirname(rval_mod$df_files$datapath[1])),
                  silent = TRUE)
        
        if(class(gs) == "try-error"){
          showModal(modalDialog(
            title = "Error parsing xml workspace",
            print(gs),
            easyClose = TRUE,
            footer = NULL
          ))
        }
        
        validate(
          need(class(gs) == "GatingSet", "No gating set imported")
        )
        
        fs <- gs@data
        
        ###################################################################################
        #get gates and transfrom gates
        
        gates <- get_gates_from_gs(gs)
        if(tools::file_ext(rval_mod$df_files$datapath[input$files_table_rows_selected[1]]) %in% c("wsp")){
          gates <- get_gates_from_ws(ws_path = rval_mod$df_files$datapath[input$files_table_rows_selected[1]], 
                                     group = input$groups)
        }
        
        ff <- fs[[1]]
        
        # time_step is needed to transform gates containing the parameter "Time"
        time_step <- as.numeric(description(ff)[["$TIMESTEP"]])
        
        params <- parameters(ff)$name
        
        pattern <- NULL
        if( length( grep("[\\<|\\>]", params) ) >0 ){
          pattern <- "[\\<|\\>]"
        }else if(length( grep("Comp-", params) ) >0){
          pattern <- "Comp-"
        }
        replacement <- ""
        if(!is.null(pattern)){
          params <- gsub(pattern = pattern, replacement = replacement, params)
        }
        
        myTrans <- lapply(params, function(x){scales::identity_trans()})
        names(myTrans) <- params
        
        gates <- transform_gates(gates = gates,
                                               pattern = pattern,
                                               replacement = replacement,
                                               transformation = myTrans, 
                                               time_step = time_step)
        
        
        ###################################################################################
        #match fcs file names and import non-compensated, non-transformed data
        
        names_imported <- flowCore::fsApply(fs, function(x){description(x)[["FILENAME"]]})
        names_imported <- basename(names_imported)
        idx_match <- match(names_imported, rval_mod$df_files$name)
        
        fs <- ncdfFlow::read.ncdfFlowSet(rval_mod$df_files$datapath[idx_match], 
                                         truncate_max_range = TRUE )
        
        flowWorkspace::pData(fs)$name <- rval_mod$df_files$name[idx_match]
        
        gs <- GatingSet(fs)
        add_gates_flowCore(gs, gates)
        rval$gating_set_list[[input$gs_name]] <- list(gating_set = gs,
                                                      parent = NULL)
        rval$gating_set_selected <- input$gs_name
        
      }else if(file_ext(rval_mod$df_files$datapath[input$files_table_rows_selected[1]]) == "rda"){
        
        res_name <- load(rval_mod$df_files$datapath[input$files_table_rows_selected[1]])
        res <- get(res_name)

        for(name in names(res)){
          fs <- build_flowset_from_df(df = res[[name]]$data, origin = res[[name]]$flow_set)
          gs <- GatingSet(fs)
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
        gs <- GatingSet(fs)
        rval$gating_set_list[[input$gs_name]] <- list(gating_set = gs,
                                                      parent = NULL)
        rval$gating_set_selected <- input$gs_name
        
        
      }else{
        
        fs <- ncdfFlow::read.ncdfFlowSet( rval_mod$df_files$datapath[input$files_table_rows_selected] , 
                                          emptyValue=FALSE, 
                                          truncate_max_range = TRUE )
        
        pData(fs)$name <- rval_mod$df_files$name[input$files_table_rows_selected]
        gs <- GatingSet(fs)
        rval$gating_set_list[[input$gs_name]] <- list(gating_set = gs,
                                                    parent = NULL)
        rval$gating_set_selected <- input$gs_name
      }
    
    print("OK4")
  })

  observeEvent(input$import_gvhd, {
    
    utils::data("GvHD", package = "flowCore")
    gs <- GatingSet(GvHD)
    rval$gating_set_list[["GvHD"]] <- list(gating_set = gs,
                                                  parent = NULL)
    rval$gating_set_selected <- "GvHD"
  
  })
  
  output$files_table <- DT::renderDT({
    validate(
      need(rval_mod$df_files, "Please select a file to import")
    )
    df <- rval_mod$df_files[ ,c("name", "size")]
    df$new_name <- basename(rval_mod$df_files$datapath)
    df$dir_name <- dirname(rval_mod$df_files$datapath)
    df
  })
  
  return(rval)
  
}

##################################################################################
# Tests
##################################################################################
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