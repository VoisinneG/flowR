#' @title   importUI and import
#' @description  A shiny Module that imports a flow set and gates from fcs files and a workspace 
#' @param id shiny id
#' @importFrom shinydashboard box
#' @import DT
#' @import shiny
importUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(
    column(width = 6,
           box(title = "Import",
               width = NULL, height = NULL,
               fileInput(inputId = ns("files"),
                         label = "Choose files",
                         multiple = TRUE)
           )
           # box(title = "Options",
           #     width = NULL, height = NULL,
           #    checkboxInput(ns("apply_biexp_inverse"), "apply inverse biexponential on gate coordinates")
           # )
    ),
    column(width = 6,
           box(title = "Flow-set",
               width = NULL, height = NULL,
               div(style = 'overflow-x: scroll', DT::dataTableOutput(ns("files_table"))),
               br(),
               selectizeInput(ns("groups"), "select groups",
                              choices = NULL,
                              selected = NULL,
                              multiple = FALSE),
               textInput(ns("fs_name"), "Flow-set name", "import"),
               actionButton(ns("load"), label = "Load selected files")
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
#' @rdname importUI
import <- function(input, output, session) {
  
  rval_mod <- reactiveValues(df_files = NULL)
  
  rval <- reactiveValues(flow_set_list = list(),
                         gates_flowCore = list(),
                         count = 0
                         )
  
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
      ws <- openWorkspace(rval_mod$df_files$datapath[input$files_table_rows_selected[1]])
      groups <- unique(getSampleGroups(ws)$groupName)
      updateSelectInput(session, "groups", choices = groups, selected = groups[1])
    }
    
  })
  
  observeEvent(input$load, {
    
    validate(
      need(length(input$files_table_rows_selected)>0, "Please select a file to load")
    )
    
    if( input$fs_name %in% names(rval$flow_set_list) ){
      showModal(modalDialog(
        title = "Name already exists",
        paste("Please choose another name", sep=""),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    validate(need(! input$fs_name %in% names(rval$flow_set_list), "Name already exists" ))
    
    # Create a Progress object
    progress <- shiny::Progress$new(min = 0, max = 1)
    on.exit(progress$close())
    progress$set(message = "Loading data", value = 0.5)
    
    
    
      
      if(file_ext(rval_mod$df_files$datapath[input$files_table_rows_selected[1]]) %in% c("xml", "wsp")){
        ws <- openWorkspace(rval_mod$df_files$datapath[input$files_table_rows_selected[1]])
  
        gs <- try(parseWorkspace(ws,
                                 name = input$groups,
                                 execute = TRUE,
                                 isNcdf = TRUE,
                                 sampNloc = "sampleNode",
                                 path = dirname(rval_mod$df_files$datapath[1])),
                  silent = TRUE)
        
        if(class(gs) != "GatingSet"){
          showModal(modalDialog(
            title = "Error parsing xml workspace",
            paste("Please try importing FCS files directly", sep=""),
            easyClose = TRUE,
            footer = NULL
          ))
        }
        
        validate(
          need(class(gs) == "GatingSet", "No gating set imported")
        )
        
        fs <- getData(gs)
        
        ###################################################################################
        #get gates and transfrom gates
        
        gates <- get_gates_from_gs(gs)
        if(file_ext(rval_mod$df_files$datapath[input$files_table_rows_selected[1]]) %in% c("wsp")){
          gates <- get_gates_from_ws(ws_path = rval_mod$df_files$datapath[input$files_table_rows_selected[1]], 
                                     group = input$groups)
        }
        
        print(gates)
        
        ff <- fs[[1]]
        
        # time_step is needed to transform gates containing the parameter "Time"
        time_step <- as.numeric(description(ff)[["$TIMESTEP"]])
        
        # Parameters with a DISPLAY = LOG have been transformed with flowJo_biexp_trans().
        # We need to apply the inverse transfrom for such parameters
        
        display <- unlist(sapply(rownames(parameters(ff)@data), FUN = function(x){
          kw <- substr(x, start = 2, stop = nchar(x))
          kw <- paste(kw, "DISPLAY", sep = "")
          disp <- ff@description[[kw]]
          if(is.null(disp)){
            disp <- "NA"
          }
          return(disp)
        }))
        names(display) <- NULL
        
        trans.log <- identity_trans()
        # if(input$apply_biexp_inverse){
        #   trans.log <- flowJo_biexp_inverse_trans()
        # }else{
        #   trans.log <- identity_trans()
        # }
        
        myTrans <- lapply(display, function(x){
          switch(x,
                 "LOG" = trans.log,
                 identity_trans())
        })
        
        params <- parameters(ff)$name
        print(params)
        
        pattern <- NULL
        if( length( grep("[\\<|\\>]", params) ) >0 ){
          pattern <- "[\\<|\\>]"
        }else if(length( grep("Comp-", params) ) >0){
          pattern <- "Comp-"
        }
        #print(pattern)
        replacement <- ""
        if(!is.null(pattern)){
          params <- gsub(pattern = pattern, replacement = replacement, params)
        }
        
        names(myTrans) <- params
        
        rval$gates_flowCore <- transform_gates(gates = gates, 
                                               pattern = pattern,
                                               replacement = replacement,
                                               transformation = myTrans, 
                                               time_step = time_step)
        
        ###################################################################################
        #match fcs file names and import non-compensated, non-transfromed data
        
        names_imported <- fsApply(fs, function(x){description(x)[["FILENAME"]]})
        names_imported <- basename(names_imported)
        #print(names_imported)
        #idx_match <- sapply(names_imported, function(x){as.numeric(strsplit(x, split= ".", fixed = TRUE)[[1]][1])})
        idx_match <- match(names_imported, rval_mod$df_files$name)
        
        fs <- ncdfFlow::read.ncdfFlowSet( rval_mod$df_files$datapath[idx_match] )
        phenoData(fs)$name <- rval_mod$df_files$name[idx_match]
        rval$flow_set_list[[input$fs_name]] <- list(flow_set = fs, name = input$fs_name, parent = NULL)
        rval$flow_set_selected <- input$fs_name
        
      }else if(file_ext(rval_mod$df_files$datapath[input$files_table_rows_selected[1]]) == "rda"){
        res_name <- load(rval_mod$df_files$datapath[input$files_table_rows_selected[1]])
        res <- get(res_name)
        
        for(i in 1:length(res)){
          
          fs <- build_flowset_from_df(df = res[[i]]$data, fs = res[[i]]$flow_set)
          pData(fs) <- res[[i]]$metadata
          
          if(is.null(res[[i]]$parent)){
            rval$transformation <- res[[i]]$transformation
            rval$trans_parameters <- res[[i]]$trans_parameters
            rval$df_spill <- res[[i]]$spill
            rval$gates_flowCore <- res[[i]]$gates
          }
          rval$flow_set_list[[res[[i]]$name]] <- list(flow_set = fs, name = res[[i]]$name, parent = res[[i]]$parent)
        }
        
        rval$flow_set_selected <- names(rval$flow_set_list)[[1]]
        
      }else if(file_ext(rval_mod$df_files$datapath[input$files_table_rows_selected[1]]) %in% c("csv", "txt")){
        df <- read.table(rval_mod$df_files$datapath[input$files_table_rows_selected[1]], header = TRUE, sep = "\t", as.is = TRUE)
        print(df)
        df$name <- basename(rval_mod$df_files$datapath[input$files_table_rows_selected[1]])
        df$subset <- "root"
        fs <- build_flowset_from_df(df)
        rval$flow_set_list[[input$fs_name]] <- list(flow_set = fs, name = input$fs_name, parent = NULL)
        rval$flow_set_selected <- input$fs_name
        
      }else{
        
        fs <- ncdfFlow::read.ncdfFlowSet( rval_mod$df_files$datapath[input$files_table_rows_selected] , emptyValue=FALSE)
        phenoData(fs)$name <- rval_mod$df_files$name[input$files_table_rows_selected]
        rval$flow_set_list[[input$fs_name]] <- list(flow_set = fs, name = input$fs_name, parent = NULL)
        rval$flow_set_selected <- input$fs_name
      }
      
      #Initialization of some reactive variables
    
      
      
      #rval$flow_set_imported <- fs
      #rval$flow_set_names <- unique(c(rval$flow_set_names, "imported"))
      #rval$flow_set_selected <- "imported"
      
    })

  output$files_table <- DT::renderDataTable({
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