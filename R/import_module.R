#' @title importUI and import
#' @description  A shiny Module that imports data and builds flow-sets
#' @param id shiny id
#' @importFrom shinydashboard box
#' @importFrom DT DTOutput
#' @import shiny
importUI <- function(id) {

  ns <- NS(id)
  
  fluidRow(
    column(width = 6,
           box(title = "Import",
               width = NULL, height = NULL,
               fileInput(inputId = ns("files"),
                         label = "Choose files",
                         multiple = TRUE),
           ),
           box(title = "Example dataset",
             width = NULL, height = NULL,
             actionButton(ns("import_gvhd"), "import dataset (GvHD)")
           )
           # box(title = "Options",
           #     width = NULL, height = NULL,
           #    checkboxInput(ns("apply_biexp_inverse"), "apply inverse biexponential on gate coordinates")
           # )
    ),
    column(width = 6,
           box(title = "Flow-set",
               width = NULL, height = NULL,
               div(style = 'overflow-x: scroll', DT::DTOutput(ns("files_table"))),
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
#' @return a reactivevalues object
#' @import shiny
#' @importFrom flowWorkspace pData
#' @importFrom flowCore fsApply
#' @importFrom CytoML open_flowjo_xml open_diva_xml flowjo_to_gatingset fj_ws_get_sample_groups
#' @importFrom ncdfFlow read.ncdfFlowSet
#' @importFrom DT renderDT
#' @importFrom tools file_ext
#' @importFrom utils read.table
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
      ws <- try( CytoML::open_flowjo_xml(rval_mod$df_files$datapath[input$files_table_rows_selected[1]]),
                 silent = TRUE)
      if(class(ws) != "try-error"){
        groups <- unique(CytoML::fj_ws_get_sample_groups(ws)$groupName)
      }
      else{
        ws <- try( CytoML::open_diva_xml(rval_mod$df_files$datapath[input$files_table_rows_selected[1]]),
                   silent = TRUE)
        if(class(ws) != "try-error"){
          groups <- unique(CytoML::diva_ws_get_sample_groups(ws)$specimen)
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
    
    rval$pdata <- NULL
    rval$df_spill <- NULL
    rval$transformation <- NULL
    rval$trans_parameters <- NULL
    rval$parameters <- NULL
    
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
        
        # # Parameters with a DISPLAY = LOG have been transformed with flowJo_biexp_trans().
        # # We need to apply the inverse transfrom for such parameters
        # 
        # display <- unlist(sapply(rownames(parameters(ff)@data), FUN = function(x){
        #   kw <- substr(x, start = 2, stop = nchar(x))
        #   kw <- paste(kw, "DISPLAY", sep = "")
        #   disp <- ff@description[[kw]]
        #   if(is.null(disp)){
        #     disp <- "NA"
        #   }
        #   return(disp)
        # }))
        # names(display) <- NULL
        # 
        # trans.log <- identity_trans()
        # # if(input$apply_biexp_inverse){
        # #   trans.log <- flowJo_biexp_inverse_trans()
        # # }else{
        # #   trans.log <- identity_trans()
        # # }
        # 
        # myTrans <- lapply(display, function(x){
        #   switch(x,
        #          "LOG" = trans.log,
        #          scales::identity_trans())
        # })
        
        
        
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
        
        rval$gates_flowCore <- transform_gates(gates = gates,
                                               pattern = pattern,
                                               replacement = replacement,
                                               transformation = myTrans, 
                                               time_step = time_step)
        
        
        ###################################################################################
        #match fcs file names and import non-compensated, non-transformed data
        
        names_imported <- flowCore::fsApply(fs, function(x){description(x)[["FILENAME"]]})
        names_imported <- basename(names_imported)
        idx_match <- match(names_imported, rval_mod$df_files$name)
        fs <- ncdfFlow::read.ncdfFlowSet( rval_mod$df_files$datapath[idx_match], truncate_max_range = TRUE )
        flowWorkspace::pData(fs)$name <- rval_mod$df_files$name[idx_match]

        rval$flow_set_list[[input$fs_name]] <- list(flow_set = fs,
                                                    spill = NULL,
                                                    metadata = pData(fs),
                                                    par = lapply(1:length(fs), function(x){parameters(fs[[x]])}),
                                                    desc = lapply(1:length(fs), function(x){description(fs[[x]])}),
                                                    spill = NULL,
                                                    parameters = NULL,
                                                    transformation = NULL,
                                                    trans_parameters = NULL,
                                                    gates = rval$gates_flowCore,
                                                    name = input$fs_name, 
                                                    parent = NULL)
        rval$flow_set_selected <- input$fs_name
        
      }else if(file_ext(rval_mod$df_files$datapath[input$files_table_rows_selected[1]]) == "rda"){
        
        res_name <- load(rval_mod$df_files$datapath[input$files_table_rows_selected[1]])
        res <- get(res_name)
        rval$flow_set_list <- res
        
        for(i in 1:length(res)){
          fs <- build_flowset_from_df(df = res[[i]]$data, origin = res[[i]])
          rval$flow_set_list[[i]]$flow_set <- fs
          print(names(rval$flow_set_list))
        }
        
        rval$flow_set_selected <- names(rval$flow_set_list)[[1]]
        
      }else if(file_ext(rval_mod$df_files$datapath[input$files_table_rows_selected[1]]) %in% c("csv", "txt")){
        
        df <- utils::read.table(rval_mod$df_files$datapath[input$files_table_rows_selected[1]], 
                                header = TRUE, 
                                sep = "\t", 
                                as.is = TRUE)
        
        df$name <- basename(rval_mod$df_files$datapath[input$files_table_rows_selected[1]])
        df$subset <- "root"
        fs <- build_flowset_from_df(df)

        rval$flow_set_list[[input$fs_name]] <- list(flow_set = fs,
                                                    spill = NULL,
                                                    metadata = pData(fs),
                                                    par = lapply(1:length(fs), function(x){parameters(fs[[x]])}),
                                                    desc = lapply(1:length(fs), function(x){description(fs[[x]])}),
                                                    spill = NULL,
                                                    parameters = NULL,
                                                    transformation = NULL,
                                                    trans_parameters = NULL,
                                                    gates = list(),
                                                    name = input$fs_name, 
                                                    parent = NULL)
        
        rval$flow_set_selected <- input$fs_name
        
      }else{
        
        fs <- ncdfFlow::read.ncdfFlowSet( rval_mod$df_files$datapath[input$files_table_rows_selected] , 
                                          emptyValue=FALSE, 
                                          truncate_max_range = TRUE )
        
        phenoData(fs)$name <- rval_mod$df_files$name[input$files_table_rows_selected]
        
        rval$flow_set_list[[input$fs_name]] <- list(flow_set = fs,
                                                    spill = NULL,
                                                    metadata = pData(fs),
                                                    #par = lapply(1:length(fs), function(x){parameters(fs[[x]])}),
                                                    #desc = lapply(1:length(fs), function(x){description(fs[[x]])}),
                                                    #spill = NULL,
                                                    #parameters = NULL,
                                                    transformation = NULL,
                                                    trans_parameters = NULL,
                                                    gates = list(),
                                                    name = input$fs_name, 
                                                    parent = NULL)
        rval$flow_set_selected <- input$fs_name
      }
      
  })

  observeEvent(input$import_gvhd, {
    
    data("GvHD", package = "flowCore")
    assign("fs", GvHD)
    
    rval$flow_set_list[["GvHD"]] <- list(flow_set = fs,
                                                spill = NULL,
                                                metadata = pData(fs),
                                                #par = lapply(1:length(fs), function(x){parameters(fs[[x]])}),
                                                #desc = lapply(1:length(fs), function(x){description(fs[[x]])}),
                                                #spill = NULL,
                                                #parameters = NULL,
                                                transformation = NULL,
                                                trans_parameters = NULL,
                                                gates = list(),
                                                name = "GvHD", 
                                                parent = NULL)
    rval$flow_set_selected <- "GvHD"
    
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