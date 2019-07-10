#' @title   metadataUI and metadata
#' @description  A shiny Module that deals with metadata
#' @param id shiny id
#' @importFrom shinydashboard box
#' @import shiny
metadataUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(
    column(width = 6,
           tabBox(title = "Metadata",
                  width = NULL, height = NULL,
                  tabPanel(title = "Table",
                           div(style = 'overflow-x: scroll', DT::dataTableOutput("pData"))      
                  ),
                  tabPanel(title = "Filter",
                           "Filter samples based on metadata",
                           uiOutput("filter_meta")
                  )
           )
    ),
    column(width = 6,
           tabBox(title = "Import",
                  width = NULL, height = NULL,
                  tabPanel(title = "Keywords",
                           selectizeInput("keyword", "select keywords", 
                                          choices = NULL, 
                                          selected = NULL,
                                          multiple = TRUE),
                           actionButton("append_keywords", label = "Add keywords"),
                           br()   
                  ),
                  tabPanel(title = "Load",
                           fileInput("file_meta", "load metadata file", multiple = FALSE),
                           selectInput("sep_meta", "column separator", choices = c("comma", "semi-column", "tab", "space"), selected = "tab"),
                           div(style = 'overflow-x: scroll', DT::dataTableOutput("meta")),
                           br(),
                           actionButton("append_meta", label = "Add metadata"),
                           actionButton("reset_meta", label = "Reset"),
                           br()
                           
                  )
           )
    )
    
  )
  
}

#' metadata server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @return a reactivevalues object with values "df_files", "flow_set_imported" and "gates_flowCore"
#' @importFrom CytoML parseWorkspace, openWorkspace
#' @import flowWorkspace
#' @import flowCore
#' @import ncdfFlow
#' @import shiny
#' @export
#' @rdname importUI
metadata <- function(input, output, session) {
  
  rval <- reactiveValues(df_files = NULL,
                         flow_set_imported = NULL,
                         gates_flowCore = list()
  )
  
  observeEvent(input$files, {
    validate(
      need(input$files$datapath, "Please select a file to import")
    )
    df <- input$files
    file.rename(from = df$datapath, to = paste(dirname(df$datapath[1]),"/", df$name, sep =""))
    df$datapath <- paste(dirname(df$datapath[1]),"/", df$name, sep ="")
    rval$df_files <- df
  })
  
  
  ##########################################################################################################
  # Select group within a workspace
  observe({
    validate(
      need(length(input$files_table_rows_selected)>0, "Please select a file to load")
    )
    if(file_ext(rval$df_files$datapath[input$files_table_rows_selected[1]]) %in% c("xml", "wsp") ){
      ws <- openWorkspace(rval$df_files$datapath[input$files_table_rows_selected[1]])
      groups <- unique(getSampleGroups(ws)$groupName)
      updateSelectInput(session, ns("groups"), choices = groups, selected = groups[1])
    }
    
  })
  
  observeEvent(input$load, {
    
    validate(
      need(length(input$files_table_rows_selected)>0, "Please select a file to load")
    )
    
    # Create a Progress object
    progress <- shiny::Progress$new(min = 0, max = 1)
    on.exit(progress$close())
    progress$set(message = "Loading data", value = 0.5)
    
    
    
    
    if(file_ext(rval$df_files$datapath[input$files_table_rows_selected[1]]) %in% c("xml", "wsp")){
      ws <- openWorkspace(rval$df_files$datapath[input$files_table_rows_selected[1]])
      
      gs <- try(parseWorkspace(ws,
                               name = input$groups,
                               execute = TRUE,
                               isNcdf = TRUE,
                               sampNloc = "sampleNode",
                               path = dirname(rval$df_files$datapath[1])),
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
      if(file_ext(rval$df_files$datapath[input$files_table_rows_selected[1]]) %in% c("wsp")){
        gates <- get_gates_from_ws(ws_path = rval$df_files$datapath[input$files_table_rows_selected[1]], 
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
      
      if(input$apply_biexp_inverse){
        trans.log <- flowJo_biexp_inverse_trans()
      }else{
        trans.log <- identity_trans()
      }
      
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
      idx_match <- match(names_imported, rval$df_files$name)
      
      fs <- ncdfFlow::read.ncdfFlowSet( rval$df_files$datapath[idx_match] )
      phenoData(fs)$name <- rval$df_files$name[idx_match]
      
      
    }else{
      fs <- ncdfFlow::read.ncdfFlowSet( rval$df_files$datapath[input$files_table_rows_selected] , emptyValue=FALSE)
      phenoData(fs)$name <- rval$df_files$name[input$files_table_rows_selected]
    }
    
    #Initialization of some reactive variables
    
    rval$flow_set_imported <- fs
    
    
  })
  
  
  
  output$files_table <- DT::renderDataTable({
    validate(
      need(rval$df_files, "Please select a file to import")
    )
    df <- rval$df_files[ ,c("name", "size")]
    df$new_name <- basename(rval$df_files$datapath)
    df$dir_name <- dirname(rval$df_files$datapath)
    df
  })
  
  return(rval)
  
}