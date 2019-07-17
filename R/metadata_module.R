#' @title   metadataUI and metadata
#' @description  A shiny Module that deals with metadata
#' @param id shiny id
#' @importFrom shinydashboard box tabBox
#' @import shiny
#' @import DT
metadataUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(
    column(width = 6,
           tabBox(title = "Metadata",
                  width = NULL, height = NULL,
                  tabPanel(title = "Table",
                           div(style = 'overflow-x: scroll', DT::dataTableOutput(ns("pData")))      
                  ),
                  tabPanel(title = "Filter",
                           "Filter samples based on metadata",
                           uiOutput(ns("filter_meta"))
                  )
           )
    ),
    column(width = 6,
           tabBox(title = "Import",
                  width = NULL, height = NULL,
                  tabPanel(title = "Keywords",
                           selectizeInput(ns("keyword"), "select keywords", 
                                          choices = NULL, 
                                          selected = NULL,
                                          multiple = TRUE),
                           actionButton(ns("append_keywords"), label = "Add keywords"),
                           br()   
                  ),
                  tabPanel(title = "Load",
                           fileInput(ns("file_meta"), "load metadata file", multiple = FALSE),
                           selectInput(ns("sep_meta"), "column separator", choices = c("comma", "semi-column", "tab", "space"), selected = "tab"),
                           div(style = 'overflow-x: scroll', DT::dataTableOutput(ns("meta"))),
                           br(),
                           actionButton(ns("append_meta"), label = "Add metadata"),
                           actionButton(ns("reset_meta"), label = "Reset"),
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
#' @import flowWorkspace
#' @import flowCore
#' @import shiny
#' @import DT
#' @export
#' @rdname importUI
metadata <- function(input, output, session, rval) {
  
  rval_mod <- reactiveValues()
  
  # observe({
  #   validate(
  #     need(rval$pdata, "No meta data available")
  #   )
  #   validate(
  #     need(rval$flow_set, "No flow set available")
  #   )
  #   validate(
  #     need(setequal(pData(rval$flow_set)$name, rval$pdata$name), "Meta data does not match with flow set samples")
  #   )
  #   
  #   pData(rval$flow_set) <- rval$pdata
  #   
  # })
  
  observe({
    
    validate(
      need(rval$flow_set, "No flow set available")
    )
    
    if(is.null(rval$pdata_original)){
      rval_mod$pdata_original <- as.data.frame(pData(rval$flow_set))
    }else if(!setequal(pData(rval$flow_set)$name, rval_mod$pdata_original$name)){
      rval_mod$pdata_original <- as.data.frame(pData(rval$flow_set))
    }
    
  })
  
  #Update available keywords
  observe({
    
    validate(
      need(rval$flow_set, "No flow set available")
    )
    
    ff <- rval$flow_set[[1]]
    
    rval_mod$keywords <- names( ff@description )
    
    updateSelectInput(session, "keyword",
                      choices = rval_mod$keywords,
                      selected = NULL)  
  })
  
  
  observe({
    
    validate(
      need(length(input$file_meta)>0, "Please select a file to load")
    )
    
    sep <- switch(input$sep_meta,
                  "comma" = ",",
                  "semi-column" = ";",
                  "tab" = "\t",
                  "space" = " ")
    
    rval_mod$df_meta_imported <- read.csv(input$file_meta$datapath, sep = sep, header = TRUE, quote = "\"", fill = TRUE, stringsAsFactors = FALSE)
    
  })
  
  observeEvent(input$append_meta, {
    print(rval_mod$df_meta_imported)
    rval_mod$df_meta <- rval_mod$df_meta_imported
  })
  
  observe({
    idx_match <- match(rval_mod$pdata_original$name, rval_mod$df_meta[,1])
    #idx_match <- idx_match[!is.na(idx_match)]
    #if(length(idx_match)>0){
    rval_mod$df_meta_mapped <- rval_mod$df_meta[idx_match, ]
    #  print(rval$df_meta_mapped)
    #}
    
  })
  
  observeEvent(input$reset_meta, {
    rval_mod$df_meta <- NULL
    rval_mod$df_meta_mapped <- NULL
  })
  
  
  
  observeEvent(input$append_keywords, {
    
    if(!is.null(rval$flow_set)){
      
      df <- NULL
      keys <- input$keyword
      
      if(length(keys)>0){
        for(key in keys){
          df <- cbind(df, fsApply(rval$flow_set, function(x){description(x)[[key]]}))
        }
        keys <- gsub(pattern = " ", replacement = "_", keys, fixed = TRUE)
        keys <- gsub(pattern = "$", replacement = "", keys, fixed = TRUE)
        df <- as.data.frame(df)
        names(df) <- keys
        row.names(df) <- pData(rval$flow_set)$name
      }
      rval_mod$df_keywords <- df
    }
  })
  
  
  observe({
    
    cat("meta")
    print(rval_mod$pdata_original)
    
    validate(
      need(rval_mod$pdata_original, "No metadata")
    )
    
    df <- rval_mod$pdata_original
    
    if(!is.null(rval_mod$df_meta_mapped)){
      if(dim(df)[1] == dim(rval_mod$df_meta_mapped)[1]){
        idx_new <- ! names(rval_mod$df_meta_mapped) %in% names(df) 
        if(sum(idx_new)>0){
          df <- cbind(df, rval_mod$df_meta_mapped[idx_new])
        }
      }
    } 
    if(!is.null(rval_mod$df_keywords)){
      if(dim(df)[1] == dim(rval_mod$df_keywords)[1] ){
        idx_new <- ! names(rval_mod$df_keywords) %in% names(df)
        if(sum(idx_new)>0){
          df <- cbind(df, rval_mod$df_keywords[idx_new])
        }
        
      }
    }
    
    if(!is.null(df)){
      rval$pdata <- df
    }
    
  })
  
  
  observeEvent(input$apply_filter_meta, {
    
    ns <- session$ns
    
    validate(
      need(rval$pdata, "No metadata")
    )
    
    df <- list()
    for(meta_var in names(rval$pdata)){
      df[[meta_var]] <- rval$pdata[[meta_var]] %in% input[[meta_var]]
    }
    
    df1 <- do.call(cbind, df)
    is_selected <- apply(X = df1, MARGIN = 1, FUN = function(x){sum(x) == length(x)})
    samples <- rval$pdata$name[is_selected]
    
    #print(samples)
    
    if(length(samples)>0){
      idx_match <- match(samples, pData(rval$flow_set)$name)
      rval$flow_set_filter <- rval$flow_set[idx_match]
      rval$flow_set_names <- unique(c(rval$flow_set_names, "filter"))
      rval$flow_set_selected <- "filter"
      #updateSelectInput(session, ns("flow_set"), choices = rval$flow_set_names, selected = "filter")
    }
    
  })
  
  output$filter_meta <- renderUI({
    
    ns <- session$ns
    
    validate(
      need(rval$pdata, "No meta data")
    )
    
    x <- list()
    
    for(meta_var in names(rval$pdata)){
      uvar <- unique(rval$pdata[[meta_var]])
      
      x[[meta_var]] <- selectizeInput(ns(meta_var), meta_var, 
                                      choices = uvar, 
                                      selected = uvar,
                                      multiple = TRUE)
      
    }
    
    if(length(x)>0){
      x[["apply_filter_meta"]] <- actionButton(ns("apply_filter_meta"), "apply filter")
    }
    
    tagList(x)
    
  })
  
  output$pData <- DT::renderDataTable({
    if(!is.null(rval$flow_set)){
      DT::datatable(rval$pdata, rownames = FALSE)
    }
  })
  
  output$meta <- DT::renderDataTable({
    validate(
      need(rval_mod$df_meta_imported, "No meta data imported")
    )
    as.data.frame(rval_mod$df_meta_imported)
  })
  
  
  return(rval)
  
}