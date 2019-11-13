#' @title   metadataUI and metadata
#' @description  A shiny Module that deals with metadata
#' @param id shiny id
#' @import shiny
#' @importFrom shinydashboard tabBox
#' @importFrom DT DTOutput DTOutput
metadataUI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(width = 6,
           tabBox(title = "Metadata",
                  width = NULL, height = NULL,
                  tabPanel(title = "Table",
                           actionButton(ns("add_column"), label = "add column"),
                           actionButton(ns("delete_column"), label = "delete column"),
                           br(),
                           br(),
                           div(style = 'overflow-x: scroll', DT::DTOutput(ns("pData"))),
                           br(),
                           actionButton(ns("apply"), label = "apply"),
                           downloadButton(ns("download_meta")),
                           br(),
                           br()
                  ),
                  tabPanel(title = "Filter",
                           "Filter samples based on metadata",
                           uiOutput(ns("filter_meta")),
                           textInput(ns("fs_name"), "Flow-set name", "filter")
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
                           div(style = 'overflow-x: scroll', DT::DTOutput(ns("meta"))),
                           br(),
                           actionButton(ns("append_meta"), label = "Add metadata"),
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
#' @param rval A reactive values object
#' @return The updated reactiveValues object \code{rval}
#' @import shiny
#' @importFrom flowWorkspace pData gs_get_pop_paths
#' @importFrom tools file_ext
#' @importFrom readxl read_excel
#' @importFrom utils read.csv write.table
#' @importFrom DT renderDT dataTableProxy editData replaceData
#' @rdname metadataUI
metadata <- function(input, output, session, rval) {
  
  rval_mod <- reactiveValues()

  
  observeEvent(input$apply, {
    validate(need(rval$flow_set, "No flow set available"))
    pData(rval$flow_set) <- rval$pdata
    pData(rval$flow_set_list[[rval$flow_set_selected]]$flow_set) <- rval$pdata
    rval$flow_set_list[[rval$flow_set_selected]]$metadata <- rval$pdata
  })
  
  observe({
    
    validate(
      need(rval$flow_set, "No flow set available")
    )
    
    if(is.null(rval$pdata)){
      rval$pdata <- as.data.frame(pData(rval$flow_set))
    }else if(!setequal(pData(rval$flow_set)$name, rval$pdata$name)){
      rval$pdata <- as.data.frame(pData(rval$flow_set))
    }
    
  })
  
  #Update available keywords
  observe({
    
    validate(need(rval$flow_set, "No flow set available"))
    
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
    
    if(tools::file_ext(input$file_meta$datapath) %in% c("txt", "csv")){
      rval_mod$df_meta_imported <- utils::read.csv(input$file_meta$datapath, sep = sep, header = TRUE, quote = "\"", fill = TRUE, stringsAsFactors = FALSE)
    }else if(tools::file_ext(input$file_meta$datapath) %in% c("xls", "xlsx")){
      rval_mod$df_meta_imported <- readxl::read_excel(input$file_meta$datapath)
    }
    
    
  })
  
  observeEvent(input$append_meta, {
    
    validate(need(rval_mod$df_meta_imported, "no metadata imported"))
    
    df_meta <- as.data.frame(rval_mod$df_meta_imported)
    idx_match <- match(rval$pdata$name, df_meta[,1])
    
    idx_match <- idx_match[!is.na(idx_match)]
    if(length(idx_match)>0){
      df_meta_mapped <- df_meta[idx_match, ]
      idx_new <- ! names(df_meta_mapped) %in% names(rval$pdata) 
      if(sum(idx_new)>0){
        rval$pdata <- cbind(rval$pdata, df_meta_mapped[idx_new])
      }
    }
    
  })
  
  
  observeEvent(input$append_keywords, {
    
    validate(need(rval$flow_set, "no flow-set available"))
    
    df <- NULL
    keys <- input$keyword
    
    if(length(keys)>0){
      for(key in keys){
        df <- cbind(df, fsApply(rval$flow_set, function(x){description(x)[[key]]}))
      }
      keys <- gsub(pattern = " ", replacement = "_", keys, fixed = TRUE)
      keys <- gsub(pattern = "$", replacement = "", keys, fixed = TRUE)
      df <- data.frame(df)
      colnames(df) <- keys
      df$name <- pData(rval$flow_set)$name
      
    }
    
    idx_match <- match(rval$pdata$name, df$name)
    idx_match <- idx_match[!is.na(idx_match)]
    if(length(idx_match)>0){
      df_keywords <- df[idx_match, ]
      idx_new <- ! names(df_keywords) %in% names(rval$pdata)
      if(sum(idx_new)>0){
        rval$pdata <- cbind(rval$pdata, df_keywords[idx_new])
      }
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
    
    if(length(samples)>0){
      
      idx_match <- match(samples, pData(rval$flow_set)$name)
      flow_set_filter <- rval$flow_set[idx_match]
      pdata <- rval$pdata
      if(length(colnames(pdata))>1){
        pData(flow_set_filter) <- pdata[idx_match, ]
      }else{
        pData(flow_set_filter)$name <- pdata[idx_match, ]
      }
      
      fs <- flow_set_filter

      rval$flow_set_list[[input$fs_name]] <- list(flow_set = fs, 
                                                  metadata = pData(fs),
                                                  parameters = lapply(1:length(fs), function(x){parameters(fs[[x]])}),
                                                  desc = lapply(1:length(fs), function(x){description(fs[[x]])}),
                                                  name = input$fs_name, 
                                                  parent = rval$flow_set_selected,
                                                  gates = rval$gates_flowCore[setdiff(gs_get_pop_paths(rval$gating_set), "root")],
                                                  spill = rval$df_spill,
                                                  transformation = rval$transformation,
                                                  trans_parameters = rval$trans_parameters)
      
      rval$flow_set_selected <- input$fs_name

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
  
  
  observeEvent(input$add_column, {
    ns <- session$ns
    showModal(modalDialog(title = "Add column",
                          textInput(ns("col_name"), "Choose a column name", ""),
                          footer = tagList(
                            modalButton("Cancel"),
                            actionButton(ns("ok"), "OK")
                          )
              )
    )

  })
  
  observeEvent(input$ok, {
    if(nchar(input$col_name)>0){
      rval$pdata[[input$col_name]] <- rep("", length(rval$pdata$name))
      removeModal()
    }
  })
  
  observeEvent(input$delete_column, {
    ns <- session$ns
    showModal(modalDialog(title = "Delete column",
                          selectInput(ns("col_name_del"), "Choose a column name", choices = names(rval$pdata), selected = NULL),
                          footer = tagList(
                            modalButton("Cancel"),
                            actionButton(ns("ok_delete"), "OK")
                          )
    )
    )
    
  })
  
  observeEvent(input$ok_delete, {
    if(nchar(input$col_name_del)>0){
      rval$pdata <- rval$pdata[-which(names(rval$pdata) == input$col_name_del)]
      removeModal()
    }
  })
  
  # edit metadata information
  output$pData <- DT::renderDT({validate(need(rval$pdata, "No metadata")); rval$pdata},
                            rownames = FALSE,
                            selection = 'none',
                            editable = 'cell',
                            server = TRUE )

  proxy = DT::dataTableProxy('pData')
  
  observeEvent(input$pData_cell_edit, {
    info = input$pData_cell_edit
    info$col <- info$col + 1
    rval$pdata <<- DT::editData(rval$pdata, info)
    DT::replaceData(proxy, rval$pdata, resetPaging = FALSE)  
  })
  

  output$meta <- DT::renderDT({
    validate(
      need(rval_mod$df_meta_imported, "No meta data imported")
    )
    as.data.frame(rval_mod$df_meta_imported)
  })
  
  output$download_meta <- downloadHandler(
    filename = "metadata.txt",
    content = function(file) {
      utils::write.table(rval$pdata, file = file, row.names = FALSE, quote = FALSE, sep = "\t")
    }
  )
  
  return(rval)
  
}