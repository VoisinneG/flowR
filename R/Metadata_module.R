#' Import, export and edit metadata associated with a GatingSet
#' @param id shiny id
#' @importFrom shinydashboard tabBox
#' @import shiny
#' @importFrom DT DTOutput
#' @export
#' @examples 
#' \dontrun{
#' # library(shiny)
#' library(shinydashboard)
#' library(flowWorkspace)
#' 
#' if (interactive()){
#' 
#'   ui <- dashboardPage(
#'     dashboardHeader(title = "Metadata"),
#'     sidebar = dashboardSidebar(disable = TRUE),
#'     body = dashboardBody(
#'       MetadataUI("module")
#'     )
#'   )
#' 
#'   server <- function(input, output, session) {
#'     rval <- reactiveValues()
#'     observe({
#'       utils::data("GvHD", package = "flowCore")
#'       rval$gating_set <- GatingSet(GvHD)
#'     })
#'     res <- callModule(Metadata, "module", rval = rval)
#'   }
#' 
#'   shinyApp(ui, server)
#' 
#' }
#' }
MetadataUI <- function(id) {
  
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
                           textInput(ns("gs_name"), "GatingSet name", "filter")
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


#' Metadata module server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param rval A reactive values object
#' @return The updated reactiveValues object \code{rval}
#' @import shiny
#' @importFrom flowWorkspace pData sampleNames
#' @importFrom tools file_ext
#' @importFrom readxl read_excel
#' @importFrom utils read.csv write.table
#' @importFrom DT renderDT dataTableProxy editData replaceData
#' @export
#' @rdname MetadataUI
Metadata <- function(input, output, session, rval) {
  
  rval_mod <- reactiveValues()

  observe({
    if(is.null(rval$update_gs)){
      rval$update_gs <- 0
    }
  })
  
  #### Update metadata ####
  observeEvent(input$apply, {
    validate(need(class(rval$gating_set)=="GatingSet", "No GatingSet available"))
    flowWorkspace::pData(rval$gating_set) <- rval_mod$pdata
    if(!is.null(rval$gating_set_selected)){
      if(rval$gating_set_selected %in% names(rval$gating_set_list)){
        flowWorkspace::pData(rval$gating_set_list[[rval$gating_set_selected]]$gating_set) <- rval_mod$pdata
      }
    }
    rval$update_gs <- rval$update_gs + 1
  })
  
  #### Get metadata from GatingSet ####
  observeEvent(rval$update_gs, {
    validate(need(class(rval$gating_set)=="GatingSet", "No GatingSet available"))
    rval_mod$pdata <- as.data.frame(flowWorkspace::pData(rval$gating_set))
  })
  
  #### Update available keywords ####
  observe({
    rval$update_gs
    validate(need(class(rval$gating_set)=="GatingSet", "No GatingSet available"))
    
    ff <- rval$gating_set@data[[1]]
    
    rval_mod$keywords <- names( ff@description )
    
    updateSelectInput(session, "keyword",
                      choices = rval_mod$keywords,
                      selected = NULL)  
  })
  
  #### Import metadata from file ####
  observe({
    
    validate(need(length(input$file_meta)>0, "Please select a file to load"))
    
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
    idx_match <- match(rval_mod$pdata$name, df_meta[,1])
    
    idx_match <- idx_match[!is.na(idx_match)]
    if(length(idx_match)>0){
      df_meta_mapped <- df_meta[idx_match, ]
      idx_new <- ! names(df_meta_mapped) %in% names(rval_mod$pdata) 
      if(sum(idx_new)>0){
        rval_mod$pdata <- cbind(rval_mod$pdata, df_meta_mapped[idx_new])
      }
    }
  })
  
  
  observeEvent(input$append_keywords, {
    
    validate(need(class(rval$gating_set)=="GatingSet", "No GatingSet available"))
    
    df <- NULL
    keys <- input$keyword
    if(length(keys)>0){
      for(key in keys){
        df <- cbind(df, fsApply(rval$gating_set@data, function(x){description(x)[[key]]}))
      }
      keys <- gsub(pattern = " ", replacement = "_", keys, fixed = TRUE)
      keys <- gsub(pattern = "$", replacement = "", keys, fixed = TRUE)
      df <- data.frame(df)
      colnames(df) <- keys
      df$name <- flowWorkspace::pData(rval$gating_set)$name
      #df$name <- flowWorkspace::sampleNames(rval$gating_set)
      
    }
    
    idx_match <- match(rval_mod$pdata$name, df$name)
    idx_match <- idx_match[!is.na(idx_match)]
    
    if(length(idx_match)>0){
      df_keywords <- df[idx_match, ]
      idx_new <- ! names(df_keywords) %in% names(rval_mod$pdata)
      
      if(sum(idx_new)>0){
        rval_mod$pdata <- cbind(rval_mod$pdata, df_keywords[idx_new])
      }
    }
  })

  #### Filter a GatingSet based on metadata ####
  observeEvent(input$apply_filter_meta, {
    
    ns <- session$ns
    
    validate(need(rval_mod$pdata, "No metadata"))
    
    df <- list()
    for(meta_var in names(rval_mod$pdata)){
      df[[meta_var]] <- rval_mod$pdata[[meta_var]] %in% input[[meta_var]]
    }
    
    df1 <- do.call(cbind, df)
    is_selected <- apply(X = df1, MARGIN = 1, FUN = function(x){sum(x) == length(x)})
    samples <- rval_mod$pdata$name[is_selected]
    
    if(length(samples)>0){
      
      if( input$gs_name %in% names(rval$gating_set_list) | nchar(input$gs_name)==0 ){
        showModal(modalDialog(
          title = "Invalid GatingSet name",
          paste("Name is empty or already exists. Please choose another name", sep=""),
          easyClose = TRUE,
          footer = NULL
        ))
      }
      
      validate(need(! input$gs_name %in% names(rval$gating_set_list), "Name already exists" ))
      
      #idx_match <- match(samples, flowWorkspace::pData(rval$gating_set)$name)
      idx_match <- match(samples, flowWorkspace::sampleNames(rval$gating_set))
      gs_filter <- GatingSet(rval$gating_set@data[idx_match])
      gates <- get_gates_from_gs(gs = rval$gating_set)
      add_gates_flowCore(gs = gs_filter, gates = gates)
      gs_filter@compensation <- rval$gating_set@compensation
      gs_filter@transformation <- rval$gating_set@transformation
      flowWorkspace::pData(gs_filter) <- rval_mod$pdata[idx_match, ]
      
      if(!is.null(rval$gating_set_selected)){
        if(rval$gating_set_selected %in% names(rval$gating_set_list)){
          flowWorkspace::pData(rval$gating_set_list[[rval$gating_set_selected]]$gating_set) <- rval_mod$pdata
        }
      }
      
      params <- colnames( gs_filter)[colnames(gs_filter) %in% names(rval$trans_parameters)]
      rval$gating_set_list[[input$gs_name]] <- list(gating_set = gs_filter,
                                                    parent = rval$gating_set_selected,
                                                    trans_parameters = rval$trans_parameters[params])
      rval$gating_set_selected <- input$gs_name
      rval$gating_set <- gs_filter
      rval$update_gs <- rval$update_gs + 1

    }
    
  })
  
  #### Build UI for filtering ####
  output$filter_meta <- renderUI({
    
    ns <- session$ns
    
    validate(need(rval_mod$pdata, "No meta data"))
    
    x <- list()
    
    for(meta_var in names(rval_mod$pdata)){
      uvar <- unique(rval_mod$pdata[[meta_var]])
      
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
  
  #### add/delete metadata column ####
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
      rval_mod$pdata[[input$col_name]] <- rep("", length(rval_mod$pdata$name))
      removeModal()
    }
  })
  
  observeEvent(input$delete_column, {
    ns <- session$ns
    showModal(modalDialog(title = "Delete column",
                          selectInput(ns("col_name_del"), 
                                      "Choose a column name", 
                                      choices = names(rval_mod$pdata), 
                                      selected = NULL),
                          footer = tagList(
                            modalButton("Cancel"),
                            actionButton(ns("ok_delete"), "OK")
                          )
    )
    )
    
  })
  
  observeEvent(input$ok_delete, {
    if(nchar(input$col_name_del)>0){
      rval_mod$pdata <- rval_mod$pdata[-which(names(rval_mod$pdata) == input$col_name_del)]
      removeModal()
    }
  })
  
  #### edit metadata information ####
  output$pData <- DT::renderDT({validate(need(rval_mod$pdata, "No metadata")); rval_mod$pdata},
                            rownames = TRUE,
                            selection = 'none',
                            editable = 'cell',
                            server = TRUE )

  proxy = DT::dataTableProxy('pData')
  
  observeEvent(input$pData_cell_edit, {
    info = input$pData_cell_edit
    info$col <- info$col + 1
    rval_mod$pdata <<- DT::editData(rval_mod$pdata, info)
    DT::replaceData(proxy, rval_mod$pdata, resetPaging = FALSE)  
  })
  
  #### Display/Download metadata ####
  
  output$meta <- DT::renderDT({
    validate(need(rval_mod$df_meta_imported, "No meta data imported"))
    as.data.frame(rval_mod$df_meta_imported)
  })
  
  output$download_meta <- downloadHandler(
    filename = "metadata.txt",
    content = function(file) {
      utils::write.table(rval_mod$pdata, file = file, row.names = TRUE, quote = FALSE, sep = "\t")
    }
  )
  
  return(rval)
  
}


#### Tests ####
#
# library(shiny)
# library(shinydashboard)
# library(flowWorkspace)
# 
# if (interactive()){
# 
#   ui <- dashboardPage(
#     dashboardHeader(title = "Metadata"),
#     sidebar = dashboardSidebar(disable = TRUE),
#     body = dashboardBody(
#       MetadataUI("module")
#     )
#   )
# 
#   server <- function(input, output, session) {
#     rval <- reactiveValues()
#     observe({
#       utils::data("GvHD", package = "flowCore")
#       rval$gating_set <- GatingSet(GvHD)
#     })
#     res <- callModule(Metadata, "module", rval = rval)
#   }
# 
#   shinyApp(ui, server)
# 
# }