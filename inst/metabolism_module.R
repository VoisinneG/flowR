
metabolismUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  
  ns <- NS(id)
  
  fluidRow(
    column(width = 4,
           box(width = NULL, height = NULL, title = "Parameters",
               box(collapsible = TRUE, collapsed = TRUE, width = NULL, height = NULL,
                   title = "Samples",
                   selectInput(ns("A_sample"),
                                  label = "A sample",
                                  choices = NULL,
                                  selected = NULL),
                   selectInput(ns("B_sample"),
                               label = "B sample",
                               choices = NULL,
                               selected = NULL),
                   selectInput(ns("C_sample"),
                               label = "C sample",
                               choices = NULL,
                               selected = NULL),
                   selectInput(ns("Z_sample"),
                               label = "Z sample",
                               choices = NULL,
                               selected = NULL)
               ),
               box(collapsible = TRUE, collapsed = TRUE, width = NULL, height = NULL,
                   title = "Subset",
                   selectizeInput(ns("gate"),
                                  label = "subset",
                                  choices = "root",
                                  selected = "root",
                                  multiple = TRUE)
               ),
               box(collapsible = TRUE, collapsed = FALSE, width = NULL, height = NULL,
                   title ="Reporter",
                   selectInput(ns("yvar"), 
                               label = "reporter variable", 
                               choices = NULL,
                               selected = NULL),
                   selectInput(ns("stat_function"), 
                               label = "statistics", 
                               choices = c("mean", "median"), 
                               selected = "mean"),
                   selectInput(ns("y_trans"), 
                               label = "Transform", 
                               choices = c("log10", "asinh", "identity", "default"), 
                               selected = "default")
               )
               
           ),
           box(width = NULL, height = NULL, title = "Create Flow-set",
               textInput(ns("fs_name"), "Flow-set name", "sub-sample"),
               actionButton(ns("create"), "Create Flow-set")
          )
    ),
    column(width = 8,
           tabBox(title = "",
                  width = NULL, height = NULL,
                  tabPanel(title = "Plot",
                           plotOutput(ns("plot1")),
                           plotOutput(ns("plot2"))
                  ),
                  tabPanel(title = "Data",
                           br(),
                           br(),
                           div(style = 'overflow-x: scroll', DT::dataTableOutput(ns("data"))),
                           br()
                  )
           )
    )
  )

}


#' metabolism server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @return a reactivevalues object with values "df_files", "flow_set_imported" and "gates_flowCore"
#' @import flowWorkspace
#' @import flowCore
#' @import shiny
#' @import DT
#' @export
#' @rdname metabolismUI
metabolism <- function(input, output, session, rval) {
  
  observe({
    validate(need(rval$plot_var, "No variables available"))
    updateSelectInput(session, "yvar", choices = rval$plot_var, selected = rval$plot_var[1]) 
  })
  
  observe({
    validate(need(rval$gates_flowCore, "no gating set"))
    updateSelectInput(session, "gate", choices = union("root", names(rval$gates_flowCore)), selected = "root")
  })
    
  observe({
    validate(need(rval$pdata, "no metadata available"))
    updateSelectInput(session, "A_sample", choices = rval$pdata$name, selected = rval$pdata$name[1])
    updateSelectInput(session, "B_sample", choices = rval$pdata$name, selected = rval$pdata$name[1])
    updateSelectInput(session, "C_sample", choices = rval$pdata$name, selected = rval$pdata$name[1])
    updateSelectInput(session, "Z_sample", choices = rval$pdata$name, selected = rval$pdata$name[1])
  }) 
  
  raw_data <- reactive({
    validate(need(rval$gating_set, "Empty gating set"))
    validate(need(input$A_sample, "Please select samples"))
    validate(need(input$B_sample, "Please select samples"))
    validate(need(input$C_sample, "Please select samples"))
    validate(need(input$Z_sample, "Please select samples"))
    validate(need(input$gate, "Please select subsets"))
    
    
    df <- get_data_gs(gs = rval$gating_set,
                      sample = unique(c(input$A_sample, input$B_sample, input$C_sample, input$Z_sample)),
                      subset = input$gate,
                      spill = rval$spill)
    df
    
 })
  
 reporter_data <- reactive({  
   
    validate(need(input$yvar, "Please select a readout variable"))
   
    df <- raw_data()
    
    idx_y <- match(input$yvar, rval$parameters$name_long)
    yvar <- rval$parameters$name[idx_y]
    
    if(input$y_trans == "default"){
      if(rval$apply_trans){
        y_trans <- rval$transformation[[yvar]]
      }else{
        y_trans <- identity_trans()
      }
    }else{
      y_trans <- switch(input$y_trans,
                        "log10" = log10_trans(),
                        "asinh" = asinh_trans(),
                        "identity" = identity_trans(),
                        NULL)
    }
    
    
    df[[yvar]] <- y_trans$transform(df[[yvar]])

    df_melt <- melt(df, id.vars = c("name", "subset"), measure.vars = yvar)
    df_melt <- df_melt[is.finite(df_melt$value), ]
    
    stat.fun <- function(...){do.call(input$stat_function, args = list(...))}
    df_cast <- dcast(df_melt, name + subset ~ variable, stat.fun, na.rm = TRUE)
    df_cast[[yvar]] <- y_trans$inverse(df_cast[[yvar]])
    df_cast <- rename(df_cast, "value" = yvar) %>% arrange(subset)
    df_cast
    
  })
  
  metabo_data <- reactive({
    df <- reporter_data()
    df_melt <- melt(df, id.vars = c("name", "subset"), measure.vars = "value")
    df_cast <- dcast(df_melt, subset ~ name + variable, mean, na.rm = TRUE)
    
    df_cast[["score_gluc_dep"]] <- (df_cast[[paste0(input$A_sample, "_value")]] - df_cast[[paste0(input$B_sample, "_value")]]) /
      (df_cast[[paste0(input$A_sample, "_value")]] - df_cast[[paste0(input$Z_sample, "_value")]]) * 100
    
    df_cast[["score_mito_dep"]] <- (df_cast[[paste0(input$A_sample, "_value")]] - df_cast[[paste0(input$C_sample, "_value")]]) /
      (df_cast[[paste0(input$A_sample, "_value")]] - df_cast[[paste0(input$Z_sample, "_value")]]) * 100
    
    df_cast
  })
  
  plot_gluc_dep <- reactive({
    df <- metabo_data()
    p <- ggplot(df, aes(x=subset, y=score_gluc_dep)) + geom_col()
    p
  })
  
  plot_mito_dep <- reactive({
    df <- metabo_data()
    p <- ggplot(df, aes(x=subset, y=score_mito_dep)) + geom_col()
    p
  })
  
  output$plot1 <- renderPlot({
    plot_gluc_dep()
  })
  
  output$plot2 <- renderPlot({
    plot_mito_dep()
  })
  
  # pop_stats <- reactive({
  #   validate(need(rval$gating_set, "No gating set available"))
  #   df <- getPopStats(rval$gating_set)
  #   df <- df[df$name %in% res$params$samples, ]
  #   df[['%']] <- sprintf("%.1f", df$Count / df$ParentCount * 100)
  #   df <- df[, c("name", "Population", "Parent", "%", "Count", "ParentCount")]
  #   
  # })
  
  output$data <- DT::renderDataTable({
    df <- metabo_data()
    #df[["value"]] <- sprintf("%.2f", df[["value"]])
    DT::datatable(df, rownames = FALSE)
  })

  observeEvent(input$create, {

    if( input$fs_name %in% names(rval$flow_set_list) ){
      showModal(modalDialog(
        title = "Name already exists",
        paste("Please choose another name", sep=""),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    validate(need(! input$fs_name %in% names(rval$flow_set_list), "Name already exists" ))
    
    df <- raw_data()
    df_metabo <- metabo_data()
    
    df[["score_gluc_dep"]] <- df_metabo[["score_gluc_dep"]][match(df$subset, df_metabo$subset)]
    df[["score_mito_dep"]] <- df_metabo[["score_mito_dep"]][match(df$subset, df_metabo$subset)]
    
    fs <- build_flowset_from_df(df, 
                                origin = rval$flow_set_list[[rval$flow_set_selected]])
    
    rval$flow_set_list[[input$fs_name]] <- list(flow_set = fs, 
                                                par = lapply(1:length(fs), function(x){parameters(fs[[x]])}),
                                                desc = lapply(1:length(fs), function(x){description(fs[[x]])}),
                                                name = input$fs_name, 
                                                parent = rval$flow_set_selected,
                                                gates = rval$gates_flowCore[setdiff(getNodes(rval$gating_set), "root")],
                                                spill = rval$df_spill,
                                                transformation = rval$transformation,
                                                trans_parameters = rval$trans_parameters)
    
    rval$flow_set_selected <- input$fs_name
    
  })
  
  # output$download_data <- downloadHandler(
  #   filename = "pop_stats.txt",
  #   content = function(file) {
  #     write.table(pop_stats(), file = file, row.names = FALSE, quote = FALSE, sep = "\t")
  #   }
  # )
  
  return( rval )
  
}