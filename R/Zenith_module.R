utils::globalVariables(c("A", "B", "C", "Z", "sdA", "sdB", "sdC", "sdZ"))

#' Analyse cell metabolism using the ZeNITH method
#' @param id shiny id
#' @import shiny
#' @importFrom shinydashboard box tabBox
#' @importFrom DT DTOutput
#' @export
ZenithUI <- function(id) {

  ns <- NS(id)
  
  fluidRow(
    column(width = 4,
           box(width = NULL, height = NULL, title = "Parameters",
               box(collapsible = TRUE, collapsed = TRUE, width = NULL, height = NULL,
                   title = "Samples",
                   selectizeInput(ns("A_sample"),
                                  label = "A sample",
                                  multiple = TRUE,
                                  choices = NULL,
                                  selected = NULL
                                  ),
                   selectizeInput(ns("B_sample"),
                               label = "B sample",
                               multiple = TRUE,
                               choices = NULL,
                               selected = NULL),
                   selectizeInput(ns("C_sample"),
                               label = "C sample",
                               multiple = TRUE,
                               choices = NULL,
                               selected = NULL),
                   selectizeInput(ns("Z_sample"),
                               label = "Z sample",
                               multiple = TRUE,
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
               textInput(ns("fs_name"), "Flow-set name", "zenith"),
               actionButton(ns("create"), "Create Flow-set")
          )
    ),
    column(width = 8,
           tabBox(title = "",
                  width = NULL, height = NULL,
                  tabPanel(title = "Plot",
                           plotlyOutput(ns("plot1")),
                           plotlyOutput(ns("plot2"))
                  ),
                  tabPanel(title = "Options",
                           checkboxInput(ns("show_error_bar"), "show error bar (multiple samples selected)", TRUE),
                           checkboxInput(ns("set_y_lim"), "Set y axis limits", TRUE),
                           numericInput(ns("ymin"), "min y value", 0),
                           numericInput(ns("ymax"), "max y value", 100)
                  ),
                  tabPanel(title = "Data",
                           br(),
                           br(),
                           div(style = 'overflow-x: scroll', DT::DTOutput(ns("data"))),
                           br()
                  )
           )
    )
  )

}


#' Zenith module server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param rval A reactive values object
#' @return The updated reactiveValues object \code{rval}
#' @import shiny
#' @importFrom flowWorkspace gs_get_pop_paths
#' @importFrom reshape2 melt dcast
#' @importFrom dplyr rename arrange mutate
#' @importFrom magrittr %>%
#' @importFrom stats sd
#' @importFrom DT datatable renderDT
#' @import ggplot2
#' @export
#' @rdname ZenithUI
Zenith <- function(input, output, session, rval) {
  
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
    
    samples_table <- table(c(input$A_sample, input$B_sample, input$C_sample, input$Z_sample))
    
    validate(need(sum(samples_table>1)==0, 
                  "Please select different samples for A, B, C and Z conditions"))
      
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

    df_melt <- reshape2::melt(df, id.vars = c("name", "subset"), measure.vars = yvar)
    df_melt <- df_melt[is.finite(df_melt$value), ]
    
    stat.fun <- function(...){do.call(input$stat_function, args = list(...))}
    df_cast <- reshape2::dcast(df_melt, name + subset ~ variable, stat.fun, na.rm = TRUE)
    df_cast[[yvar]] <- y_trans$inverse(df_cast[[yvar]])
    df_cast <- dplyr::rename(df_cast, "value" = yvar) %>% dplyr::arrange(subset)
    df_cast
    
  })
  
  metabo_data <- reactive({
    df <- reporter_data()
    df_melt <- reshape2::melt(df, id.vars = c("name", "subset"), measure.vars = "value")
    df_cast <- reshape2::dcast(df_melt, subset ~ name + variable, mean, na.rm = TRUE)
    
    df_melt2 <- reshape2::melt(df_cast)
    df_melt2$variable <- as.character(df_melt2$variable)
    df_melt2$variable[df_melt2$variable %in% paste0(input$A_sample, "_value")] <- "A"
    df_melt2$variable[df_melt2$variable %in% paste0(input$B_sample, "_value")] <- "B"
    df_melt2$variable[df_melt2$variable %in% paste0(input$C_sample, "_value")] <- "C"
    df_melt2$variable[df_melt2$variable %in% paste0(input$Z_sample, "_value")] <- "Z"
    
    df_mean <- reshape2::dcast(df_melt2, subset ~ variable, mean)
    df_sd <- reshape2::dcast(df_melt2, subset ~ variable, stats::sd)
    names(df_sd) <- paste0("sd", names(df_sd))
    
    df_mean <- cbind(df_mean, df_sd[-1])
    
    df_mean <- df_mean %>% dplyr::mutate(mean_score_gluc_dep = 100*(A-B)/(A-Z), 
                                  mean_score_mito_dep = 100*(A-C)/(A-Z),
                                  sd_score_gluc_dep = 100*(abs(sdA*(Z-B)/(A-Z)^2) + abs(sdB/(Z-A)) + abs(sdZ*(A-B)/(A-Z)^2)),
                                  sd_score_mito_dep = 100*(abs(sdA*(Z-C)/(A-Z)^2) + abs(sdC/(Z-A)) + abs(sdZ*(A-C)/(A-Z)^2)))

    df_mean
  })
  
  plot_gluc_dep <- reactive({
    df <- metabo_data()
    df[["ymin"]] <-  df[["mean_score_gluc_dep"]] -  df[["sd_score_gluc_dep"]]
    df[["ymax"]] <-  df[["mean_score_gluc_dep"]] +  df[["sd_score_gluc_dep"]]
    df[["subset"]] <- factor(df[["subset"]], levels = input$gate)
    p <- ggplot(df, aes_string(x="subset", y="mean_score_gluc_dep", color = "subset", fill = "subset")) +
      geom_col(alpha = 0.5)
    
    if(input$show_error_bar){
      p <- p + geom_errorbar(mapping = aes_string(ymin = "ymin", ymax = "ymax"), width = 0.25)
    }
    
    
    if(input$set_y_lim){
      p <- p + coord_cartesian(ylim = c(input$ymin, input$ymax))
    }
    p
  })
  
  plot_mito_dep <- reactive({
    df <- metabo_data()
    df[["ymin"]] <-  df[["mean_score_mito_dep"]] -  df[["sd_score_mito_dep"]]
    df[["ymax"]] <-  df[["mean_score_mito_dep"]] +  df[["sd_score_mito_dep"]]
    df[["subset"]] <- factor(df[["subset"]], levels = input$gate)
    
    p <- ggplot(df, aes_string(x="subset", y="mean_score_mito_dep", color = "subset", fill = "subset")) + 
      geom_col(alpha = 0.5)
    
    if(input$show_error_bar){
      p <- p + geom_errorbar(mapping = aes_string(ymin = "ymin", ymax = "ymax"), width = 0.25)
    }
    
    if(input$set_y_lim){
      p <- p + coord_cartesian(ylim = c(input$ymin, input$ymax))
    }
    
    p
  })
  
  output$plot1 <- renderPlotly({
    plot_gluc_dep()
  })
  
  output$plot2 <- renderPlotly({
    plot_mito_dep()
  })
  
  output$data <- DT::renderDT({
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
    
    df[["mean_score_gluc_dep"]] <- df_metabo[["mean_score_gluc_dep"]][match(df$subset, df_metabo$subset)]
    df[["mean_score_mito_dep"]] <- df_metabo[["mean_score_mito_dep"]][match(df$subset, df_metabo$subset)]
    
    fs <- build_flowset_from_df(df, 
                                origin = rval$flow_set_list[[rval$flow_set_selected]]$flow_set)
    
    rval$flow_set_list[[input$fs_name]] <- list(flow_set = fs, 
                                                par = lapply(1:length(fs), function(x){parameters(fs[[x]])}),
                                                desc = lapply(1:length(fs), function(x){description(fs[[x]])}),
                                                name = input$fs_name, 
                                                parent = rval$flow_set_selected,
                                                gates = rval$gates_flowCore[setdiff(gs_get_pop_paths(rval$gating_set), "root")],
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