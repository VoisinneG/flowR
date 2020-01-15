utils::globalVariables(c("A", "B", "C", "Z", "sdA", "sdB", "sdC", "sdZ"))

#' Analyse cell metabolism using the ZeNITH method
#' @param id shiny id
#' @import shiny
#' @importFrom shinydashboard box tabBox
#' @importFrom DT DTOutput
#' @importFrom plotly plotlyOutput
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
                   selectizeInput(ns("subset"),
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
           box(width = NULL, height = NULL, title = "Create GatingSet",
               textInput(ns("gs_name"), "GatingSet name", "zenith"),
               actionButton(ns("create"), "Create GatingSet")
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
                           br(),
                           downloadButton(ns("download_data"), label = "Download")
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
#' @importFrom plotly renderPlotly
#' @import ggplot2
#' @export
#' @rdname ZenithUI
Zenith <- function(input, output, session, rval) {
  
  observe({
    if(is.null(rval$update_gs)){
      rval$update_gs <- 0
    }
  })
  
  ### get parameters from GatingSet ################################################################
  choices <- reactive({
    rval$update_gs
    validate(need(class(rval$gating_set) == "GatingSet", "input is not a GatingSet"))
    
    plot_var <- parameters(rval$gating_set@data[[1]])$name
    
    validate(need(length(plot_var)>0, "No variables in GatingSet"))

    desc <- parameters(rval$gating_set@data[[1]])$desc
    
    labels <- sapply(1:length(plot_var), function(x){
      if(is.na(desc[x])){
        plot_var[x]
      }else{
        paste(plot_var[x], "(", desc[x], ")")
      }
    })
    names(plot_var) <- labels
    names(labels) <- plot_var 
    
    return( 
      list(sample = pData(rval$gating_set)$name,
           subset = gs_get_pop_paths(rval$gating_set),
           plot_var = plot_var,
           transformation = rval$gating_set@transformation,
           compensation = rval$gating_set@compensation
      )
    )
  })
  
  ### Update UI ####################################################################################
  
  observe({
    updateSelectInput(session, "yvar", 
                      choices = choices()$plot_var, 
                      selected = choices()$plot_var[1]) 
  })
  
  observe({
    updateSelectInput(session, "subset", 
                      choices = choices()$subset, 
                      selected = "root")
  })
    
  observe({
    updateSelectInput(session, "A_sample", choices = choices()$sample, selected = choices()$sample[1])
    updateSelectInput(session, "B_sample", choices = choices()$sample, selected = choices()$sample[1])
    updateSelectInput(session, "C_sample", choices = choices()$sample, selected = choices()$sample[1])
    updateSelectInput(session, "Z_sample", choices = choices()$sample, selected = choices()$sample[1])
  }) 
  
  ### Get raw data  #################################################################################
  raw_data <- reactive({
    validate(need(class(rval$gating_set) == "GatingSet", "input is not a GatingSet"))
    validate(need(input$A_sample, "Please select samples"))
    validate(need(input$B_sample, "Please select samples"))
    validate(need(input$C_sample, "Please select samples"))
    validate(need(input$Z_sample, "Please select samples"))
    
    samples_table <- table(c(input$A_sample, input$B_sample, input$C_sample, input$Z_sample))
    
    validate(need(sum(samples_table>1)==0, 
                  "Please select different samples for A, B, C and Z conditions"))
      
    validate(need(input$subset, "Please select subsets"))
    
    df <- get_data_gs(gs = rval$gating_set,
                      sample = unique(c(input$A_sample, 
                                        input$B_sample, 
                                        input$C_sample, 
                                        input$Z_sample)),
                      subset = input$subset,
                      spill = choices()$compensation, 
                      return_comp_data = TRUE)
    df
    
 })

 ### Transform puromycin reporter intensity  ######################################################
  
 reporter_data <- reactive({  
   
    validate(need(input$yvar, "Please select a readout variable"))
   
    df <- raw_data()
    yvar <- input$yvar
    
    y_trans <- identity_trans()
    if(input$y_trans == "default"){
      if(!is.null(rval$apply_trans)){
        y_trans <- choices()$transformation[[yvar]]
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
  
  ### Compute dependency on glycolysis and ox-phos #################################################
  
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
                                  sd_score_gluc_dep = 100*(abs(sdA*(Z-B)/(A-Z)^2) + 
                                                             abs(sdB/(Z-A)) + 
                                                             abs(sdZ*(A-B)/(A-Z)^2)),
                                  sd_score_mito_dep = 100*(abs(sdA*(Z-C)/(A-Z)^2) + 
                                                             abs(sdC/(Z-A)) + 
                                                             abs(sdZ*(A-C)/(A-Z)^2)))

    df_mean
  })
  
  ### Plot results ##################################################################################
  
  plot_gluc_dep <- reactive({
    df <- metabo_data()
    
    df[["ymin"]] <-  df[["mean_score_gluc_dep"]] -  df[["sd_score_gluc_dep"]]
    df[["ymax"]] <-  df[["mean_score_gluc_dep"]] +  df[["sd_score_gluc_dep"]]
    df[["subset"]] <- factor(df[["subset"]], levels = input$subset)
    
    p <- ggplot(df, aes_string(x="subset", 
                               y="mean_score_gluc_dep", 
                               color = "subset", 
                               fill = "subset")) +
      ylab("%") +
      ggtitle("Dependency on Glycolysis") + 
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
    df[["subset"]] <- factor(df[["subset"]], levels = input$subset)
    
    p <- ggplot(df, aes_string(x="subset", 
                               y="mean_score_mito_dep", 
                               color = "subset", 
                               fill = "subset")) + 
      ggtitle("Dependency on Ox-Phos") + 
      ylab("%") + 
      geom_col(alpha = 0.5)
    
    if(input$show_error_bar){
      p <- p + geom_errorbar(mapping = aes_string(ymin = "ymin", ymax = "ymax"), width = 0.25)
    }
    
    if(input$set_y_lim){
      p <- p + coord_cartesian(ylim = c(input$ymin, input$ymax))
    }
    
    p
  })
  
  output$plot1 <- plotly::renderPlotly({
    plot_gluc_dep()
  })

  output$plot2 <- plotly::renderPlotly({
    plot_mito_dep()
  })
  
  #### Display/Download data ####
  
  output$data <- DT::renderDT({
    df <- metabo_data()
    #df[["value"]] <- sprintf("%.2f", df[["value"]])
    DT::datatable(df, rownames = FALSE)
  })
  
  output$download_data <- downloadHandler(
    filename = "zenith_results.txt",
    content = function(file) {
      utils::write.table(metabo_data(), file = file, row.names = FALSE, quote = FALSE, sep = "\t")
    }
  )

  
  ### Create GatingSet #############################################################################
  
  observeEvent(input$create, {

    if( input$gs_name %in% names(rval$gating_set_list) ){
      showModal(modalDialog(
        title = "Name already exists",
        paste("Please choose another name", sep=""),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    validate(need(! input$gs_name %in% names(rval$gating_set_list), "Name already exists" ))
    
    df <- raw_data()
    df_metabo <- metabo_data()
    idx_match <- match(df$subset, df_metabo$subset)
    df[["mean_score_gluc_dep"]] <- df_metabo[["mean_score_gluc_dep"]][idx_match]
    df[["mean_score_mito_dep"]] <- df_metabo[["mean_score_mito_dep"]][idx_match]
    
    fs <- build_flowset_from_df(df, 
                                origin = rval$gating_set@data)
    
    gs <- GatingSet(fs)
    gates <- get_gates_from_gs(rval$gating_set)
    add_gates_flowCore(gs = gs, gates = gates)
    gs@compensation <- choices()$compensation
    gs@transformation <- choices()$transformation
    
    if(!is.null(rval$gating_set_selected)){
      if(rval$gating_set_selected %in% names(rval$gating_set_list)){
        rval$gating_set_list[[input$gs_name]] <- list(gating_set = gs,
                                                      parent = rval$gating_set_selected)
        
        rval$gating_set_selected <- input$gs_name
        rval$update_gs <- rval$update_gs + 1
      }
    }
    
  })
  
  return( rval )
  
}


##################################################################################
# Tests
##################################################################################
# 
# library(shiny)
# library(shinydashboard)
# library(flowWorkspace)
# library(flowCore)
# library(ggplot2)
# library(plotly)
# if (interactive()){
# 
#   ui <- dashboardPage(
#     dashboardHeader(title = "Zenith"),
#     sidebar = dashboardSidebar(disable = TRUE),
#     body = dashboardBody(
#       fluidRow(
#         column(12, box(width = NULL, ZenithUI("module")))
#       )
#     )
#   )
# 
#   server <- function(input, output, session) {
# 
#     rval <- reactiveValues()
# 
#     observe({
#       load("../flowR_utils/demo-data/Rafa2Gui/analysis/cluster.rda")
#       gs <- GatingSet(res$cluster$flow_set)
#       add_gates_flowCore(gs, res$cluster$gates)
#       rval$gating_set <- gs
#       #utils::data("GvHD", package = "flowCore")
#       #rval$gating_set <- GatingSet(GvHD)
#     })
# 
#     rval <- callModule(Zenith, "module", rval = rval)
# 
#   }
# 
#   shinyApp(ui, server)
# 
# }