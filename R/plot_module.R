#' @title   plotUI and plot
#' @description  A shiny Module that deals with metadata
#' @param id shiny id
#' @importFrom shinydashboard box tabBox
#' @import shiny
#' @import DT
plotUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(
    column(width = 4,
           tabBox(title = "",
                  width = NULL, height = NULL,
                  tabPanel("Samples",
                           div(style = 'overflow-x: scroll', DT::dataTableOutput(ns("files_selection_table")))
                  ),
                  tabPanel("Subset",
                           selectizeInput(ns("gate"), 
                                          label = "subset", 
                                          choices = "root", 
                                          selected = "root",
                                          multiple = TRUE)
                  ),
                  tabPanel("Variables",
                           selectizeInput(ns("xvar"), 
                                          multiple = TRUE,
                                          label = "x variable", 
                                          choices = NULL, 
                                          selected = NULL),
                           selectizeInput(ns("yvar"), 
                                          multiple = TRUE,
                                          label = "y variable", 
                                          choices = NULL, 
                                          selected = NULL),
                           selectizeInput(ns("color_var"), 
                                          multiple = TRUE,
                                          label = "color variable",
                                          choices = "none",
                                          selected = "none"),
                           selectInput(ns("split_variable"),
                                       label = "select variable used to split plots",
                                       choices = c("x variable", "y variable", "color variable"),
                                       selected = "x variable"
                           ),
                           numericInput(ns("nrow_split"), label = "Number of rows", value = 1)
                           
                           
                  )
           ) 
    ),
    column(width = 8,
           tabBox(title = "Plot",
                  width = NULL, height = NULL,
                  tabPanel("Plot",
                           actionButton(ns("update_plot"), "update"),
                           br(),
                           uiOutput(ns("ui_plot"))
                           #plotOutput("plot_focus", height = input$nrow_split * 300)
                           
                  ),
                  tabPanel("Options",
                           selectInput(ns("plot_type"), label = "plot type",
                                       choices = c("hexagonal", "histogram", "dots", "contour"),
                                       selected = "histogram"),
                           checkboxInput(ns("legend"), "show legend", value = TRUE),
                           checkboxInput(ns("norm"), "normalize (set max to 1)", value = TRUE),
                           checkboxInput(ns("smooth"), "smooth", value = FALSE),
                           checkboxInput(ns("ridges"), "ridges", value = FALSE),
                           selectizeInput(ns("facet_var"), 
                                          multiple =TRUE,
                                          label = "facet variables", 
                                          choices = "name", 
                                          selected = "name"),
                           selectizeInput(ns("group_var"), 
                                          multiple =FALSE,
                                          label = "group variable", 
                                          choices = c("name","subset"), 
                                          selected = "subset"),
                           selectizeInput(ns("yridges_var"), 
                                          multiple =FALSE,
                                          label = "y ridges variable", 
                                          choices = c("name","subset"), 
                                          selected = "subset"),
                           selectInput(ns("legend_pos"), label = "legend position",
                                       choices = c("right", "top", "left", "bottom"),
                                       selected = "right"),
                           numericInput(ns("bin_number"), label = "number of bins", value = 50),
                           numericInput(ns("alpha"), label = "alpha", value = 0.5),
                           numericInput(ns("size"), label = "size", value = 1),
                           numericInput(ns("row_size"), label = "row size (px)", value = 400)
                  ),
                  tabPanel("Save",
                           numericInput(ns("width_plot"), label = "width", value = 5),
                           numericInput(ns("height_plot"), label = "height", value = 5),
                           downloadButton(ns("download_plot"), "Save plot")
                  )
                  
           )
           
    )
    
  )
  
}


#' transform server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @return a reactivevalues object with values "df_files", "flow_set_imported" and "gates_flowCore"
#' @import flowWorkspace
#' @import flowCore
#' @import shiny
#' @import DT
#' @export
#' @rdname plotUI
plot<- function(input, output, session, rval) {
  
  `%then%` <- shiny:::`%OR%`
  
  observe({
    
    validate(
      need(rval$pdata, "No metadata available")
    )
    validate(
      need(rval$plot_var, "No plotting variables")
    )
    facet_var_default <- "name"
    group_var_default <- "subset"
    color_var_default <- "subset"
    plot_type_default <- "hexagonal"
    plot_type_gate_default <- "hexagonal"
    
    # if(rval$flow_set_selected == "t-SNE"){
    #   facet_var_default <- NULL
    #   group_var_default <- "name"
    #   color_var_default <- "name"
    #   plot_type_default <- "dots"
    #   plot_type_gate_default <- "dots"
    #   
    #   
    # }
    # 
    # if(rval$flow_set_selected == "cluster"){
    #   facet_var_default <- NULL
    #   group_var_default <- NULL
    #   color_var_default <- "cluster"
    #   plot_type_default <- "dots"
    #   plot_type_gate_default <- "dots"
    # }
    
    updateSelectInput(session, "plot_type", selected = plot_type_default)
    
    updateSelectInput(session, "facet_var", 
                      choices = c("subset", names(rval$pdata)), 
                      selected = facet_var_default )

    updateSelectInput(session, "group_var", 
                      choices = c("subset", names(rval$pdata)), 
                      selected = group_var_default)

    updateSelectInput(session, "yridges_var", 
                      choices = c("subset", names(rval$pdata)), 
                      selected = group_var_default)
    
    updateSelectInput(session, "color_var", 
                      choices = c("subset", names(rval$pdata), rval$plot_var), 
                      selected = color_var_default)
    
  })
  
  
  
  observe({
    
    validate(
      need(rval$plot_var, "No plotting parameters")
    )
    
    print(rval$plot_var)

    xvar_default <- rval$plot_var[1]
    yvar_default <- rval$plot_var[2]
    
    updateSelectInput(session, "xvar", choices = rval$plot_var, selected = xvar_default)
    updateSelectInput(session, "yvar", choices = rval$plot_var, selected = yvar_default)
    
    
    # if(! setequal(rval$plot_var, rval$parameters$name_long)){
    #   
    #   #print("update plot vars")
    #   rval$plot_var <- rval$parameters$name_long
    #   names(rval$plot_var) <- NULL
    #   
    #   if(length(rval$plot_var)>1){
    #     
    #     
    #     xvar_default <- rval$plot_var[1]
    #     yvar_default <- rval$plot_var[2]
    #     
    #     if(input$flow_set == "dim-reduction"){
    #       xvar_default <- rval$dim_red_var[1]
    #       yvar_default <- rval$dim_red_var[2]
    #     }
    #     
    #     if(input$flow_set == "cluster"){
    #       if("tSNE1" %in% rval$parameters$name_long){
    #         xvar_default <- "tSNE1"
    #       }
    #       if("tSNE2" %in% rval$parameters$name_long){
    #         yvar_default <- "tSNE2"
    #       }
    #     }
    #     
    #     updateSelectInput(session, "xvar_show", choices = rval$plot_var, selected = xvar_default)
    #     #updateSelectInput(session, "xvar_trans", choices = rval$plot_var, selected = xvar_default)
    #     #updateSelectInput(session, "yvar_trans", choices = rval$plot_var, selected = yvar_default)
    #     updateSelectInput(session, "xvar_gate", choices = rval$plot_var, selected = xvar_default)
    #     updateSelectInput(session, "yvar_gate", choices = rval$plot_var, selected = yvar_default)
    #     updateSelectInput(session, "yvar_stat", choices = rval$plot_var, selected = xvar_default)
    #     updateSelectInput(session, "xvar", choices = rval$plot_var, selected = xvar_default)
    #     updateSelectInput(session, "yvar", choices = rval$plot_var, selected = yvar_default)
    #     
    #     # updateSelectInput(session, "color_var_gate", choices = c("none", rval$plot_var), selected = "none")
    #     # updateSelectInput(session, "color_var", choices = c("none", rval$plot_var), selected = "none")
    #     # updateSelectInput(session, "color_var_trans", choices = c("none", rval$plot_var), selected = "none")
    #     # updateSelectInput(session, "color_var_comp", choices = c("none", rval$plot_var), selected = "none")
    #     
    #     
    #   }
    # }
  })
  
  observe({
    updateSelectInput(session, "gate", choices = union("root", names(rval$gates_flowCore)), selected = "root")
  })
  
  update_data_plot_focus <- eventReactive(input$update_plot, {
    data_plot_focus()
  })
  
  data_plot_focus <- reactive({
    validate(
      need(rval$gating_set, "Empty gating set") %then%
        need(input$files_selection_table_rows_selected, "Please select samples") %then%
        need(input$gate, "Please select subsets")
    )
    
    print("get data plot_focus")
    df <- get_data_gs(gs = rval$gating_set, 
                      sample = rval$pdata$name[input$files_selection_table_rows_selected],
                      subset = input$gate, 
                      spill = rval$spill)
    return(df)
    
  })
  
  output$files_selection_table <- DT::renderDataTable({
    if(!is.null(rval$pdata)){
      data.frame("name" = rval$pdata$name, row.names = NULL)
    }
  })
  
  output$plot_focus <- renderPlot({
    plot_focus()
  })
  
  plot_focus <- eventReactive(input$update_plot, {
    
    # validate(
    #   need(rval$gating_set, "Empty gating set") %then%
    #     need(input$files_selection_table_rows_selected, "Please select samples") %then%
    #     need(input$gate, "Please select subsets")
    # )
    
    
    idx_x <- match(input$xvar, rval$parameters$name_long)
    idx_y <- match(input$yvar, rval$parameters$name_long)
    xvar <- rval$parameters$name[idx_x]
    yvar <- rval$parameters$name[idx_y]
    
    color_var <- input$color_var
    if(!is.null(input$color_var)){
      
      for(i in 1:length(input$color_var)){
        if(input$color_var[i] %in% rval$parameters$name_long){
          color_var[i] <- rval$parameters$name[match(input$color_var[i], rval$parameters$name_long)]
        }else{
          color_var[i] <- input$color_var[i]
        }
      }
    }
    
    
    
    #color_var <- rval$parameters$name[match(input$color_var_gate, rval$parameters$name_long)]
    #color_var <- input$color_var
    
    axis_labels <- rval$parameters$name_long
    names(axis_labels) <- rval$parameters$name
    
    transformation <- NULL
    if(rval$apply_trans){
      transformation <- rval$transformation
    }
    
    plist <- list()
    
    split_var <- switch(input$split_variable, 
                        "x variable" = "xvar",
                        "y variable" = "yvar",
                        "color variable" = "color_var"
    )
    
    for(i in 1:length(input[[split_var]])){
      
      color_var_int <- color_var[1]
      xvar_int <- xvar[1]
      yvar_int <- yvar[1]
      
      if(split_var == "color_var"){
        color_var_int <- color_var[i]
      }else if(split_var == "xvar"){
        xvar_int <- xvar[i]
      }else if(split_var == "yvar"){
        yvar_int <- yvar[i]
      }
      
      
      p <- plot_gs(df = update_data_plot_focus(),
                   gs = rval$gating_set, 
                   sample = rval$pdata$name[input$files_selection_table_rows_selected],
                   subset = input$gate, 
                   spill = rval$spill,
                   xvar = xvar_int, 
                   yvar = yvar_int, 
                   color_var = color_var_int, 
                   #gate = NULL, 
                   type = input$plot_type, 
                   bins = input$bin_number,
                   alpha = input$alpha,
                   size = input$size,
                   norm_density = input$norm,
                   smooth = input$smooth,
                   ridges = input$ridges,
                   transformation =  transformation,
                   facet_vars = input$facet_var,
                   group_var = input$group_var,
                   yridges_var = input$yridges_var,
                   show.legend = input$legend,
                   axis_labels = axis_labels,
                   legend.position = input$legend_pos)
      
      if(!is.null(p)){
        p <- p + xlab(input$xvar) 
        if(input$plot_type != "histogram"){
          p <- p + ylab(input$yvar)
        }
      }
      
      plist[[i]] <- p
      
    }
    
    
    n <- length(plist)
    
    nrow <- min(n, input$nrow_split)
    ncol <- ceiling(n/nrow)
    g <- marrangeGrob(plist, nrow = nrow, ncol = ncol, top = "")
    
    # if(input$split_direction == "horizontal"){
    #   g <- marrangeGrob(plist, nrow = n, ncol = 1)
    # }else{
    #   g <- marrangeGrob(plist, nrow = 1, ncol = n)
    # }
    
    g
    
  })
  
  plot_height <- eventReactive(input$update_plot,{
    split_var <- switch(input$split_variable, 
                        "x variable" = "xvar",
                        "y variable" = "yvar",
                        "color variable" = "color_var"
    )
    
    min(input$nrow_split, length(input[[split_var]])) * input$row_size + 50
    
  })
  
  output$ui_plot <- renderUI({
    
    ns <- session$ns
    
    if(input$update_plot){
      # split_var <- switch(input$split_variable, 
      #                     "x variable" = "xvar",
      #                     "y variable" = "yvar",
      #                     "color variable" = "color_var"
      # )
      plotOutput(ns("plot_focus"), height = plot_height())
    }
    
    
  })
  
  output$download_plot <- downloadHandler(
    filename = "plot.pdf",
    content = function(file) {
      pdf(file, width = input$width_plot, height = input$height_plot)
      print(plot_focus())
      dev.off()
    }
  )
  
  
  return(rval)
  
  
}