#BiocManager::install("flowCore")
#BiocManager::install("flowViz")
library(openCyto)
library(flowWorkspace)
library(ggcyto)
library(ggridges)
#library(ncdfFlow)

library(shiny)
library(shinyBS)
library(shinydashboard)
library(DT)
library(tools)

#library(shinyTree)
library(sp)

library(viridis)
#library(plotly)
library(igraph)
library(scales)
#library(ggplot2)
#library(ggrepel)
#library(data.table)
#library(plotly)
#library(heatmaply)
#library(RColorBrewer)
#library(viridis)

flowJo_biexp_inverse_trans <- function (..., n = 6, equal.space = FALSE){
  trans <- flowJoTrans(..., inverse = TRUE)
  inv <- flowJoTrans(...)
  flow_trans(name = "flowJo_biexp_inverse", trans.fun = trans, inverse.fun = inv, 
             n = n, equal.space = equal.space)
}


transform_values <- function(x, scale, ...){
  xtrans <- switch(scale,
                   "linear" = identity_trans()$inverse(x),
                   "log10" = log10_trans()$inverse(x),
                   "asinh" = flowJo_fasinh_trans()$inverse(x),
                   "logicle" = logicle_trans()$inverse(x))
                   #"logicle" = as.numeric(logicleTransform(...)(x)))
         
}

##########################################################################################################
#User interface ----

options(shiny.maxRequestSize = 300*1024^2)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "Import_tab",
            fluidRow(
              #dataTableOutput("files_table"),
              column(width = 6,
                     box(title = "Import",
                         width = NULL, height = NULL,
                         fileInput(inputId = "files", 
                                   label = "Choose files", 
                                   multiple = TRUE)
                     ),
                     box(title = "Input files",
                         width = NULL, height = NULL,
                         dataTableOutput("files_table"),
                         br(),
                         br(),
                         numericInput("N_lines", label = "Number of cells to import", value = 3000),
                         actionButton("load", label = "Load selected files")
                     )
              ),
              column(width = 6,
                     tabBox(title = "Info",
                            width = NULL, height = NULL,
                            tabPanel(title = "summary",
                                     verbatimTextOutput("message")
                            ),
                            tabPanel(title = "parameters",
                                     div(style = 'overflow-x: scroll', DT::dataTableOutput("parameters_table"))
                                     
                            ),
                            tabPanel(title = "metadata",
                                     div(style = 'overflow-x: scroll', DT::dataTableOutput("pData"))
                                     
                            )
                     )
              )
            )
      
    ),
    tabItem(tabName = "Gates_tab",
            fluidRow(
              column(width = 4,
                     tabBox(title = "Sample",
                         width = NULL, height = NULL,
                         tabPanel("Select",
                                 selectInput("sample_selected", label = "Sample", choices = NULL, selected = NULL),
                                 actionButton("previous_frame", "previous"),
                                 actionButton("next_frame", "next"),
                                 br(),
                                 br(),
                                 selectInput("xvar_gate", label = "x variable", choices = NULL, selected = NULL),
                                 selectInput("yvar_gate", label = "y variable", choices = NULL, selected = NULL)
                                 )
                     ),
                     tabBox(title = "Gates",
                         width = NULL, height = NULL,
                         tabPanel("Select",
                                  selectInput("gate_selected", 
                                              label = "gate", 
                                              choices = "root", 
                                              selected = "root"),
                                  actionButton("show_gate", "show gate")
                                  ),
                         tabPanel("Create",
                                  #selectInput("gate_type",
                                  #            label = "Type of gate",
                                  #            choices = c("rectangular", "polygonal"),
                                  #            selected = "rectangular"),
                                  #selectInput("parent_gate", label = "Select parent gate", choices = "root", selected = "root"),
                                  textInput("gate_name", label = "Enter gate name", value = ""),
                                  actionButton("create_gate", "create gate"),
                                  actionButton("reset_gate", "reset gate")
                                  ),
                         tabPanel("Delete",
                                  selectInput("gate_to_delete", 
                                              label = "Delete gate", 
                                              choices = NULL, 
                                              selected = NULL),
                                  actionButton("delete_gate", "Delete")
                                  )
                         
                         
                     ),
                     box(title = "Message_gate",
                         width = NULL, height = NULL,
                         verbatimTextOutput("message_gate")
                     )
              ),
              column(width = 8,
                     tabBox(title = "Plot",
                         width = NULL, height = NULL,
                         tabPanel("Plot",
                                  plotOutput("plotGate2",
                                             brush = "plot_brush",
                                             click = "plot_click",
                                             dblclick = "plot_dblclick")
                         
                         ),
                         tabPanel("Scale axis",
                                  selectInput("x_scale_gate", 
                                              label = "x scale", 
                                              choices = c("linear", "log10", "asinh", "logicle"), 
                                              selected = "linear"),
                                  selectInput("y_scale_gate", 
                                              label = "y scale", 
                                              choices = c("linear", "log10", "asinh", "logicle"), 
                                              selected = "linear"),
                                  checkboxInput("freeze_limits", label = "freeze plot limits", value = TRUE)
                                  
                         ),
                         tabPanel("Options",
                                  selectInput("plot_type_gate", label = "plot type",
                                              choices = c("hexagonal", "contour", "dots"),
                                              selected = "hexagonal"),
                                  selectInput("color_var", "color variable",
                                              choices = "none",
                                              selected = "none"),
                                  selectInput("color_trans", "color scale",
                                              choices = c("linear","log10"),
                                              selected = "linear"),
                                  numericInput("bin_number_gate", label = "number of bins", value = 150),
                                  numericInput("alpha_gate", label = "alpha", value = 0.5),
                                  numericInput("size_gate", label = "size", value = 1)
                                  
                                  )
                     ),
                     # box("Plot gate",
                     #     width = NULL, height = NULL,
                     #     plotOutput("plotGate_flowCore",
                     #                brush = "plot_brush_1",
                     #                click = "plot_click_1",
                     #                dblclick = "plot_dblclick_1")
                     # ),
                     box(title = "Gates hierarchy",
                         width = NULL, height = NULL,
                         plotOutput("tree")
                     )
              )
              
            )
    ),
    tabItem(tabName = "Plot_tab",
            fluidRow(
              column(width = 4,
                     tabBox(title = "Sample",
                            width = NULL, height = NULL,
                            tabPanel("Select",
                                     selectInput("gate", label = "gate", choices = "root", selected = "root"),
                                     selectInput("xvar", label = "x variable", choices = NULL, selected = NULL),
                                     selectInput("yvar", label = "y variable", choices = NULL, selected = NULL)
                            )
                     ),
                     box(title = "Select samples",
                         width = NULL, height = NULL,
                         div(style = 'overflow-x: scroll', DT::dataTableOutput("files_selection_table"))
                         
                     )
              ),
              column(width = 8,
                     tabBox(title = "Plot",
                            width = NULL, height = NULL,
                            tabPanel("Plot",
                                     plotOutput("plot_focus")
                                     
                            ),
                            tabPanel("Scale axis",
                                     selectInput("x_scale", 
                                                 label = "x scale", 
                                                 choices = c("linear", "log10", "asinh", "logicle"), 
                                                 selected = "linear"),
                                     selectInput("y_scale", 
                                                 label = "y scale", 
                                                 choices = c("linear", "log10", "asinh", "logicle"), 
                                                 selected = "linear")
                                     #checkboxInput("freeze_limits", label = "freeze plot limits", value = TRUE)
                                     
                            ),
                            tabPanel("Options",
                                     selectInput("plot_type", label = "plot type",
                                                 choices = c("hexagonal", "histogram"),
                                                 selected = "histogram"),
                                     checkboxInput("norm", "normalize (set max to 1)", value = TRUE),
                                     checkboxInput("smooth", "smooth", value = FALSE),
                                     checkboxInput("ridges", "ridges", value = FALSE),
                                     checkboxInput("facet", "faceting", value = TRUE),
                                     selectizeInput("facet_var", 
                                                    multiple =TRUE,
                                                    label = "facet variables", 
                                                    choices = "name", 
                                                    selected = "name"),     
                                     numericInput("bw", label = "smoothing bandwidth", value = 0.01),
                                     numericInput("bin_number", label = "number of bins", value = 150),
                                     numericInput("alpha", label = "alpha", value = 0.5),
                                     numericInput("size", label = "size", value = 2)
                                     
                            )
                     )
              )
              
              
          )
    )
    
    
  )
)



sidebar <- dashboardSidebar(
  sidebarMenu(id = "menu",
              menuItem("Import",
                       tabName = "Import_tab", 
                       startExpanded = FALSE,
                       icon = icon("check-circle")
                       
              ),
              menuItem("Gates",
                       tabName = "Gates_tab", 
                       startExpanded = FALSE,
                       icon = icon("check-circle")
                       
              ),
              menuItem("Plot",
                       tabName = "Plot_tab", 
                       startExpanded = FALSE,
                       icon = icon("check-circle")
                       
              )
              
  )
  
  
)

ui <- dashboardPage(
  dashboardHeader(title = "flowShine"),
  sidebar,
  body
)

##########################################################################################################
# Server logic ----
server <- function(session, input, output) {
  
  rval <- reactiveValues(df_files = NULL, 
                         flow_set = NULL,
                         gating_set = NULL,
                         idx_ff_gate = NULL,
                         parameters = NULL,
                         pdata = NULL,
                         gates = list(),
                         gate_focus = "root",
                         df_gate_focus = NULL,
                         gates_flowCore = list(),
                         min_val = NULL,
                         max_val = NULL,
                         transformation = list()
                         )
  
  gate <- reactiveValues(x = NULL, y = NULL)

  # observe({
  #   rval$gates[["root"]] <- list(
  #     "name" = "root",
  #     "parent" = NULL
  #   )
  # })
  
  
  # Select files
  observe({
    
    validate(
      need(input$files$datapath, "Please select a file to import")
    )
    rval$df_files <- input$files
    
  })
  
  # load data
  observeEvent(input$load, {
    validate(
      need(length(input$files_table_rows_selected)>0, "Please select a file to load")
    )
    
    
    if(length(rval$df_files$datapath)>0){
      
      if(file_ext(rval$df_files$datapath[input$files_table_rows_selected[1]]) == "xml"){
        ws <- openWorkspace(rval$df_files$datapath[input$files_table_rows_selected[1]])
        show(ws)
        rval$gating_set <- try(parseWorkspace(ws,
                                              name = "All Samples",
                                              execute = TRUE, 
                                              isNcdf = TRUE,
                                              path = "./data/"))
        
        nodes <- getNodes(rval$gating_set)
        
        for(node in setdiff(nodes, "root")){
          g <- getGate(rval$gating_set[[1]], node)
          parent <- getParent(rval$gating_set[[1]], node)
          rval$gates_flowCore[[basename(node)]] <- list(gate = g, parent = basename(parent))
        }   
          
        rval$flow_set <- getData(rval$gating_set)

      }else{
        rval$flow_set <- read.flowSet( rval$df_files$datapath[input$files_table_rows_selected], 
                                       which.lines = input$N_lines)
        
        phenoData(rval$flow_set)$name <- rval$df_files$name[input$files_table_rows_selected]
      }
      
      ff <- rval$flow_set[[1]]
      desc <- parameters(ff)$desc
      name <- parameters(ff)$name
      name_long <-  name
      name_long[!is.na(desc)] <- paste(name[!is.na(desc)], " (", desc[!is.na(desc)], ")", sep = "")
      
      cat(rownames(parameters(ff)@data))
      
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
      print(display)
      
      rval$parameters <- data.frame(name = name, 
                                    desc = desc,
                                    name_long = name_long,
                                    display = display,
                                    range = parameters(ff)@data$range,
                                    minRange = parameters(ff)@data$minRange,
                                    maxRange = parameters(ff)@data$maxRange,
                                    stringsAsFactors = FALSE)
      
      
                                    #row.names = NULL)
      
      rval$parameters$transform <- "linear"
      rval$parameters$transform[rval$parameters$display == "LOG"] <- "logicle"
     
      rval$transformation <- lapply(rval$parameters$transform, function(x){
        switch(x,
               "linear" = identity_trans(),
               "logicle" = logicle_trans())
      })
      
      names(rval$transformation) <- rval$parameters$name
      
      if(file_ext(rval$df_files$datapath[input$files_table_rows_selected[1]]) == "xml"){
        myTrans <- lapply(rval$parameters$display, function(x){
          switch(x,
                 "LOG" = flowJo_biexp_inverse_trans(),
                 identity_trans())
        })
        myTrans <- transformerList(rval$parameters$name, myTrans)
        rval$gating_set <- transform(rval$gating_set, myTrans)
        recompute(rval$gating_set)
        rval$flow_set <- getData(rval$gating_set)
      }
        
      plot_var <- rval$parameters$name_long
      names(plot_var) <- NULL
      
      if(length(plot_var)>1){
        updateSelectInput(session, "xvar_gate", choices = plot_var, selected = plot_var[1])
        updateSelectInput(session, "yvar_gate", choices = plot_var, selected = plot_var[2])
        updateSelectInput(session, "xvar", choices = plot_var, selected = plot_var[1])
        updateSelectInput(session, "yvar", choices = plot_var, selected = plot_var[2])
        updateSelectInput(session, "color_var", choices = c("none", plot_var), selected = "none")
      }
      
      
      
      # Compute max values for each parameter in flow set
      #min_val_pos <- as.data.frame(fsApply(rval$flow_set, each_col, function(x){min(x[x>0 & !is.na(x)])}))
      min_val <- as.data.frame(fsApply(rval$flow_set, each_col, min, na.rm = TRUE))
      #rval$min_val_pos <- apply(min_val_pos , 2, min)
      rval$min_val <- apply(min_val, 2, min)
      
      #max_val_pos <- as.data.frame(fsApply(rval$flow_set, each_col, function(x){max(x[x>0 & !is.na(x)])}))
      max_val <- as.data.frame(fsApply(rval$flow_set, each_col, max,  na.rm = TRUE))
      #rval$rvmax_val_pos <- apply(max_val_pos , 2, max)
      rval$max_val <- apply(max_val, 2, max)
      
      #names(rval$max_val) <- rval$parameters$name_long[match(names(rval$max_val), rval$parameters$name)]
      #names(rval$min_val) <- rval$parameters$name_long[match(names(rval$min_val), rval$parameters$name)]
      #names(rval$min_val_pos) <- rval$parameters$name_long[match(names(rval$min_val_pos), rval$parameters$name)]
      
      #########################################################################################################
      # Create gating set
      if(file_ext(rval$df_files$datapath[input$files_table_rows_selected[1]]) != "xml"){
        
        rval$gating_set <- GatingSet(rval$flow_set)
        
        # add existing gates
        ngates <- length(rval$gates_flowCore)
        if(ngates>0){
          gs <- rval$gating_set
          idx <- 1:ngates
          while(length(idx)>0){
            
            i_added <- NULL
            for(i in 1:length(idx)){
              if(rval$gates_flowCore[[idx[i]]]$parent %in% union(basename(getNodes(gs)), "root") ){
                add(gs, 
                    rval$gates_flowCore[[idx[i]]]$gate, 
                    parent = rval$gates_flowCore[[idx[i]]]$parent, 
                    name = names(rval$gates_flowCore)[idx[i]])
                i_added <- c(i_added, i)
              }
            }
            idx <- idx[-i_added]
          }
          rval$gating_set <- gs
        }
        recompute(rval$gating_set)
      }
      
    }
    
    #########################################################################################################
    # update gates
    
    updateSelectInput(session, "gate_selected", choices = c("root", names(rval$gates_flowCore)), selected = "root")
    updateSelectInput(session, "gate_to_delete", choices = names(rval$gates_flowCore))
    updateSelectInput(session, "gate", choices = c("root", names(rval$gates_flowCore)), selected = "root")
    
    
    
    #########################################################################################################
    # Metadata
    rval$pdata <- data.frame(name = pData(rval$flow_set)$name,
                             tube = fsApply(rval$flow_set, function(x){description(x)[["TUBE NAME"]]}),
                             experiment = fsApply(rval$flow_set, function(x){description(x)[["EXPERIMENT NAME"]]}),
                             src = fsApply(rval$flow_set, function(x){description(x)[["$SRC"]]}),
                             events = fsApply(rval$flow_set, function(x){description(x)[["$TOT"]]}))
    
    
    pData(rval$gating_set) <- rval$pdata 
    rval$flow_set <- getData(rval$gating_set)
    
    updateSelectInput(session, "facet_var", 
                      choices = names(rval$pdata), 
                      selected = "name")
    
    updateSelectInput(session, "sample_selected",
                      choices = phenoData(rval$flow_set)$name,
                      selected = phenoData(rval$flow_set)$name[1])
    
  })
  
  
  
  observeEvent(input$sample_selected, {
    if(!is.null(rval$flow_set)){
      rval$idx_ff_gate <- which(rval$pdata$name == input$sample_selected)
      cat(rval$idx_ff_gate)
    }
  })
  
  observeEvent(input$next_frame, {
    if(!is.null(rval$flow_set)){
      idx <- which(rval$pdata$name == input$sample_selected)
      idx <- idx +1
      if(idx > length(rval$pdata$name)){
        idx <- 1
      }
      updateSelectInput(session, "sample_selected", selected = rval$pdata$name[idx])
      
    }
  })
  
  observeEvent(input$previous_frame, {
    if(!is.null(rval$flow_set)){
      idx <- which(rval$pdata$name == input$sample_selected)
      idx <- idx - 1
      if(idx < 1){
        idx <- length(rval$pdata$name)
      }
      updateSelectInput(session, "sample_selected", selected = rval$pdata$name[idx])
    }
  })
  
  # observe({
  #   if(!is.null(rval$flow_set)){
  #     
  #     plot_var <- parameters(rval$flow_set[[1]])$desc
  #     plot_var <- plot_var[!is.na(plot_var)]
  #     
  #     names(plot_var) <- NULL
  # 
  #     if(length(plot_var)>1){
  #       updateSelectInput(session, "xvar", choices = plot_var, selected = plot_var[1])
  #       updateSelectInput(session, "yvar", choices = plot_var, selected = plot_var[2])
  #     }
  #   }
  # })
  
  ##########################################################################################################
  # Observe functions for gating
  
    observeEvent(input$plot_click, {
      gate$x <- c(gate$x, transform_values(input$plot_click$x, input$x_scale_gate))
      gate$y <- c(gate$y, transform_values(input$plot_click$y, input$y_scale_gate))
      cat("click")
    })
    
    observeEvent(input$plot_brush, {
      brush <- input$plot_brush
      cat("brush")
      if (!is.null(brush)) {
        gate$x <- sapply(c(brush$xmin, brush$xmax, brush$xmax, brush$xmin), 
                         transform_values, 
                         scale = input$x_scale_gate)
        gate$y <- sapply(c(brush$ymin, brush$ymin, brush$ymax, brush$ymax),
                         transform_values, 
                         scale = input$y_scale_gate)
        session$resetBrush("plot_brush")
        
      }
    })
    
    observeEvent(input$plot_dblclick, {
      gate$x <- NULL
      gate$y <- NULL
      cat("dblclick")
      session$resetBrush("plot_brush")
    })
    
    observeEvent(input$reset_gate, {
      gate$x <- NULL
      gate$y <- NULL
      session$resetBrush("plot_brush")
    })
    
    # observeEvent(input$xvar_gate, {
    #    gate$x <- NULL
    #    gate$y <- NULL
    #    session$resetBrush("plot_brush")
    # })
    # 
    # observeEvent(input$yvar_gate, {
    #   gate$x <- NULL
    #   gate$y <- NULL
    #   session$resetBrush("plot_brush")
    # })
  
  
    observeEvent(input$create_gate, {
      
      if(input$gate_name %in% basename(getNodes(rval$gating_set))){
        showModal(modalDialog(
          title = "Error",
          "Gate name already exists! Please choose another name.",
          easyClose = TRUE,
          footer = NULL
        ))
      }else{
        if(!is.null(gate$x)){
          polygon <- data.frame(x =gate$x, y = gate$y)
          hpts <- chull(polygon)
          hpts <- c(hpts, hpts[1])
          polygon <- polygon[hpts, ]
          
          # rval$gates[[input$gate_name]] <- list(
          #   "name" = input$gate_name,
          #   "parent" = rval$gate_focus,
          #   "xvar" = input$xvar_gate, 
          #   "yvar" = input$yvar_gate,
          #   "type" = "polygonal_gate",
          #   "path" = polygon
          # )
          
          boundaries = list("x" = polygon$x, "y" = polygon$y )
          names(boundaries) <- c(rval$parameters$name[match(input$xvar_gate, rval$parameters$name_long)],
                                 rval$parameters$name[match(input$yvar_gate, rval$parameters$name_long)])
          
          poly_gate <- polygonGate(.gate = boundaries, filterId=input$gate_name)
          rval$gates_flowCore[[input$gate_name]] <- list(gate = poly_gate, parent = rval$gate_focus)
          add(rval$gating_set, poly_gate, parent = rval$gate_focus, name = input$gate_name)
          
          recompute(rval$gating_set)

          updateSelectInput(session, "gate_selected", choices = basename(getNodes(rval$gating_set)), selected = input$gate_name)
          updateSelectInput(session, "gate_to_delete", choices = setdiff(basename(getNodes(rval$gating_set)), "root"))
          updateSelectInput(session, "gate", choices = basename(getNodes(rval$gating_set)), selected = "root")
          
          
          gate$x <- NULL
          gate$y <- NULL
          
        }
      }
        
    })

    
    observeEvent(input$gate_selected, {
      rval$gate_focus <- input$gate_selected
    })
    
    observeEvent(input$delete_gate, {
      if(input$gate_to_delete != "root"){
        
        idx_gh <- which( basename(getNodes(rval$gating_set)) == input$gate_to_delete )
        target_gate <- basename(getNodes(rval$gating_set)[idx_gh])
        child_gates <- basename(getChildren(rval$gating_set[[1]], target_gate))
        idx_delete <- which( names(rval$gates_flowCore) %in% c(target_gate, child_gates) )
        rval$gates_flowCore <- rval$gates_flowCore[-idx_delete]
        
        Rm(target_gate, rval$gating_set)
        recompute(rval$gating_set)
        
        updateSelectInput(session, "gate_selected", choices = basename(getNodes(rval$gating_set)), selected = "root")
        updateSelectInput(session, "gate_to_delete", choices = setdiff(basename(getNodes(rval$gating_set)), "root"))
        updateSelectInput(session, "gate", choices = basename(getNodes(rval$gating_set)), selected = "root")
        
      }
      
    })
    
    observeEvent(input$show_gate, {
      if(input$gate_selected != "root"){
        g <- rval$gates_flowCore[[input$gate_selected]]$gate@boundaries
        g <- as.data.frame(g)
        names(g) <- rval$parameters$name_long[match(names(g), rval$parameters$name)]
        
        updateSelectInput(session, "xvar_gate", selected = names(g)[1])
        updateSelectInput(session, "yvar_gate", selected = names(g)[2])
        
        gate$x <- g[[1]]
        gate$y <- g[[2]]
      }
      updateSelectInput(session, "gate_selected",  
                        selected = rval$gates_flowCore[[input$gate_selected]]$parent)

    })
    
    
    
    observeEvent(input$y_scale_gate, {
      if(!is.null(rval$parameters)){
        idx <- match(input$yvar_gate, rval$parameters$name_long)
        rval$transformation[[idx]] <- switch(input$y_scale_gate,
                                             "linear" = identity_trans(),
                                             "logicle" = logicle_trans(),
                                             "log10" = log10_trans(),
                                             "asinh" = flowJo_fasinh_trans())
      }
      
    })
    
    observeEvent(input$x_scale_gate, {
      if(!is.null(rval$parameters)){
        idx <- match(input$xvar_gate, rval$parameters$name_long)
        rval$transformation[[idx]] <- switch(input$x_scale_gate,
                                             "linear" = identity_trans(),
                                             "logicle" = logicle_trans(),
                                             "log10" = log10_trans(),
                                             "asinh" = flowJo_fasinh_trans())
      }
    })
    
    observeEvent(input$y_scale, {
      if(!is.null(rval$parameters)){
        idx <- match(input$yvar, rval$parameters$name_long)
        rval$transformation[[idx]] <- switch(input$y_scale,
                                             "linear" = identity_trans(),
                                             "logicle" = logicle_trans(),
                                             "log10" = log10_trans(),
                                             "asinh" = flowJo_fasinh_trans())
      }
    })
    
    observeEvent(input$x_scale_gate, {
      if(!is.null(rval$parameters)){
        idx <- match(input$xvar, rval$parameters$name_long)
        rval$transformation[[idx]] <- switch(input$x_scale,
                                             "linear" = identity_trans(),
                                             "logicle" = logicle_trans(),
                                             "log10" = log10_trans(),
                                             "asinh" = flowJo_fasinh_trans())
      }
    })

    observeEvent(input$yvar_gate, {
      updateSelectInput(session, "y_scale_gate", 
                        selected = rval$parameters$transform[match(input$yvar_gate, rval$parameters$name_long)])
    })
    
    observeEvent(input$xvar_gate, {
      updateSelectInput(session, "x_scale_gate", 
                        selected = rval$parameters$transform[match(input$xvar_gate, rval$parameters$name_long)])
    })
    
    observeEvent(input$yvar, {
      updateSelectInput(session, "y_scale", 
                        selected = rval$parameters$transform[match(input$yvar, rval$parameters$name_long)])
    })
    
    observeEvent(input$xvar, {
      updateSelectInput(session, "x_scale", 
                        selected = rval$parameters$transform[match(input$xvar, rval$parameters$name_long)])
    })
    
    observeEvent(input$x_scale_gate, {
      rval$parameters$transform[match(input$xvar_gate, rval$parameters$name_long)] <- input$x_scale_gate
    })
    
    observeEvent(input$y_scale_gate, {
      rval$parameters$transform[match(input$yvar_gate, rval$parameters$name_long)] <- input$y_scale_gate
    })
    
    observeEvent(input$x_scale, {
      rval$parameters$transform[match(input$xvar, rval$parameters$name_long)] <- input$x_scale
    })
    
    observeEvent(input$y_scale, {
      rval$parameters$transform[match(input$yvar, rval$parameters$name_long)] <- input$y_scale
    })
    
    observe({
      if(input$plot_type_gate == "dots"){
        ff <- getData(rval$gating_set[[rval$idx_ff_gate]], y = rval$gate_focus)
        df <- as.data.frame(exprs(ff))
        df$identifier <- input$sample_selected
        rval$df_gate_focus <- df
      }
      
    })
    
    
  ##########################################################################################################
  # Output
    
  output$files_table <- renderDataTable({
    validate(
      need(rval$df_files, "Please select a file to import")
    )
    rval$df_files[ ,c("name", "size")]
  })
  
  output$message <- renderText({
    paste("You have loaded", length(rval$flow_set), "items")
  })
  
  output$parameters_table <- renderDataTable({
     df <- rval$parameters
     df$minRange <- format(df$minRange, digits = 2)
     df$maxRange <- format(df$maxRange, digits = 2)
     df
  })
  
  output$pData <- DT::renderDataTable({
    if(!is.null(rval$flow_set)){
      DT::datatable(rval$pdata, rownames = FALSE)
    }
  })
  
  output$files_selection_table <- DT::renderDataTable({
    if(!is.null(rval$flow_set)){
      data.frame("name" = rval$pdata$name, row.names = NULL)
    }
  })
  
  output$message_gate <- renderPrint({
    #print(rval$gates[[input$gate_selected]])
    
      
        print(rval$gates_flowCore)
    
    
  })
   
  output$tree <- renderPlot({
    
    validate(
      need(length(rval$gates_flowCore)>0, "Empty gates set")
    )
    
    df <- lapply(names(rval$gates_flowCore), function(x){
      if(x != "root"){
        data.frame("source" = rval$gates_flowCore[[x]]$parent, "target" = x)}
      else{NULL}}
    )
    
    df <- do.call(rbind, df)
    net <- igraph::graph.data.frame(df, directed=TRUE)
    net$layout <- layout_as_tree(net)
    
    plot.igraph(net, 
                layout = layout_as_tree, 
                vertex.size = 50, 
                vertex.color = rgb(0,0,0,0.2), 
                vertex.frame.color = NA, 
                vertex.label.color = "black", 
                vertex.label.family = "Helvetica",
                edge.arrow.size = 0.3, 
                vertex.label.cex = 0.7)

  })
  
  output$plot_focus <- renderPlot({
    
    validate(
      need(rval$gating_set, "Empty gating set")
    )
    
    validate(
      need(input$files_selection_table_rows_selected, "Please select samples")
    )
    
    idx_x <- match(input$xvar, rval$parameters$name_long)
    idx_y <- match(input$yvar, rval$parameters$name_long)
    
    if(input$plot_type== "hexagonal"){
      p <- ggcyto(rval$gating_set[input$files_selection_table_rows_selected], 
                  aes_(x = as.name(rval$parameters$name[idx_x]), 
                       y = as.name(rval$parameters$name[idx_y]) ),
                  subset = input$gate)+
        geom_hex(bins = input$bin_number) +
        scale_fill_viridis() +
        scale_x_continuous(trans = rval$transformation[[idx_x]]) +
        scale_y_continuous(trans = rval$transformation[[idx_y]]) 
        #facet_wrap()
      
    }
    
    if(input$plot_type == "histogram"){
      p <- ggcyto(rval$gating_set[input$files_selection_table_rows_selected], 
                  aes_(x = as.name(rval$parameters$name[idx_x])),
                  subset = input$gate) +
        scale_x_continuous(trans = rval$transformation[[idx_x]])
        #facet_wrap()

      if(input$norm){
        stat_var <- "stat(ndensity)"
      }else{
        stat_var <- "stat(density)"
      }
      
      if(input$smooth){

        if(input$ridges){
          p <- p + geom_density_ridges(mapping = aes_string(fill = "name", color = "name", y = "name", height = stat_var), 
                                alpha = input$alpha, 
                                bw = input$bw,
                                stat = "density",
                                show.legend = FALSE
                                )
        }else{
          p <- p + geom_density(mapping = aes_string(fill = "name", color = "name", y = stat_var), 
                                alpha = input$alpha, 
                                bw = input$bw)
        }

      }else{
          p <- p + geom_histogram(mapping = aes_string(fill = "name", color = "name", y = stat_var), 
                                  alpha = input$alpha, 
                                  bins = input$bin_number, 
                                  position = "identity") 
      }
      
      if(input$facet & !is.null(input$facet_var)){
        p <- p + facet_wrap(input$facet_var, labeller = label_both)
      }else{
        p <- p + facet_null()
      }
    }

    as.ggplot(p)

  })


  # output$plotGate_flowCore <- renderPlot({
  #   cat(getNodes(rval$gating_set))
  # 
  #   if(input$gate_selected == "root"){
  #     autoplot(getData(rval$gating_set)[[rval$idx_ff_gate]], 
  #              x = rval$parameters$name[match(input$xvar_gate, rval$parameters$name_long)], 
  #              y = rval$parameters$name[match(input$yvar_gate, rval$parameters$name_long)], 
  #              bins = input$bin_number_gate)
  #   }else{
  #     plotGate(rval$gating_set, input$gate_selected)
  #   }
  #   
  # })
  
  # output$plotGate <- renderPlot({
  #   
  #   validate(
  #     need(rval$idx_ff_gate, "Please select a sample")
  #   )
  #   
  #   df <- rval$df_gate_focus
  #   
  #   validate(
  #     need(dim(df)[1]>0, "No cells in selection")
  #   )
  #   
  #   df$x <- df[[input$xvar_gate]]
  #   df$y <- df[[input$yvar_gate]]
  #   
  #   if(input$freeze_limits){
  #     xlim <- c(rval$min_val[[input$xvar_gate]], rval$max_val[[input$xvar_gate]])
  #     if(input$x_scale_gate != "linear"){
  #       xlim <- c(rval$min_val_pos[[input$xvar_gate]], rval$max_val[[input$xvar_gate]])
  #     }
  #     ylim <- c(rval$min_val[[input$yvar_gate]], rval$max_val[[input$yvar_gate]])
  #     if(input$y_scale_gate != "linear"){
  #       ylim <- c(rval$min_val_pos[[input$yvar_gate]], rval$max_val[[input$yvar_gate]])
  #     }
  #   }else{
  #     xlim <- range(df$x, na.rm = TRUE)
  #     if(input$x_scale_gate != "linear"){
  #       xlim <- range(df$x[df$x>0], na.rm = TRUE)
  #     }
  #     ylim <- range(df$y, na.rm = TRUE)
  #     if(input$y_scale_gate != "linear"){
  #       ylim <- range(df$y[df$y>0], na.rm = TRUE)
  #     }
  #   }  
  #   
  #   binwidth = c(xlim[2]-xlim[1], ylim[2]-ylim[1]) 
  #   binwidth[1] <- switch(input$x_scale_gate,
  #                         "linear" = binwidth[1],
  #                         "log10" = log10(binwidth[1]),
  #                         "asinh" = flowJo_fasinh_trans()$transform(binwidth[1]),
  #                         "logicle" = logicle_trans()$transform(binwidth[1])
  #                         )
  #   binwidth[2] <- switch(input$y_scale_gate,
  #                         "linear" = binwidth[2],
  #                         "log10" = log10(binwidth[2]),
  #                         "asinh" = flowJo_fasinh_trans()$transform(binwidth[2]),
  #                         "logicle" = logicle_trans()$transform(binwidth[2])
  #   )
  #   
  #   binwidth <- binwidth / input$bin_number_gate
  #   cat(binwidth)
  #   cat("\n")
  #   
  #   if(!is.null(df)){
  #     
  #       p <- ggplot(df,
  #                   aes(x = x, y = y))
  # 
  #       
  #       p <- p + 
  #         xlab(input$xvar_gate) +
  #         ylab(input$yvar_gate) +
  #         ggtitle(paste(input$sample_selected, " / ", input$gate_selected, sep = ""))
  #       
  #       if(input$plot_type_gate == "hexagonal"){
  #         p <- p + geom_hex(binwidth = binwidth) +
  #           scale_fill_viridis()
  #       }
  #       
  #       if(input$plot_type_gate == "contour"){
  #         p <- p + 
  #           geom_point(color  = "black", 
  #                             alpha = input$alpha_gate, 
  #                             size = input$size_gate) + 
  #           stat_density_2d(aes(fill = stat(level)), geom = "polygon") +
  #           scale_fill_viridis_c()
  #           
  #       }
  #       
  #       if(input$plot_type_gate == "dots"){
  #         if(input$color_var != "none"){
  #           trans = switch(input$color_trans,
  #                          "linear" = "identity",
  #                          "log10" = "log10")
  #           df$colour <- df[[input$color_var]]
  #           p <- p + geom_point(data = df, mapping = aes(colour = colour), 
  #                               inherit.aes = TRUE, 
  #                               alpha = input$alpha_gate, 
  #                               size = input$size_gate) +
  #             scale_colour_viridis(trans = trans, name = input$color_var)
  #         }else{
  #           p <- p + geom_point(alpha = input$alpha_gate, size = input$size_gate) +
  #             scale_color_viridis()
  #         }
  #         
  #       }
  #         
  #       
  #       p <- p + switch(input$x_scale_gate,
  #                       "linear" = scale_x_continuous(),
  #                       "log10" = scale_x_log10(),
  #                       "asinh" = scale_x_continuous(trans = flowJo_fasinh_trans(), 
  #                                                    breaks = log10_trans()$breaks(xlim),
  #                                                    minor_breaks = log10_trans()$minor_breaks(
  #                                                      b=log10_trans()$breaks(xlim), 
  #                                                      limits = xlim,
  #                                                      n=2)),
  #                       "logicle" = scale_x_continuous(trans = logicle_trans(), 
  #                                                      breaks = log10_trans()$breaks(xlim),
  #                                                      minor_breaks = log10_trans()$minor_breaks(
  #                                                        b=log10_trans()$breaks(xlim), 
  #                                                        limits = xlim,
  #                                                        n=2))
  #                                                      )
  #       
  #       p <- p + switch(input$y_scale_gate,
  #                       "linear" = scale_y_continuous(),
  #                       "log10" = scale_y_log10(),
  #                       "asinh" = scale_y_continuous(trans = flowJo_fasinh_trans(), 
  #                                                    breaks = log10_trans()$breaks(ylim),
  #                                                    minor_breaks = log10_trans()$minor_breaks(
  #                                                      b=log10_trans()$breaks(ylim), 
  #                                                      limits = ylim,
  #                                                      n=2)),
  #                       "logicle" = scale_y_continuous(trans = logicle_trans(), 
  #                                                      breaks = log10_trans()$breaks(ylim),
  #                                                      minor_breaks = log10_trans()$minor_breaks(
  #                                                        b=log10_trans()$breaks(ylim), 
  #                                                        limits = ylim,
  #                                                        n=2))
  #                                                      )
  # 
  #     
  #     if(!is.null(gate$x)){
  #       
  #       #polygon <- data.frame(x = c(gates$x, gates$x[1]), y = c(gates$y, gates$y[1]))
  #       polygon <- data.frame(x = gate$x, y = gate$y)
  #       hpts <- chull(polygon)
  #       hpts <- c(hpts, hpts[1])
  #       polygon <- polygon[hpts, ]
  #       
  #       p <- p +
  #         geom_path(polygon, mapping = aes(x=x, y=y), color = "red") +
  #         geom_polygon(data=polygon,
  #                      fill="red",
  #                      alpha=0.1)
  #     }
  #     
  #       
  #     
  #       p <- p + coord_cartesian(xlim = xlim, ylim = ylim, expand = TRUE)
  #     
  #       
  #     p
  #     
  #   }
  # })
  # 
  
  output$plotGate2 <- renderPlot({
    
    validate(
      need(rval$gating_set, "Empty gating set")
    )
    
    validate(
      need(rval$idx_ff_gate, "Please select a sample")
    )
    
    idx_x <-match(input$xvar_gate, rval$parameters$name_long)
    idx_y <- match(input$yvar_gate, rval$parameters$name_long)
    
    xlim <- NULL
    ylim <- NULL
    if(input$freeze_limits){
      xlim <- c(rval$min_val[[idx_x]], rval$max_val[[idx_x]])
      ylim <- c(rval$min_val[[idx_y]], rval$max_val[[idx_y]])
    }
    
    ##################################################################################
    # plot density hexagonal
    
    if(input$plot_type_gate == "hexagonal"){
      p <- ggcyto(rval$gating_set[[rval$idx_ff_gate]], subset = rval$gate_focus,
                  aes_(x = as.name( rval$parameters$name[idx_x]), 
                       y = as.name( rval$parameters$name[idx_y]) ) )+
        geom_hex(bins = input$bin_number_gate) +
        scale_fill_viridis()
      
      p <- as.ggplot(p)
        
    }
    
    ##################################################################################
    # plot dots
    
    if(input$plot_type_gate == "dots"){
      
      
      df <- rval$df_gate_focus
    
      
      p <- ggplot(df,
      #p <- ggplot(rval$gating_set[[rval$idx_ff_gate]], subset = rval$gate_focus,
                  aes_(x = as.name( rval$parameters$name[idx_x]), 
                       y = as.name( rval$parameters$name[idx_y])))
      
      #p <- as.ggplot(p)
      
      #p <- ggplot(df, aes_(x = as.name( rval$parameters$name[idx_x]), 
      #                     y = as.name( rval$parameters$name[idx_y]) ))
      
      if(input$color_var != "none"){
        
        idx_col <- match(input$color_var, rval$parameters$name_long)
        #color_var <- rval$parameters$name[ idx_col ]
        #df$colour <- df[[color_var]]
        
         p <- p + geom_point(mapping = aes_(colour = as.name(rval$parameters$name[ idx_col ])), 
                             alpha = input$alpha_gate, size = input$size_gate) +
           scale_colour_viridis(trans = rval$transformation[[idx_col]], name = input$color_var)
 
      }else{
        p <- p + geom_point(alpha = input$alpha_gate, size = input$size_gate)
      }
      
      p <- p + theme(plot.title = element_text(face = "bold") ) + facet_wrap(facets = "identifier")
    }
    
    ##################################################################################
    # plot gate
    
    if(!is.null(gate$x)){
      
      #polygon <- data.frame(x = c(gates$x, gates$x[1]), y = c(gates$y, gates$y[1]))
      polygon <- data.frame(x = gate$x, y = gate$y)
      hpts <- chull(polygon)
      hpts <- c(hpts, hpts[1])
      polygon <- polygon[hpts, ]
      names(polygon) <- c(rval$parameters$name[idx_x], rval$parameters$name[idx_y])
      
      xlim <- range(c(xlim, polygon[[1]]))
      ylim <- range(c(ylim, polygon[[2]]))
      
      p <- p +
        geom_path(data = polygon, color = "red") +
        geom_polygon(data=polygon,
                     fill="red",
                     alpha=0.1)
      
    }
    
    ##################################################################################
    # general plot parameters

    p <- p + 
      xlab(input$xvar_gate) +
      ylab(input$yvar_gate) +
      ggtitle(paste(input$sample_selected, " / ", input$gate_selected, sep = "")) +
      scale_x_continuous(trans = rval$transformation[[idx_x]], limits = xlim) +
      scale_y_continuous(trans = rval$transformation[[idx_y]], limits = ylim)
    
    p 

  })
      
}
  
shinyApp(ui, server)