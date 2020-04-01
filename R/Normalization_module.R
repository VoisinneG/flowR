
#'@import shiny
#'@export
NormalizationUI <- function(id){
 
   ns <- NS(id)
  
  fluidRow(

    column(4,
           box(width = 12, title = "Parameter",

               radioButtons(inputId = ns("norm_default"),
                            label = "Choose gating normalization option",
                            choices = c("Default" = 1,
                                        "Personalized" = 2),
                            selected = 1,
                            inline = F),
               # h4(br("Apply modification")),
               actionButton(inputId = ns("Add_gate"),
                            label = "create gates by default"),
               hr(),
               selectInput(inputId = ns("beads_select_input"), label = "Choices the channels beads", multiple = T, choices = NULL),
               plotGatingSetInput(ns("plot_preview"))

           )
    ),

    column(8,
           box(width = 12, title = "Plot",
               simpleDisplayUI(ns("simple_display"))
           )
    ),
    box(width = 12,title = "Normalization graphical representation",
        plotOutput(ns("norm_plot"))
        )

  )
}

#'@import premessa
#'@export
Normalization <- function(input, output, session, rval){

  #### call module ######################################################################################

  callModule(plotGatingSet, "param_plot", rval, show_gates = T)

  plot_params <- reactiveValues()

  gate_plot <- reactive({

    plot_params$show_gates <- TRUE

    callModule(plotGatingSet, "plot_preview",
                                    rval,
                                    plot_params = plot_params,
                                    show_gates = T)




  })

callModule(simpleDisplay, "simple_display", gate_plot()$plot)

  # mydata <- reactiveValues(x_values = c(), y_values = c())

  # observe({
  #   dist <- xy_range_str(plot_display$params$plot_brush)
  #   print(dist)
  # })

  # get the parameters from the mouse brush selection
   # xy_range_str <- function(e) {
   #  list <- list()
   #  if(is.null(e)) return("NULL")
   #  list$xmin <- round(e$xmin, 1)
   #  list$xmax <- round(e$xmax, 1)
   #  list$ymin <- round(e$ymin, 1)
   #  list$ymax <- round(e$ymax, 1)
   #  return(list)
   # }

   ## test subset ####
   m_normed <- reactiveValues(norm = NULL)
   m_norm_tmp <- reactiveValues(m = NULL, norm.res = NULL, beads.cols.names.used = NULL)

   observeEvent(input$Add_gate, {
     validate(need(length(input$beads_select_input) > 0, "Need to choice beads before to apply normalization"))
     normalize_reactive()
   })

   beads.cols.names <- reactive({
     input$beads_select_input
   })

   observe({
     rg <- flowCore::rectangleGate(filterId = "beads1", list("Bead1(La139)Di" = c(10, 200), "(Ir193)Di" = c(-50, 50)))
     rg2 <- rectangleGate(filterId = "beads2", list("Bead2(Pr141)Di" = c(10, 200), "(Ir193)Di" = c(-50, 50)))
     rg3 <- rectangleGate(filterId = "beads3", list("CD11c(Tb159)Di" = c(10, 200), "(Ir193)Di" = c(-50, 50)))
     rg4 <- rectangleGate(filterId = "beads4", list("Bead3(Tm169)Di" = c(10, 200), "(Ir193)Di" = c(-50, 50)))
     rg5 <- rectangleGate(filterId = "beads5", list("Bead4(Lu175)Di" = c(10, 200), "(Ir193)Di" = c(-50, 50)))

     flowWorkspace::gs_pop_add(rval$gating_set, rg)
     flowWorkspace::gs_pop_add(rval$gating_set, rg2, parent = "/beads1")
     flowWorkspace::gs_pop_add(rval$gating_set, rg3, parent = "/beads1/beads2")
     flowWorkspace::gs_pop_add(rval$gating_set, rg4, parent = "/beads1/beads2/beads3")
     flowWorkspace::gs_pop_add(rval$gating_set, rg5, parent = "/beads1/beads2/beads3/beads4")

     recompute(rval$gating_set)
   })

   normalize_reactive <- reactive({
     if(input$norm_default == 1){

       # beads.cols.names <- c("Bead1(La139)Di", "Bead2(Pr141)Di", "CD11c(Tb159)Di", "Bead3(Tm169)Di", "Bead4(Lu175)Di")
       m_norm_tmp$beads.cols.names.used <- input$beads_select_input
       sample_names <- sampleNames(rval$gating_set)
       # print(sample_names)
       beads.gate <- list()

       l <- list(x = asinh(c(10, 200)/5), y= asinh(c(-50, 50)/5))

       for(i in 1:length(sample_names)){

         for(j in 1:length(m_norm_tmp$beads.cols.names.used)){
           name <- paste(m_norm_tmp$beads.cols.names.used[[j]])

           smp <- paste(sample_names[i])
           # print(smp)
           tmp <- list(x = c(l$x), y = c(l$y))

           beads.gate[[smp]][[name]] <- tmp

         }

       }

       # print(beads.gate)
       # beads.gate[["20120222_cells_found.fcs"]] <- list("Bead1(La139)Di" = list(x=asinh(c(10, 200)/5), y=asinh(c(-50, 50)/5)),
       #                                                  "Bead2(Pr141)Di" = list(x=asinh(c(10, 200)/5), y=asinh(c(-50, 50)/5)),
       #                                                  "CD11c(Tb159)Di" = list(x=asinh(c(10, 200)/5), y=asinh(c(-50, 50)/5)),
       #                                                  "Bead3(Tm169)Di" = list(x=asinh(c(10, 200)/5), y=asinh(c(-50, 50)/5)),
       #                                                  "Bead4(Lu175)Di" = list(x=asinh(c(10, 200)/5), y=asinh(c(-50, 50)/5)))
       # beads.gate[["20120229_cells_found.fcs"]] <- list("Bead1(La139)Di" = list(x=asinh(c(10, 200)/5), y=asinh(c(-50, 50)/5)),
       #                                                  "Bead2(Pr141)Di" = list(x=asinh(c(10, 200)/5), y=asinh(c(-50, 50)/5)),
       #                                                  "CD11c(Tb159)Di" = list(x=asinh(c(10, 200)/5), y=asinh(c(-50, 50)/5)),
       #                                                  "Bead3(Tm169)Di" = list(x=asinh(c(10, 200)/5), y=asinh(c(-50, 50)/5)),
       #                                                  "Bead4(Lu175)Di" = list(x=asinh(c(10, 200)/5), y=asinh(c(-50, 50)/5)))

       df_list <- list()

       # compute baseline
       df <- get_data_gs(gs = rval$gating_set, sample = sampleNames(rval$gating_set), subset = "beads5")
       # beads.cols.names <- c("Bead1(La139)Di", "Bead2(Pr141)Di", "CD11c(Tb159)Di", "Bead3(Tm169)Di", "Bead4(Lu175)Di")
       df$id <- 1
       df_stat <- flowR:::compute_stats(df = df,
                                        y_trans = asinh_trans(scale = 5),
                                        apply_inverse = TRUE,
                                        yvar = m_norm_tmp$beads.cols.names.used,
                                        stat_function = "median",
                                        id.vars = c("id"))
       baseline.data <- unlist(df_stat[, -1])

       for(sample in sampleNames(rval$gating_set)){
         df <- get_data_gs(rval$gating_set, sample = sample, subset = "root")
         m_norm_tmp$m <- as.matrix(df[-which(names(df) %in% c("name", "subset"))])
         beads.data <- get_data_gs(rval$gating_set, sample = sample, subset = "beads5")
         beads.data <- as.matrix(beads.data[-which(names(beads.data) %in% c("name", "subset"))])
         m_norm_tmp$norm.res <- premessa:::correct_data_channels(m = m_norm_tmp$m,
                                                                 beads.data = beads.data,
                                                                 baseline = baseline.data,
                                                                 beads.col.names = m_norm_tmp$beads.cols.names.used,
                                                                 time.col.name = "Time")
         m_normed$norm <- m_norm_tmp$norm.res$m.normed
         print(m_normed$norm)
         beads.events <- gh_pop_get_indices(rval$gating_set[[sample]], "beads5")


         m_normed$norm <- cbind(m_normed$norm,
                                beadDist = premessa:::get_mahalanobis_distance_from_beads(m_normed$norm,
                                                                                          beads.events,
                                                                                          m_norm_tmp$beads.cols.names.used))

         colnames(m_normed$norm) <- paste(colnames(m_normed$norm), "norm")
         colnames(m_normed$norm)[colnames(m_normed$norm)=="beadDist norm"] <- "beadDist"

         df_list[[sample]] <- cbind(df, as.data.frame(m_normed$norm))
         # print(df_list[["20120229_cells_found.fcs"]])
         # print(sample)
       }

       df <- do.call(rbind, df_list)
       # print(df)
       # print(df)
       # print(attributes(rval))
       fs_norm <- build_flowset_from_df(df = df)
       # print(fs_norm)
       # #build GatingSet
       # print(names(rval$gating_set))
       # fs_norm <- build_flowset_from_df(df = df, origin = fs)
       # gs_norm <- build_gatingset_from_df(df = df, gs_origin = rval$gating_set)
       gs_norm <- GatingSet(fs_norm)

       rval$gating_set <- gs_norm
     }
   })

   output$norm_plot <- renderPlot({
     validate(need(!is.null(m_normed$norm), "No current normalisation apply"))
     print(m_normed$norm)
     # print(sampleNames(rval$gating_set))
     # premessa:::plot_distance_from_beads(m_normed$norm, x.var = "Bead1(La139)Di", y.var = "(Ir191)Di")
     premessa:::plot_beads_over_time(beads.data = m_norm_tmp$m, beads.normed = m_norm_tmp$norm.res$beads.normed, beads.cols = m_norm_tmp$beads.cols.names.used)
   })

   # update multiple select input from channel names

   chan_names <- reactive({
     names <- flowWorkspace::colnames(rval$gating_set)
     return(names)
   })

   # observe({
   #   print(input$beads_select_input)
   #   })
   observe({
     updateSelectInput(session, "beads_select_input", label = "Choices the channels beads", choices = chan_names(), selected = m_norm_tmp$beads.cols.names.used)
   })

}


#### Tests ####
#
# library(shiny)
# library(shinydashboard)
# library(flowWorkspace)
# library(flowCore)
# 
if (interactive()){

  ui <- dashboardPage(
    dashboardHeader(title = "flowAI"),
    sidebar = dashboardSidebar(disable = TRUE),
    body = dashboardBody(
      NormalizationUI("module")
    )
  )

  server <- function(input, output, session) {
    rval <- reactiveValues()
    observe({
      #utils::data("GvHD", package = "flowCore")
      #rval$gating_set <- GatingSet(GvHD)
      # utils::data("Bcells", package = "flowAI")
      # rval$gating_set <- flowWorkspace::GatingSet(Bcells)

      fs <- read.ncdfFlowSet(files = c("/mnt/NAS7/Workspace/hammamiy/data_premasse/20120222_cells_found.fcs",
                                       "/mnt/NAS7/Workspace/hammamiy/data_premasse/20120229_cells_found.fcs"))

      gs <- GatingSet(fs)
      rval$gating_set <- gs
    })
    res <- callModule(Normalization, "module", rval = rval)
  }

  shinyApp(ui, server)

}
