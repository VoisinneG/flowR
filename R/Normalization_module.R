
#'@import shiny
#'@export
NormalizationUI <- function(id){
  
  ns <- NS(id)
  
  fluidRow(
    
    column(4,
           box(width = 12, title = "Parameter",
               selectInput(inputId = ns("select_beads_gates_default")
                           , label = "Select beads channel to apply default gates"
                           , choices = NULL,
                          multiple = T),
               actionButton(inputId = ns("apply_default_gates"), label = "Apply default gates"),
               
               hr(),
               # radioButtons(inputId = ns("norm_default"),
               #              label = "Choose gating normalization option",
               #              choices = c("Default" = 1,
               #                          "Personalized" = 2),
               #              selected = 1,
               #              inline = F),
               # h4(br("Apply modification")),
               
               
               selectInput(inputId = ns("gates_subset_select"), label = "Choice the gates of references", choices = NULL),
               hr(),
               selectInput(inputId = ns("beads_select_input"), label = "Choices the channels beads", multiple = T, choices = NULL),
               
               hr(),
               actionButton(inputId = ns("normalize_button"), label = "Apply normalization")
               
               
               
           ),
           box(width = 12, title = "Create GatingSet",
               textInput(inputId = ns("gating_set_norm_text"), label = "Entry the names of normalize GatingSet", value = NULL),
               actionButton(inputId = ns("create_gs"), label = "Create GatingSet")
           )
    ),
    
    column(8,
           box(width = 12, title = "Plot",
               tabsetPanel(
                 tabPanel("Plot visualization",
                          simpleDisplayUI(ns("simple_display"))
                 ),
                 
                 tabPanel("Plot options",
                          plotCytoUI(ns("plot_preview"))
                 )
               )
               
           ),
           
           box(width = 12,title = "Normalization graphical representation",
               tabsetPanel(
                 tabPanel("Before/After normalization",
                          plotOutput(ns("norm_plot"))
                 ),
                 tabPanel("beads distance",
                          plotOutput(ns("norm_plot_dist")),
                          column(4, selectInput(inputId = ns("select_sample_input"), label = "Select sample", choices = NULL, selected = NULL)),
                          column(4, selectInput(inputId = ns("x_dist_input"), label = "X axis", choices = NULL, selected = NULL)),
                          column(4, selectInput(inputId = ns("y_dist_input"), label = "Y axis", choices = NULL, selected = NULL))
                 )
               )
               
               
           )
    )
    
    
  )
}

#'@import premessa
#'@export
Normalization <- function(input, output, session, rval){
  # Setup temporary gs reactiveValues
  
  gs_tmp <- reactiveValues(gs_norm = rval)
  
  
  #### call module ######################################################################################
  
  plot_params <- reactiveValues(plot_type = "dots", subset = c("root", NULL), color_var = "subset", xvar = NULL, yvar = NULL, split_var = "yvar")
  

  
  
  res <- callModule(plotCyto, "plot_preview", 
                    gs_tmp$gs_norm,
                    plot_params = plot_params,
                    show_gates = T,
                    simple_plot = F)
  

  callModule(simpleDisplay, "simple_display", res$plot)
  
  ### Setup gates of references subset ######################################################################
  
  gate_reference <- reactive({
    if(!is.null(rval$gating_set)){
      res <- get_gates_from_gs(rval$gating_set)

      return(names(res))
    } else {
      return(NULL)
    }

  })
  
  ### Update selectInput #########################################################################

  
  # update multiple select input from channel names
  chan_names <- reactive({
    validate(
      need(!is.null(rval$gating_set), "")
    )
    names <- flowWorkspace::colnames(rval$gating_set)
    return(names)
  })
  
  ### update select_beads_gates_default and apply default gates on these chan ########################################################
  
  # update selectinput beads gates 
  observe({
    updateSelectInput(session = session, 
                      inputId = "select_beads_gates_default",
                      choices = chan_names(),
                      selected = find_chan_desc_beads()
                      )
  })
  
  # search in the current parameters the desc corresponding to "beads"
  
  find_chan_desc_beads <- reactive({
    validate(need(!is.null(rval$gating_set), "gs is null"))
    
    # find the similary beads from different sample
    all_beads_description <- lapply(rval$gating_set@data@frames, function(x){

      pattern_result <- flowR::description(x)[which(grepl("[Bb]ead", flowR::description(x)))]
      # print(grepl("[Bb]ead", flowR::description(x)))
      
      names <- colnames(rval$gating_set)[which(colnames(rval$gating_set) %in% pattern_result)]
      intersect(colnames(rval$gating_set),names)
    })

    #remove duplicated beads in vector
    unlist_beads_sample <- unlist(all_beads_description)
    removed_duplicated_beads <- Map(`[`, all_beads_description, relist(!duplicated(unlist_beads_sample), skeleton = all_beads_description))
    res <- unlist(removed_duplicated_beads, use.names = F)

    return(res)
  })
  
  # Update the selectinput of subset correponding to gate and create default gate 
  observeEvent(input$apply_default_gates ,{
    
    # idx_gh <- which(flowWorkspace::gs_get_pop_paths(rval$gating_set) == paste0("/", input$select_beads_gates_default))
    search_same_subset <- which(flowWorkspace::gs_get_pop_paths(rval$gating_set) %in% paste0("/",input$select_beads_gates_default))
    # print(flowWorkspace::gs_get_pop_paths(rval$gating_set)[search_same_subset])
    # print(search_same_subset)
    # flowWorkspace::gs_get_pop_paths(rval$gating_set)
    
    # if subset is existing (remove the subset & add new subset) or keep subset 
    if(length(search_same_subset) > 0){
      ns <- session$ns
      showModal(
        modalDialog(title = "Gates is existing",
                    "Would you like to remove the subset currently existing",
                  footer = tagList(actionButton(ns("remove_subset"), "Remove subset"), 
                                   modalButton("Keep subset")
                                   )
                             )
      )
    } else {
      
      arbritrary_value <- list(x = c(10, 200))
      sapply(input$select_beads_gates_default, function(x){
        
        names(arbritrary_value) <- x # rename x to the corresponding channel selected
        rg <- flowCore::rectangleGate(filterId = x, arbritrary_value)
        flowWorkspace::gs_pop_add(rval$gating_set, rg)
      })
      
      recompute(rval$gating_set)
      rval$update_gs <- rval$update_gs + 1
      
      updateSelectInput(session = session,
                        inputId = "gates_subset_select",
                        label = "Choice the gates of references",
                        choices = gate_reference()
      )
    }
  })
  
  ### Remove subset if existing when users apply default gates ############################################################
  
  observeEvent(input$remove_subset, {
    
    id_to_remove <- which(flowWorkspace::gs_get_pop_paths(rval$gating_set) %in% paste0("/",input$select_beads_gates_default))
    target_gate <- flowWorkspace::gs_get_pop_paths(rval$gating_set)[id_to_remove]
    
    #remove actual target gate
    sapply(target_gate, function(x){
      flowWorkspace::gs_pop_remove(gs = rval$gating_set, node = x) # remove all subset present from the target (if we have the same subset here)
    })
    #update rval$gating_set after removing all target
    recompute(rval$gating_set)
    rval$update_gs <- rval$update_gs + 1
    
    # add new subset (gate)
    target <- gsub(pattern = "^/", replacement = "", x = target_gate) # remove the "/" of the beads name
    
    # add the default gate after removing the subset 
    arbritrary_value <- list(x = c(10, 200))
    sapply(input$select_beads_gates_default, function(x){
      names(arbritrary_value) <- x # rename x to the corresponding channel selected
      rg <- flowCore::rectangleGate(filterId = x, arbritrary_value)
      flowWorkspace::gs_pop_add(rval$gating_set, rg)
    })
    
    # reupdate gs
    recompute(rval$gating_set)
    rval$update_gs <- rval$update_gs + 1
    
    new_gate_references <- names(get_gates_from_gs(rval$gating_set))
    updateSelectInput(session = session,
                      inputId = "gates_subset_select",
                      label = "Choice the gates of references",
                      choices = new_gate_references
    )
    removeModal()
  })
  
  ## update subset params_plot
  
  observe({
    validate(need(!is.null(input$gates_subset_select), ""))
    plot_params$subset <- c("root", input$gates_subset_select)
  })
  
  # update yvar ("beads selected") & xvar = "FSC" if fsc is not present in chan take the first parameter
  
  observe({
    validate(
      need(!is.null(input$gates_subset_select), ""),
      need(!is.null(rval$gating_set), "")
           )
    
    # print(input$select_beads_gates_default)
    plot_params$yvar <- input$select_beads_gates_default
    
    if(length(grep("FSC", colnames(rval$gating_set))) >= 1){
      position_grep <- which(grepl("FSC", colnames(rval$gating_set)))
      
      plot_params$xvar <- colnames(rval$gating_set)[position_grep][1]
    } else {
      plot_params$xvar <- colnames(rval$gating_set)[1]
    }

  })
  
  #### Update beads input from default or personalized choices #######################################################################
  observe({

      updateSelectInput(session, "beads_select_input",
                        label = "Choices the channels beads",
                        choices = chan_names(),
                        selected = input$select_beads_gates_default)
    # selected = m_norm_tmp$beads.cols.names.used
  })
  
  # update x & y selectinput for the plot dist
  observe({
    updateSelectInput(session, 
                         "x_dist_input",
                         label = "X axis",
                         choices = chan_names(),
                         selected = chan_names()[1])
    
    updateSelectInput(session = session, 
                      inputId = "y_dist_input",
                      label = "Y axis",
                      choices = chan_names(),
                      selected = chan_names()[2])
    
    updateSelectInput(session = session,
                      inputId = "select_sample_input",
                      label = "Select sample",
                      choices = sampleNames(rval$gating_set),
                      selected = sampleNames(rval$gating_set)[1])
    
  })
  
  ## Normalization from subset & specific "beads selected" #######################################################
  
  #setup reactiveVal
  m_normed <- reactiveValues(norm = NULL)
  m_norm_tmp <- reactiveValues(m = NULL, norm.res = NULL, beads.cols.names.used = NULL)
 
  
  # update beads for normalization
  beads.cols.names <- reactive({
    input$beads_select_input
  })
  
  normalize_reactive <- reactive({
      
      m_norm_tmp$beads.cols.names.used <- input$beads_select_input
      sample_names <- sampleNames(rval$gating_set)
      
      beads.gate <- list()
      
      # if users takes specific gates (from specific subset)
      df_list <- list()
      
      # compute baseline
      df <- get_data_gs(gs = rval$gating_set, sample = sampleNames(rval$gating_set), subset = input$gates_subset_select)
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
        beads.data <- get_data_gs(rval$gating_set, sample = sample, subset = input$gates_subset_select)
        beads.data <- as.matrix(beads.data[-which(names(beads.data) %in% c("name", "subset"))])
        m_norm_tmp$norm.res <- premessa:::correct_data_channels(m = m_norm_tmp$m,
                                                                beads.data = beads.data,
                                                                baseline = baseline.data,
                                                                beads.col.names = m_norm_tmp$beads.cols.names.used,
                                                                time.col.name = "Time")
        m_normed$norm <- m_norm_tmp$norm.res$m.normed
        # print(m_normed$norm)
        beads.events <- gh_pop_get_indices(rval$gating_set[[sample]], input$gates_subset_select)
        
        
        m_normed$norm <- cbind(m_normed$norm,
                               beadDist = premessa:::get_mahalanobis_distance_from_beads(m_normed$norm,
                                                                                         beads.events,
                                                                                         m_norm_tmp$beads.cols.names.used))
        
        colnames(m_normed$norm) <- paste(colnames(m_normed$norm), "norm")
        colnames(m_normed$norm)[colnames(m_normed$norm)=="beadDist norm"] <- "beadDist"
        
        df_list[[sample]] <- cbind(df, as.data.frame(m_normed$norm))
        
      }
      
      df <- do.call(rbind, df_list)
      
      fs_norm <- build_flowset_from_df(df = df)
      
      # #build GatingSet
      gs_norm <- build_gatingset_from_df(df = df, gs_origin = rval$gating_set)
      
      return(gs_norm)
      # rval$gating_set <- gs_norm
      
    
  })
  
  ### make normalisation ##################################################################################################
  
  # Apply temporary normalization preview 
  
  observeEvent(input$normalize_button, {
    gs_tmp$gs_norm <- normalize_reactive()
    print("Normalization finished")
  })
  
  # create the new gating set from the normalization
  observeEvent(input$create_gs, {
    validate(need(length(input$beads_select_input) > 0, "Need to choice beads before to apply normalization"))
    
    gs_norm <- normalize_reactive()
    # print(names(gs_norm))
    # print("test")
    params <- colnames(gs_norm)[colnames(gs_norm) %in% names(rval$trans_parameters)]
    rval$gating_set_list[[input$gating_set_norm_text]] <- list(gating_set = gs_norm,
                                                                parent = rval$gating_set_selected,
                                                                trans_parameters = rval$trans_parameters[params]
    )
    rval$gating_set_selected <- input$gating_set_norm_text
    
    rval$gating_set <- gs_norm
    rval$update_gs <- rval$update_gs + 1
    
  })
  

  #### get df (compute stat) for making plot before/after norm ########################

  get_stat_for_plot <- reactive({

    # get name of parameter normalized
    if(length(grepl("norm", colnames(gs_tmp$gs_norm))) > 0 ){
      
      beads <- colnames(gs_tmp$gs_norm)[match(m_norm_tmp$beads.cols.names.used, colnames(gs_tmp$gs_norm))]
      beads_norm <- colnames(gs_tmp$gs_norm)[match(paste0(m_norm_tmp$beads.cols.names.used, " norm"), colnames(gs_tmp$gs_norm))]

      df <- get_data_gs(gs = gs_tmp$gs_norm, sample = flowR::sampleNames(gs_tmp$gs_norm), subset = input$gates_subset_select)
      
    } else{
      beads <- colnames(rval$gating_set)[match(input$beads_select_input, colnames(rval$gating_set))]
      beads_norm <- NULL
      df <- get_data_gs(gs = rval$gating_set, sample = flowR::sampleNames(rval$gating_set), subset = input$gates_subset_select)
    }
    
    
    # get one vector before/after norm
    beads_before_after <- c(beads, beads_norm)
    
    df_stat <- flowR:::compute_stats(df = df,
                                     y_trans = asinh_trans(scale = 5),
                                     apply_inverse = TRUE,
                                     yvar = beads_before_after,
                                     stat_function = "median"
    )
    
    return(list
           (df_stat,
             beads,
             beads_norm)
           )
  })
  
  ### Make normalization plot #######################################################################################
  output$norm_plot <- renderPlot({
    # validate(need(!is.null(m_normed$norm), "No current normalisation apply"))
    validate(
      need(!is.null(rval$gating_set), "No current gatingSet"),
      need(input$gates_subset_select != "", "Need gates (subset) to visualize the plot")
             )
    
    # print(input$gates_subset_select)
    data <- melt(get_stat_for_plot()[[1]],value.name = "Signal median", variable.name = "Parameters")

    colnames(data)[colnames(data) == "name"] <- "Sample"
    
    data$norm_aspect <- "Before"
    data$norm_aspect[which(grepl("norm" , data$Parameters))] <- "After"
    
    plotting <- ggplot(data = data, mapping = aes(x = Sample, y = `Signal median`)) 
    plotting <- plotting + geom_point(aes(colour = Parameters)) 
    plotting <- plotting + geom_line(data = data, aes(x = Sample, y = `Signal median`, color = Parameters, group = Parameters))
    plotting <- plotting + facet_grid(norm_aspect ~ .)
    plotting
    # premessa:::plot_beads_over_time(beads.data = m_norm_tmp$m, beads.normed = m_norm_tmp$norm.res$beads.normed, beads.cols = m_norm_tmp$beads.cols.names.used)
  })
  
  output$norm_plot_dist <- renderPlot({
    # validate(need(rval$gating_set@[[]], "No current normalisation apply"))
    premessa:::plot_distance_from_beads(exprs(rval$gating_set@data[[input$select_sample_input]]), x.var = input$x_dist_input, y.var = input$y_dist_input)
  })
  
  
  return(rval)
  
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
      # utils::data("GvHD", package = "flowCore")
      # rval$gating_set <- GatingSet(GvHD)
      # utils::data("Bcells", package = "flowAI")
      # rval$gating_set <- flowWorkspace::GatingSet(Bcells)

      fs <- read.ncdfFlowSet(files = c("/mnt/NAS7/Workspace/hammamiy/data_premasse/20120222_cells_found.fcs",
                                       "/mnt/NAS7/Workspace/hammamiy/data_premasse/20120229_cells_found.fcs"))

      gs <- GatingSet(fs)

      # rg <- flowCore::rectangleGate(filterId = "beads1", list("Bead1(La139)Di" = c(10, 200), "(Ir193)Di" = c(-50, 50)))
      # rg2 <- flowCore::rectangleGate(filterId = "beads2", list("Bead2(Pr141)Di" = c(10, 200), "(Ir193)Di" = c(-50, 50)))
      # rg3 <- flowCore::rectangleGate(filterId = "beads3", list("CD11c(Tb159)Di" = c(10, 200), "(Ir193)Di" = c(-50, 50)))
      # rg4 <- flowCore::rectangleGate(filterId = "beads4", list("Bead3(Tm169)Di" = c(10, 200), "(Ir193)Di" = c(-50, 50)))
      # rg5 <- flowCore::rectangleGate(filterId = "beads5", list("Bead4(Lu175)Di" = c(10, 200), "(Ir193)Di" = c(-50, 50)))

      # flowWorkspace::gs_pop_add(gs, rg)
      # flowWorkspace::gs_pop_add(gs, rg2, parent = "/beads1")
      # flowWorkspace::gs_pop_add(gs, rg3, parent = "/beads1/beads2")
      # flowWorkspace::gs_pop_add(gs, rg4, parent = "/beads1/beads2/beads3")
      # flowWorkspace::gs_pop_add(gs, rg5, parent = "/beads1/beads2/beads3/beads4")
      # recompute(gs)



      rval$gating_set <- gs

      # rg <- flowCore::rectangleGate(filterId = "beads1", list("APC-Cy7-A" = c(10, 200), "Pacific Blue-A" = c(-50, 50)))
      # flowWorkspace::gs_pop_add(rval$gating_set, rg)
      # recompute(rval$gating_set)


    })
    res <- callModule(Normalization, "module", rval = rval)
  }

  shinyApp(ui, server)

}
