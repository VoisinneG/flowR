library(scales)
library(viridis)
library(ggcyto)

plot_gs <- function(gs, 
                    idx, 
                    subset,
                    xvar = NULL, 
                    yvar = NULL,
                    axis_labels = NULL,
                    color_var = NA, 
                    data_range = NULL, 
                    gate=NULL, 
                    polygon_gate = NULL,
                    type = "hexagonal", 
                    bins = 30,
                    alpha = 0.5,
                    size = 1,
                    transformation = NULL,
                    default_trans = identity_trans(),
                    facet_vars = "name",
                    norm_density = TRUE,
                    smooth = FALSE,
                    ridges = FALSE,
                    bw = 0.01){
  
  
  if(is.null(transformation)){
    transformation <- lapply(gs@data@colnames, function(x){default_trans})
    names(transformation) <- gs@data@colnames
  }
  
  if(!is.null(gate)){
    if(class(gate) == "character"){
      
      gate_int <- lapply(gate, function(x){
        if(x!="root"){
          getGate(gs[[idx[1]]], x)
        }else{
          NULL
        }})
      names(gate_int) <- gate
      gate <- gate_int
      
    }else if(grep("Gate", class(gate)) >0){
      gate <- list(gate)
    }
  }
  
  
  
  if(is.null(xvar)){
    
    if(subset != "root"){
      g <- getGate(gs[[idx[1]]], y= subset)
      if(.hasSlot(g, "boundaries")){
        xvar <- colnames(g@boundaries)[1]
        if(dim(g@boundaries)[2]>1){
          yvar <- colnames(g@boundaries)[2]
        }
      }
    }else{
      xvar <- gs@data@colnames[1]
      yvar <- gs@data@colnames[2]
    }
    
    if(!is.null(gate)){
      for(i in 1:length(gate)){
        if(.hasSlot(gate[[i]], "boundaries")){
          xvar <- colnames(gate[[i]]@boundaries)[1]
          yvar <- try(colnames(gate[[i]]@boundaries)[2], silent = TRUE)
          break
        }
      }
    }
    
  }

  xlim <- NULL
  ylim <- NULL
  if(!is.null(data_range)){
    xlim <- data_range[[xvar]]
    ylim <- data_range[[yvar]]
  }
  
  ##################################################################################
  # plot density hexagonal
  
  if(type == "hexagonal"){
    p <- ggcyto(gs[idx], subset = subset,
                aes_(x = as.name( xvar ), 
                     y = as.name( yvar ) ) )+
      geom_hex(bins = bins) +
      scale_fill_viridis()
    
    p <- as.ggplot(p)
  }
  
  ##################################################################################
  # plot histogram
  
  if(type == "histogram"){
    p <- ggcyto(gs[idx], subset = subset,
                aes_(x = as.name( xvar )))
    
    p <- as.ggplot(p)
    
    if(norm_density){
      stat_var <- "stat(ndensity)"
    }else{
      stat_var <- "stat(density)"
    }
    
    if(smooth){
      if(ridges){
        p <- p + geom_density_ridges(mapping = aes_string(fill = "name", color = "name", y = "name", height = stat_var), 
                                     alpha = alpha, bw = bw,
                                     stat = "density",show.legend = FALSE)
      }else{
        p <- p + geom_density(mapping = aes_string(fill = "name", color = "name", y = stat_var), 
                              alpha = alpha, bw = bw)
      }
    }else{
      p <- p + geom_histogram(mapping = aes_string(fill = "name", color = "name", y = stat_var), 
                              alpha = alpha,  bins = bins, position = "identity", boundary = 0) 
    }

  }
  
  ##################################################################################
  # plot dots
  
  if(type == "dots"){
    
    
    fs <- getData(gs, subset = subset)
    df <- NULL
    for(i in 1:length(idx)){
      ff <- fs[[idx[i]]]
      df_int <- as.data.frame(exprs(ff))
      df_int[["name"]] <- pData(gs)$name[idx[i]]
      df <- rbind(df, df_int)
    }
    for(facet in setdiff(facet_vars[facet_vars %in% names(pData(gs))], "name")){
      df_int[[facet]] <- pData(gs)[[facet_var]][match(df_int[["name"]], pData(gs)$name)]
    }
    
    
    p <- ggplot(df,
                aes_(x = as.name( xvar ), 
                     y = as.name( yvar )))
    
    if(color_var %in% gs@data@colnames){
      idx_col <- match(color_var, names(df))
      p <- p + geom_point(mapping = aes_(colour = as.name(color_var)),
                         alpha = alpha, size = size)
      if(!is.null(transformation)){
        p <- p + scale_colour_viridis(trans = transformation[[color_var]], name = color_var)
      }else{
        p <- p + scale_colour_viridis()
      }
        
    }else{
      p <- p + geom_point(alpha = alpha, size = size)
    }
    
    p <- p + theme(plot.title = element_text(face = "bold") ) + facet_wrap(facets = "identifier")
  }
  
  ##################################################################################
  # plot gate
  
  if(type != "histogram" & !is.null(gate)){
    
    print(gate)
    
    for(j in 1:length(gate)){
      
      gate_int <- gate[[j]]
      
      if(.hasSlot(gate_int, "boundaries") ){
        
        polygon <- as.data.frame(gate_int@boundaries)
        idx_match <- match(c(xvar, yvar), names(polygon))
        
        if(sum(is.na(idx_match))==0){
          
          df_trans <- polygon
          
          df_trans[,idx_match[1]] <- transformation[[xvar]]$transform(df_trans[,idx_match[1]])
          df_trans[,idx_match[2]] <- transformation[[yvar]]$transform(df_trans[,idx_match[2]])
          center <- apply(df_trans , MARGIN = 2, FUN = mean)
          
          polygon <- rbind(polygon, polygon[1,])
          
          
          
          
          p <- p +
            geom_path(data = polygon, color = "red") +
            geom_polygon(data=polygon,
                         fill="red",
                         alpha=0.1) +
            annotate("text", 
                     x=transformation[[xvar]]$inverse(center[idx_match[1]]), 
                     y=transformation[[yvar]]$inverse(center[idx_match[2]]),
                     label = gate_int@filterId,
                     color = "red")
        }else{
          warning(paste("gate is not defined with parameters ", xvar, "and", yvar, sep = "" ))
        }
      }
    }
    
  }
  
  ##################################################################################
  # plot polygon gate
  if(type != "histogram" & setequal(names(polygon_gate), c("x", "y"))){
    if(!is.null(polygon_gate$x)){
      polygon <- data.frame(x = polygon_gate$x, y = polygon_gate$y)
      polygon <- rbind(polygon, polygon[1,])
      names(polygon) <- c(xvar, yvar)
      
      if(!is.null(data_range)){
        xlim <- range(c(data_range[[xvar]], polygon[,1]))
        ylim <- range(c(data_range[[yvar]], polygon[,2]))
      }
      
      p <- p +
        geom_path(data = polygon, color = "red") +
        geom_polygon(data=polygon,
                     fill="red",
                     alpha=0.1) 
    }
    
    
  }
  
  
  ##################################################################################
  # general plot parameters
 
  if(!is.null(facet_vars)){
    p <- p + facet_wrap(facets = sapply(facet_vars, as.name), labeller = label_both)
  }else{
    p <- p + facet_null()
  }
  
  labx <- ifelse(is.null(axis_labels), xvar, axis_labels[[xvar]])
  laby <- ifelse(is.null(axis_labels), yvar, axis_labels[[yvar]])
  
  p <- p + 
    ggtitle(subset) +
    scale_x_continuous(name = labx, trans = transformation[[xvar]], limits = xlim) 
  
  if(type != "histogram"){
    p <- p + scale_y_continuous(name = laby, trans = transformation[[yvar]], limits = ylim) 
  }
  
  p
  
}


plot_gh <- function(gs, idx, ...){
  if(length(idx) != 1){
    stop("length of idx must be equal to 1")
  }
  child_nodes <- getChildren(gs[[idx]], "root")
  plist <- list()
  count <- 0
  
  #plot gates descending the gh until there are no more children gates
  while(length(child_nodes)>0){
    
    child_nodes_int <- NULL
    nodes_to_plot <- child_nodes
    
    #plot together gates that share the same set of parameters
    while(length(nodes_to_plot) > 0){
      
      par_nodes <- lapply(nodes_to_plot, function(x){
        try(colnames(getGate(gs[[idx]], x)@boundaries), silent = TRUE)})
      
      same_par <- sapply(par_nodes, function(x){setequal(x, par_nodes[[1]])})
      
      parent <- getParent(gs[[idx]], nodes_to_plot[1])
      count <- count + 1
      plist[[count]] <- plot_gs(gs, idx, subset = parent, gate = nodes_to_plot[same_par], ...)
      
      all_children <- unlist(sapply(nodes_to_plot[same_par], function(x){getChildren(gs[[1]], x)}))
      names(all_children) <- NULL
      
      child_nodes_int <- c(child_nodes_int, all_children)
      nodes_to_plot <- setdiff(nodes_to_plot, nodes_to_plot[same_par])
    }
    
    child_nodes <- child_nodes_int

  }
  return(plist)
}