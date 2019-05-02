library(scales)
library(viridis)
library(ggcyto)
library(data.table)
library(ggsignif)
#library(sf) # need libudunits2-dev

get_data_gs <- function(gs, 
                        idx, 
                        subset
                        ){
  df <- NULL
  for(k in 1:length(subset)){
    
    for(i in 1:length(idx)){
      ff <- getData(gs[[idx[i]]], subset[k])
      #ff <- fs[[i]]
      df_int <- as.data.frame(exprs(ff))
      df_int[["name"]] <- pData(gs)$name[idx[i]]
      df_int[["subset"]] <- subset[k]
      df <- rbind(df, df_int)
    }
  }
  
  df[["subset"]] <- factor(df[["subset"]], levels = subset)
  df[["name"]] <- factor(df[["name"]], levels = pData(gs)$name[idx])
  
  return(df)
  
}

add_columns_from_metadata <- function(df, 
                                      metadata,
                                      color_var = NULL, 
                                      facet_vars = "name",
                                      group_var = "name",
                                      yridges_var = NULL){
  
  if(!is.null(facet_vars)){
    facet_vars <- facet_vars[facet_vars %in% names(metadata)]
  }
  
  if(!is.null(color_var)){
    color_var <- color_var[color_var %in% names(metadata)]
  }
  
  new_vars <- unique(setdiff( c(yridges_var, group_var, facet_vars, color_var), 
                              c("name","subset")))
  if(length(new_vars)>0){
    for(variable in new_vars){
      df[[variable]] <- metadata[[variable]][match(df[["name"]], metadata$name)]
    }
  }
  
  print(names(df))
  
  return(df)
}

plot_gs <- function(df = NULL,
                    gs, 
                    idx, 
                    subset,
                    xvar = NULL, 
                    yvar = NULL,
                    axis_labels = NULL,
                    color_var = NULL, 
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
                    group_var = "name",
                    yridges_var = "name",
                    norm_density = TRUE,
                    smooth = FALSE,
                    ridges = FALSE,
                    bw = 0.1,
                    show.legend = TRUE
                    ){
  
  
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
  
  
  if(is.null(df)){
    
    df <- get_data_gs(gs = gs,
                      idx = idx, 
                      subset = subset)
  }else{
    df <- df[df$name %in% pData(gs)[["name"]][idx] & 
               df$subset %in% subset, ]
  } 
  
  df <- add_columns_from_metadata(df,
                                  metadata = pData(gs),
                                  color_var = color_var, 
                                  facet_vars = facet_vars,
                                  group_var = group_var,
                                  yridges_var = yridges_var)
    
  
 
  print(names(df))
    
  ##################################################################################
  # plot density hexagonal
  if(type == "hexagonal"){
    
    
    p <- ggplot(df,
                aes_(x = as.name( xvar ), 
                     y = as.name( yvar ) ) )+
      geom_hex(bins = bins, show.legend = show.legend) +
      scale_fill_viridis()
    
    #p <- as.ggplot(p)
  }
  
  ##################################################################################
  # plot histogram
  
  if(type == "histogram"){
    
    p <- ggplot(df,
                aes_(x = as.name( xvar )))
    
    if(norm_density){
      stat_var <- "stat(ndensity)"
    }else{
      stat_var <- "stat(density)"
    }
    
    if(smooth){
      if(ridges){
        p <- p + geom_density_ridges(mapping = aes_string(fill = group_var, 
                                                          color = group_var, 
                                                          y = yridges_var, 
                                                          height = stat_var), 
                                     alpha = alpha, 
                                     bw = bw,
                                     stat = "density",
                                     show.legend = show.legend)
      }else{
        p <- p + geom_density(mapping = aes_string(fill = group_var, color = group_var, y = stat_var), 
                              alpha = alpha, 
                              bw = bw, 
                              show.legend = show.legend)
      }
    }else{
      p <- p + geom_histogram(mapping = aes_string(fill = group_var, color = group_var, y = stat_var), 
                              alpha = alpha,  
                              bins = bins, 
                              position = "identity", 
                              boundary = 0, 
                              show.legend = show.legend) 
    }
    
  }
  
  ##################################################################################
  # plot dots
  
  if(type %in% c("dots", "contour")){
    
    p <- ggplot(df,
                aes_(x = as.name( xvar ), 
                     y = as.name( yvar )))
    
    if(type == "dots"){
      if(color_var %in% gs@data@colnames){
        #idx_col <- match(color_var, names(df))
        p <- p + geom_point(mapping = aes_(colour = as.name(color_var)),
                           alpha = alpha, 
                           size = size, 
                           show.legend = show.legend)
        
        p <- p + scale_colour_viridis(trans = transformation[[color_var]], name = color_var)
        
      }else{
        p <- p + geom_point(mapping = aes_string(colour = group_var), 
                            alpha = alpha, 
                            size = size, 
                            show.legend = show.legend)
      }
    }

    
    if(type == "contour"){
      p <- p + geom_density_2d(mapping = aes_string(color = group_var), 
                              alpha = alpha, 
                              size =size, 
                              n = bins, 
                              show.legend = show.legend) 
    }
    
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
          
          
          center <- c(mean(df_trans[,idx_match[1]]), mean(df_trans[,idx_match[2]]))
          
          polygon <- rbind(polygon, polygon[1,])

          xlim <- range(c(xlim, polygon[,idx_match[1]]))
          ylim <- range(c(ylim, polygon[,idx_match[2]]))
          
          #trueCentroids = gCentroid(sids,byid=TRUE)
          
          
          df_label <- data.frame(x = transformation[[xvar]]$inverse(center[idx_match[1]]), 
                                 y = transformation[[yvar]]$inverse(center[idx_match[2]]),
                                 label = gate_int@filterId)
          
          p <- p +
            geom_path(data = polygon, color = "red") +
            geom_polygon(data=polygon,
                         fill="red",
                         alpha=0.1) +
            geom_label(data = df_label, aes(x=x, y=y, label = label),
                       fill = "white", color = "red", hjust = "middle", vjust = "center")
            # annotate("text", 
            #          x=transformation[[xvar]]$inverse(center[idx_match[1]]), 
            #          y=transformation[[yvar]]$inverse(center[idx_match[2]]),
            #          label = gate_int@filterId,
            #          color = "red", fill = "white")
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
      
      # if(!is.null(data_range)){
      #   xlim <- range(c(data_range[[xvar]], polygon[,1]))
      #   ylim <- range(c(data_range[[yvar]], polygon[,2]))
      # }
      
      xlim <- range(c(xlim, polygon[,1]))
      ylim <- range(c(ylim, polygon[,2]))
      
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
  p <- p + scale_x_continuous(name = labx, trans = transformation[[xvar]], limits = xlim) 
  p <- p + theme(plot.title = element_text(face = "bold") )
  
  if(length(subset)==1){
    p <- p + ggtitle(subset)
  }
  
  if(type != "histogram"){
    laby <- ifelse(is.null(axis_labels), yvar, axis_labels[[yvar]])
    p <- p + scale_y_continuous(name = laby, trans = transformation[[yvar]], limits = ylim) 
  }
  
  p
  
}


plot_gh <- function(df, gs, idx, ...){
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
      plist[[count]] <- plot_gs(df, gs, idx, subset = parent, gate = nodes_to_plot[same_par], ...)
      
      all_children <- unlist(sapply(nodes_to_plot[same_par], function(x){getChildren(gs[[1]], x)}))
      names(all_children) <- NULL
      
      child_nodes_int <- c(child_nodes_int, all_children)
      nodes_to_plot <- setdiff(nodes_to_plot, nodes_to_plot[same_par])
    }
    
    child_nodes <- child_nodes_int

  }
  return(plist)
}


plot_stat <- function(df = NULL,
                      gs,
                      idx, 
                      subset,
                      yvar = NULL,
                      type = "bar",
                      color_var = NULL, 
                      transformation = NULL,
                      default_trans = identity_trans(),
                      facet_vars = "name",
                      group_var = "subset",
                      show.legend = TRUE
                    
){
  
  #if(log10_trans){
  #  trans <- log10_trans()
  #}else{
    trans <- identity_trans()
  #}
  
  if(is.null(transformation)){
   transformation <- lapply(gs@data@colnames, function(x){trans})
   names(transformation) <- gs@data@colnames
  }
  
  
  if(is.null(yvar)){
    yvar <- gs@data@colnames[1]
  }
  
  #ylim <- NULL
  #if(!is.null(data_range)){
  #  ylim <- data_range[[yvar]]
  #}
  if(is.null(df)){
    df <- get_data_gs(gs = gs,
                      idx = idx,
                      subset = subset)
  }else{
    df <- df[df$name %in% pData(gs)[["name"]][idx] & 
               df$subset %in% subset, ]
  } 
  
  
  for(i in 1:length(yvar)){
    df[[yvar[i]]] <- transformation[[yvar[i]]]$transform(df[[yvar[i]]])
    #df[[yvar[i]]] <- trans$transform(df[[yvar[i]]])
  }
  
  df_melt <- melt(df, id.vars = c("name", "subset"), measure.vars = yvar)
  #df_melt$value[!is.finite(df_melt$value)] <- NA
  
  df_cast <- dcast(df_melt, name + subset ~ variable, mean, na.rm = TRUE)
  
  
  #for(i in 1:length(yvar)){
  #  df_cast[[yvar[i]]] <- transformation[[yvar[i]]]$inverse(df_cast[[yvar[i]]])
  #  #df_cast[[yvar[i]]] <- trans$inverse(df_cast[[yvar[i]]])
  #}
  
  df_scale <- df_cast
  df_melt2 <- melt(df_scale, id.vars = c("name", "subset") )
  
  df_melt2 <- add_columns_from_metadata(df_melt2,
                                  metadata = pData(gs),
                                  color_var = color_var,
                                  facet_vars = facet_vars,
                                  group_var = group_var)
  
  print(names(df_melt2))
  
  df_melt2 <- df_melt2[df_melt2$variable %in% yvar, ]

  ylim <- c( min(df_melt2$value, na.rm = TRUE)*(1-50/100), max(df_melt2$value, na.rm = TRUE)*(1+50/100))
  
  if(type == "tile"){
    
    p <- ggplot(df_melt2, aes_string( x = group_var ))
    p <- p + geom_tile(mapping = aes_string(y = "variable", fill = "value"),
                       show.legend = show.legend)
    
    
    p <- p + scale_fill_distiller( palette = "Spectral", limits = ylim) +
      scale_y_discrete(labels = NULL, name = "")
    
    # if(!is.null(facet_vars)){
    #   formula_facet <- as.formula(paste(". ~", paste(facet_vars, collapse = " + ")))
    # }else{
    #   formula_facet <- NULL
    # }
  }
  
  if(type == "bar"){
    
    p <- ggplot(df_melt2, aes_string(x = group_var, y = "value"))
    
    p <- p + 
      geom_bar(alpha = 0.5, stat = "summary", fun.y = "mean") + 
      geom_point( mapping = aes_(colour = as.name(color_var)), 
                  inherit.aes = TRUE, 
                  position = position_jitter(width = 0.25, height = 0),
                  alpha = 0.5,
                  size = 3,
                  show.legend = show.legend) +
      geom_signif(comparisons = list(c(1,2)),
                  test="t.test",
                  test.args = list("paired"=FALSE))
    
    #p <- p + scale_y_continuous(trans = trans)
    p <- p + coord_cartesian(ylim = ylim)
    
      #geom_boxplot(mapping = aes_string( y = "value", fill = "variable")) +
      #geom_point(mapping = aes_string(y = "value"))
    
  }
  
  if(!is.null(facet_vars)){
    formula_facet <- as.formula(paste("variable ~", paste(facet_vars, collapse = " + ")))
  }else{
    formula_facet <- as.formula("variable ~ .")
  }
  
  #if(!is.null(formula_facet)){
    p <- p + facet_grid(formula_facet,
                        labeller = label_both, 
                        scales = "free")
  #}
  
  
  ##################################################################################
  # general plot parameters
  
  
  p <- p + theme( axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                  strip.text.y = element_text(angle = 0)
                  )
      

  
  p
  
}
