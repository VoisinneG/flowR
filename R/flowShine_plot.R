library(scales)
library(viridis)
library(ggcyto)
library(data.table)
library(ggsignif)
#library(sf) # need libudunits2-dev
#devtools::install_github("JinmiaoChenLab/ClusterX")
library(ClusterX)
#devtools::install_github("JinmiaoChenLab/Rphenograph")
library(Rphenograph)
library(Rtsne)

flowJo_biexp_inverse_trans <- function (..., n = 6, equal.space = FALSE){
  trans <- flowJoTrans(..., inverse = TRUE)
  inv <- flowJoTrans(...)
  flow_trans(name = "flowJo_biexp_inverse", trans.fun = trans, inverse.fun = inv, 
             n = n, equal.space = equal.space)
}

asinh_transform <- function(b=5, inverse = FALSE){ 
  if(inverse){
    function(x){b*sinh(x)} 
  }else{
    function(x){asinh(x/b)} 
  }
}

asinh_trans <- function (..., n = 6, equal.space = FALSE){
  trans <- asinh_transform(...)
  inv <- asinh_transform(..., inverse = TRUE)
  flow_trans(name = "asinh", trans.fun = trans, inverse.fun = inv, 
             n = n, equal.space = equal.space)
}


ellipse_path <- function(cov, mean, n = 100){
  
  eg <- eigen(cov)
  a <- sqrt(eg$values[1])
  b <- sqrt(eg$values[2])
  alpha <- acos(eg$vectors[1,1])
  
  x <- NULL
  y <- NULL

  for(i in 1:(n+1)){
    theta <- (i-1)*pi*2/n
    xr <- a*cos(theta)
    yr <- b*sin(theta)
    x <- c(x, cos(alpha)*xr - sin(alpha)*yr + mean[1])
    y <- c(y, sin(alpha)*xr + cos(alpha)*yr + mean[2])
  }
  
  df <- data.frame(x = x, y= y)
  names(df)[1:2] <- colnames(cov)
  
  return(df)
}

get_gates_from_gs <- function(gs){
  
  nodes <- getNodes(gs)
  gates <- list()
  
  for(node in setdiff(nodes, "root")){
    g <- getGate(gs[[1]], node)
    parent <- getParent(gs[[1]], node)
    gates[[node]] <- list(gate = g, parent = parent)
  } 
  
  print(gates)
  return(gates)
  
}

add_gates_flowCore <- function(gs, gates){
  
  ngates <- length(gates)
  print(gates)
  
  if(ngates>0){
    
    idx <- 1:ngates
    
    while(length(idx)>0){
      
      i_added <- NULL
      
      for(i in 1:length(idx)){
        
        g <- gates[[idx[i]]]
        
        print(g$parent)
        print(union(getNodes(gs), "root"))
        
        if(g$parent %in% union(getNodes(gs), "root") ){
          
          print(names(gates)[idx[i]])
          
          add(gs,
              g$gate,
              parent = g$parent,
              name = g$gate@filterId)
          
          i_added <- c(i_added, i)
        }
      }
      idx <- idx[-i_added]
    }
    
    recompute(gs)
  }
  
  return(gs)
}

get_data_gs <- function(gs,
                        idx,
                        subset,
                        Ncells = NULL,
                        spill = NULL,
                        updateProgress = NULL
){
  
  if(!is.null(spill)){
    gates <- get_gates_from_gs(gs)
    fs <- getData(gs[idx])
    spill_list <- lapply(1:length(idx), function(x){return(spill)})
    names(spill_list) <- sampleNames(fs)
    fs <- compensate(fs, spill_list)
    gs <- GatingSet(fs)
    gs <- add_gates_flowCore(gs, gates)
    idx <- 1:length(gs)
  }
  
  df <- list()
  count <- 0
  
  for(i in 1:length(idx)){
    
    for(k in 1:length(subset)){
    
      ff <- getData(gs[[idx[i]]], subset[k])
      df_int <- as.data.frame(exprs(ff))
      
      if(dim(df_int)[1]>0){
        
        df_int[["name"]] <- pData(gs)$name[idx[i]]
        df_int[["subset"]] <- subset[k]
        #df <- rbind(df, df_int)
        
        if(!is.null(Ncells)){
          if(Ncells < dim(df_int)[1]){
            df_int <- df_int[sample(1:dim(df_int)[1], Ncells, replace = FALSE), ] 
          }
        }
        count <- count + 1
        df[[count]] <- df_int
      }
      
      
      if(is.function(updateProgress)){
        value <- count / (length(subset)*length(idx))*100
        updateProgress(value = value, detail = paste(format(value, digits=0), "%", sep = ""))
      }
        
        
      
      
    }
  }
  
  df_tot <- do.call(rbind, df)
  df_tot[["subset"]] <- factor(df_tot[["subset"]], levels = subset)
  df_tot[["name"]] <- factor(df_tot[["name"]], levels = pData(gs)$name[idx])
  
  return(df_tot)
  
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
                              names(df)))
  if(length(new_vars)>0){
    for(variable in new_vars){
      df[[variable]] <- metadata[[variable]][match(df[["name"]], metadata$name)]
    }
  }
  
  #print(names(df))
  
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
                    min_value = NULL,
                    gate=NULL, 
                    polygon_gate = NULL,
                    type = "hexagonal", 
                    bins = 30,
                    alpha = 0.5,
                    size = 1,
                    transformation = NULL,
                    default_trans = identity_trans(),
                    spill = NULL,
                    facet_vars = "name",
                    group_var = "name",
                    yridges_var = "name",
                    norm_density = TRUE,
                    smooth = FALSE,
                    ridges = FALSE,
                    bw = 0.1,
                    show.legend = TRUE
                    ){
  
  
  
  
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
        if(class(gate[[i]]) %in% c("rectangleGate", "polygonGate")){
          xvar <- colnames(gate[[i]]@boundaries)[1]
          yvar <- try(colnames(gate[[i]]@boundaries)[2], silent = TRUE)
          break
        }
        if(class(gate[[i]])=="ellipsoidGate"){
          xvar <- colnames(gate[[i]]@cov)[1]
          yvar <- try(colnames(gate[[i]]@cov)[2], silent = TRUE)
          break
        }
      }
    }
    
  }

  
  if(is.null(transformation)){
    trans_var <- unique(c(xvar, yvar, color_var))
    transformation <- lapply(trans_var, function(x){default_trans})
    names(transformation) <- trans_var
  }
  
  
  
  
  if(is.null(df)){
    
    df <- get_data_gs(gs = gs,
                      idx = idx,
                      subset = subset,
                      spill = spill)
  }else{
    #df <- df[df$name %in% pData(gs)[["name"]][idx],  names(df) %in% c("name", subset, xvar, yvar)]
    df <- df[df$name %in% pData(gs)[["name"]][idx] & df$subset %in% subset, ]
  }
  
  df <- add_columns_from_metadata(df,
                                  metadata = pData(gs),
                                  color_var = color_var,
                                  facet_vars = facet_vars,
                                  group_var = group_var,
                                  yridges_var = yridges_var)
 
  #print(names(df))
    
  
  xlim <- range(df[[xvar]])
  ylim <- range(df[[yvar]])
  if(!is.null(data_range)){
    xlim <- data_range[[xvar]]
    ylim <- data_range[[yvar]]
  }
  
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
                                     #bw = dist(transformation[[xvar]]$transform( range(df[[xvar]]) / bins ))[1], 
                                     #bw = dist(range(df[[xvar]])/bins)[1], 
                                     bw = 1/bins, 
                                     stat = "density",
                                     show.legend = show.legend)
      }else{
        p <- p + geom_density(mapping = aes_string(fill = group_var, color = group_var, y = stat_var), 
                              alpha = alpha, 
                              #bw = dist(transformation[[xvar]]$transform( range(df[[xvar]]) / bins ))[1], 
                              #bw = dist(range(df[[xvar]])/bins)[1], 
                              bw = 1/bins, 
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
      if(!is.null(color_var)){
        
          #idx_col <- match(color_var, names(df))
          p <- p + geom_point(mapping = aes_(colour = as.name(color_var)),
                              alpha = alpha, 
                              size = size, 
                              show.legend = show.legend)
          
          if(color_var %in% gs@data@colnames){
            p <- p + scale_colour_viridis(trans = transformation[[color_var]], name = color_var)
          }
        
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
    
    #print(gate)
    
    for(j in 1:length(gate)){
      
      gate_int <- gate[[j]]
      
      if(class(gate_int) %in% c("rectangleGate", "polygonGate") ){
        polygon <- as.data.frame(gate_int@boundaries)
      }else if(class(gate_int) %in% c("ellipsoidGate")){
        cov <- gate_int@cov
        mean <- gate_int@mean
        polygon <- ellipse_path(cov = cov, mean = mean)
      }else{
        warning("gate format not supported")
        break
      }
      
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
 
  if(!is.null(min_value)){
    xlim[1] <- min_value
    ylim[1] <- min_value
  }
  
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


plot_gh <- function(df = NULL, gs, idx, spill = NULL, ...){
  
  if(length(idx) != 1){
    stop("length of idx must be equal to 1")
  }
  
  if(is.null(df)){
    
    df <- get_data_gs(gs = gs,
                      idx = idx,
                      subset = getNodes(gs),
                      spill = spill)
    
  }
  
  child_nodes <- getChildren(gs[[idx]], "root")
  plist <- list()
  count <- 0
  
  #plot gates descending the gh until there are no more children gates
  while(length(child_nodes)>0){
    
    child_nodes_int <- NULL
    nodes_to_plot <- child_nodes
    all_parents <- sapply(nodes_to_plot, function(x){getParent(gs[[idx]], x)})
    names(all_parents) <- NULL
    #print(all_parents)
    
    for(parent in unique(all_parents)){
      
      idx_parent <- which(all_parents == parent)
      
      nodes_to_plot_parent <- nodes_to_plot[idx_parent]
      
    #plot together gates that share the same set of parameters
      
      while(length(nodes_to_plot_parent) > 0){
        
          
          par_nodes <- lapply(nodes_to_plot_parent, function(x){
            
            #print(x)
            g <- getGate(gs[[idx]], x)
            
            if(class(g) %in% c("rectangleGate", "polygonGate")){
              try(colnames(g@boundaries), silent = TRUE)
            }else if(class(g) %in% c("ellipsoidGate")){
              try(colnames(g@cov), silent = TRUE)
            }
          })
          
          same_par <- sapply(par_nodes, function(x){setequal(x, par_nodes[[1]])})
          
          #parent <- getParent(gs[[idx]], nodes_to_plot[1])
          count <- count + 1
          #print(parent)
          #print(unique(df$subset))
          plist[[count]] <- plot_gs(df = df, gs=gs, idx=idx, subset = parent, gate = nodes_to_plot_parent[same_par], ...)
          
          all_children <- unlist(sapply(nodes_to_plot_parent[same_par], function(x){getChildren(gs[[1]], x)}))
          names(all_children) <- NULL
          
          child_nodes_int <- c(child_nodes_int, all_children)
          nodes_to_plot_parent <- setdiff(nodes_to_plot_parent, nodes_to_plot_parent[same_par])
          
      }
      
    }
    
    child_nodes <- child_nodes_int

  }
  return(plist)
}


plot_stat <- function(df = NULL,
                      gs,
                      idx, 
                      subset,
                      yvar,
                      type = "bar",
                      color_var = NULL, 
                      axis_labels = NULL,
                      transformation = NULL,
                      spill = NULL,
                      default_trans = identity_trans(),
                      scale_values = FALSE,
                      free_y_scale = TRUE,
                      max_scale = 0,
                      facet_vars = "name",
                      group_var = "subset",
                      expand_factor = 0.1,
                      stat_function = "mean",
                      show.legend = TRUE,
                      y_trans = NULL
                    
){
  
  #if(log10_trans){
  #  trans <- log10_trans()
  #}else{
  #  trans <- identity_trans()
  #}
  
  if(!is.null(y_trans)){
   transformation <- lapply(yvar, function(x){y_trans})
   names(transformation) <- yvar
  }
  
  trans_name <-  unique(unlist(sapply(transformation[yvar], function(tf){tf$name})))
  
  
  #ylim <- NULL
  #if(!is.null(data_range)){
  #  ylim <- data_range[[yvar]]
  #}
  if(is.null(df)){
    df <- get_data_gs(gs = gs,
                      idx = idx,
                      subset = subset,
                      spill = spill)
  }else{
    df <- df[df$name %in% pData(gs)[["name"]][idx] & 
               df$subset %in% subset, ]
  } 
  
  
  for(i in 1:length(yvar)){
    df[[yvar[i]]] <- transformation[[yvar[i]]]$transform(df[[yvar[i]]])
    #df[[yvar[i]]] <- trans$transform(df[[yvar[i]]])
  }
  
  df_melt <- melt(df, id.vars = c("name", "subset"), measure.vars = yvar)
  df_melt <- df_melt[is.finite(df_melt$value), ]
  
  stat.fun <- function(...){do.call(stat_function, args = list(...))}
  df_cast <- dcast(df_melt, name + subset ~ variable, stat.fun, na.rm = TRUE)
  
  
  for(i in 1:length(yvar)){
    df_cast[[yvar[i]]] <- transformation[[yvar[i]]]$inverse(df_cast[[yvar[i]]])
  #  #df_cast[[yvar[i]]] <- trans$inverse(df_cast[[yvar[i]]])
  }
  
  df_scale <- df_cast
  if(scale_values){
    df_scale[-c(1:2)] <- scale(df_cast[-c(1:2)])
  }
  
  df_melt2 <- melt(df_scale, id.vars = c("name", "subset") )
  
  
  
  df_melt2 <- add_columns_from_metadata(df_melt2,
                                  metadata = pData(gs),
                                  color_var = color_var,
                                  facet_vars = facet_vars,
                                  group_var = group_var)
  
  #print(names(df_melt2))
  
  df_melt2 <- df_melt2[df_melt2$variable %in% yvar, ]

  ylim <- NULL
  #scale_y <- "fixed"
  
  
  
  if(!free_y_scale){
    delta = range(df_melt2$value)
    delta <- abs(delta[2]-delta[1])
    ylim <- c( min(df_melt2$value, na.rm = TRUE) - expand_factor*delta, 
               max(df_melt2$value, na.rm = TRUE) + expand_factor*delta)
    #scale_y <- "free"
  }
  
  if(scale_values & max_scale > 0){
    df_melt2$value[df_melt2$value > max_scale] <- max_scale
    df_melt2$value[df_melt2$value < -max_scale] <- -max_scale
    ylim <- c(-max_scale - expand_factor*2*max_scale, 
              max_scale + expand_factor*2*max_scale)
    #scale_y <- "free_y"
    #main_title <- "Scaled values (Z-score)"
  }
  
  # if(free_y_scale){
  #   scale_y <- "free_y"
  # }
  
  df_melt2$variable <- as.character(df_melt2$variable)
  
  if(!is.null(axis_labels)){
    #print(axis_labels)
    for(i in 1:length(yvar)){
      df_melt2$variable[df_melt2$variable == yvar[i]] <- axis_labels[[yvar[i]]]
    }
  }
  
  
  
  if(type == "tile"){
    
    p <- ggplot(df_melt2, aes_string( x = group_var ))
    p <- p + geom_tile(mapping = aes_string(y = "variable", fill = "value"),
                       show.legend = show.legend)
    
    
    p <- p + scale_fill_distiller(palette = "Spectral", limits = ylim) +
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
    
    if(!is.null(y_trans)){
      p <- p + scale_y_continuous(trans = y_trans)
    }
    
    if(length(grep("log", trans_name))>0){
      ylim[1] <- max(c(1, ylim[1])) 
    }
    
    p <- p + coord_cartesian(ylim = ylim, expand = FALSE)
    
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
                        #scales = scale_y,
                        scales = "free")
  #}
  
  
  ##################################################################################
  # general plot parameters
  
  
  p <- p + theme( axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                  strip.text.y = element_text(angle = 0)
                  )
    
  trans_name_plot <- trans_name
  if(length(trans_name)>1){
    trans_name_plot <- "defined by variable"
  }
  
  p <- p + ggtitle(paste("statistic : ",stat_function, " / transform : ", trans_name_plot, sep = "")) 

  p
  
}

dim_reduction <- function(df=NULL,
                          gs,
                          idx,
                          subset,
                          Ncells = NULL,
                          spill = NULL,
                          yvar,
                          transformation = NULL,
                          y_trans = NULL,
                          perplexity = 50,
                          method = "tSNE"){
  
  yvar <- yvar[yvar %in% gs@data@colnames]
  
  if(!is.null(y_trans)){
    transformation <- lapply(yvar, function(x){y_trans})
    names(transformation) <- yvar
  }
  
  trans_name <-  unique(unlist(sapply(transformation[yvar], function(tf){tf$name})))
  
  if(is.null(df)){
    df <- get_data_gs(gs = gs,
                      idx = idx,
                      subset = subset,
                      spill = spill)
  }else{
    df <- df[df$name %in% pData(gs)[["name"]][idx] & 
               df$subset %in% subset, ]
  } 
  
  
  df_trans <- df
  for(i in 1:length(yvar)){
    df_trans[[yvar[i]]] <- transformation[[yvar[i]]]$transform(df[[yvar[i]]])
  }
  
  #idx_filter <- which(rowSums(is.na(df_trans[, yvar])) > 0 | rowSums(!is.finite(df_trans[, yvar])) > 0)
  
  idx_filter <- which(rowSums(is.na(df_trans[, yvar])) > 0)
  
  if(length(idx_filter)>0){
    message(paste("Filter out ", length(idx_filter), " cells with NA values", sep =""))
  }
  df_trans <- df_trans[-idx_filter, ]
  
  Ncells_tSNE <- dim(df_trans)[1]
  message(paste("Running tSNE with ", Ncells_tSNE, " cells and ",  length(yvar), " parameters", sep = ""))
  
  if(Ncells > 3000){
    message("This may take a while... Try with less cells.")
  }
  
  tSNE <- Rtsne(df_trans[ , yvar], perplexity = perplexity)
  df_tSNE <- tSNE$Y
  colnames(df_tSNE) <- c("tSNE1","tSNE2")
  
  return(cbind(df[-idx_filter, ], df_tSNE))
  
}
