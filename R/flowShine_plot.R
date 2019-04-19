
plot_gs <- function(gs, 
                    idx, 
                    subset,
                    xvar = NULL, 
                    yvar = NULL, 
                    color_var = NA, 
                    xlim = NULL, 
                    ylim=NULL, 
                    gate=NULL, 
                    type = "hexagonal", 
                    bins = 30,
                    alpha = 0.5,
                    size = 1,
                    transformation = NULL,
                    facet_vars = "name",
                    norm_density = TRUE,
                    smooth = FALSE,
                    ridges = FALSE,
                    bw = 0.01){
  
  
  if(is.null(transformation)){
    transformation <- lapply(gs@data@colnames, function(x){identity_trans()} )
    names(transformation) <- gs@data@colnames
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
      print(c(xvar,yvar))
    }else{
      xvar <- gs@data@colnames[1]
      yvar <- gs@data@colnames[2]
    }
    
    
    
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
  
  if(type != "histogram"){
    
    if(class(gate) == "character"){
      
      gate <- lapply(gate, function(x){
        if(x!="root"){
          getGate(gs[[idx[1]]], x)
        }else{
          NULL
        }})
      
    }else if(grep("Gate", class(gate)) >0){
      gate <- list(gate)
    }
    
    print(gate)
    
    for(j in 1:length(gate)){
      
      gate_int <- gate[[j]]
      
      if(.hasSlot(gate_int, "boundaries") ){
        
        polygon <- as.data.frame(gate_int@boundaries)
        idx_match <- match(c(xvar, yvar), names(polygon))
        
        if(sum(is.na(idx_match))==0){
          xlim <- range(c(xlim, polygon[[idx_match[1]]]))
          ylim <- range(c(ylim, polygon[[idx_match[2]]]))
          
          p <- p +
            geom_path(data = polygon, color = "red") +
            geom_polygon(data=polygon,
                         fill="red",
                         alpha=0.1)
        }else{
          warning(paste("gate is not defined with parameters ",xvar, "and", yvar, sep = "" ))
        }
      }
    }
    
  }
    
  
  
  ##################################################################################
  # general plot parameters
  
  p <- p + 
    xlab(xvar) +
    ggtitle(subset) +
    scale_x_continuous(trans = transformation[[xvar]], limits = xlim) 
  
  if(type != "histogram"){
    p <- p + scale_y_continuous(trans = transformation[[yvar]], limits = ylim) +
      ylab(yvar)
  }
    
  
  if(!is.null(facet_vars)){
    p <- p + facet_wrap(facets = sapply(facet_vars, as.name), labeller = label_both)
  }else{
    p <- p + facet_null()
  }
  
  p
  
}