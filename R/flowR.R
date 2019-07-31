# library(scales)
# library(viridis)
# library(ggcyto)
# library(data.table)
# library(ggsignif)
# #library(sf) # need libudunits2-dev
# #devtools::install_github("JinmiaoChenLab/ClusterX")
# library(ClusterX)
# #devtools::install_github("JinmiaoChenLab/Rphenograph")
# library(Rphenograph)
# library(Rtsne)
# #BiocManager::install("Biobase")
# library("Biobase")
# library(xml2)

####################################################################################################
# Parse workspace

#' @import xml2
parseSampleNodes <- function(x){
  name <- xml_text(xml_find_all(x, ".//@name"))[1]
  sampleID <- xml_integer(xml_find_all(x, ".//@sampleID"))[1]
  return(list("name" = name, "sampleID" = sampleID))
}

#' @import xml2
parseGroupNodes <- function(x){
  name <- xml_text(xml_find_all(x, ".//@name"))[1]
  sampleID <- xml_integer(xml_find_all(xml_find_all(x, ".//SampleRefs"), ".//@sampleID"))
  return(list("name" = name, "sampleID" = sampleID))
}

#' @import xml2
parseGate <- function(x){
  res <- list()
  
  name <- xml_text( xml_find_all(xml_parent(x), ".//@name")[1] )
  
  if( xml_name( xml_parent(xml_parent(xml_parent(x))) ) == "Population"){
    parent <- xml_text(xml_find_all( xml_parent(xml_parent(xml_parent(x))), ".//@name")[1])
  }else{
    parent <- "root"
  }
  
  # find all parent gates recursively
  all_parents <- find_all_parent_gates(x)
  
  if(length(all_parents)>0){
    parent_long <- paste("/",paste(all_parents, collapse = "/"), sep = "")
  }else{
    parent_long <- "root"
  }
  
  name_long <- paste("/",paste(c(all_parents, name), collapse = "/"), sep = "")

  type <- xml_name(xml_child(x))
  dim <- xml_text(xml_find_all(xml_find_all(x, ".//gating:dimension"), ".//@data-type:name"))
  res <- c(res, list("name" = name, 
                     "parent" =  parent, 
                     "name_long" = name_long, 
                     "parent_long" = parent_long, 
                     "type" = type, 
                     "dim" = dim))
  
  if(type == "RectangleGate"){
    min <- xml_double(xml_find_all(x, ".//@gating:min"))
    max <- xml_double(xml_find_all(x, ".//@gating:max"))
    m <- rbind(min, max)
    colnames(m) <- dim
    res <- c(res, list("boundaries" = m))
  }
  if(type == "PolygonGate" ){
    vertexes <- xml_double(xml_find_all(xml_find_all(x, ".//gating:vertex"), ".//@data-type:value"))
    polygon <- matrix(vertexes, nrow = 2)
    polygon <- t(polygon)
    colnames(polygon) <- res[["dim"]]
    res <- c(res, list("polygon" = polygon))
  }
  return(res)
}

#' @description  find all parent gates recursively
#' @import xml2
find_all_parent_gates <- function(x){
  all_parents <- NULL
  y <- x
  while(xml_name( xml_parent(xml_parent(xml_parent(y))) ) == "Population"){
    all_parents <- c(xml_text(xml_find_all( xml_parent(xml_parent(xml_parent(y))), ".//@name")[1]), all_parents)
    y <- xml_parent(xml_parent(xml_parent(y)))
    idx_gate <- which( xml_name( xml_children(y) ) == "Gate")
    if(length(idx_gate)>0){
      y <- xml_children(y)[[idx_gate]]
    }else{
      break
    }
    
  }
  return(all_parents)
}


####################################################################################################
# Transformations

#' @import flowWorkspace
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

#' @import flowWorkspace
asinh_trans <- function (..., n = 6, equal.space = FALSE){
  trans <- asinh_transform(...)
  inv <- asinh_transform(..., inverse = TRUE)
  flow_trans(name = "asinh", trans.fun = trans, inverse.fun = inv, 
             n = n, equal.space = equal.space)
}

####################################################################################################
# Gating

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


#' @import flowWorkspace
get_gates_from_gs <- function(gs){
  
  nodes <- getNodes(gs)
  gates <- list()
  
  for(node in setdiff(nodes, "root")){
    g <- getGate(gs[[1]], node)
    print(names(g@parameters))
    parent <- getParent(gs[[1]], node)
    gates[[node]] <- list(gate = g, parent = parent)
  } 
  
  #print(gates)
  return(gates)
  
}

#' @import xml2
#' @import flowCore
get_gates_from_ws <- function(ws_path, group = NULL){
  
  ws <- read_xml(ws_path)
  
  # get Groups info
  GroupNodes <- xml_find_all(ws, "//GroupNode")
  group_info <-  lapply(GroupNodes, parseGroupNodes)
  group_info <- data.frame( do.call(rbind, group_info ) )
  #print(group_info)
  
  # get Samples info
  SampleNodes <- xml_find_all(ws, "//SampleNode")
  sample_info <- lapply(SampleNodes , parseSampleNodes)
  sample_info <- data.frame( do.call(rbind, sample_info ) )
  #print(sample_info)
  
  if(is.null(group)){
    group_selected <- group_info$name[1]
  }else{
    group_selected <- group
  }
  
  # get Gates for first sample in group
  sampleID <- group_info$sampleID[[which(group_info$name == group_selected)]][1]
  if(length(sampleID) > 0){
    SampleNode <- SampleNodes[ which( sample_info$sampleID == sampleID) ]
  }else{
    stop("Could not find sample in group")
  }
  
  #print(SampleNode)
  gates <- lapply(xml_find_all(SampleNode, ".//Gate"), parseGate)
  
  #print(gates)
  
  gate_list <- list()
  
  for(i in 1:length(gates)){
    
    parent <- gates[[i]]$parent_long
    name <- gates[[i]]$name_long
    
    parent <- gsub(" ", "_", parent)
    name <- gsub(" ", "_", name)
    
    if(gates[[i]]$type == "RectangleGate"){
      boundaries <- gates[[i]]$boundaries
      g <- rectangleGate(.gate = boundaries, filterId = basename(name))
      gate_list[[name]] <- list(gate = g, parent = parent)
    }else if(gates[[i]]$type == "PolygonGate"){
      polygon <- gates[[i]]$polygon
      g <- polygonGate(.gate = polygon, filterId = basename(name) )
      gate_list[[name]] <- list(gate = g, parent = parent)
    }else{
      warning(paste("gate type", gates[[i]]$type, "not supported"))
      #g <- NULL
      #gate_list[[name_long]] <- list(gate = g, parent = parent)
    }
    
    
  } 
  return(gate_list)
  
}

#' @import flowWorkspace
add_gates_flowCore <- function(gs, gates){
  
  new_gates_name <- setdiff(names(gates), getNodes(gs))
  gates <- gates[new_gates_name]
  
  ngates <- length(gates)

  
  if(ngates>0){
    
    idx <- 1:ngates
    
    while(length(idx)>0){
      
      i_added <- NULL
      
      for(i in 1:length(idx)){
        
        g <- gates[[idx[i]]]
        
        #print(g)
        #print(g$parent)
        #print(union(getNodes(gs), "root"))
        
        if(g$parent %in% union(getNodes(gs), "root") ){
          
          #print(names(gates)[idx[i]])
          #print( names(g$gate@parameters))
          #print( gs@data@colnames )
          
          if( !is.null(names(g$gate@parameters)) & length( setdiff( names(g$gate@parameters), gs@data@colnames) ) == 0 ){
            print("add before")
            add(gs,
                g$gate,
                parent = g$parent,
                name = g$gate@filterId)
            print("add after")
          }else{
            warning("Could not find gate parameters in flowData")
          }
          
          
          i_added <- c(i_added, i)
        }
      }
      if(!is.null(i_added)){
        idx <- idx[-i_added]
      }else{
        break
      }
      
    }
    recompute(gs)
  }
  
  print("OK")
  
  return(gs)
}

#' @import flowWorkspace
transform_gates <- function(gates, 
                            transformation = NULL, 
                            pattern = "[\\<|\\>]", 
                            replacement = "",
                            time_step = as.numeric(description(ff)[["$TIMESTEP"]]) ){
  
  # transform gate coordinates
  
  ngates <- length(gates)
  
  if(ngates>0){
    
    for(i in 1:ngates){
      
      g <- gates[[i]]
      
      
      if(class(g$gate) == "polygonGate"){
        
        polygon <- g$gate@boundaries
        if(!is.null(pattern)){
          colnames(polygon) <- gsub(pattern = pattern, replacement = replacement, colnames(polygon))
        }
        
        
        if(!is.null(transformation)){
          for(j in 1:length(colnames(polygon))){
            polygon[,j] <- transformation[[colnames(polygon)[j]]]$transform(polygon[,j])
            if(colnames(polygon)[j] == "Time"){
              polygon[,j] <- polygon[,j]/time_step
            }
          }
        }
        
        
        trans_gate <- polygonGate(.gate = polygon, filterId=g$gate@filterId)
      }
      
      if(class(g$gate) == "rectangleGate"){
        
        polygon <- rbind(g$gate@min, g$gate@max)
        if(!is.null(pattern)){
          colnames(polygon) <- gsub(pattern = pattern, replacement = replacement, colnames(polygon))
        }

        
        if(!is.null(transformation)){
          for(j in 1:length(colnames(polygon))){
            polygon[,j] <- transformation[[colnames(polygon)[j]]]$transform(polygon[,j])
            
            if(colnames(polygon)[j] == "Time"){
              polygon[,j] <- polygon[,j]/time_step
            }
          }
        }
        
        trans_gate <- rectangleGate(.gate = polygon, filterId=g$gate@filterId)
      }
      
      if(class(g$gate) == "ellipsoidGate"){
        
        cov <- g$gate@cov
        mean <- g$gate@mean
        polygon <- as.matrix(ellipse_path(cov = cov, mean = mean))
        
        if(!is.null(pattern)){
          colnames(polygon) <- gsub(pattern = pattern, replacement = replacement, colnames(polygon))
        }

        
        if(!is.null(transformation)){
          for(j in 1:length(colnames(polygon))){
            polygon[,j] <- transformation[[colnames(polygon)[j]]]$transform(polygon[,j])
            
            if(colnames(polygon)[j] == "Time"){
              polygon[,j] <- polygon[,j]/time_step
            }
          }
        }
        trans_gate <- polygonGate(.gate = polygon, filterId=g$gate@filterId)
      }
      
      gates[[i]] <- list(gate = trans_gate, parent = g$parent)
    }
    
  }
  
  return(gates)
  
}


####################################################################################################
# Getting data

#' @import flowWorkspace
#' @import flowCore
get_data_gs <- function(gs,
                        sample,
                        subset,
                        Ncells = NULL,
                        spill = NULL,
                        return_comp_data = TRUE,
                        updateProgress = NULL
){
  
  idx <- match(sample, pData(gs)$name)
  idx <- idx[!is.na(idx)]
  print(sample)
  
  if(length(idx) == 0){
    stop("sample not found in gating set")
  }
  
  if(!is.null(spill)){
    gates <- get_gates_from_gs(gs)
    fs <- getData(gs[idx])
    spill_list <- lapply(1:length(idx), function(x){return(spill)})
    names(spill_list) <- sampleNames(fs)
    fs <- compensate(fs, spill_list)
    gs_comp <- GatingSet(fs)
    gs_comp <- add_gates_flowCore(gs_comp, gates)
    print(colnames(gs_comp))
    print(getNodes(gs_comp))
    print(pData(gs_comp)$name)
  }
  
  df <- list()
  count <- 0
  
  for(i in 1:length(idx)){
    
    for(k in 1:length(subset)){
      
      print(as.name(subset[k]))
      print(subset[k])
      
      idx_subset <- NULL
      if(!is.null(spill)){
        idx_comp <- match(pData(gs)$name[idx[i]], pData(gs_comp)$name)
        print("idx_comp")
        print(idx_comp)
      }
      
      if(subset[k] != "root"){
        if(!is.null(spill)){
          print("idx1\n")
          idx_subset <- getIndices(gs_comp[[idx_comp]], as.name(subset[k]))[[1]]
          print("ok idx1\n")
        }else{
          print("ok idx2\n")
          idx_subset <- getIndices(gs[[idx[i]]], as.name(subset[k]))[[1]]
          print("ok idx2\n")
        }
      }
      
      if(return_comp_data & !is.null(spill) ){
        ff <- getData(gs_comp[[idx_comp]])
      }else{
        ff <- getData(gs[[idx[i]]])
      }
      
      
      df_int <- as.data.frame(exprs(ff))
      if(!is.null(idx_subset)){
        df_int <- df_int[idx_subset, ]
      }
      
      print("ok sub\n")
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
  
  if(length(df_tot[["name"]]) > 0){
    return(df_tot)
  }else{
    return(NULL)
  }
  
  
}

add_columns_from_metadata <- function(df,
                                      metadata
                                      #color_var = NULL, 
                                      #facet_vars = "name",
                                      #group_var = "name",
                                      #yridges_var = NULL
                                      ){
  
  # if(!is.null(facet_vars)){
  #   facet_vars <- facet_vars[facet_vars %in% names(metadata)]
  # }
  # 
  # if(!is.null(color_var)){
  #   color_var <- color_var[color_var %in% names(metadata)]
  # }
  # 
  # new_vars <- unique(setdiff( c(yridges_var, 
  #                               #group_var, 
  #                               facet_vars, 
  #                               color_var), 
  #                             names(df)))
  
  new_vars <- unique(setdiff(names(metadata), names(df)))
  
  if(length(new_vars)>0){
    for(variable in new_vars){
      df[[variable]] <- metadata[[variable]][match(df[["name"]], metadata$name)]
    }
  }
  
  #print(names(df))
  
  return(df)
}


####################################################################################################
# Plotting

#' @import flowWorkspace
#' @import ggplot2
#' @import ggridges
#' @import viridis
#' @import scales
plot_gs <- function(df = NULL,
                    gs = NULL, 
                    sample,
                    subset,
                    xvar = NULL,
                    yvar = NULL,
                    axis_labels = NULL,
                    color_var = "name", 
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
                    metadata = NULL,
                    facet_vars = "name",
                    #group_var = "name",
                    yridges_var = "name",
                    norm_density = TRUE,
                    smooth = FALSE,
                    ridges = FALSE,
                    bw = 0.1,
                    show.legend = TRUE,
                    legend.position = "right",
                    use_log10_count = TRUE
                    ){
  
  
  
  if(!is.null(gs)){
    idx <- match(sample, pData(gs)$name)
    #print(idx)
  }
  
  if(!is.null(gate)){
    if(class(gate) == "character"){
      if(!is.null(gs)){
        
        gate_int <- lapply(gate, function(x){
          if(x!="root"){
            getGate(gs[[idx[1]]], x)
          }else{
            NULL
          }})
        names(gate_int) <- gate
        gate <- gate_int
      }else{
        gate <- NULL
      }
      
    }else if(grep("Gate", class(gate)) >0){
      gate <- list(gate)
    }
  }

  
  if(is.null(xvar)){
    
    if(subset[1] != "root"){
      g <- getGate(gs[[idx[1]]], y= subset[1])
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
        if(class(gate[[i]])== "polygonGate"){
          xvar <- colnames(gate[[i]]@boundaries)[1]
          yvar <- try(colnames(gate[[i]]@boundaries)[2], silent = TRUE)
          break
        }
        if(class(gate[[i]]) == "rectangleGate"){
          xvar <- names(gate[[i]]@min)[1]
          yvar <- try(names(gate[[i]]@min)[2], silent = TRUE)
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
                      sample = sample,
                      subset = subset,
                      spill = spill)
    
  }else{
    #df <- df[df$name %in% pData(gs)[["name"]][idx],  names(df) %in% c("name", subset, xvar, yvar)]
    df <- df[df$name %in% sample & df$subset %in% subset, ]
  }
  
  # if("cluster" %in% names(df)){
  #   df[["cluster"]] <- as.factor(df[["cluster"]])
  # }
  
  if( !setequal( xvar[xvar %in% names(df)], xvar ) | !setequal( yvar[yvar %in% names(df)], yvar ) ){
    warning("Some variables could not be found in flowData")
    return(NULL)
  }
  
  if(is.null(metadata) & !is.null(gs)){
    metadata <- pData(gs)
   
  }
  
  if(!is.null(metadata)){
    df <- add_columns_from_metadata(df,
                                    metadata = metadata
                                    #color_var = color_var,
                                    #facet_vars = facet_vars,
                                    #group_var = group_var,
                                    #yridges_var = yridges_var
                                    )
  }
    
  xlim <- range(df[[xvar]])
  ylim <- range(df[[yvar]])
  if(!is.null(data_range)){
    xlim <- data_range[[xvar]]
    ylim <- data_range[[yvar]]
  }
  
  if(!is.null(color_var)){
    if(color_var == "cluster"){
      df[["cluster"]] <- as.factor(df[["cluster"]])
    }
  }
  
  # if(is.null(color_var)){
  #   color_var <- group_var
  # }
  
  ##################################################################################
  # plot density hexagonal
  if(type == "hexagonal"){
    
    
    p <- ggplot(df,
                aes_(x = as.name( xvar ), 
                     y = as.name( yvar ) ) )+
      geom_hex(bins = bins, show.legend = show.legend)
    
    if(use_log10_count){
      p <- p + scale_fill_viridis(trans = log10_trans())
    }else{
      p <- p + scale_fill_viridis()
    }
      
    
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
        p <- p + geom_density_ridges(mapping = aes_string(fill = color_var, 
                                                          color = color_var, 
                                                          y = yridges_var, 
                                                          height = stat_var), 
                                     alpha = alpha, 
                                     #bw = dist(transformation[[xvar]]$transform( range(df[[xvar]]) / bins ))[1], 
                                     #bw = dist(range(df[[xvar]])/bins)[1], 
                                     bw = 1/bins, 
                                     stat = "density",
                                     show.legend = show.legend)
      }else{
        p <- p + geom_density(mapping = aes_string(fill = color_var, color = color_var, y = stat_var), 
                              alpha = alpha, 
                              #bw = dist(transformation[[xvar]]$transform( range(df[[xvar]]) / bins ))[1], 
                              #bw = dist(range(df[[xvar]])/bins)[1], 
                              bw = 1/bins, 
                              show.legend = show.legend)
      }
    }else{
      p <- p + geom_histogram(mapping = aes_string(fill = color_var, color = color_var, y = stat_var), 
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
                aes_string(x = as.name( xvar ), 
                     y = as.name( yvar )))
    
    if(type == "dots"){

      if(!is.null(color_var)){
        
          #idx_col <- match(color_var, names(df))
          p <- p + geom_point(mapping = aes_( #group = as.name(group_var), 
                                              colour = as.name(color_var)),
                              alpha = alpha, 
                              size = size, 
                              show.legend = show.legend)
          
          if(color_var %in% gs@data@colnames){
            if(color_var != "cluster"){
              p <- p + scale_colour_viridis(trans = transformation[[color_var]], name = color_var)
            }
            
          }
        
      }else{
        p <- p + geom_point(mapping = aes_string(colour = color_var), 
                            alpha = alpha, 
                            size = size, 
                            show.legend = show.legend)
      }
    }

    
    if(type == "contour"){
      p <- p + geom_density_2d(mapping = aes_string(color = color_var), 
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
      
      
      if(class(gate_int) == "polygonGate" ){
        if(length(unique(colnames(gate_int@boundaries)))>1){
          polygon <- as.data.frame(gate_int@boundaries)
        }
      }else if(class(gate_int) == "rectangleGate"){
        idx_x <- match(xvar, names(gate_int@min))
        idx_y <- match(yvar, names(gate_int@min))
        if(!is.na(idx_x)){
          range_x <- c(gate_int@min[idx_x], gate_int@max[idx_x])
        }else{
          range_x <- xlim
        }
        if(!is.na(idx_y)){
          range_y <- c(gate_int@min[idx_y], gate_int@max[idx_y])
        }else{
          range_y <- ylim
        }
        
        polygon <- data.frame(x = c(range_x[1], range_x[2], range_x[2], range_x[1]),
                                 y = c(range_y[1], range_y[1], range_y[2], range_y[2]))

        names(polygon) <- c(xvar, yvar)
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
                         alpha=0.05) +
            geom_label(data = df_label, aes(x=x, y=y, label = label),
                       fill = rgb(1,1,1,0.5), color = "red", hjust = "middle", vjust = "center")
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
      if(xvar != yvar){
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
                       alpha=0.05) 
      }
      
    }
    
    
  }
  
  
  ##################################################################################
  # general plot parameters
 
  if(!is.null(min_value)){
    xlim[1] <- min_value
    ylim[1] <- min_value
  }
  
  
  if(!is.null(facet_vars)){
    formula_facet <- as.formula(paste(" ~", paste(facet_vars, collapse = " + ")))
    p <- p + facet_grid(formula_facet,
                        labeller = label_both, 
                        #scales = scale_y,
                        scales = "free")
  }else{
    p <- p + facet_null()
  }
  

  # if(!is.null(facet_vars)){
  #   p <- p + facet_wrap(facets = sapply(facet_vars, as.name), labeller = label_both)
  # }else{
  #   p <- p + facet_null()
  # }
  
  labx <- ifelse(is.null(axis_labels), xvar, axis_labels[[xvar]])
  p <- p + scale_x_continuous(name = labx, trans = transformation[[xvar]], limits = xlim) 
  p <- p + theme(plot.title = element_text(face = "bold"),
                 legend.position = legend.position)
  
  if(length(subset)==1){
    p <- p + ggtitle(subset)
  }
  
  if(type != "histogram"){
    laby <- ifelse(is.null(axis_labels), yvar, axis_labels[[yvar]])
    p <- p + scale_y_continuous(name = laby, trans = transformation[[yvar]], limits = ylim) 
  }
  
  p
  
}

#' @import flowWorkspace
plot_gh <- function(df = NULL, gs, sample, spill = NULL, ...){
  
  # if(length(sample) != 1){
  #   stop("length of idx must be equal to 1")
  # }
  
  if(is.null(df)){
    
    df <- get_data_gs(gs = gs,
                      sample = sample,
                      subset = getNodes(gs),
                      spill = spill)
    
  }
  
  idx <- match(sample, pData(gs)$name)
  
  child_nodes <- getChildren(gs[[idx[1]]], "root")
  plist <- list()
  count <- 0
  
  #plot gates descending the gh until there are no more children gates
  while(length(child_nodes)>0){
    
    child_nodes_int <- NULL
    nodes_to_plot <- child_nodes
    all_parents <- sapply(nodes_to_plot, function(x){getParent(gs[[idx[1]]], x)})
    names(all_parents) <- NULL
    #print(all_parents)
    
    for(parent in unique(all_parents)){
      
      idx_parent <- which(all_parents == parent)
      
      nodes_to_plot_parent <- nodes_to_plot[idx_parent]
      
    #plot together gates that share the same set of parameters
      
      while(length(nodes_to_plot_parent) > 0){
        
          
          par_nodes <- lapply(nodes_to_plot_parent, function(x){
            
            #print(x)
            g <- getGate(gs[[idx[1]]], x)
            
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
          plist[[count]] <- plot_gs(df = df, gs=gs, sample=sample, subset = parent, gate = nodes_to_plot_parent[same_par], ...)
          
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

#' @import flowWorkspace
#' @import ggplot2
#' @import viridis
#' @import scales
#' @import ggsignif
#' @import data.table
plot_stat <- function(df = NULL,
                      gs,
                      sample, 
                      subset,
                      yvar,
                      type = "bar",
                      metadata = NULL,
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
                      expand_factor = 0.2,
                      stat_function = "mean",
                      show.legend = TRUE,
                      y_trans = NULL,
                      strip.text.y.angle = 0
                    
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
                      sample = sample,
                      subset = subset,
                      spill = spill)
  }else{
    df <- df[df$name %in% sample & df$subset %in% subset, ]
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
  
  
  if(is.null(metadata) & !is.null(gs)){
    metadata <- pData(gs)
  }
  
  if(!is.null(metadata)){
    df_melt2 <- add_columns_from_metadata(df_melt2,
                                          metadata = metadata
                                          #color_var = color_var,
                                          #facet_vars = facet_vars,
                                          #group_var = group_var
                                          )
  }
  
  
  #print(names(df_melt2))
  
  df_melt2 <- df_melt2[df_melt2$variable %in% yvar, ]

  ylim <- NULL
  #scale_y <- "fixed"
  
  
  
  if(!free_y_scale){
    if(!is.null(y_trans)){
      rg = y_trans$transform(range(df_melt2$value))
    }else{
      rg = range(df_melt2$value)
    }
    delta <- abs(rg[2]-rg[1])
    
    if(!is.null(y_trans)){
      ylim <- y_trans$inverse(c( min(rg) - expand_factor*delta, 
                 max(rg) + expand_factor*delta))
    }else{
      ylim <- c( min(rg) - expand_factor*delta, 
                 max(rg) + expand_factor*delta)
    }
    
    # ylim <- c( min(df_melt2$value, na.rm = TRUE) - expand_factor*delta, 
    #            max(df_melt2$value, na.rm = TRUE) + expand_factor*delta)
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
    
    if(!is.null(ylim)){
      if(!is.finite(ylim[1])){
        ylim[1] <- 1
      }
    }
    
   
    p <- p + coord_cartesian(ylim = ylim, expand = free_y_scale)
    
    
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
                  strip.text.y = element_text(angle = strip.text.y.angle)
                  )
    
  trans_name_plot <- trans_name
  if(length(trans_name)>1){
    trans_name_plot <- "defined by variable"
  }
  
  p <- p + ggtitle(paste("statistic : ",stat_function, " / transform : ", trans_name_plot, sep = "")) 

  p
  
}


####################################################################################################
# Dimensionality Reduction 

#' @import Rtsne
#' @import umap
#' @import scales
dim_reduction <- function(df,
                          yvar,
                          Ncells = NULL,
                          transformation = NULL,
                          y_trans = log10_trans(),
                          perplexity = 50,
                          method = "tSNE"){
  
  idx_cells_kept <- 1:dim(df)[1]
  
  if(is.numeric(Ncells)){
    if(Ncells<=0){
      warning("Parameter Ncells must be > 0")
      return(NULL)
    }
  }
  
  if(!is.null(y_trans)){
    transformation <- lapply(yvar, function(x){y_trans})
    names(transformation) <- yvar
  }
  
  trans_name <-  unique(unlist(sapply(transformation[yvar], function(tf){tf$name})))
  
  #print(yvar)
  #print(trans_name)
  
  df_trans <- df
  df_filter <- df
  
  #print(yvar)
  #print(names(df_trans))
  #print(names(transformation))
  
  for(i in 1:length(yvar)){
    df_trans[[yvar[i]]] <- transformation[[yvar[i]]]$transform(df[[yvar[i]]])
  }

  cell_has_non_finite <- apply(X = df_trans[, yvar], MARGIN = 1, FUN = function(x){sum(!is.finite(x) )>0})
  cell_has_na <- rowSums(is.na(df_trans[, yvar])) > 0
  idx_filter <- which(cell_has_na | cell_has_non_finite)
  
  if(length(idx_filter)>0){
    message(paste("Filter out ", length(idx_filter), " cells with NA or non-finite values", sep =""))
    df_trans <- df_trans[-idx_filter, ]
    df_filter <- df_filter[-idx_filter, ]
    idx_cells_kept <- idx_cells_kept[-idx_filter]
  }
  
  if(!is.null(Ncells) & is.numeric(Ncells)){
    Ncells_used <- min(dim(df_trans)[1], Ncells)
    idx_cells <- sample(1:dim(df_trans)[1], Ncells_used, replace = FALSE)
  }else{
    Ncells_used <- dim(df_trans)[1]
    idx_cells <- 1:dim(df_trans)[1]
  }
  
  idx_cells_kept <- idx_cells_kept[idx_cells]
  print(length(idx_cells_kept))
  
  message(paste("Running ", method, " with ", Ncells_used, " cells and ",  length(yvar), " parameters", sep = ""))
  
  if(Ncells_used > 3000){
    message("This may take a while... Try with less cells.")
  }

  if(method == "tSNE"){
    tSNE <- Rtsne(df_trans[ idx_cells , yvar], perplexity = perplexity)
    df_tSNE <- tSNE$Y
    colnames(df_tSNE) <- c("tSNE1","tSNE2")
    return(list( df = cbind(df_filter[idx_cells, ], df_tSNE), keep = idx_cells_kept, vars = c("tSNE1","tSNE2")))
  }
  
  if(method == "umap"){
    df_umap <- umap(df_trans[ idx_cells , yvar])
    df_umap <- df_umap$layout
    colnames(df_umap) <- c("UMAP1","UMAP2")
    return(list( df = cbind(df_filter[idx_cells, ], df_umap), keep = idx_cells_kept, vars = c("UMAP1","UMAP2")))
  }
  
  return(NULL)
  
}





####################################################################################################
# Clustering

#' @import ClusterX
#' @import scales
get_cluster <- function(df,
                        yvar,
                        transformation = NULL,
                        y_trans = identity_trans(),
                        dc=3, 
                        alpha = 0.001,
                        method = "ClusterX"){
         
  idx_cells_kept <- 1:dim(df)[1]
  
  if(!is.null(y_trans)){
    transformation <- lapply(yvar, function(x){y_trans})
    names(transformation) <- yvar
  }
  
  trans_name <-  unique(unlist(sapply(transformation[yvar], function(tf){tf$name})))
  
  #print(yvar)
  #print(trans_name)
  
  df_trans <- df
  df_filter <- df
  
  
  for(i in 1:length(yvar)){
    df_trans[[yvar[i]]] <- transformation[[yvar[i]]]$transform(df[[yvar[i]]])
  }
  
  cell_has_non_finite <- apply(X = df_trans[, yvar], MARGIN = 1, FUN = function(x){sum(!is.finite(x) )>0})
  cell_has_na <- rowSums(is.na(df_trans[, yvar])) > 0
  idx_filter <- which(cell_has_na | cell_has_non_finite)
  
  if(length(idx_filter)>0){
    message(paste("Filter out ", length(idx_filter), " cells with NA or non-finite values", sep =""))
    df_trans <- df_trans[-idx_filter, ]
    df_filter <- df_filter[-idx_filter, ]
    idx_cells_kept <- idx_cells_kept[-idx_filter]
  }
  
  message(paste("Clustering ", dim(df_trans)[1], " cells using 'CluserX' on ",  length(yvar), " parameters", sep = ""))
  
  DC <- ClusterX(df_trans[ , yvar], dc = dc, alpha = alpha)
  df_filter$cluster <- DC$cluster
  
  return(list(df = df_filter, keep = idx_cells_kept))
  
}


####################################################################################################
# Build FlowSet

#' @import flowCore
build_flowset_from_df <- function(df,
                                  fs = NULL,
                                  chanel_col = setdiff(names(df), c("name", "subset")), 
                                  sample_col = "name"){
  
  samples <- unique(df[[sample_col]])
  ff_list <- list()
  
  for(sample in samples){
    
    idx_cells <- which(df[[sample_col]] == sample)
    ncells <- length(idx_cells)
    if(ncells >0){
      df_sample <- df[ idx_cells , chanel_col]
      
      
      M <- as.matrix(df_sample)
      
      par <- NULL
      desc <- NULL
      
      if(!is.null(fs)){
        
        idx <- match(sample, pData(fs)$name)
        
        if(!is.na(idx)){
          
          par <- parameters(fs[[idx]])
          desc <- description(fs[[idx]])
          new_par <- setdiff(chanel_col, par@data$name)
          npar <- length(par@data$name)
          
          for(param in new_par){
            npar <- npar +1
            rg <- range(df_sample[[param]])
            par@data <- rbind(par@data, c(param, NA, diff(rg), rg[1], rg[2]))
            rownames(par@data)[npar] <- paste("$P",npar, sep = "")
            desc[[paste("$P",npar,"DISPLAY",sep="")]] <- NA
          }
          
          desc[["$TOT"]] <- dim(df_sample)[1]
        }
      }
      
      if(!is.null(par) & !is.null(desc)){
        ff_list[[sample]] <- flowFrame(exprs = M,
                                       parameters = par, 
                                       description = desc)
      }else{
        ff_list[[sample]] <- flowFrame(exprs = M)
      }
      
    }

  }
  
  if(length(ff_list)>0){
    fs_new <- flowSet(ff_list)
    if(!is.null(fs)){
      pdata <- data.frame(pData(fs))
      idx_match <- match(samples, pdata$name)
      if(length(colnames(pdata))>1){
        pData(fs_new) <- pdata[idx_match, ]
      }else{
        phenoData(fs_new)$name <- pdata[idx_match, ]
      }
    }
  }else{
    fs_new <- NULL
  }
  
  return(fs_new)
  
}
