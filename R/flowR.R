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
# Parse workspace and gates from xml files

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

#' @import xml2
#' @import flowCore
get_gates_from_ws <- function(ws_path, group = NULL){
  
  ws <- read_xml(ws_path)
  
  # get Groups info
  GroupNodes <- xml_find_all(ws, "//GroupNode")
  group_info <- lapply(GroupNodes, parseGroupNodes)
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

get_all_descendants <- function(named_list, names){
  
  parents <- sapply(named_list, function(x){x$parent}) 
  children <- names(named_list)[parents %in% names]
  children_all <- children
  while(length(children)>0){
    children <- get_all_descendants(named_list, children)
    children_all <- unique(c(children_all, children))
  }
  return(children_all)
  
}

get_all_ancestors <- function(named_list, names){
  
  parents <- sapply(named_list, function(x){x$parent}) 
  parents <- unlist(parents[names(named_list) %in% names])
  parents_all <- parents
  while(length(parents)>0){
    parents <- get_all_ancestors(named_list, parents)
    parents_all <- unique(c(parents_all, parents))
  }
  return(parents_all)
  
}

#' @import flowWorkspace
get_gates_from_gs <- function(gs){
  
  nodes <- getNodes(gs)
  gates <- list()
  
  for(node in setdiff(nodes, "root")){
    g <- getGate(gs[[1]], node)
    #print(names(g@parameters))
    parent <- getParent(gs[[1]], node)
    gates[[node]] <- list(gate = g, parent = parent)
  } 
  
  return(gates)
  
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
        
        if(g$parent %in% union(getNodes(gs), "root") ){
          
          if( !is.null(names(g$gate@parameters)) & length( setdiff( names(g$gate@parameters), gs@data@colnames) ) == 0 ){
            
            add(gs,
                g$gate,
                parent = g$parent,
                name = g$gate@filterId)
            
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
#' @import flowCore
getPopStatsPlus <- function(gs, spill = NULL, filter = NULL){
  
  fs <- getData(gs)
  gates <- get_gates_from_gs(gs) 
  if(!is.null(filter)){
    fs <- Subset(fs, filter)
  }
  
  if(!is.null(spill)){
    spill <- spill[row.names(spill) %in% colnames(gs), colnames(spill) %in% colnames(gs)]
    spill_list <- lapply(1:length(fs), function(x){return(spill)})
    names(spill_list) <- sampleNames(fs)
    fs <- compensate(fs, spill_list)
  }
  
  gs_comp <- GatingSet(fs)
  gs_comp <- add_gates_flowCore(gs_comp, gates)
  df <- getPopStats(gs_comp)
  
  df$name <- sapply(df$name, function(x){strsplit(x, split = "_[0-9]+$")[[1]][1]})
  df_root <- data.frame(name = pData(fs)$name)
  df_root$name <- pData(fs)$name
  df_root$Population <- "root"
  df_root$Count <- sapply(1:length(fs), function(x){dim(fs[[x]]@exprs)[1]})
  df_root$Parent <- NA
  df_root$ParentCount <- NA
  
  # df_root <- df
  # df_root <- df_root[df_root$Parent == "root", ]
  # 
  # df_root$Population <- df_root$Parent
  # df_root$Count <- df_root$ParentCount
  # df_root$Parent <- NA
  # df_root$ParentCount <- NA
  # 
  # name <- unique(df_root$name)
  # idx <- match(name, df_root$name)
  # 
  # df_merge <- rbind(df, df_root[idx, ])
  
  df_merge <- rbind(df, df_root)
  df_merge
}

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
  
  if(length(idx) != length(sample)){
    stop("sample not found in gating set")
  }
  
  gs_comp <- gs
  
  if(!is.null(spill)){
    spill <- spill[row.names(spill) %in% colnames(gs), colnames(spill) %in% colnames(gs)]
    gates <- get_gates_from_gs(gs)
    fs <- getData(gs[idx])
    spill_list <- lapply(1:length(idx), function(x){return(spill)})
    names(spill_list) <- sampleNames(fs)
    fs <- compensate(fs, spill_list)
    gs_comp <- GatingSet(fs)
    gs_comp <- add_gates_flowCore(gs_comp, gates)
  }
  
  df <- list()
  count <- 0
  
  for(i in 1:length(idx)){
    
    for(k in 1:length(subset)){
      
      idx_subset <- NULL
      if(!is.null(spill)){
        idx_comp <- match(pData(gs)$name[idx[i]], pData(gs_comp)$name)
      }
      
      if(subset[k] != "root"){
        if(!is.null(spill)){
          idx_subset <- getIndices(gs_comp[[idx_comp]], as.name(subset[k]))[[1]]
        }else{
          idx_subset <- getIndices(gs[[idx[i]]], as.name(subset[k]))[[1]]
        }
      }
      
      if(return_comp_data & !is.null(spill) ){
        ff <- getData(gs_comp[[idx_comp]])
      }else{
        ff <- getData(gs[[idx[i]]])
      }

      df_int <- as.data.frame(flowCore::exprs(ff))
      if(!is.null(idx_subset)){
        df_int <- df_int[idx_subset, ]
      }

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

#' Add metadata columns
#' @description  Add metadata columns
#' @param df data.frame with a column \code{name} used to map metadata.
#' Metadata should also contain a column \code{name}
#' @return a data.frame with additional columns
#' @export
add_columns_from_metadata <- function(df,
                                      metadata
                                      #color_var = NULL, 
                                      #facet_vars = "name",
                                      #group_var = "name",
                                      #yridges_var = NULL
                                      ){

  
  if(! "name" %in% names(df)){
    warning("Could not find column 'name' in data.frame. No metadata added.")
    return(df)
  }
  
  new_vars <- unique(setdiff(names(metadata), names(df)))
  
  if(length(new_vars)>0){
    for(variable in new_vars){
      df[[variable]] <- metadata[[variable]][match(df[["name"]], metadata$name)]
    }
  }

  return(df)
}


####################################################################################################
# Plotting

get_plot_data <- function(gs,
                          df=NULL, 
                          sample,
                          subset,
                          spill = NULL,
                          metadata = NULL){
                       
                      
  if(is.null(df)){
    df <- get_data_gs(gs = gs,
                      sample = sample,
                      subset = subset,
                      spill = spill)
  }else{
    df <- df[df$name %in% sample & df$subset %in% subset, ]
  }
  
  if(is.null(metadata) & !is.null(gs)){
    metadata <- pData(gs)
  }
  
  if(!is.null(metadata)){
    df <- add_columns_from_metadata(df,
                                    metadata = metadata)
  }
  
  if("cluster" %in% names(df)){
    df[["cluster"]] <- as.factor(df[["cluster"]])  
  }
  
  return(df)
  
}


plot_gs_data <- function(df,
                         plot_type,
                         plot_args = list()
                         ){
  
  p <- do.call(paste("plot", plot_type, sep="_"), 
               list(args = c(list(df=df), plot_args)))

  return(p)
}


#' @import ggplot2
#' @import viridis
#' @import flowWorkspace
#' @import rlang
format_plot <- function(p,
                        options = list()){
  
  xvar <- NULL
  yvar <- NULL
  if("x" %in% names(p$mapping)){
    xvar <- as.character(quo_get_expr(p$mapping$x))
  }
  if("y" %in% names(p$mapping)){
    yvar <- as.character(quo_get_expr(p$mapping$y))
  }
  
  xlim <- NULL
  ylim <- NULL
  color_var <- p$plot_env$color_var
  
  facet_scales <- "fixed"
  # if(!is_null(color_var)){
  #   if(color_var == "none"){
  #     color_var <- NULL
  #   }
  # }
  
  ############################################################################33
  #default parameters
  default_trans <- identity_trans()
  
  for(var in names(options)){
    assign(var, options[[var]])
  }
  
  ############################################################################33
  #transformations
  
  #transformation <- options$transformation
  #if(is.null(transformation)){
    
  #}
  transformation <- list()
  
  for(var in names(options$transformation)){
    transformation[[var]] <- options$transformation[[var]]
  }
  
  
  if(!is.null(xvar)){
    labx <- ifelse(is.null(options$axis_labels[[xvar]]), xvar, options$axis_labels[[xvar]])
    if(xvar %in% names(transformation)){
      p <- p + scale_x_continuous(name = labx, trans = transformation[[xvar]], limits = xlim) 
    }else{
      p <- p + scale_x_continuous(name = labx, trans = default_trans, limits = xlim)
    }
  }
  
  if(!is.null(yvar)){
    laby <- ifelse(is.null(options$axis_labels[[yvar]]), yvar, options$axis_labels[[yvar]])
    if(yvar %in% names(transformation)){
      p <- p + scale_y_continuous(name = laby, trans = transformation[[yvar]], limits = ylim) 
    }else{
      p <- p + scale_y_continuous(name = laby, trans = default_trans, limits = ylim) 
    }
  }

  if(p$plot_env$plot_type == "dots"){
    
    if(!is.null(color_var)){
      
      color_var_name <- options$color_var_name

      if( as.character(color_var) %in% setdiff(names(transformation), "cluster")){
        if(is.null(color_var_name)){
          color_var_name <- color_var
        }
        p <- p + scale_colour_viridis(trans = transformation[[color_var]],
                                      name = color_var_name)
      }
    }
  }

    ############################################################################33
    #facet
    
    if(!is.null(options$facet_vars)){
           formula_facet <- as.formula(paste(" ~", paste(options$facet_vars, collapse = " + ")))
           p <- p + facet_grid(formula_facet,
                               labeller = label_both, 
                               #scales = scale_y,
                               scales = facet_scales)
    }
    
  ############################################################################
  #theme
  
  if(length(unique(p$data$subset))==1){
    p <- p + ggtitle(unique(p$data$subset))
  }
  
  if(!is.null(options$theme_name)){
    theme_function <- function(...){
      do.call(options$theme_name, list(...))
    }
    p <- p + theme_function()
  }
  
  if(!is.null(options$legend.position)){
    p <- p + theme(legend.position = options$legend.position)
  }
  
  p <- p + theme(plot.title = element_text(face = "bold"))
  
  return(p)
  
}

plot_hexagonal <- function(args = list()){
  
  plot_type <- "hexagonal"
  bins <- 100
  use_log10_count <- TRUE
  
  for(var in names(args)){
    assign(var, args[[var]])
  }
  
  p <- ggplot(df,
              aes_(x = as.name( xvar ), 
                   y = as.name( yvar ) ) ) +
    geom_hex(bins = bins)
  
  if(use_log10_count){
    p <- p + scale_fill_viridis(trans = log10_trans())
  }else{
    p <- p + scale_fill_viridis()
  }
  
  p
}

#'@import ggridges
plot_histogram <- function(args = list()){
  
  plot_type <- "histogram"
  
  color_var <- NULL
  group_var <- NULL
  smooth <- FALSE
  ridges <- FALSE
  yridges_var <- "name"
  norm_density <- TRUE
  bins <- 100
  alpha <- 0.1
  
  for(var in names(args)){
    assign(var, args[[var]])
  }
  
  p <- ggplot(df,
              aes_(x = as.name( xvar )))
  
  if(norm_density){
    stat_var <- "stat(ndensity)"
  }else{
    stat_var <- "stat(density)"
  }
  
  if(!is.null(color_var)){
    if(color_var == "none"){
      color_var <- NULL
    }
  }
  
  if(!is.null(color_var)){
    color_var <- as.name(color_var)
  }
  
  if(!is.null(group_var)){
    if(group_var == "none"){
      group_var <- NULL
    }
  }
  
  if(!is.null(group_var)){
    group_var <- as.name(group_var)
  }
  
  if(!is.null(yridges_var)){
    yridges_var <- as.name(yridges_var)
  }

  if(smooth){
    if(ridges){
      p <- p + geom_density_ridges(mapping = aes_string(fill = color_var, 
                                                        color = color_var, 
                                                        group = group_var,
                                                        y = yridges_var, 
                                                        height = stat_var), 
                                   alpha = alpha, 
                                   bw = 1/bins, 
                                   stat = "density")
    }else{
      p <- p + geom_density(mapping = aes_string(fill = color_var, 
                                                 color = color_var, 
                                                 group = group_var,
                                                 y = stat_var), 
                            alpha = alpha,
                            bw = 1/bins)
    }
  }else{
    p <- p + geom_histogram(mapping = aes_string(fill = color_var, 
                                                 color = color_var, 
                                                 group = group_var,
                                                 y = stat_var), 
                            alpha = alpha,  
                            bins = bins, 
                            position = "identity", 
                            boundary = 0) 
  }
  
  return(p)
  
}

plot_dots <-function(args = list()){
  
  plot_type <- "dots"
  
  color_var <- NULL
  group_var <- NULL
  bins <- 100
  alpha <- 0.1
  size <- 0.1
  
  for(var in names(args)){
    assign(var, args[[var]])
  }
  
  if(!is.null(color_var)){
    if(color_var == "none"){
      color_var <- NULL
    }
  }
  
  if(!is.null(color_var)){
    color_var <- as.name(color_var)
  }
  
  if(!is.null(group_var)){
    if(group_var == "none"){
      group_var <- NULL
    }
  }
  
  if(!is.null(group_var)){
    group_var <- as.name(group_var)
  }
  
  p <- ggplot(df,
              aes_string(x = as.name( xvar ), 
                         y = as.name( yvar ),
                         colour = color_var,
                         group = group_var)) + 
    geom_point(alpha = alpha, 
               size = size)
   
  
  #if(!is.null(color_var)){
      # p <- p + geom_point(mapping =  aes_string(colour = color_var),
      #                     alpha = alpha, 
      #                     size = size)
  # }else{
  #   p <- p + geom_point(alpha = alpha, 
  #                       size = size)
  # }
  
  return(p)
  
}


plot_contour <-function(args = list()){
  
  plot_type <- "contour"

  color_var <- NULL
  group_var <- NULL
  fill <- NULL
  bins <- 30
  alpha <- 0.5
  size <- 0.25
  show_outliers <- TRUE
  
  for(var in names(args)){
    assign(var, args[[var]])
  }
  
  if(!is.null(color_var)){
    if(color_var == "none" ){
      color_var <- NULL
    }
  }
  
  # if(!is.null(color_var)){
  #   if(! color_var %in% c("subset", "name")) color_var <- NULL
  # }
  
  if(!is.null(color_var)){
    color_var <- as.name(color_var)
  }
  
  if(!is.null(group_var)){
    if(group_var == "none"){
      group_var <- NULL
    }
  }
  
  if(!is.null(group_var)){
    group_var <- as.name(group_var)
  }
  
  
  p <- ggplot(df,
              aes_string(x = as.name( xvar ), 
                         y = as.name( yvar )))



  if(show_outliers){
    
    p <- p + geom_point(size = 0.1)
    alpha <- 1
    fill <- "white"
  }
  
  
  
   if(!is.null(color_var)){
    #p <- p + stat_density2d(aes_string(alpha='..level..', fill='..level..'), 
    p <- p + stat_density2d(aes_string(colour = color_var, group = group_var),
                            fill = fill,
                            size=size, 
                            alpha= alpha,
                            geom="polygon",
                            bins=bins
                            )
    
    #scale_fill_gradient(low = "yellow", high = "red") 
   }else{
    p <- p + stat_density2d(aes_string(group = group_var),
                            fill = fill,
                            size=size,
                            alpha= alpha,
                            color = "black",
                            geom="polygon",
                            bins=bins
                            )
  }

  return(p)
}


#' @import pheatmap
#' @import dplyr
#' @import forcats
plot_stat_heatmap <-function(args = list()){
  
  plot_type <- "stat_heatmap"
  stat_var <- NULL
  annotation_vars <- NULL
  group_var <- NULL
  cluster_rows <- FALSE
  cluster_cols <- FALSE
  scale = "none"
  legend <- TRUE
  
  for(var in names(args)){
    assign(var, args[[var]])
  }
  
  df <- as.data.frame(df)

  labels_col <- 1:dim(df)[1]
  
  if(!is.null(group_var)){
    df <- df[order(df[[group_var[1]]]), ]
    #labels_col <- df$group_var
  }
  
  
  row.names(df) <- 1:dim(df)[1]
  print(df)
  
  idx <- which(names(df) %in% c("name", "subset", annotation_vars))
  #idx_annot <- which(names(df) %in% annotation_vars)
  idx_annot <- idx
  print(idx)
  
  df_stat <- df[-idx]
  if(!is.null(stat_var)){
    df_stat <- df_stat[which(names(df_stat) %in% stat_var)]
  }
  annotation <- df[idx_annot]

  df_plot <- t(df_stat)
  colnames(df_plot) <- 1:dim(df_plot)[2]
  
  if(dim(df_plot)[1]<2){
    cluster_rows <- FALSE
  }
  if(dim(df_plot)[2]<2){
    cluster_cols <- FALSE
  }
  p <- pheatmap(df_plot, show_colnames = FALSE,
                labels_col = labels_col,
                scale = scale,
                annotation_col = annotation,
                cluster_rows = cluster_rows,
                cluster_cols = cluster_cols,
                legend = legend

  )
  
  return(p)
}

#' @import ggplot2
#' @import dplyr
#' @import ggrepel
plot_stat_pca <-function(args = list()){
  
  plot_type <- "stat_pca"
  PCx <- "PC1"
  PCy <- "PC2"
  stat_var <- NULL
  annotation_vars <- NULL
  color_var <- NULL
  label_var <- "name"
  scale <- TRUE

  for(var in names(args)){
    assign(var, args[[var]])
  }
  
  df <- as.data.frame(df)
  
  idx <- which(names(df) %in% c("name", "subset", annotation_vars))
  idx_annot <- idx
  
  df_stat <- df[-idx]
  if(!is.null(stat_var)){
    df_stat <- df_stat[which(names(df_stat) %in% stat_var)]
  }
  df_stat <- df_stat[ , which(sapply(df_stat, is.numeric)) ] 
  
  
  rownames(df_stat) <- 1:dim(df_stat)[1]
  
  annotation <- df[idx_annot]
  rownames(annotation) <- 1:dim(annotation)[1]
  
  pca_res <- prcomp( df_stat, center = scale, scale. = scale)
  
  df_pca <- as.data.frame(pca_res$x)
  df_pca <- cbind(df_pca, annotation[match(row.names(annotation), row.names(df_pca)), ])
  
  if(!is.null(color_var)){
    color_var <- as.name(color_var)
  }
  if(!is.null(label_var)){
    label_var <- as.name(label_var)
  }
  
  p <- ggplot(df_pca, aes_string(x=as.name(PCx), 
                                        y=as.name(PCy),
                                        color = color_var, 
                                        label = label_var)) +
    geom_point() +
    geom_text_repel(show.legend = FALSE)
  
  return(p)
}

#' @import ggrepel
add_polygon_layer <-function(p,
                             polygon = NULL,
                             label = NULL){
  
  if(p$plot_env$plot_type != "histogram" & setequal(names(polygon), c("x", "y"))){
    if(!is.null(polygon$x)){
      
      polygon <- data.frame(x = polygon$x, y = polygon$y)
      polygon <- rbind(polygon, polygon[1,])
      
      p <- p +
        geom_path(data = polygon, mapping = aes(x=x, y=y), color = "red") +
        geom_polygon(data=polygon, mapping = aes(x=x, y=y),
                     fill="red",
                     alpha=0.05)
      if(!is.null(label)){
        df_label <- data.frame(x=mean(polygon$x), y= mean(polygon$y))
        p <- p +  geom_label_repel(data = df_label, force = 4,
                             mapping = aes(x=x, y=y), 
                             label = label, 
                             fill = rgb(1,1,1,0.85), 
                             color = "red", 
                             nudge_y = 0, 
                             nudge_x =0, 
                             point.padding = 0,
                             max.iter = 30000)
                             #hjust = "middle", vjust = "center")
      }

    }
    
  }
  
  return(p)
  
}

#' @importFrom sp over
#' @import rlang
add_gate <- function(p, gate){
  
  if(is.null(gate) | p$plot_env$plot_type == "histogram"){
    return(p)
  }
  
  polygon <- polygon <- get_gate_coordinates(gate)
  
  xvar <- as.character(quo_get_expr(p$mapping$x))
  yvar <- as.character(quo_get_expr(p$mapping$y))
  
  
  if(setequal(c(xvar, yvar), names(polygon))){ 
    
    in_poly <- point.in.polygon(p$data[[xvar]], 
                                p$data[[yvar]], 
                                polygon[[xvar]],
                                polygon[[yvar]], 
                                mode.checked=FALSE)
    
    perc_in_poly <- sprintf("%.1f", sum(in_poly)/length(in_poly)*100)
    
    idx_match <- match(c(xvar, yvar), names(polygon))
    names(polygon)[idx_match] <- c("x", "y")
    
    label <- paste(gate@filterId, " (", perc_in_poly, "%)", sep="")
    p <- add_polygon_layer(p, polygon = polygon, label = label)
  }
  
  return(p)

}


plot_gs <- function(gs,
                     df = NULL,
                     sample = NULL,
                     subset = NULL,
                     spill = NULL,
                     metadata = NULL,
                     plot_type = "contour",
                     plot_args = list(),
                     options = list(),
                     gate = NULL){
                     
  
  if(! "xvar" %in% names(plot_args)){
    plot_args[["xvar"]] <- gs@data@colnames[1]
  }
  if(! "yvar" %in% names(plot_args)){
    plot_args[["yvar"]] <- gs@data@colnames[2]
  }
  
  if(is.null(sample)) sample <-  pData(gs)$name[1]
  if(is.null(subset)) subset <- getNodes(gs)[1]
       
  df <- get_plot_data(df = df,
                      gs = gs, 
                      sample = sample,
                      subset = subset,
                      spill = spill, 
                      metadata = metadata)
  
  print(names(plot_args))
  
  p <- plot_gs_data(df = df,
                    plot_type = plot_type,
                    plot_args = plot_args)
  
  p <- format_plot(p, options = options)
  
  if(!is.null(gate)){
    for(gate_name in setdiff(gate, "root")){
      g <- getGate(gs[[1]], gate_name)
      p <- add_gate(p, g)
    }
  }
  
  
  return(p)
}

#' Plot all gates for a given sample of a gating set
#' @description  Plot all gates for a given sample of a gating set
#' @param df data.frame with columns \code{name} and \code{subset} 
#' containing sample and subset names respectively and
#' columns with plot variables. Ignored if \code{NULL}
#' @param gs a gating set
#' @param sample sample names
#' @param selected_subsets subset names
#' @param spill spillover matrix. If NULL, uncompensated data is used both for gating and plotting.
#' @param ... parameters passed to \code{plot_gs()}
#' @return a list of ggplot objects
#' @import flowWorkspace
plot_gh <- function( gs, 
                      df = NULL,
                      sample = NULL,
                      selected_subsets = NULL,
                      spill = NULL,
                      plot_type = "contour",
                      plot_args = list(), 
                      options = list()){
  
  # if(length(sample) != 1){
  #   stop("length of idx must be equal to 1")
  # }
  if(is.null(sample)){sample = pData(gs)$name[1]}
  
  idx <- match(sample, pData(gs)$name)
  
  if(is.null(selected_subsets)){
    selected_subsets <- setdiff(getNodes(gs), "root")
    subset <- getNodes(gs)
  }else{
    subset <- selected_subsets[selected_subsets %in% getNodes(gs)]
    parent_subsets <- sapply(subset, function(x){getParent(gs[[idx[1]]], x)})
    subset <- union(subset, parent_subsets)
  }
  
  if(is.null(df)){
    
    df <- get_data_gs(gs = gs,
                      sample = sample,
                      subset = subset,
                      spill = spill)
    
  }
  
  child_nodes <- getChildren(gs[[idx[1]]], "root")
  child_nodes <- child_nodes[child_nodes %in% selected_subsets]
  
  plist <- list()
  count <- 0
  
  #plot gates descending the gh until there are no more children gates
  while(length(child_nodes)>0){
    
    child_nodes_int <- NULL
    nodes_to_plot <- child_nodes
    all_parents <- sapply(nodes_to_plot, function(x){getParent(gs[[idx[1]]], x)})
    names(all_parents) <- NULL
    
    for(parent in unique(all_parents)){
      
      idx_parent <- which(all_parents == parent)
      
      nodes_to_plot_parent <- nodes_to_plot[idx_parent]
      
      #plot together gates that share the same set of parameters
      
      while(length(nodes_to_plot_parent) > 0){
        
        
        par_nodes <- lapply(nodes_to_plot_parent, function(x){
          
          g <- getGate(gs[[idx[1]]], x)
          
          if(class(g) %in% c("polygonGate")){
            try(colnames(g@boundaries), silent = TRUE)
          }else if(class(g) %in% c("ellipsoidGate")){
            try(colnames(g@cov), silent = TRUE)
          }else if(class(g) %in% c("rectangleGate")){
            try(names(g@min), silent = TRUE)
          }
          
        })
        
        same_par <- sapply(par_nodes, function(x){setequal(x, par_nodes[[1]])})
        
        count <- count + 1

        plot_args$xvar <- par_nodes[[1]][1]
        plot_args$yvar <- par_nodes[[1]][2]

        plist[[count]] <- plot_gs(df = df, 
                                   gs=gs, 
                                   sample=sample, 
                                   subset = parent, 
                                   gate = nodes_to_plot_parent[same_par], 
                                   plot_type = plot_type,
                                   plot_args = plot_args,
                                   options = options)
        
        all_children <- unlist(sapply(nodes_to_plot_parent[same_par], function(x){getChildren(gs[[1]], x)}))
        names(all_children) <- NULL
        
        child_nodes_int <- c(child_nodes_int, all_children)
        nodes_to_plot_parent <- setdiff(nodes_to_plot_parent, nodes_to_plot_parent[same_par])
        
      }
      
    }
    
    child_nodes <- child_nodes_int[child_nodes_int %in% selected_subsets]
    
  }
  return(plist)
}


plot_gate <- function(gate,
                     df = NULL,
                     gs,
                     sample = NULL,
                     spill = NULL,
                     metadata = NULL,
                     plot_type = "contour",
                     plot_args = list(),
                     options = list()){
  
  gate <- getGate(gs[[1]], gate)
  
  polygon <- get_gate_coordinates(gate)
  subset <- getParent(gs,  gate_name)
  plot_args[["xvar"]] <- names(polygon)[1]
  
  if(length(names(polygon))>1){plot_args$yvar <- names(polygon)[2]}
  
  if(is.null(sample)) sample <-  pData(gs)$name[1]

  df <- get_plot_data(df = df,
                      gs = gs, 
                      sample = sample,
                      subset = subset,
                      spill = spill, 
                      metadata = metadata)
  
  p <- plot_gs_data(df = df,
                    plot_type = plot_type,
                    plot_args = plot_args)
  
  p <- format_plot(p, options = options)
  
  p <- add_gate(p, gate)
  
  return(p)
}

get_gate_coordinates <- function(gate){
  
  polygon <- NULL
  
  if(class(gate) == "polygonGate" ){
    if(length(unique(colnames(gate@boundaries)))>1){
      polygon <- as.data.frame(gate@boundaries)
    }
  }else if(class(gate) == "rectangleGate"){
    
    polygon <- data.frame(x = c(gate@min[1], gate@max[1], gate@max[1], gate@min[1]),
                          y = c(gate@min[2], gate@min[2], gate@max[2], gate@max[2]))
    
    colnames(polygon) <- names(gate@min)
    
  }else if(class(gate) == "ellipsoidGate"){
    cov <- gate_int@cov
    mean <- gate_int@mean
    polygon <- ellipse_path(cov = cov, mean = mean)
  }else{
    warning("gate format not supported")
    break
  }
  
  return(polygon)
  
}

#' @import scales
compute_stats <- function(df = NULL,
                          gs = NULL,
                          spill = NULL,
                          transformation=NULL,
                          stat_function = "mean",
                          y_trans = identity_trans(),
                          apply_inverse = TRUE,
                          var_names = NULL
                          ){
  
  inverse <- NULL
  id.vars <- c("name", "subset")
  yvar <- setdiff(names(df), id.vars)
  custom_name <- NULL
  
  if(stat_function == "GeoMean"){
    y_trans = log_trans()
    apply_inverse = TRUE
    stat_function = "mean"
    custom_name <- "GeoMean"
    warning("Note that negative values will be discarded when computing the geometric mean")
  }
  
  if(stat_function == "median"){
    y_trans = identity_trans()
    apply_inverse = FALSE
  }
  
  if(is.null(var_names)){
    var_names <- yvar
    names(var_names) <- yvar
  }
  
  if(! stat_function %in% c("cell count", "percentage")){
    if(!is.null(yvar)){

      if(!is.null(y_trans)){
        transformation <- lapply(yvar, function(x){y_trans})
        names(transformation) <- yvar
      }
      
      
      for(i in 1:length(yvar)){
        df[[yvar[i]]] <- transformation[[yvar[i]]]$transform(df[[yvar[i]]])
      }
      
      
      df_melt <- melt(df, id.vars = id.vars, measure.vars = yvar)
      df_melt <- df_melt[is.finite(df_melt$value), ]
      
      
      stat.fun <- function(...){do.call(stat_function, args = list(...))}
      df_cast <- dcast(df_melt, 
                       formula = as.formula(paste(paste(id.vars, collapse = " + "), " ~ variable", sep ="")), 
                       fun.aggregate =  stat.fun, 
                       na.rm = TRUE)
      
      if(apply_inverse){
        for(i in 1:length(yvar)){
          df_cast[[yvar[i]]] <- transformation[[yvar[i]]]$inverse(df_cast[[yvar[i]]])
          if(transformation[[yvar[i]]]$name != "identity"){
            inverse <- "inverse"
          }
        }
      }
     
      
      for(i in 1:length(yvar)){
        idx <- which(names(df_cast) == yvar[i])
        trans_name <- NULL
        if(transformation[[yvar[i]]]$name != "identity"){
          trans_name <- transformation[[yvar[i]]]$name
        }
        if(is.null(custom_name)){
          names(df_cast)[idx] <- paste(stat_function, trans_name, inverse, var_names[[i]])
        }else{
          names(df_cast)[idx] <- paste(custom_name,  var_names[[i]])
        }
        
      }
    }
    
  }else{
    if(!is.null(gs)){
      variable <- switch(stat_function,
                         "cell count" = "Count",
                         "percentage" = "perc_parent")
      
      df_pop_stat <- as.data.frame(getPopStatsPlus(gs, spill = spill))
      df_pop_stat <- dplyr::rename(df_pop_stat, subset = Population)
      df_pop_stat[['perc_parent']] <- df_pop_stat$Count / df_pop_stat$ParentCount * 100
      df_cast <- df_pop_stat[c("name", "subset", variable)]

      df_cast <- df_cast[df_cast$name %in% unique(as.character(df$name)) & 
                           df_cast$subset %in% unique(as.character(df$subset)), ]
      
    }
    
    
    #df_cast <- df[df$name %in% sample & df$subset %in% subset, ]
    
    # idx <- which(as.character(df$name) %in% sample & as.character(df$subset) %in% subset)
    # print(idx)
    # if(length(idx)>0){
    #   df_cast <- df[idx, ]
    # }else{
    #   df_cast <- NULL
    # }

    
    # trans_name <- NULL
    # yvar <- switch(stat_function,
    #                "cell count" = "Count",
    #                "percentage" = "perc_parent")
    # id.vars <- c("name", "subset")
    
    
  }
  
  return(df_cast)
  
}

plot_stat <- function(df = NULL,
                      gs,
                      sample, 
                      subset,
                      yvar,
                      type = "bar",
                      metadata = NULL,
                      color_var = "subset", 
                      axis_labels = NULL,
                      transformation = NULL,
                      spill = NULL,
                      default_trans = identity_trans(),
                      scale_values = FALSE,
                      free_y_scale = TRUE,
                      max_scale = 0,
                      facet_vars = NULL,
                      group_var = "subset",
                      expand_factor = 0.2,
                      stat_function = "mean",
                      show.legend = TRUE,
                      y_trans = NULL,
                      strip.text.y.angle = 0,
                      theme_name = "theme_gray",
                      Rowv = TRUE,
                      Colv = TRUE
){
  
  theme_function <- function(...){
    do.call(theme_name, list(...))
  }
                    
  if(!is.logical(scale_values)){
    scale_values <- FALSE
  }
  
  if(!is.logical(free_y_scale)){
    free_y_scale <- TRUE
  }
  
  if(!is.numeric(max_scale)){
    max_scale <- 0
  }
  
  extra_facet <- NULL
  if(!is.null(facet_vars)){
     extra_facet <- facet_vars[facet_vars %in% "cluster"]
     if(length(extra_facet) == 0){
       extra_facet <- NULL
     }
  }
  
  #if(log10_trans){
  #  trans <- log10_trans()
  #}else{
  #  trans <- identity_trans()
  #}
  
  if(! stat_function %in% c("cell count", "percentage")){
    if(!is.null(yvar)){
  
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
      }
  
      id.vars <- unique(c("name", "subset", extra_facet))
      
      df_melt <- melt(df, id.vars = id.vars, measure.vars = yvar)
      df_melt <- df_melt[is.finite(df_melt$value), ]
      
      
      stat.fun <- function(...){do.call(stat_function, args = list(...))}
      df_cast <- dcast(df_melt, 
                       formula = as.formula(paste(paste(id.vars, collapse = " + "), " ~ variable", sep ="")), 
                       fun.aggregate =  stat.fun, 
                       na.rm = TRUE)
      
      
      for(i in 1:length(yvar)){
        print(yvar[i])
        print(transformation[[yvar[i]]])
        df_cast[[yvar[i]]] <- transformation[[yvar[i]]]$inverse(df_cast[[yvar[i]]])
        #  #df_cast[[yvar[i]]] <- trans$inverse(df_cast[[yvar[i]]])
      }
    }
    
  }else{
    
    if(!is.null(extra_facet)){

        id.vars <- unique(c("name", "subset", extra_facet))
        
        df_data <- get_data_gs(gs = gs,
                               sample = sample,
                               subset = subset,
                               spill = spill)
        
        var_levels <- unique(df_data[[extra_facet]])
        
        df <- NULL
        for(level in var_levels){
          df_gate <- data.frame(x  = c(level-0.5, level+0.5))
          names(df_gate) <- extra_facet
          filter <- rectangleGate(filterId = "filter", df_gate )
          print(filter)
          df_filter <- getPopStatsPlus(gs, spill = spill, filter = filter)
          df_filter[[extra_facet]] <- level
          df <- rbind(df, df_filter)
        }
      
    }else{
      id.vars <- c("name", "subset")
      df <- getPopStatsPlus(gs, spill = spill, filter = NULL)
    }
    
    df <- dplyr::rename(df, subset = Population)
    df[['perc_parent']] <- df$Count / df$ParentCount * 100

    print(df)
    
    idx <- which(as.character(df$name) %in% sample & as.character(df$subset) %in% subset)
    print(idx)
    if(length(idx)>0){
      df_cast <- df[idx, ]
    }else{
      df_cast <- NULL
    }
    
    
    # if(length(idx)>0){
    #   df_cast <- df[idx, ]
    # }else{
    #   df_cast <- NULL
    #   return(NULL)
    # }
    
    trans_name <- NULL
    yvar <- switch(stat_function,
                   "cell count" = "Count",
                   "percentage" = "perc_parent")
    #
    
  }
  
  
  
  
  df_scale <- df_cast
  if(scale_values){
    df_scale[-which(names(df_cast) %in% id.vars)] <- scale(df_cast[-which(names(df_cast) %in% id.vars)])
  }

  print(df_cast)
  print(df_scale)
  print(id.vars)
  print(yvar)
  
  df_melt2 <- data.table::melt(df_scale, id.vars = id.vars, measure.vars = yvar )
  
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

  df_melt2 <- df_melt2[df_melt2$variable %in% yvar, ]

  ylim <- NULL

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
    #df_melt2$value[df_melt2$value > max_scale] <- max_scale
    #df_melt2$value[df_melt2$value < -max_scale] <- -max_scale
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
    for(i in 1:length(yvar)){
      if(yvar[i] %in% names(axis_labels)){
        df_melt2$variable[df_melt2$variable == yvar[i]] <- axis_labels[[yvar[i]]]
      }
    }
  }
  
  print(names(df_melt2))
  
  if(type == "heatmap"){
    
    df <- as.data.frame(df_melt2)
    # s <- group_var
    # df <- df[ , c("variable", s, "value")]
    # print(df)
    # print(class(df_melt2))
    
    df_cast2 <- data.table::dcast(df[, c("variable", c(group_var, extra_facet), "value")],
                      formula = as.formula(paste("variable ~", paste(c(group_var, extra_facet), collapse = " + "))),
                      fun.aggregate = mean )
    
    # df_cast2 <- data.table::dcast(df_melt2[, c("variable", "subset", "value")],
    #                               formula = as.formula(paste("variable ~", paste("subset", collapse = " + "))), 
    #                               fun.aggregate = mean )
    
    row_labels <- df_cast2$variable
    p <- heatmaply(df_cast2[-1], 
                   labRow = row_labels,
                   margins = c(80, 80, 50, 0),
                   Rowv = Rowv,
                   Colv = Colv
                   )
    return(list(plot = p, data = df_melt2))
  }
  
  
  if(type == "tile"){
    
    p <- ggplot(df_melt2, aes_string( x = group_var ))
    p <- p + geom_tile(mapping = aes_string(y = "variable", fill = "value"),
                       show.legend = show.legend)
    
    
    p <- p + scale_fill_viridis(limits = ylim) +
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
                  show.legend = show.legend) 
      # geom_signif(comparisons = list(c(1,2)),
      #             test="t.test",
      #             test.args = list("paired"=FALSE))
    
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
  
  
  p <- p +
    theme_function() +
    theme( axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
           strip.text.y = element_text(angle = strip.text.y.angle)
    )

  if(!is.null(trans_name)){
    trans_name_plot <- trans_name
    if(length(trans_name)>1){
      trans_name_plot <- "defined by variable"
    }
    p <- p + ggtitle(paste("statistic : ", stat_function, " / transform : ", trans_name_plot, sep = ""))
  }else{
    p <- p + ggtitle(paste("statistic : ", stat_function))
  }

  return(list(plot = p, data = df_melt2))
  
}


####################################################################################################
# Dimensionality Reduction 

#' Perform dimensionality reduction
#' @description  Perform dimensionality reduction
#' @param df a data.frame with only numeric variables.
#' @param yvar names of df's variables used to perform dimensionality reduction
#' @param Ncells Maximum number of cells without any NA values sampled from df. 
#' If NULL, all cells without any NA values are used
#' @param transformation  Named list of \code{trans} objects. 
#' List names should correspond to variable names.
#' @param y_trans default \code{trans} object to be used if \code{transformation} is NULL.
#' @param perplexity t-SNE perplexity parameter
#' @param dims Number of dimensions
#' @param method Name of the method used. Either "tSNE" or "umap"
#' @return a data.frame with additionnal columns : 
#' "tSNE1" and "tSNE2" for method 'tSNE', "UMAP1" and "UMAP2" for method 'umap'
#' @import Rtsne
#' @import umap
#' @import scales
dim_reduction <- function(df,
                          yvar,
                          Ncells = NULL,
                          transformation = NULL,
                          y_trans = log10_trans(),
                          perplexity = 50,
                          dims = 2,
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
  
  if(!is.null(Ncells) & is.numeric(Ncells)){
    Ncells_used <- min(dim(df_trans)[1], Ncells)
    idx_cells <- sample(1:dim(df_trans)[1], Ncells_used, replace = FALSE)
  }else{
    Ncells_used <- dim(df_trans)[1]
    idx_cells <- 1:dim(df_trans)[1]
  }
  
  idx_cells_kept <- idx_cells_kept[idx_cells]
  
  message(paste("Running ", method, " with ", Ncells_used, " cells and ",  length(yvar), " parameters", sep = ""))
  
  if(Ncells_used > 3000){
    message("This may take a while... Try with less cells.")
  }

  if(method == "tSNE"){
    tSNE <- Rtsne(df_trans[ idx_cells , yvar], perplexity = perplexity, dims = dims)
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

#' Identify clusters
#' @description  Identify clusters
#' @param df a data.frame with only numeric variables.
#' @param yvar names of df's variables used to find clusters
#' @param transformation  Named list of \code{trans} objects. 
#' List names should correspond to variable names.
#' @param y_trans default \code{trans} object to be used if \code{transformation} is NULL.
#' @param dc ClusterX dc parameter
#' @param alpha ClusterX alpha parameter
#' @param method Name of the method used. Either "ClusterX" or "Rphenograph".
#' @return a data.frame with the additionnal column "cluster"
#' @import ClusterX
#' @import Rphenograph
#' @import igraph
#' @import scales
get_cluster <- function(df,
                        yvar,
                        transformation = NULL,
                        y_trans = identity_trans(),
                        dc=3, 
                        alpha = 0.001,
                        k = 100,
                        method = "ClusterX"){
         
  idx_cells_kept <- 1:dim(df)[1]
  
  if(!is.null(y_trans)){
    transformation <- lapply(yvar, function(x){y_trans})
    names(transformation) <- yvar
  }
  
  trans_name <-  unique(unlist(sapply(transformation[yvar], function(tf){tf$name})))
  
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
  
  
  if(method == "Rphenograph"){
    #return(list(df = df_filter, keep = idx_cells_kept))
    message(paste("Clustering ", dim(df_trans)[1], " cells using 'Rphenograph' on ",  length(yvar), " parameters", sep = ""))
    Rphenograph_out <- Rphenograph(df_trans[ , yvar], k = k)
    df_filter$cluster <- igraph::membership(Rphenograph_out[[2]])
  }else if(method == "ClusterX"){
    message(paste("Clustering ", dim(df_trans)[1], " cells using 'CluserX' on ",  length(yvar), " parameters", sep = ""))
    DC <- ClusterX(df_trans[ , yvar], dc = dc, alpha = alpha)
    df_filter$cluster <- DC$cluster
  }
 
  return(list(df = df_filter, keep = idx_cells_kept))
  
}


####################################################################################################
# Build FlowSet

#' Build a flowSet from a data.frame
#' @description Build a flowSet from a data.frame
#' @param df data.frame with a column \code{sample_col} containing sample names and
#' columns with flow variables.
#' @param origin flowSet from which to retrieve flowFrames description and parameters slots. 
#' Ignored if NULL.
#' @param chanel_col Names of df's columns to be used as flow variables 
#' @param sample_col Name of df's column containing sample names
#' @return a flowSet
#' @import flowCore
build_flowset_from_df <- function(df,
                                  origin = NULL,
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
      
      if(!is.null(origin)){
        
        idx <- match(sample, pData(origin$flow_set)$name)
        
        if(!is.na(idx)){
          
          par <- parameters(origin$flow_set[[idx]])
          #par <- origin$par[[idx]]
          
          par@data$name <- as.character(par@data$name)
          par@data$desc <- as.character(par@data$desc)
          
          desc <- description(origin$flow_set[[idx]])
          #desc <- origin$desc[[idx]]
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
    if(!is.null(origin)){
      pdata <- data.frame(pData(origin$flow_set))
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
