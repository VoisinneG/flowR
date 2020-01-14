utils::globalVariables(c("df", "xvar", "yvar", "x", "y"))

####################################################################################################
# Parse workspace and gates from xml files (.wsp flowJO workspace files)
####################################################################################################

#' Return the name and ID of a SampleNode section
#' @param x a xml document
#' @import xml2
parseSampleNodes <- function(x){
  name <- xml_text(xml_find_all(x, ".//@name"))[1]
  sampleID <- xml_integer(xml_find_all(x, ".//@sampleID"))[1]
  return(list("name" = name, "sampleID" = sampleID))
}

#' Return the name and IDs of samples in a  GroupNode section
#' @param x a xml document
#' @import xml2
parseGroupNodes <- function(x){
  name <- xml_text(xml_find_all(x, ".//@name"))[1]
  sampleID <- xml_integer(xml_find_all(xml_find_all(x, ".//SampleRefs"), ".//@sampleID"))
  return(list("name" = name, "sampleID" = sampleID))
}

#' find all parent gates of a given Gate section, recursively
#' @param x a xml document
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

#' Extract all gates from a flowJO workspace
#' @param ws_path path to the workspace
#' @param group Names of the sample groups to be considered
#' @import xml2
#' @importFrom flowCore rectangleGate polygonGate
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
    group_selected <- unlist(group_info$name)[1]
  }else{
    group_selected <- group
  }

  
  # get Gates for first sample in group
  sampleID <- group_info$sampleID[[which(unlist(group_info$name) == group_selected)]][1]
  if(length(sampleID) > 0){
    SampleNode <- SampleNodes[ which(unlist(sample_info$sampleID) == sampleID) ]
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
      g <- flowCore::rectangleGate(.gate = boundaries, filterId = basename(name))
      gate_list[[name]] <- list(gate = g, parent = parent)
    }else if(gates[[i]]$type == "PolygonGate"){
      polygon <- gates[[i]]$polygon
      g <- flowCore::polygonGate(.gate = polygon, filterId = basename(name) )
      gate_list[[name]] <- list(gate = g, parent = parent)
    }else{
      warning(paste("gate type", gates[[i]]$type, "not supported"))
      #g <- NULL
      #gate_list[[name_long]] <- list(gate = g, parent = parent)
    }
    
    
  } 
  return(gate_list)
  
}

#' Return relevant info from a Gate section
#' @param x a xml document
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


####################################################################################################
# Transformations
####################################################################################################

#' @importFrom flowWorkspace flowJoTrans flow_trans
flowJo_biexp_inverse_trans <- function (..., n = 6, equal.space = FALSE){
  trans <- flowWorkspace::flowJoTrans(..., inverse = TRUE)
  inv <- flowWorkspace::flowJoTrans(...)
  flow_trans(name = "flowJo_biexp_inverse", trans.fun = trans, inverse.fun = inv, 
             n = n, equal.space = equal.space)
}

#' Scaled hyperbolic arc-sine function
#' @param b scale
#' @param inverse use inverse function?
asinh_transform <- function(b=5, inverse = FALSE){
  if(inverse){
    function(x){b*sinh(x)} 
  }else{
    function(x){asinh(x/b)} 
  }
}

#' Scaled hyperbolic arc-sine transformation
#' @param ... arguments passed to \code{asinh_transform()}
#' @param n desired number of breaks (see \code{flow_trans()})
#' @param equal.space whether breaks at equal-spaced intervals (see \code{flow_trans()})
#' @importFrom flowWorkspace flow_trans
asinh_trans <- function (..., n = 6, equal.space = FALSE){
  trans <- asinh_transform(...)
  inv <- asinh_transform(..., inverse = TRUE)
  flow_trans(name = "asinh", trans.fun = trans, inverse.fun = inv, 
             n = n, equal.space = equal.space)
}

####################################################################################################
# Gating
####################################################################################################

#' Return coordinates of flowCore gate.
#' @param gate a flowCore gate either from class "polygonGate", "rectangleGate" or "ellipsoidGate"
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
    cov <- gate@cov
    mean <- gate@mean
    polygon <- ellipse_path(cov = cov, mean = mean)
  }else{
    warning("gate format not supported")
  }
  
  return(polygon)
  
}

#' Return coordinates of points along an ellipse defined by its covariance matrix and its center
#' @param cov covariance matrix
#' @param mean coordinates of the center of the ellipse
#' @param n number of points to return along the ellipse
#' @return a data.frame with point cordinates
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

#' Return all descendants from a set of nodes in a tree
#' @param named_list a list representing a tree. It must be named according to tree node names and 
#' each of its element must have a field 'parent' containing the name of its parent node.
#' @param names Names of the nodes for which all descendants must be returned
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

#' Return all ancestors from a set of nodes in a tree
#' @param named_list a list representing a tree. It must be named according to tree node names and 
#' each of its element must have a field 'parent' containing the name of its parent node.
#' @param names Names of the nodes for which all ancestors must be returned
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

#' Build a gating hierarchy from a GatingSet
#' @param gs a GatingSet
#' @return a named list representing the gating hierarchy. 
#' Each element has a field 'gate' with a flowCore filter object 
#' and a field 'parent' with the name of its parent gate.
#' @importFrom flowWorkspace gs_get_pop_paths gh_pop_get_gate gs_pop_get_parent
get_gates_from_gs <- function(gs){
  
  nodes <- flowWorkspace::gs_get_pop_paths(gs)
  gates <- list()
  
  for(node in setdiff(nodes, "root")){
    g <- flowWorkspace::gh_pop_get_gate(gs[[1]], node)
    #print(names(g@parameters))
    parent <- flowWorkspace::gs_pop_get_parent(gs[[1]], node)
    gates[[node]] <- list(gate = g, parent = parent)
  } 
  
  return(gates)
}

#' Add gates from a gating hierarchy to a GatingSet
#' @param gs a GatingSet
#' @param gates a named list representing the gating hierarchy. 
#' Each element must have a field 'gate' with a flowCore filter object 
#' and a field 'parent' with the name of its parent gate.
#' @importFrom flowWorkspace gs_get_pop_paths gs_pop_add recompute colnames
add_gates_flowCore <- function(gs, gates){
  
  new_gates_name <- setdiff(names(gates), flowWorkspace::gs_get_pop_paths(gs))
  gates <- gates[new_gates_name]
  
  ngates <- length(gates)

  
  if(ngates>0){
    
    idx <- 1:ngates
    
    while(length(idx)>0){
      
      i_added <- NULL
      
      for(i in 1:length(idx)){
        
        g <- gates[[idx[i]]]
        
        if(g$parent %in% union(flowWorkspace::gs_get_pop_paths(gs), "root") ){
          
          if( !is.null(names(g$gate@parameters)) & 
              length( setdiff( names(g$gate@parameters), flowWorkspace::colnames(gs)) ) == 0 ){
            
            flowWorkspace::gs_pop_add(gs,
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
    flowWorkspace::recompute(gs)
  }
  
  return(gs)
}


#' Transform gates coordinates and modify names of parameters.
#' @param gates a named list representing the gating hierarchy.
#' @param transformation A list of trans objects. 
#' Each element must be named after a parameter and contain the transfomation to apply for this parameter.
#' @param pattern pattern to be replaced in the names of gate coordinates
#' @param replacement Character string that is to replace 'pattern' in in the names of gate coordinates
#' @param time_step value of the time step used to transform gates with the 'Time' parameter. Ignored if NULL.
#' @importFrom flowCore polygonGate rectangleGate
transform_gates <- function(gates,
                            transformation = NULL, 
                            pattern = "[\\<|\\>]",
                            replacement = "",
                            time_step = NULL ){
  
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
        
        for(j in 1:length(colnames(polygon))){
          if(colnames(polygon)[j] %in% names(transformation)){
            polygon[,j] <- transformation[[colnames(polygon)[j]]]$transform(polygon[,j])
          }
          if(colnames(polygon)[j] == "Time"){
            if(!is.null(time_step)){
              polygon[,j] <- polygon[,j]/time_step
            }
          }
        }
        
        trans_gate <- flowCore::polygonGate(.gate = polygon, filterId=g$gate@filterId)
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
        
        trans_gate <- flowCore::rectangleGate(.gate = polygon, filterId=g$gate@filterId)
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
        trans_gate <- flowCore::polygonGate(.gate = polygon, filterId=g$gate@filterId)
      }
      
      gates[[i]] <- list(gate = trans_gate, parent = g$parent)
    }
    
  }
  
  return(gates)
  
}


####################################################################################################
# Getting data
####################################################################################################

#' Return statistics for all subsets ans samples in a GatingSet
#' @param gs a GatingSet
#' @param spill spillover matrix. If NULL, uncompensated data is used for gating
#' @param filter a filter object applied before computing statistics. Ignored if NULL.
#' @importFrom flowCore Subset
#' @importFrom flowWorkspace GatingSet gs_pop_get_count_fast
getPopStatsPlus <- function(gs, spill = NULL, filter = NULL){
  
  fs <- gs@data
  gates <- get_gates_from_gs(gs) 
  if(!is.null(filter)){
    fs <- flowCore::Subset(fs, filter)
  }
  
  if(!is.null(spill)){
    fs <- compensate(fs, spill)
  }
  
  gs_comp <- flowWorkspace::GatingSet(fs)
  gs_comp <- add_gates_flowCore(gs_comp, gates)
  df <- flowWorkspace::gs_pop_get_count_fast(gs_comp)
  
  df$name <- sapply(df$name, function(x){strsplit(x, split = "_[0-9]+$")[[1]][1]})
  df_root <- data.frame(name = pData(fs)$name)
  df_root$name <- pData(fs)$name
  df_root$Population <- "root"
  df_root$Count <- sapply(1:length(fs), function(x){dim(fs[[x]]@exprs)[1]})
  df_root$Parent <- NA
  df_root$ParentCount <- NA
  
  df_merge <- rbind(df, df_root)
  df_merge
}

#' Return data from a GatingSet
#' @param gs a GatingSet
#' @param sample Names of samples from the GatingSet 
#' (as returned by \code{pData(gs)$name})
#' @param subset Names of subsets from the GatingSet 
#' (as returned by \code{flowWorkspace::gs_get_pop_paths(gs)})
#' @param Ncells number of cells to sample from the GatingSet
#' @param spill spillover matrix. If NULL, uncompensated data is used for gating. 
#' Uncompensated data is returned if parameter 'return_comp_data' is TRUE.
#' @param return_comp_data logical. Should compensated data be returned ?
#' @param updateProgress function used in shiny to update a progress bar
#' @return a data.frame
#' @importFrom flowWorkspace colnames GatingSet gh_pop_get_indices pData
#' @importFrom flowCore compensate
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
    #spill <- spill[row.names(spill) %in% flowWorkspace::colnames(gs), 
    #               colnames(spill) %in% flowWorkspace::colnames(gs)]
    gates <- get_gates_from_gs(gs)
    fs <- gs@data[idx]
    #spill_list <- lapply(1:length(idx), function(x){return(spill)})
    #names(spill_list) <- flowCore::pData(fs)$name
    fs <- flowCore::compensate(fs, spill[idx])
    gs_comp <- flowWorkspace::GatingSet(fs)
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
          idx_subset <- flowWorkspace::gh_pop_get_indices(gs_comp[[idx_comp]], subset[k])
        }else{
          idx_subset <- flowWorkspace::gh_pop_get_indices(gs[[idx[i]]], subset[k])
        }
      }
      
      if(return_comp_data & !is.null(spill) ){
        ff <- gs_comp@data[[idx_comp]]
      }else{
        ff <- gs@data[[idx[i]]]
      }
      

      df_int <- as.data.frame(ff@exprs)
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
#' @param metadata a data.frame containing metadata associated to samples. 
#' Must have a column \code{name} used for mapping.
#' @return a data.frame with additional columns
add_columns_from_metadata <- function(df,
                                      metadata
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

#' @importFrom reshape2 melt dcast
#' @importFrom scales identity_trans log_trans
#' @importFrom stats as.formula
#' @importFrom dplyr rename
compute_stats <- function(df = NULL,
                          gs = NULL,
                          spill = NULL,
                          transformation=NULL,
                          stat_function = "mean",
                          y_trans = identity_trans(),
                          apply_inverse = TRUE,
                          yvar = NULL,
                          var_names = NULL,
                          id.vars = c("name", "subset")
){
  
  inverse <- NULL
  
  if(is.null(yvar)){
    yvar <- setdiff(names(df), id.vars)
  }
  
  custom_name <- NULL
  
  if(stat_function == "GeoMean"){
    y_trans = scales::log_trans()
    apply_inverse = TRUE
    stat_function = "mean"
    custom_name <- "GeoMean"
    warning("Note that negative values will be discarded when computing the geometric mean")
  }
  
  if(stat_function == "median"){
    y_trans = scales::identity_trans()
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
      
      
      df_melt <- reshape2::melt(df, id.vars = id.vars, measure.vars = yvar)
      df_melt <- df_melt[is.finite(df_melt$value), ]
      
      
      stat.fun <- function(...){do.call(stat_function, args = list(...))}
      df_cast <- reshape2::dcast(df_melt, 
                                 formula = stats::as.formula(paste(paste(id.vars, collapse = " + "), " ~ variable", sep ="")), 
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
      
      
      # for(i in 1:length(yvar)){
      #   idx <- which(names(df_cast) == yvar[i])
      #   trans_name <- NULL
      #   if(transformation[[yvar[i]]]$name != "identity"){
      #     trans_name <- transformation[[yvar[i]]]$name
      #   }
      #   if(is.null(custom_name)){
      #     names(df_cast)[idx] <- paste(stat_function, trans_name, inverse, var_names[[i]])
      #   }else{
      #     names(df_cast)[idx] <- paste(custom_name,  var_names[[i]])
      #   }
      #   
      # }
      
      
    }
    
  }else{
    if(!is.null(gs)){
      variable <- switch(stat_function,
                         "cell count" = "Count",
                         "percentage" = "perc_parent")
      
      df_pop_stat <- as.data.frame(getPopStatsPlus(gs, spill = spill))
      df_pop_stat <- dplyr::rename(df_pop_stat, subset = "Population")
      df_pop_stat[['perc_parent']] <- df_pop_stat$Count / df_pop_stat$ParentCount * 100
      df_cast <- df_pop_stat[c("name", "subset", variable)]
      
      df_cast <- df_cast[df_cast$name %in% unique(as.character(df$name)) & 
                           df_cast$subset %in% unique(as.character(df$subset)), ]
      
    }
  }
  
  return(df_cast)
  
}

#' Return data used for plotting a GatingSet
#' @param gs a GatingSet
#' @param df data.frame with columns \code{name} and \code{subset} 
#' containing sample and subset names respectively and
#' columns with plot variables. 
#' Ignored if \code{NULL}. 
#' Otherwise supersedes parameters 'gs', 'sample', 'spill', 'metadata'.
#' @param sample Names of samples from the GatingSet 
#' (as returned by \code{pData(gs)$name})
#' @param subset Names of subsets from the GatingSet
#' (as returned by \code{gs_get_pop_paths(gs)})
#' @param Ncells number of cells to sample from the GatingSet
#' @param spill spillover matrix. If NULL, uncompensated data is returned and used for gating.
#' @param metadata a data.frame containing metadata associated to samples.
#' Must have a column \code{name} used for mapping.
#' @return a data.frame
get_plot_data <- function(gs,
                          df=NULL, 
                          sample,
                          subset,
                          Ncells = NULL,
                          spill = NULL,
                          metadata = NULL){
  
  
  if(is.null(df)){
    df <- get_data_gs(gs = gs,
                      sample = sample,
                      subset = subset,
                      spill = spill,
                      Ncells = Ncells)
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

####################################################################################################
# Plotting
####################################################################################################

#' Generates a plot from data
#' @param df data.frame with plot data (as returned by \code{get_plot_data})
#' @param plot_type Name of the type of plot to generate. 
#' Available types are 'hexagonal', 'histogram', 'dots', 'contour' (for single cell data) and 
#' 'heatmap', 'bar', 'tile', 'pca' (for aggregated data). 
#' The plot function corresponding to a given plot type must have the same name 
#' as the plot type with the preffix 'plot_' (for instance 'plot_hexagonal()')
#' @param plot_args list of arguments passed to the plot function
#' @return a plot (plot class depends on the plot function)
call_plot_function <- function(df,
                         plot_type,
                         plot_args = list()
                         ){
  
  p <- do.call(paste("plot", plot_type, sep="_"), 
               list(args = c(list(df=df), plot_args)))

  return(p)
}



####################################################################################################
# Generate plot for data with single cell resolution (plotGatingSetInput_module)

#' Generates a hexagonal heatmap of 2d bin counts (see ggplot2::geom_hex)
#' @param args list of arguments. 
#' Mandatory arguments : a data.frame 'df', x and y plot variables 'xvar' and 'yvar'.
#' Other arguments can include :
#' 'bins' : numeric, number of bins, passed to 'ggplot2::geom_hex()'
#' 'use_log10_count' : logical, transform bin counts using log10
#' 'option' : name of the viridis palette
#' @import ggplot2
#' @importFrom viridis scale_fill_viridis
plot_hexagonal <- function(args = list()){
  
  plot_type <- "hexagonal"
  bins <- 100
  use_log10_count <- TRUE
  option <- "viridis"
  
  if(length(unlist(args[c("xvar", "yvar")])) != 2 ){
    return(NULL)
  }
  
  for(var in names(args)){
    assign(var, args[[var]])
  }
  
  p <- ggplot(df,
              aes_(x = as.name( xvar ), 
                   y = as.name( yvar ) ) ) +
    geom_hex(bins = bins)
  
  if(use_log10_count){
    p <- p + scale_fill_viridis(trans = log10_trans(), option = option)
  }else{
    p <- p + scale_fill_viridis(option = option)
  }
  
  p
}

#' Generates an histogram or a density plot
#' @param args list of arguments. 
#' Mandatory arguments : a data.frame 'df', x plot variable 'xvar'.
#' Other arguments can include :
#' 'color_var' : variable used for the aesthetic 'color'
#' 'group_var' : variable used for the aesthetic 'group'
#' 'smooth' : logical. Should a density plot be generated?
#' 'ridges': logical. Ignored if 'smooth' is FALSE. Shift different density plots according to 'yridges_var'
#' 'yridges_var' : variable used for the aesthetic 'y' in 'geom_density_ridges'. Ignored if 'ridges' is FALSE. 
#' 'norm_density' : logical. Set maximal y value to 1?
#' 'bins' : number of bins.
#' (If 'smooth' is TRUE, the inverse of 'bins' is used as the value for the bandwidth parameter 'bw')
#' 'alpha' : transparency (between 0 and 1)
#' @import ggplot2
#' @importFrom ggridges geom_density_ridges
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
  
  if(is.null(args["xvar"])){
    return(NULL)
  }
  
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
    if(color_var %in% names(df)){
      color_var <- as.name(color_var)
    }
  }
  
  if(!is.null(group_var)){
    if(group_var == "none"){
      group_var <- NULL
    }
  }
  
  if(!is.null(group_var)){
    if(group_var %in% names(df)){
      group_var <- as.name(group_var)
    }
  }
  
  if(!is.null(yridges_var)){
    if(yridges_var %in% names(df)){
      yridges_var <- as.name(yridges_var)
    }
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
                            boundary = 0
                            ) 
  }
  
  return(p)
  
}

#' Generates a dot plot
#' @param args list of arguments. 
#' Mandatory arguments : a data.frame 'df', x and y plot variable 'xvar' and 'yvar'.
#' Other arguments can include :
#' 'color_var' : variable used for the aesthetic 'color'
#' 'group_var' : variable used for the aesthetic 'group'
#' 'alpha' : dot transparency (between 0 and 1)
#' 'size' : dot size
#' 'show_label' : logical; add labels for each group (as defined by 'id.vars')
#' 'id.vars' : variable defining groups for which a label should be displayed 
#' (superseded by 'color_var' and 'group_var')
#' @import ggplot2
#' @importFrom ggrepel geom_label_repel
plot_dots <-function(args = list()){
  
  plot_type <- "dots"
  id.vars <- "subset"
  show_label <- FALSE
  color_var <- NULL
  group_var <- NULL
  alpha <- 0.1
  size <- 0.1
  
  if(length(unlist(args[c("xvar", "yvar")])) != 2 ){
    return(NULL)
  }
  
  for(var in names(args)){
    assign(var, args[[var]])
  }
  
  if(!is.null(color_var)){
    if(color_var == "none"){
      color_var <- NULL
    }else{
      if(color_var %in% names(df)){
        id.vars <- color_var
      }
    }
  }
  
  if(!is.null(color_var)){
    if(color_var %in% names(df)){
      color_var <- as.name(color_var)
    }
  }
  
  if(!is.null(group_var)){
    if(group_var == "none"){
      group_var <- NULL
    }else{
      if(group_var %in% names(df)){
        id.vars <- group_var
      }
    }
  }
  
  if(!is.null(group_var)){
    if(group_var %in% names(df)){
      group_var <- as.name(group_var)
    }
  }
  p <- ggplot(df,
              aes_string(x = as.name( xvar ), 
                         y = as.name( yvar ),
                         colour = color_var,
                         group = group_var)) + 
    geom_point(alpha = alpha, 
               size = size)
  if(show_label){
    df_stat <- compute_stats(df = df,
                             stat_function = "median",
                             yvar = c(xvar, yvar),
                             id.vars = id.vars)
    
    p <- p + geom_label_repel(mapping = aes_string(x = as.name( xvar ), 
                                                  y = as.name( yvar ),
                                                  color = id.vars,
                                                  label = id.vars), 
                             data = df_stat,
                             fill = "white")
  }
  return(p)
  
}

#' Generates a contour plot
#' @param args list of arguments. 
#' Mandatory arguments : a data.frame 'df', x and y plot variable 'xvar' and 'yvar'.
#' Other arguments can include :
#' 'color_var' : variable used for the aesthetic 'color'
#' 'group_var' : variable used for the aesthetic 'group'
#' 'show_outliers' : logical; Display all cells in the background
#' 'fill' : fill parameter for contour plot (passed to 'ggplot2::stat_density_2d()')
#' 'bins' : number of grid points in each direction. (passed as parameter 'n' to 'ggplot2::geom_density_2d()')
#' 'alpha' : contour line transparency (between 0 and 1). If 'show_outliers' is TRUE, used to set outliers dot transparency.
#' 'size' : contour line size. If 'show_outliers' is TRUE, used to set outliers dot size.
#' @import ggplot2
plot_contour <-function(args = list()){
  
  plot_type <- "contour"

  color_var <- NULL
  group_var <- NULL
  
  bins <- 30
  alpha <- 0.75
  size <- 0.2
  show_outliers <- FALSE
  
  if(length(unlist(args[c("xvar", "yvar")])) != 2 ){
    return(NULL)
  }
  
  for(var in names(args)){
    assign(var, args[[var]])
  }
  
  if(!is.null(color_var)){
    if(color_var == "none" ){
      color_var <- NULL
    }
  }
  
  if(!is.null(color_var)){
    if(color_var %in% names(df)){
      color_var <- as.name(color_var)
    }
  }
  
  if(!is.null(group_var)){
    if(group_var == "none"){
      group_var <- NULL
    }
  }
  
  if(!is.null(group_var)){
    if(color_var %in% names(df)){
      group_var <- as.name(group_var)
    }
  }
  
  
  p <- ggplot(df,
              aes_string(x = as.name( xvar ),
                         y = as.name( yvar ),
                         colour = color_var,
                         group = group_var))


  
  if(show_outliers){
    p <- p + geom_point(size = size, alpha = alpha)
    size <- 0.1
    alpha <- 1
  }
  
  
  
   if(!is.null(color_var)){
    
     if(show_outliers){
       p <- p + stat_density2d(aes_string(colour = color_var, group = group_var),
                               fill = "white",
                               size=0,
                               geom="polygon",
                               n=bins
       )
     }
     
    p <- p + geom_density2d(aes_string(colour = color_var, group = group_var),
                            size=size,
                            alpha= alpha,
                            n=bins
    )
      
   }else{
     if(show_outliers){
       p <- p + stat_density2d(aes_string(group = group_var),
                               fill = "white",
                               size=0,
                               geom="polygon",
                               n=bins
       )
     }
     
     p <- p + geom_density2d(aes_string(group = group_var),
                             color = "black",
                             size=size,
                             alpha= alpha,
                             n=bins
     )
    
  }

  return(p)
}

####################################################################################################
# Generate plot for aggregated data (plotStatInput_module)
####################################################################################################

#' Generates a heatmap
#' @param args list of arguments. 
#' Mandatory argument : a data.frame or a matrix 'df'
#' Other arguments can include :
#' 'stat_var' : names of numeric columns to include in the heatmap
#' 'group_var' : name of the column used to order x coordinates
#' 'cluster_y' : logical; cluster y axis variables
#' 'cluster_x' : logical; cluster x axis variables
#' 'show.legend' : logical; show legend
#' 'option' : name of the viridis palette
#' @importFrom viridis viridis
#' @importFrom pheatmap pheatmap
plot_heatmap <-function(args = list()){
  
  plot_type <- "heatmap"
  stat_var <- NULL
  annotation_vars <- NULL
  group_var <- NULL
  cluster_y <- FALSE
  cluster_x <- FALSE
  show.legend <- TRUE
  option <- "viridis"
  
  if(is.null(args["df"])){
    return(NULL)
  }
  
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

  
  if(is.null(annotation_vars)){
    annotation_vars <- c("name", "subset")
  }
  idx <- which(names(df) %in% annotation_vars)
  idx_annot <- idx
  
  df_stat <- df[-idx]
  
  if(!is.null(stat_var)){
    df_stat <- df_stat[which(names(df_stat) %in% stat_var)]
  }else{
    df_stat <- df_stat[which(sapply(df_stat, is.numeric))]
  }
  
  annotation <- df[idx_annot]
  
  df_plot <- t(df_stat)
  colnames(df_plot) <- 1:dim(df_plot)[2]
  
  if(dim(df_plot)[1]<2){
    cluster_y <- FALSE
  }
  if(dim(df_plot)[2]<2){
    cluster_x <- FALSE
  }
  
  p <- pheatmap(df_plot, show_colnames = FALSE, 
                color = viridis(16, option = option),
                labels_col = labels_col,
                scale = "none",
                annotation_col = annotation,
                cluster_rows = cluster_y,
                cluster_cols = cluster_x,
                legend = show.legend
  )
  
  return(p)
}

#' Generates a bar plot
#' @param args list of arguments. 
#' Mandatory argument : a data.frame 'df'
#' Other arguments can include :
#' 'stat_var' : names of numeric columns to include in the plot
#' 'group_var' : name of the column used to define x axis groups
#' 'color_var' : name of the column used for the aesthetic 'color' in 'ggplot2::geom_point()'
#' 'show.legend' : logical; show legend
#' @import ggplot2
#' @importFrom reshape2 melt
plot_bar <-function(args = list()){
  
  plot_type <- "bar"
  stat_var <- NULL
  group_var <- NULL
  color_var <- NULL
  show.legend <- TRUE
  
  if(is.null(args["df"])){
    return(NULL)
  }
  
  for(var in names(args)){
    assign(var, args[[var]])
  }
  
  df <- as.data.frame(df)
  
  id.vars <- names(df)[which(!sapply(df, is.numeric))]
  df_melt <- reshape2::melt(df, id.vars = id.vars)
  
  #print(df_melt)
  
  if(is.null(stat_var)){
    stat_var <- names(df)[which(sapply(df, is.numeric))]
  }
  
  if(!is.null(group_var)){
    if(group_var %in% names(df)){
      group_var <- as.name(group_var)
    }
  }
  
  if(!is.null(color_var)){
    if(color_var %in% names(df)){
      color_var <- as.name(color_var)
    }
  }
  
  df_melt <- df_melt[df_melt$variable %in% stat_var, ]
  
  p <- ggplot(df_melt, aes_string(x = group_var, y = "value"))
  
  p <- p + 
    geom_bar(mapping = aes_string(fill = group_var), alpha = 0.5, stat = "summary", fun.y = "mean") + 
    geom_point( mapping = aes_string(colour = color_var), 
                inherit.aes = TRUE, 
                position = position_jitter(width = 0.25, height = 0),
                alpha = 0.5,
                size = 3,
                show.legend = show.legend)

  
  return(p)
  
}

#' Generates a tile plot
#' @param args list of arguments. 
#' Mandatory argument : a data.frame 'df'
#' Other arguments can include :
#' 'stat_var' : names of numeric columns to include in the plot
#' 'group_var' : name of the column used to define x axis groups
#' 'show.legend' : logical; show legend
#' 'option' : name of the viridis palette
#' @import ggplot2
#' @importFrom viridis scale_fill_viridis
#' @importFrom reshape2 melt
plot_tile <-function(args = list()){
  
  plot_type <- "tile"
  stat_var <- NULL
  group_var <- NULL
  show.legend <- TRUE
  option <- "viridis"
  
  if(is.null(args["df"])){
    return(NULL)
  }
  
  for(var in names(args)){
    assign(var, args[[var]])
  }
  
  df <- as.data.frame(df)
  
  id.vars <- names(df)[which(!sapply(df, is.numeric))]
  df_melt <- reshape2::melt(df, id.vars = id.vars)
  
  #print(df_melt)
  
  if(is.null(stat_var)){
    stat_var <- names(df)[which(sapply(df, is.numeric))]
  }
  
  if(!is.null(group_var)){
    if(group_var %in% names(df)){
      group_var <- as.name(group_var)
    }
  }

  
  df_melt <- df_melt[df_melt$variable %in% stat_var, ]
  
  p <- ggplot(df_melt, aes_string(x = group_var, y = "variable", fill = "value")) +
    geom_tile(show.legend = show.legend) +
    scale_fill_viridis(option = option)
  
  return(p)
  
}

#' Perform a PCA and plot the result
#' @param args list of arguments. 
#' Mandatory argument : a data.frame 'df'
#' Other arguments can include :
#' 'stat_var' : names of numeric columns used to perform the PCA.
#' All numeric variables are used by default.
#' 'PCx' : x axis variable ("PC1" by default)
#' 'PCy' : y axis variable ("PC2" by default)
#' 'color_var' : name of the column used to color points
#' 'label_var' : name of the column used to label points
#' 'scale' : logical; scale values by variable before performing the PCA
#' @import ggplot2
#' @importFrom ggrepel geom_text_repel
#' @importFrom stats prcomp
plot_pca <-function(args = list()){
  
  plot_type <- "pca"
  PCx <- "PC1"
  PCy <- "PC2"
  stat_var <- NULL
  annotation_vars <- NULL
  color_var <- NULL
  label_var <- "name"
  scale <- FALSE
  
  if(is.null(args["df"])){
    return(NULL)
  }
  
  for(var in names(args)){
    assign(var, args[[var]])
  }
  
  df <- as.data.frame(df)
  
  idx <- which(names(df) %in% c("name", "subset", color_var, label_var))
  idx_annot <- idx
  
  df_stat <- df[-idx]
  if(!is.null(stat_var)){
    df_stat <- df_stat[which(names(df_stat) %in% stat_var)]
  }
  
  idx_cols <- which(sapply(df_stat, is.numeric))
  if(length(idx_cols)>1){
    df_stat <- df_stat[ , idx_cols ] 
  }else{
    stop("Not enough numeric variables to perform PCA")
  }
  
  rownames(df_stat) <- 1:dim(df_stat)[1]
  
  annotation <- df[idx_annot]
  rownames(annotation) <- 1:dim(annotation)[1]
  
  pca_res <- stats::prcomp( df_stat, center = scale, scale. = scale)
  
  df_pca <- as.data.frame(pca_res$x)
  df_pca <- cbind(df_pca, annotation[match(row.names(annotation), row.names(df_pca)), ])
  
  if(!is.null(color_var)){
    if(color_var %in% names(df_pca)){
      color_var <- as.name(color_var)
    }
  }
  if(!is.null(label_var)){
    if(label_var %in% names(df_pca)){
      label_var <- as.name(label_var)
    }
  }
  
  p <- ggplot(df_pca, aes_string(x=as.name(PCx), 
                                 y=as.name(PCy),
                                 color = color_var, 
                                 label = label_var)) +
    geom_point() +
    geom_text_repel(show.legend = FALSE)
  
  return(p)
}


####################################################################################################
# Add plot layers
####################################################################################################

#' @import ggplot2
#' @importFrom grDevices rgb
#' @importFrom ggrepel geom_label_repel
add_polygon_layer <- function(p,
                             polygon = NULL,
                             label = NULL){
  
  if(p$plot_env$plot_type != "histogram" & setequal(names(polygon), c("x", "y"))){
    if(!is.null(polygon$x)){
      if(length(polygon$x)>1){
        polygon <- data.frame(x = polygon$x, y = polygon$y)
        polygon <- rbind(polygon, polygon[1,])
        
        ########################################################################
        # Adjust plot limits
        layer_info <- layer_scales(p)
        
        update_range_x <- FALSE
        
        if(!is.null(layer_info$x$limits) & 
           "RangeContinuous" %in% class(layer_info$x$range) ){
          
          xrange <- layer_info$x$trans$inverse(layer_info$x$limits)
          if(!is.null(xrange)){
            if(max(polygon$x) > max(xrange)){
              update_range_x <- TRUE
              xrange[2] <- max(polygon$x)
            }
            if(min(polygon$x) < min(xrange)){
              update_range_x <- TRUE
              xrange[1] <- min(polygon$x)
            }
          }
        }
        
        if(update_range_x){
          p <- p + scale_x_continuous(name = layer_info$x$name, 
                                      trans = layer_info$x$trans, 
                                      limits = xrange)
          
        }
        
        update_range_y <- FALSE
        
        if(!is.null(layer_info$x$limits) & 
           "RangeContinuous" %in% class(layer_info$x$range) ){
          
          yrange <- layer_info$y$trans$inverse(layer_info$y$limits)
          if(!is.null(yrange)){
            if(max(polygon$y) > max(yrange)){
              update_range_y <- TRUE
              yrange[2] <- max(polygon$y)
            }
            if(min(polygon$y) < min(yrange)){
              update_range_y <- TRUE
              yrange[1] <- min(polygon$y)
            }
          }
        }
        if(update_range_y){
          p <- p + scale_y_continuous(name = layer_info$y$name, 
                                      trans = layer_info$y$trans, 
                                      limits = yrange)
        }
        
        ########################################################################3
        
        
        
        p <- p +
          geom_path(data = polygon, mapping = aes(x=x, y=y), color = "red", inherit.aes = FALSE) +
          geom_polygon(data=polygon, mapping = aes(x=x, y=y), 
                       inherit.aes = FALSE,
                       fill="red",
                       alpha=0.05)
        if(!is.null(label)){
          df_label <- data.frame(x=mean(polygon$x), y= mean(polygon$y))
          p <- p +  geom_label_repel(data = df_label, force = 4, inherit.aes = FALSE,
                                     mapping = aes(x=x, y=y), 
                                     label = label, 
                                     fill = grDevices::rgb(1,1,1,0.85), 
                                     color = "red", 
                                     nudge_y = 0, 
                                     nudge_x =0, 
                                     point.padding = 0)
        }
      }
      

    }
    
  }
  return(p)
  
}

#' @importFrom sp point.in.polygon
#' @importFrom rlang quo_get_expr
add_gate <- function(p, gate){
  
  if(is.null(gate) | p$plot_env$plot_type == "histogram"){
    return(p)
  }
  
  polygon <- get_gate_coordinates(gate)
  
  xvar <- NULL
  yvar <- NULL
  color_var <- NULL
  
  if("x" %in% names(p$mapping)){
    if("quosure" %in% class(p$mapping$x)){
      xvar <- as.character(rlang::quo_get_expr(p$mapping$x))
    }
  }
  
  if("y" %in% names(p$mapping)){
    if("quosure" %in% class(p$mapping$x)){
      yvar <- as.character(rlang::quo_get_expr(p$mapping$y))
    }
  }
  
  if("colour" %in% names(p$mapping)){
    if("quosure" %in% class(p$mapping$colour)){
      color_var <- as.character(rlang::quo_get_expr(p$mapping$colour))
    }
  }
  
  if(setequal(c(xvar, yvar), names(polygon))){ 
    
    in_poly <- sp::point.in.polygon(p$data[[xvar]], 
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

####################################################################################################
# Format plot (legend, scale, labels ...)
####################################################################################################

#' Format a ggplot object
#' @param p a ggplot object
#' @param options  list of plot format options. Names of options include:
#' xlim : x-axis range
#' ylim : y-axis range
#' transformation : named list of trans objects 
#' default_trans : default trans object (set to 'identity_trans()' by default). 
#' Used only if 'transformation' is not an element of 'options'.
#' axis_labels : named list with axis labels (each element should be named after a plot variable)
#' color_var_name : name to display for color variable
#' facet_var : names of the variables used for facetting plots along the x-axis
#' facet_yvar : names of the variables used for facetting plots along the y-axis
#' scales : control scaling across facets (passed to 'facet_grid()'), Set to "fixed" by default
#' option : name of the viridis palette
#' theme : name of the ggplot theme ("gray" by default)
#' legend.position : legend position
#' @import ggplot2
#' @import viridis
#' @importFrom flowWorkspace logicle_trans
#' @importFrom stats as.formula
#' @importFrom rlang quo_get_expr
#' @importFrom scales identity_trans
#' @return a ggplot object
format_plot <- function(p,
                        options = list()){
  
  xvar <- NULL
  yvar <- NULL
  
  #print(names(p$mapping))

  if("x" %in% names(p$mapping)){
    if("quosure" %in% class(p$mapping$x)){
      xvar <- as.character(rlang::quo_get_expr(p$mapping$x))
    }
  }
  
  if("y" %in% names(p$mapping)){
    if("quosure" %in% class(p$mapping$x)){
      yvar <- as.character(rlang::quo_get_expr(p$mapping$y))
    }
  }
  
  xlim <- NULL
  ylim <- NULL
  
  transformation <- list()
  axis_labels <- list()
  axis_limits <- list()
  
  color_var <- as.character(p$plot_env$color_var)
  
  facet_yvar <- NULL
  if(!is.null(p$plot_env$plot_type)){
    if(p$plot_env$plot_type == "bar"){
      facet_yvar <- "variable"
    }
  }
  
  
  ############################################################################
  #default parameters
  
  var_options <- c("xlim", "ylim", "transformation", "default_trans",
                   "axis_labels", "axis_limits", "color_var_name", "facet_var", "facet_yvar",
                   "scales", "option", "theme", "legend.position")
  
  for(var in intersect(names(options), var_options)){
    assign(var, options[[var]])
  }

  #facet scales
  if(is.null(options$scales)){
    scales <- "fixed"
  }
  
  #default viridis palette
  if(is.null(options$option)){
    option <- "viridis"
  }

  #default transformation
  if(is.null(options$default_trans)){
    default_trans <- scales::identity_trans()
  }
  
  
  
  ############################################################################33
  #transformations
  
  if(!is.null(xvar)){
    if(length(xvar) == 1){
      
      labx <- ifelse(xvar %in% names(axis_labels), axis_labels[[xvar]], xvar)
      trans_x <- ifelse(xvar %in% names(transformation), transformation[[xvar]], default_trans)
      xlim <- axis_limits[[xvar]]
      
      if(is.double(p$data[[xvar]])){
        p <- p + scale_x_continuous(name = labx, trans = trans_x, limits = xlim) 
      }else{
        p <- p + scale_x_discrete(name = labx) 
      }
     
    }
  }
  
  if(!is.null(yvar)){
    if(length(yvar) == 1){
      
      laby <- ifelse(yvar %in% names(options$axis_labels), options$axis_labels[[yvar]], yvar)
      trans_y <- ifelse(yvar %in% names(transformation), transformation[[yvar]], default_trans)
      ylim <- axis_limits[[yvar]]

      if(is.double(p$data[[yvar]])){
        p <- p + scale_y_continuous(name = laby, trans = trans_y, limits = ylim) 
      }else{
        p <- p + scale_y_discrete(name = laby) 
      }
        
    }
  }
  
  if(!is.null(p$plot_env$plot_type)){
    if(p$plot_env$plot_type == "dots"){
      
      if(!is.null(color_var)){
        if(length(color_var) == 1){

          label_color <- ifelse(color_var %in% names(options$axis_labels), options$axis_labels[[color_var]], color_var)
          trans_col <- ifelse(color_var %in% names(transformation), transformation[[color_var]], default_trans)
          is_cont <- ifelse(color_var %in% names(p$data), is.double(p$data[[color_var]]), FALSE)
          
          if(is_cont){
            p <- p + viridis::scale_colour_viridis(trans = trans_col,
                                                   name = label_color,
                                                   option = option)
          }
        }
      }
    }
  }
  

  ############################################################################33
  #facet
  if(!is.null(options$facet_var) | !is.null(facet_yvar)){
    
    left_formula <- paste(facet_yvar, collapse = " + ")
    right_formula <- "."
    if(!is.null(options$facet_var)){
      if(options$facet_var != ""){
        right_formula <- paste(options$facet_var, collapse = " + ")
      }
    }
    
    #print(paste(left_formula, "~", right_formula))
    formula_facet <- stats::as.formula(paste(left_formula, "~", right_formula))
    
    p <- p + facet_grid(formula_facet,
                        labeller = label_both, 
                        #scales = scale_y,
                        scales = scales)
  }
  
  ############################################################################
  #theme
  if(length(unique(p$data$subset))==1){
    p <- p + ggtitle(unique(p$data$subset))
  }
  
  if("theme" %in% names(options)){
    if(!is.null(options$theme)){
      if(options$theme != ""){
        theme_name = paste("theme_", options$theme, sep = "")
        theme_function <- function(...){
          do.call(theme_name, list(...))
        }
        p <- p + theme_function()
      }
    }
    
  }
  
  
  if(!is.null(options$legend.position)){
    p <- p + theme(legend.position = options$legend.position)
  }
  
  p <- p + theme(plot.title = element_text(face = "bold"))

  return(p)
  
}

####################################################################################################
# Main plot functions
####################################################################################################

#' Plot a GatingSet
#' @param df a data.frame with plot data resulting from a call of \code{get_plot_data}. 
#' Supersedes parameters 'gs', 'sample', 'spill', 'metadata'
#' @param gs a GatingSet
#' @param sample Names of samples from the GatingSet 
#' (as returned by \code{pData(gs)$name})
#' @param subset Names of subsets from the GatingSet 
#' (as returned by \code{gs_get_pop_paths(gs)})
#' @param spill spillover matrix. If NULL, uncompensated data is used for gating and plotting.
#' @param metadata a data.frame containing metadata associated to samples.
#' Must have a column \code{name} used for mapping.
#' @param plot_type name of the plot type
#' @param plot_args  list of plot parameters passed to the plot function. 
#' Plot parameters depend on the plot type selected.
#' @param options  list of plot format options passed to \code{format_plot()}
#' @param gate_name Names of the gates to add to the plot (if it is compatible with plot parameters).
#' Ignored if NULL.
#' @importFrom flowWorkspace gs_get_pop_paths pData gh_pop_get_gate
#' @return a plot
plot_gs <- function(gs,
                    df = NULL,
                    sample = NULL,
                    subset = NULL,
                    spill = NULL,
                    metadata = NULL,
                    plot_type = "hexagonal",
                    plot_args = list(),
                    options = list(),
                    gate_name = NULL){
  
  
  if(! "xvar" %in% names(plot_args)){
    plot_args[["xvar"]] <- colnames(gs)[1]
  }
  if(! "yvar" %in% names(plot_args)){
    plot_args[["yvar"]] <- colnames(gs)[2]
  }
  
  if(is.null(sample)) sample <-  flowWorkspace::pData(gs)$name[1]
  if(is.null(subset)) subset <- flowWorkspace::gs_get_pop_paths(gs)[1]

  df <- get_plot_data(df = df,
                      gs = gs, 
                      sample = sample,
                      subset = subset,
                      spill = spill, 
                      metadata = metadata)
  
  p <- call_plot_function(df = df,
                          plot_type = plot_type,
                          plot_args = plot_args)
  
  p <- format_plot(p, options = options)
  
  if(!is.null(gate_name)){
    for(gateName in setdiff(gate_name, "root")){
      g <- flowWorkspace::gh_pop_get_gate(gs[[1]], gateName)
      p <- add_gate(p, g)
    }
  }
  return(p)
}

#' Plot aggregated data from a GatingSet
#' @param df a data.frame with plot data resulting from a call of \code{get_plot_data}. 
#' Supersedes parameters 'gs', 'sample', 'spill'
#' @param gs a GatingSet
#' @param sample Names of samples from the GatingSet 
#' (as returned by \code{pData(gs)$name})
#' @param subset Names of subsets from the GatingSet 
#' (as returned by \code{gs_get_pop_paths(gs)})
#' @param spill spillover matrix. If NULL, uncompensated data is used for gating and plotting.
#' @param metadata a data.frame containing metadata associated to samples.
#' Must have a column \code{name} used for mapping.
#' @param transformation A list of trans objects. 
#' Each element must be named after a parameter and contain the transfomation to apply for this parameter.
#' If NULL, the default transformation 'y_trans' is applied.
#' @param stat_function Name of the function to be applied on transformed data for each sample and subset.
#' @param y_trans Default trans object (set to 'identity_trans()' by default). 
#' Used only if parameter 'transformation' is NULL. 
#' @param apply_inverse logical; Apply inverse transformation before returning the data.
#' @param yvar Names of the variables for which to compute statistics
#' @param var_names Replace 'yvar' with custom names
#' @param scale logical; Should data be scaled for each 'yvar' variable?
#' @param plot_type name of the plot type
#' @param plot_args  list of plot parameters passed to the plot function. 
#' Plot parameters depend on the plot type selected.
#' @param options  list of plot format options passed to \code{format_plot()}
#' @importFrom flowWorkspace gs_get_pop_paths pData
#' @importFrom scales identity_trans
#' @return a list with a plot and the corresponding plot data
plot_stat <- function(df = NULL,
                      gs,
                      sample = NULL,
                      subset = NULL,
                      spill = NULL,
                      metadata = NULL,
                      transformation=NULL,
                      stat_function = "mean",
                      y_trans = identity_trans(),
                      apply_inverse = TRUE,
                      yvar = NULL,
                      var_names = NULL,
                      scale = FALSE,
                      plot_type = "heatmap",
                      plot_args = list(),
                      options = list() ){
                        
  
  
  if(is.null(sample)) sample <- flowWorkspace::pData(gs)$name[1]
  if(is.null(subset)) subset <- flowWorkspace::gs_get_pop_paths(gs)[1]
  
  if(is.null(df)){
    df <- get_plot_data(df = df,
                        gs = gs,
                        sample = sample,
                        subset = subset,
                        spill = spill, 
                        metadata = NULL)
  }
  
  df <- df[df$name %in% sample & df$subset %in% subset, ]
  
  df_stat <- compute_stats(df = df,
                      gs = gs,
                      spill = spill,
                      transformation = transformation,
                      stat_function = stat_function,
                      y_trans = y_trans,
                      apply_inverse = apply_inverse,
                      yvar = yvar,
                      var_names = var_names,
                      id.vars = c("name", "subset"))
  
  if(scale){
    df_stat <- scale_values(df_stat, id.vars = c("name", "subset"))
  }                          
  
  df_stat <- add_columns_from_metadata(df_stat, metadata = metadata)
  
  plot_args$stat_var <- switch(stat_function,
                               "cell count" = "Count",
                               "percentage" = "perc_parent",
                               yvar)
  
  plot_args$annotation_vars <- unique(c("name", "subset", names(metadata)))
  
  p <- call_plot_function(df = df_stat,
                          plot_type = plot_type,
                          plot_args = plot_args)
  
  
  if("plot_type" %in% names(p$plot_env)){
   p <- format_plot(p, options = options)
  }
  
  return( list(plot = p, data = df_stat) )
}

#' Plot all gates for a given sample of a GatingSet
#' @description  Plot all gates for a given sample of a gating set
#' @param df data.frame with columns \code{name} and \code{subset} 
#' containing sample and subset names respectively and
#' columns with plot variables. Ignored if \code{NULL}. 
#' Otherwise supersedes parameters 'gs', 'sample', 'spill', 'metadata'.
#' @param gs a GatingSet
#' @param sample sample names
#' @param Ncells number of cells to sample from the GatingSet
#' @param selected_subsets subset names. if NULL, all gates are drawn.
#' @param spill spillover matrix. If NULL, uncompensated data is used both for gating and plotting.
#' @param plot_type name of the plot type
#' @param plot_args  list of plot parameters passed to \code{plot_gs()}
#' @param options  list of plot format options passed to \code{format_plot()}
#' @return a list of ggplot objects
#' @importFrom flowWorkspace gs_get_pop_paths gs_pop_get_parent gs_pop_get_children
plot_gh <- function( gs, 
                      df = NULL,
                      sample = NULL,
                      Ncells = NULL,
                      selected_subsets = NULL,
                      spill = gs@compensation,
                      plot_type = "hexagonal",
                      plot_args = list(), 
                      options = list()){
  
  if(is.null(sample)){sample = pData(gs)$name[1]}
  
  idx <- match(sample, pData(gs)$name)
  
  if(is.null(selected_subsets)){
    selected_subsets <- setdiff(flowWorkspace::gs_get_pop_paths(gs), "root")
    subset <- flowWorkspace::gs_get_pop_paths(gs)
  }else{
    subset <- selected_subsets[selected_subsets %in% flowWorkspace::gs_get_pop_paths(gs)]
    parent_subsets <- sapply(subset, function(x){flowWorkspace::gs_pop_get_parent(gs[[idx[1]]], x)})
    subset <- union(subset, parent_subsets)
  }
  
  if(is.null(df)){
    
    df <- get_data_gs(gs = gs,
                      sample = sample,
                      subset = subset,
                      spill = spill,
                      Ncells = Ncells)
    
  }
  
  child_nodes <- flowWorkspace::gs_pop_get_children(gs[[idx[1]]], "root")
  child_nodes <- child_nodes[child_nodes %in% selected_subsets]
  
  plist <- list()
  count <- 0
  
  #plot gates descending the gh until there are no more children gates
  while(length(child_nodes)>0){
    
    child_nodes_int <- NULL
    nodes_to_plot <- child_nodes
    all_parents <- sapply(nodes_to_plot, function(x){flowWorkspace::gs_pop_get_parent(gs[[idx[1]]], x)})
    names(all_parents) <- NULL
    
    for(parent in unique(all_parents)){
      
      idx_parent <- which(all_parents == parent)
      
      nodes_to_plot_parent <- nodes_to_plot[idx_parent]
      
      #plot together gates that share the same set of parameters
      
      while(length(nodes_to_plot_parent) > 0){
        
        
        par_nodes <- lapply(nodes_to_plot_parent, function(x){
          
          g <- flowWorkspace::gh_pop_get_gate(gs[[idx[1]]], x)
          
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
                                   gate_name = nodes_to_plot_parent[same_par], 
                                   plot_type = plot_type,
                                   plot_args = plot_args,
                                   options = options)
        
        all_children <- unlist(sapply(nodes_to_plot_parent[same_par], function(x){flowWorkspace::gs_pop_get_children(gs[[1]], x)}))
        names(all_children) <- NULL
        
        child_nodes_int <- c(child_nodes_int, all_children)
        nodes_to_plot_parent <- setdiff(nodes_to_plot_parent, nodes_to_plot_parent[same_par])
        
      }
      
    }
    
    child_nodes <- child_nodes_int[child_nodes_int %in% selected_subsets]
    
  }
  return(plist)
}

#' Plot a gate from a GatingSet
#' @param gate_name name of the gate
#' @param df a data.frame with plot data resulting from a call of \code{get_plot_data}. 
#' Supersedes parameters 'gs', 'sample', 'spill', 'metadata'
#' @param gs a GatingSet
#' @param sample Set of samples from 'gs'
#' @param spill compensation 
#' @param metadata a data.frame containing metadata associated to samples.
#' Must have a column \code{name} used for mapping.
#' @param plot_type name of the plot type
#' @param plot_args  list of plot parameters passed to \code{plot_gs()}
#' @param options  list of plot format options passed to \code{format_plot()}
#' @importFrom flowWorkspace gh_pop_get_gate
plot_gate <- function(gate_name,
                     df = NULL,
                     gs,
                     sample = NULL,
                     spill = NULL,
                     metadata = NULL,
                     plot_type = "contour",
                     plot_args = list(),
                     options = list()){
  
  gate <- flowWorkspace::gh_pop_get_gate(gs[[1]], gate_name)
  
  polygon <- get_gate_coordinates(gate)
  subset <- flowWorkspace::gs_pop_get_parent(gs,  gate_name)
  plot_args[["xvar"]] <- names(polygon)[1]
  
  if(length(names(polygon))>1){plot_args$yvar <- names(polygon)[2]}
  
  if(is.null(sample)) sample <-  pData(gs)$name[1]

  df <- get_plot_data(df = df,
                      gs = gs, 
                      sample = sample,
                      subset = subset,
                      spill = spill, 
                      metadata = metadata)
  
  p <- call_plot_function(df = df,
                    plot_type = plot_type,
                    plot_args = plot_args)
  
  p <- format_plot(p, options = options)
  
  p <- add_gate(p, gate)
  
  return(p)
}



#' scale column of a data frame
#' @param df a data.frame
#' @param id.vars Names of df's columns that should not be scaled
#' @return a data.frame
scale_values <- function(df, id.vars = NULL){
  
  if(is.null(id.vars)){
    id.vars <- names(df)[which(!sapply(df, is.numeric))]
  }
  
  df_scale <- df
  df_scale[-which(names(df) %in% id.vars)] <- scale(df[-which(names(df) %in% id.vars)])
  
  df_scale
}


####################################################################################################
# Dimensionality Reduction 
####################################################################################################

#' Perform dimensionality reduction
#' @description  Perform dimensionality reduction
#' @param df a data.frame with only numeric variables.
#' @param yvar names of df's variables used to perform dimensionality reduction
#' @param Ncells Maximum number of cells without any NA values sampled from df. 
#' If NULL, all cells without any NA values are used
#' @param transformation  Named list of \code{trans} objects. 
#' List names should correspond to variable names.
#' @param y_trans default \code{trans} object to be used if \code{transformation} is NULL.
#' @param perplexity t-SNE perplexity parameter (passed to \code{Rtsne:Rstne()})
#' @param dims Number of dimensions (passed to \code{Rtsne:Rstne()})
#' @param method Name of the method used. Either "tSNE" or "umap"
#' @param check_duplicates logical. Checks whether duplicates are present (passed to \code{Rtsne:Rstne()})
#' @return a data.frame with additionnal columns : 
#' "tSNE1" and "tSNE2" for method 'tSNE', "UMAP1" and "UMAP2" for method 'umap'
#' @importFrom Rtsne Rtsne
#' @importFrom umap umap
#' @importFrom scales log10_trans
dim_reduction <- function(df,
                          yvar,
                          Ncells = NULL,
                          transformation = NULL,
                          y_trans = log10_trans(),
                          perplexity = 50,
                          dims = 2,
                          method = "tSNE",
                          check_duplicates = FALSE){
  
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
    tSNE <- Rtsne(df_trans[ idx_cells , yvar], perplexity = perplexity, dims = dims, check_duplicates = check_duplicates)
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
####################################################################################################

#' Identify clusters
#' @description  Identify clusters
#' @param df a data.frame with only numeric variables.
#' @param yvar names of df's variables used to find clusters
#' @param transformation  Named list of \code{trans} objects. 
#' List names should correspond to variable names.
#' @param y_trans default \code{trans} object to be used if \code{transformation} is NULL.
#' @param dc ClusterX dc parameter
#' @param alpha ClusterX alpha parameter
#' @param method Name of the method used. Either "FlowSOM", "ClusterX", "Rphenograph".
#' @param k integer; number of nearest neighbours (passed to \code{Rphenograph()})
#' @param k_meta Maximum number of clusters to try out (passed to \code{FlowSOM::MetaClustering()})
#' @param scale logical; Scale values before building SOM (for method 'FlowSOM' only)
#' @return a data.frame with the additionnal column "cluster"
#' @importFrom FlowSOM BuildSOM BuildMST MetaClustering
#' @importFrom Rphenograph Rphenograph
#' @importFrom ClusterX ClusterX
#' @importFrom igraph membership
#' @importFrom scales identity_trans
get_cluster <- function(df,
                        yvar,
                        transformation = NULL,
                        y_trans = identity_trans(),
                        dc=3, 
                        alpha = 0.001,
                        k = 100,
                        k_meta = 8,
                        scale = FALSE,
                        method = "FlowSOM"){
         
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
  
  if(method == "FlowSOM"){
    
    data <- as.matrix(df_trans[, which(names(df_trans) %in% yvar)])
    fSOM <- list(data = data,
                 compensate = FALSE,
                 spillover = NULL,
                 transform = FALSE,
                 scale = scale,
                 prettyColnames = colnames(data))
    
    message(paste("Clustering ", dim(data)[1], " cells using 'FlowSOM' on ",  length(yvar), " parameters", sep = ""))
    
    fSOM <- BuildSOM(fSOM, colsToUse = which(colnames(data) %in% yvar))
    fSOM <- BuildMST(fSOM)
    fSOM$metaClustering <- MetaClustering(fSOM$map$codes, "metaClustering_consensus", max=k_meta)
    
    metaClustering_perCell <- fSOM$metaClustering[fSOM$map$mapping[,1]]
    df_filter$cluster <- metaClustering_perCell
    
    return(list(df = df_filter, keep = idx_cells_kept, fSOM = fSOM))
    
  }else if(method == "Rphenograph"){
    # warning("Rphenograph is not supported")
    # df_filter$cluster <- 1 
    # return(list(df = df_filter, keep = idx_cells_kept))
    message(paste("Clustering ", dim(df_trans)[1], " cells using 'Rphenograph' on ",  length(yvar), " parameters", sep = ""))
    Rphenograph_out <- Rphenograph(df_trans[ , yvar], k = k)
    df_filter$cluster <- igraph::membership(Rphenograph_out[[2]])
  }else if(method == "ClusterX"){
    # warning("ClusterX is not supported")
    # df_filter$cluster <- 1 
    # return(list(df = df_filter, keep = idx_cells_kept))
    message(paste("Clustering ", dim(df_trans)[1], " cells using 'CluserX' on ",  length(yvar), " parameters", sep = ""))
    DC <- ClusterX(df_trans[ , yvar], dc = dc, alpha = alpha)
    df_filter$cluster <- DC$cluster
  }
 
  return(list(df = df_filter, keep = idx_cells_kept))
  
}


####################################################################################################
# Build FlowSet
####################################################################################################

#' Build a flowSet from a data.frame
#' @description Build a flowSet from a data.frame
#' @param df data.frame with a column \code{sample_col} containing sample names and
#' columns with flow variables.
#' @param origin flowSet from which to retrieve flowFrames description and parameters slots. 
#' Ignored if NULL.
#' @param chanel_col Names of df's columns to be used as flow variables 
#' @param sample_col Name of df's column containing sample names
#' @return a flowSet
#' @importFrom flowCore description parameters flowFrame flowSet
#' @importFrom flowWorkspace pData
#' @examples
#' \dontrun{
#' utils::data("GvHD", package = "flowCore")
#' gs <- GatingSet(GvHD)
#' df <- get_data_gs(gs = gs, sample = pData(gs)$name[1:3], subset = "root", Ncells = 1000)
#' fs <- build_flowset_from_df(df = df, origin = gs@data)
#' pData(fs)}
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
        
        idx <- match(sample,flowWorkspace::pData(origin)$name)
        
        if(!is.na(idx)){
          
          par <- parameters(origin[[idx]])
          #par <- origin$par[[idx]]
          
          par@data$name <- as.character(par@data$name)
          par@data$desc <- as.character(par@data$desc)
          
          desc <- description(origin[[idx]])
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
    if("flowSet" %in% class(origin) ){
      pdata <- data.frame(flowWorkspace::pData(origin))
      idx_match <- match(samples, pdata$name)
      if(length(colnames(pdata))>1){
        pData(fs_new) <- pdata[idx_match, ]
      }else{
        flowWorkspace::pData(fs_new)$name <- pdata[idx_match, ]
      }
    }
  }else{
    fs_new <- NULL
  }
  
  return(fs_new)
  
}

