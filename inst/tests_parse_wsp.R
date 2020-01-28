library(CytoML)
library(openCyto)
library(xml2)


# Parse flowJO workspace

ws <- read_xml("./data/tetra/workspace.wsp")

gates <- get_gates_from_ws(ws_path = "./data/tetra/workspace.wsp", 
                           group = "All Samples")

gs <- parseWorkspace(ws,
                     name = "All Samples",
                     execute = TRUE,
                     isNcdf = TRUE,
                     sampNloc = "sampleNode")

# import diva worspace

ws <- openDiva("./data/FE03BMGV/FE03BMGV.xml")
getSampleGroups(ws)
getSamples(ws)
gs <- parseWorkspace(ws,
                     name = c("Ankrd13a", "Ablim1"),
                     execute = TRUE,
                     isNcdf = TRUE,
                     sampNloc = "sampleNode")

#Parse diva xml
xt <- read_xml("../flowR_utils/demo-data/JL04BMVLG-Valentin/JL04BMVLG.xml")
templates <-  xml_find_all(xt, ".//worksheet_template")
gatesNode <- xml_find_first(wtNode[[2]], "//gates")
gate_set <- xml_find_all(gatesNode, ".//gate")


parseTemplatesNodes <- function(x){
  attrs <- as.list(xml_attrs(x))
  if("name" %in% names(attrs)){
    return(attrs$name)
  }else{return(NA)}
}

get_templates_from_ws_diva <- function(ws_path){
  xt <- read_xml(ws_path)
  templates <-  xml_find_all(xt, ".//worksheet_template")
  template_names <- unlist(lapply(templates, parseTemplatesNodes))
  return(template_names[!is.na(template_names)])
}

get_gates_from_ws_diva <- function(ws_path, template = NULL){
  xt <- read_xml(ws_path)
  templates <-  xml_find_all(xt, ".//worksheet_template")
  template_names <- unlist(lapply(templates, parseTemplatesNodes))
  if(!is.null(template) & template %in% template_names){
    idx_template <- which(template_names == template)
  }else{
    idx_template <- 1
  }
  
  gatesNode <- xml_find_first(templates[[idx_template]], ".//gates")
  gate_set <- xml_find_all(gatesNode, ".//gate")
  gates <- lapply(gate_set, parseGateDiva)
  
  gate_list <- list()
  
  for(i in 1:length(gates)){
    if(!is.null(gates[[i]])){
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

  } 
  return(gate_list)
  
  return(gates)
}

parseGateDiva <- function(gateNode){
  
  res <- list()
  gate <- gateNode
  
  fullname <- xml_attr(gate, "fullname")
  
  if(fullname == "All Events"){
    return(NULL)
  }
  
  name_long <- gsub(fixed = FALSE, pattern = "\\\\", replacement = "/", x= fullname)
  name_long <- gsub("All Events", "", name_long)
  
  name <- xml_text(xml_find_all(gate, ".//name"))
  parent <- xml_text(xml_find_all(gate, ".//parent"))
  
  parent <- gsub(fixed = FALSE, pattern = "\\\\", replacement = "/", x= parent)
  parent <- gsub("All Events", "", parent)
  parent_long <- parent
  parent <- basename(parent)
  
  
  res <- c(res, list("name" = name,
                     "parent" =  parent, 
                     "name_long" = name_long, 
                     "parent_long" = parent_long
                     ))
  
  region <- xml_find_all(gate, ".//region")
  
  xparm <- xml_attr(region, "xparm")
  yparm <- xml_attr(region, "yparm")
  type <- xml_attr(region, "type")
  
  region <- xml_find_all(gate, ".//region")
  points <- xml_find_first(region, ".//points")
  vertexes <- xml_find_all(points, ".//point")
  
  print(res$diva_type)
  
  m <- do.call(rbind, lapply(vertexes, function(v){
    x <- as.numeric(xml_attr(v, "x"))
    y <- as.numeric(xml_attr(v, "y"))
    return(data.frame(x = x,
                      y = y))
    }))
  m <- as.matrix(m)
  colnames(m) <- c(xparm, yparm)
  
  print(m)
  
  if(type == "INTERVAL_REGION"){
    res <- c(res, list("type" = "RectangleGate",
                       "boundaries" = rbind(m[1,1], m[2,1])
                       ))
  }else if(type %in% c("RECTANGLE_REGION", "POLYGON_REGION")){
    res <- c(res, list("type" = "PolygonGate",
                       "polygon" = m
    ))
  }
  
  return(res)
  
}
