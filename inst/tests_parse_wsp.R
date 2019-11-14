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
xt <- read_xml("./data/FE03BMGV/FE03BMGV.xml")
gateNodes <- xml_find_all(xt, "//gates")
gate_set <- xml_find_all(gateNodes[[1]], ".//gate")

gate <- gate_set[[5]]
fullname <- xml_attr(gate, "fullname")
name <- xml_text(xml_find_all(gate, ".//name"))
parent <- xml_text(xml_find_all(gate, ".//parent"))
region <- xml_find_all(gate, ".//region")
xparm <- xml_attr(region, "xparm")
yparm <- xml_attr(region, "yparm")
type <- xml_attr(region, "type")
is_x_parameter_scaled <- xml_text(xml_find_all(gate, ".//is_x_parameter_scaled"))
is_y_parameter_scaled <- xml_text(xml_find_all(gate, ".//is_y_parameter_scaled"))
x_parameter_scale_value <- xml_integer(xml_find_all(gate, ".//x_parameter_scale_value"))
y_parameter_scale_value <- xml_integer(xml_find_all(gate, ".//y_parameter_scale_value"))
is_x_parameter_log <- xml_text(xml_find_all(gate, ".//is_x_parameter_log"))
is_y_parameter_log <- xml_text(xml_find_all(gate, ".//is_y_parameter_log"))

points <- xml_find_first(region, ".//points")
vertexes <- xml_find_all(points, ".//point")
m <- do.call(rbind, lapply(vertexes, function(v){return(list(x=xml_attr(v, "x"), y =xml_attr(v, "y")))}))


#####################################################33333

