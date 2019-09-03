library(flowWorkspace)
library(CytoML)
library(openCyto)

library(FlowSOM)
library(slingshot)
library(ClusterX)
library(scales)

ws <- openWorkspace(file = "./data/tetra/workspace.wsp")

gs <- parseWorkspace(ws,
                     name = "All Samples",
                     execute = TRUE,
                     isNcdf = TRUE,
                     sampNloc = "sampleNode")

plot_gs(gs = gs, sample = pData(gs)$name[1], subset = "root", type = "dots", facet_vars = NULL, color_var = NULL)

fs <- getData(gs)

df <- get_data_gs(gs = gs, sample = pData(gs)$name[8], subset = c("/live/CD8+/p0", "/live/CD8+/p1") )

df <- df[ sample(1:dim(df)[1], 1000), ]

df_cluster <- get_cluster(df, yvar = names(df)[4:7], y_trans = logicle_trans() )

data <- as.matrix(df[, -which(names(df) %in% c("name", "subset", "cluster"))])

fSOM <- list(data = data, 
             compensate = FALSE,
             spillover = NULL,
             transform = TRUE,
             toTransform = colnames(data)[4:7],
             transformFunction = log_trans,
             scale = TRUE,
             prettyColnames = colnames(data))

fSOM <- BuildSOM(fSOM, colsToUse = c(4:7))
fSOM <- BuildMST(fSOM,tSNE=TRUE)
metaClustering <- metaClustering_consensus(fSOM$map$codes,k=7)

df$metaClustering <- metaClustering
  
fSOM <- UpdateNodeSize(fSOM, reset=TRUE)

PlotPies(fSOM, cellTypes=df$subset, backgroundValues = as.factor(metaClustering))

PlotStars(fSOM, backgroundValues = as.factor(metaClustering))

#save parameters and description data

par <- lapply(1:length(fs), function(x){parameters(fs[[x]])})
desc <- lapply(1:length(fs), function(x){description(fs[[x]])})

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
