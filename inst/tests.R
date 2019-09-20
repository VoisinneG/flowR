library(flowWorkspace)
library(CytoML)
library(openCyto)
library(scales)

#library(FlowSOM)
#library(slingshot)

library(ClusterX)
library(dplyr)
library(data.table)
library(ggplot2)
library(rlang)
library(viridis)
library(sp)
library(pheatmap)

ws <- openWorkspace(file = "./data/tetra/workspace.wsp")

gs <- parseWorkspace(ws,
                     name = "All Samples",
                     execute = TRUE,
                     isNcdf = TRUE,
                     sampNloc = "sampleNode")

####################################################################################
metadata <- read.csv(file = "./data/tetra/meta.csv")
sample <-  pData(gs)$name[1:4]
subset <- getNodes(gs)[1:3]

#df <- get_plot_data(gs = gs, sample = pData(gs)$name, subset = getNodes(gs), metadata = NULL)
df <- get_plot_data(gs = gs, sample = sample, subset = subset, metadata = NULL)


plot_var <- names(df)
axis_labels <- paste(plot_var, "c")
names(axis_labels) <- plot_var

df_cast1 <- compute_stats(df, gs =gs, stat_function = "cell count", y_trans = logicle_trans(), var_names = axis_labels)
df_cast2 <- compute_stats(df, stat_function = "median", y_trans = logicle_trans(), var_names = axis_labels)
df_stats <- merge.data.frame(df_cast1, df_cast2, by = c("name", "subset"))
df_stats <- add_columns_from_metadata(df_stats, metadata = metadata)

p <- plot_gs_data(df=df_stats,
                  plot_type = "stat_pca",
                  plot_args = list(annotation_vars = names(metadata),
                                   color_var = "stim",
                                   label_var = "dose")
)

p <- format_plot(p,
                 options = list(show.legend = FALSE, 
                                facet_vars = c("subset")
                                ))

p <- plot_gs_data(df=df, 
             plot_type = "contour",
             plot_args = list(xvar = "Comp-Time",
                              yvar = "Comp-R-APC-A")
             )

p <- format_plot(p,
                 options = list(color_var_name = "ok", 
                                show.legend = FALSE, 
                                facet_vars = c("name", "subset"),
                                axis_labels = axis_labels))
  
p2 <- add_polygon_layer(p, polygon_gate = data.frame(x=c(50000, 1000000, 1000000), y = c(0,0,1e5)))

gates <- get_gates_from_gs(gs)
p3 <- add_gate(p, gate = gates[[1]]$gate)


plot_gate(gs = gs, sample = sample, gate =  getNodes(gs)[6])


transformation[["Comp-Time"]] <- identity_trans()
plot_gs2(gs = gs, 
         gates = NULL,
         plot_type = "hexagonal", 
         plot_args = list(bins = 50), 
         options=list(transformation = transformation))

####################################################################################

metadata <- read.csv(file = "./data/tetra/meta.csv")

p <- plot_stat(gs = gs, metadata = metadata, group_var = "stim",
               stat_function = "mean", yvar = c("Comp-Time", "Comp-FSC-A"), 
          sample = pData(gs)$name[1:3], 
          subset = getNodes(gs), y_trans = identity_trans())$plot

p + theme(strip.background = element_rect(fill = "white"), legend.position = "none")

names(p)
p$data
names(p$plot_env)

setNode(gs, "/live/CD8+/p0", "undivided")

gates <- get_gates_from_gs(gs)

p <- plot_gs(gs = gs, sample = pData(gs)$name[1], subset = "root", type = "dots", facet_vars = NULL, color_var = NULL)
names(p)
p$data
names(p$plot_env)


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
