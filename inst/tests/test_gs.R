library(flowWorkspace)
library(CytoML)
library("flowCore")

data("GvHD")
gs <- GatingSet(GvHD)

fs <- read.ncdfFlowSet(files = "~/ownCloud/FlowR_project/flowR_utils/demo-data/JL04BMVLG-Valentin/Tumor_T_001_012.fcs")
#fs <- read.ncdfFlowSet(files = "../flowR_utils/demo-data/OC17BMGV/s1-Tube_001-006.fcs")

gs <- GatingSet(fs)
transfo <- lapply(colnames(gs), logicle_trans)
names(transfo) <- colnames(gs)
gs@transformation <- transfo
spill <- gs@data[[1]]@description[["SPILL"]]
comp <- lapply(pData(gs)$name, function(x){spill})
names(comp) <- pData(gs)$name
gs@compensation <- comp
gates <- get_gates_from_ws_diva("../flowR_utils/demo-data/JL04BMVLG-Valentin/JL04BMVLG.xml", template = "Gating")
#gates <- get_gates_from_ws_diva("../flowR_utils/demo-data/OC17BMGV/OC17BMGV.xml")

gates <- transform_gates(gates, pattern = "Comp-", replacement = "")
add_gates_flowCore(gs, gates)


p <- plot_gate(gate_name = names(gates)[3], gs = gs, plot_type = "dots", options = list(transformation=gs@transformation), spill = gs@compensation)
quosure(p$mapping$x)
p + coord_cartesian(ylim=c(10^3, 10^6))

plot_gate(gate_name = "/P1/P2", gs = gs, plot_type = "dots", options = list(transformation=gs@transformation), spill = gs@compensation)

plot_gh(gs, options = list(transformation=gs@transformation))

copy_gate(gs = gs, name = gs_get_pop_paths(gs)[4], parent = "root")


df <- get_data_gs(gs)
df_cluster <- get_cluster(df, yvar = names(df)[4:7], y_trans = logicle_trans() )
fSOM <- df_cluster$fSOM
p <- PlotPies(fSOM, cellTypes=as.factor(df$name))
PlotStars(fSOM, backgroundValues = as.factor(metaClustering))
PlotMarker(fSOM,"Comp-FSC-A")