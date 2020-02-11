library(flowWorkspace)
library(CytoML)
library("flowCore")

data("GvHD")
gs <- GatingSet(GvHD)

fs <- read.ncdfFlowSet(files = "../flowR_utils/demo-data/2019-Exp-Tumor-042 (Lung Carcinoma)/DE17BMVLG/Tumor_Tube_001.fcs")
#fs <- read.ncdfFlowSet(files = "../flowR_utils/demo-data/OC17BMGV/s1-Tube_001-006.fcs")

gs <- GatingSet(fs)
transfo <- lapply(colnames(gs), logicle_trans)
names(transfo) <- colnames(gs)
gs@transformation <- transfo
gates <- get_gates_from_ws_diva("../flowR_utils/demo-data/2019-Exp-Tumor-042 (Lung Carcinoma)/DE17BMVLG/DE17BMVLG.xml")
#gates <- get_gates_from_ws_diva("../flowR_utils/demo-data/OC17BMGV/OC17BMGV.xml")

gates <- transform_gates(gates, pattern = "Comp-", replacement = "")
add_gates_flowCore(gs, gates)
plot_gh(gs, options = list(transformation=gs@transformation))

copy_gate(gs = gs, name = gs_get_pop_paths(gs)[4], parent = "root")


df <- get_data_gs(gs)
df_cluster <- get_cluster(df, yvar = names(df)[4:7], y_trans = logicle_trans() )
fSOM <- df_cluster$fSOM
p <- PlotPies(fSOM, cellTypes=as.factor(df$name))
PlotStars(fSOM, backgroundValues = as.factor(metaClustering))
PlotMarker(fSOM,"Comp-FSC-A")