library(flowWorkspace)
library(CytoML)
library("flowCore")

data("GvHD")
gs <- GatingSet(GvHD)

fs <- read.ncdfFlowSet(files = "../flowR_utils/demo-data/JL04BMVLG-Valentin/Tumor_T_001_012.fcs")
gs <- GatingSet(fs)
gates <- get_gates_from_ws("../flowR_utils/demo-data/JL04BMVLG-Valentin/Tumor-testFlowR.wsp", group = "Tumor")
gates <- transform_gates(gates, pattern = "Comp-", replacement = "")
add_gates_flowCore(gs, gates)
copy_gate(gs = gs, name = gs_get_pop_paths(gs)[4], parent = "root")



