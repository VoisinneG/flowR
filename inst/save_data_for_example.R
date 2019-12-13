library(flowWorkspace)
library(CytoML)
library(openCyto)

ws <- open_flowjo_xml(file = "./demo-data/JL04BMVLG-Valentin/Tumor-testFlowR.wsp")
groups <- fj_ws_get_sample_groups(ws)
groups <- as.character(groups$groupName)
gates <- get_gates_from_ws(ws_path = "./demo-data/JL04BMVLG-Valentin/Tumor-testFlowR.wsp", group = groups[1])

# not working to build flowSet
#gs <- flowjo_to_gatingset(ws, execute = FALSE,  name = groups[1])
#gates_gs <- get_gates_from_gs(gs)
#fs <- gs_pop_get_data(gs)

# OK
files <-  list.files("./demo-data/JL04BMVLG-Valentin/")
files <- files[ grep("\\.fcs$", files) ]
fs <- read.ncdfFlowSet(files = paste0("./demo-data/JL04BMVLG-Valentin/", files))
gs <- GatingSet(fs)

# add gates
time_step <- as.numeric(description(fs[[1]])[["$TIMESTEP"]])
gates <- transform_gates(gates = gates, pattern = "Comp-", replacement = "", time_step = time_step)
add_gates_flowCore(gs = gs, gates = gates)

spill <- lapply(1:length(fs), function(i){fs[[i]]@description$SPILL})
names(spill) <- fs@origSampleVector
gs@compensation <- spill
compensate(fs, spillover = gs@compensation)


transformation <-  lapply(colnames(gs), logicle_trans)
names(transformation) <- colnames(gs)
gs@transformation <- transformation


save(fs, file = "./inst/ext/fs.rda")
save_gs(gs, "./inst/ext/gs")
save(spill, file = "./inst/ext/spill.rda")
file.copy(from = "./demo-data/JL04BMVLG-Valentin/Tumor-testFlowR.wsp", to = "./inst/ext/workspace.wsp")

########################################
gs <- load_gs("./inst/ext/gs")
ws <- open_flowjo_xml(file = "./inst/ext/workspace.wsp")
