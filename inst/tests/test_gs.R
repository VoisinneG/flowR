library(flowWorkspace)
library(CytoML)

gs <- load_gs("./inst/ext/gs")
ws <- open_flowjo_xml(file = "./inst/ext/workspace.wsp")

print(gs_get_pop_paths(gs))


gs <- load_gs("./inst/ext/gs")

gs@compensation <- NULL

