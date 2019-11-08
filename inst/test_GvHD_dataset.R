library(flowCore)
library(flowWorkspace)

data("GvHD", package = "flowCore")

gs <- GatingSet(GvHD)

plot_gs(gs)

gh_pop_get_indices(gs[1], "root")

gh_pop_get_data(gs[1])

exprs(gs@data[[1]])


