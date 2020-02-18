library(ggcyto)
library(flowWorkspaceData)

dataDir <- system.file("extdata",package="flowWorkspaceData")
gs_orig <- load_gs(list.files(dataDir, pattern = "gs_bcell_auto",full = TRUE))
gs <- gs_clone(gs_orig)


autoplot(gs, "CD3")
p <- ggcyto(gs@data[sampleNames(gs)], aes_string(x=as.name('CD3'), y=as.name('FSC-A'))) + 
  geom_histogram() + 
  facet_wrap(NULL)
p + geom_gate('CD3')


p <- autoplot(gs[sampleNames(gs)[1]], "FSC-A", bins = 64)
p

p + ggcyto_par_set(limits = "data")
p + labs_cyto("channel")


p + geom_overlay("IgD-CD27-", alpha = 0.5, size = 0.1, color = "purple")


# plot a population without gate
fs <- gs_pop_get_data(gs, "CD20") #extract the gated data as a flowSet
autoplot(fs, "CD20", "CD19") #plot 2D


gs1 <- gs[1]
gs2 <- gs[2]
#construct the ggcyto object for gs1
p <- ggcyto(gs, aes(cd3, cd19)) + geom_point() + facet_wrap( ~ name)
p <- p + geom_gate("CD3") #add gate
p <- p + geom_stats(type = "percent")
p
#customize the stats layer
p <- p + geom_stats(type = "count", size = 6,  color = "red", fill = "green", adjust = 0.7)

#customize the layer
p <- p + labs_cyto("channel")
#customize the axis limits
p <- p + ggcyto_par_set(limits = "instrument")
#add another population as the overlay dots
p <- p + geom_overlay("IgD-CD27-", col = "black", size = 1.2, alpha = 0.4)
#hide the legend
p <- p + guides(fill=FALSE)
p
