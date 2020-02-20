library(ggcyto)
library(flowWorkspaceData)

dataDir <- system.file("extdata",package="flowWorkspaceData")
gs <- load_gs(list.files(dataDir, pattern = "gs_bcell_auto",full = TRUE))

gs_get_pop_paths(gs)
gates <- get_gates_from_gs(gs)
fs <- gs_pop_get_data(gs, "Live") # cannot get data from multiple susbsets


transformation <- lapply(colnames(gs@data), logicle_trans)
names(transformation) <- colnames(gs@data)

spill_list <- lapply(sampleNames(fs), function(x){
  spill <- description(fs[[x]])[["SPILL"]]
  colnames(spill) <- parameters(fs[[x]])$name[match(colnames(spill), parameters(fs[[x]])$desc)]
  return(spill)})
names(spill_list) <- sampleNames(fs)
fs <- flowCore::compensate(fs, spill_list)

p <- call_plot_function(data = fs, 
                        plot_type = "hexagonal", 
                        plot_args = list(xvar = 'Live', smooth = TRUE, ridges = TRUE,
                                         yridges_var = "name", yvar = 'FSC-A', bins = 30, alpha = 0.5)
                        )
p <- ggplot(fs, aes(x=`FSC-A`, y=`SSC-A`)) + geom_point()


p1 <- format_plot(p, options = list(transformation = transformation))

gate <- gs_pop_get_gate(gs, "CD19andCD20")

p2 <- add_gate_to_plot(p, gate)

p2 <- p + ggcyto::geom_gate(gate) + 
  ggcyto::geom_stats(gate = gate, type = c("gate_name", "percent"), 
                     fill = grDevices::rgb(1,1,1,0.75))

as.ggplot(p2)

p2 + ggcyto_par_set(limits = "data")
p2 + labs_cyto("channel")


p3 <- p2 + ggcyto::geom_overlay(data = "CD20", size = 0.3, alpha = 0.7)


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
