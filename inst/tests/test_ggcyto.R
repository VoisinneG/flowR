library(ggcyto)
library(flowWorkspaceData)
library(flowWorkspace)
library("flowAI")

data("Bcells")
gs <- GatingSet(Bcells)
pdata_gs <- pData(gs)
pdata_gs$name <- row.names(pdata_gs)
pData(gs) <- pdata_gs
fs <- gs@data

data("GvHD")
gs <- GatingSet(GvHD)

# pb with scaling of ggcyto plots
p <- ggcyto::ggcyto(data = gs@data[1:3], aes(x=`FSC-H`, y=`SSC-A`)) + geom_hexagonal()

p <- ggcyto::ggcyto(data = gs@data[1:10], aes(x=`FSC-H`, y=`SSC-H`, color = `FSC-A`)) + geom_point()
#p <- format_plot(p=p, options = list(axis_limits=list("FSC-A" = c(0,100000))) )
#p + scale_x_logicle(limits=c(0,100000))
#p + scale_x_continuous(limits=c(0,100000))

p <- ggcyto::ggcyto(data = gs@data[1:3], aes(x=`FSC-A`, y=`SSC-A`))
p <- p + geom_hex()

p <- call_plot_function(data = gs@data[1:3], plot_type = "hexagonal", plot_args = list(xvar = "FSC-A", yvar = "SSC-A"))
p <- as.ggplot(p)
p <- p + facet_wrap(NULL)
p <- format_plot(p=p, options = list(axis_limits=list("FSC-A" = c(0,100000)),
                                     transformation = list("FSC-A" = logicle_trans())) )
p

p <- p + facet_wrap(~Patient)

samp <- read.FCS(system.file("extdata","0877408774.B08", package="flowCore"))
rectGate <- rectangleGate(filterId="nonDebris", list("FSC-H"=c(200,Inf), "SSC-H"=NULL) )
fr <- filter(samp,rectGate)
class(fr)
summary(fr)

data(GvHD)
foo <- GvHD[1:3]
fr2 <- filter(foo, rectGate)
class(fr2)
summary(fr2)

fs <- Subset(foo, rectGate)

p$coordinates$limits$x <- c("min"=0, "max" = 100000)

p <- p + scale_x_continuous(trans = logicle_trans())
p <- p + scale_y_continuous(trans = logicle_trans(), limits = c(0, 100000))

p + coord_trans(limx = c(0,100000), x = "logicle")



p <- plot_gs_ggcyto(gs, options=list(axis_limits=list("FSC-A" = c(0,100000))))

p <- plot_gs_ggcyto(gs, plot_args = list(xvar = "FSC-A", yvar = "SSC-A"))

p <- plot_gs_ggcyto(gs, plot_args = list(xvar = "FSC-A", yvar = "SSC-A"), 
                    options = list(axis_limits = list()))

p <- p + scale_x_continuous(limits = c(0,100000)) #+ geom_hex()
p + coord_cartesian(xlim = c(0,10000), ylim = NULL, default = FALSE)

### Bcells - test adding gates ############################################################
library(flowAI)
data("Bcells")
gs <- GatingSet(Bcells)

sampleNames(gs)
colnames(gs)
rg <- rectangleGate(list("FSC-A" = c(0, 100000)), filterId = "rg")

gs_pop_add(gs, rg, parent = "root")
gates <- get_gates_from_gs(gs)

gs2 <- GatingSet(gs@data)

add_gates_flowCore(gs2, gates)

#### gs_bcell_auto - test scales in ggcyto

dataDir <- system.file("extdata",package="flowWorkspaceData")
gs <- load_gs(list.files(dataDir, pattern = "gs_bcell_auto",full = TRUE))

gs_get_pop_paths(gs)
gates <- get_gates_from_gs(gs)
df <- get_plot_data(gs, sample = sampleNames(gs)[1], subset = "Live")

fs <- gs@data
gs2 <- GatingSet(fs)
add_gates_flowCore(gs2, list("g1" = list(gate = gates$`/boundary`$gate$`12828_1_Bcell_C01.fcs`, parent = "root")))
gates2 <- get_gates_from_gs(gs2)

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
#p <- ggplot(fs, aes(x=`FSC-A`, y=`SSC-A`)) + geom_point()

p <- as.ggplot(p)

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
