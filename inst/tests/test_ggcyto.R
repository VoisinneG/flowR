library(ggcyto)
library(flowWorkspaceData)
library(flowWorkspace)
library("flowAI")
devtools::load_all()


data("Bcells")
gs <- GatingSet(Bcells)
pdata_gs <- pData(gs)
pdata_gs$name <- row.names(pdata_gs)
pData(gs) <- pdata_gs
fs <- gs@data


data("GvHD")
gs <- GatingSet(GvHD)

# pb with scaling of ggcyto plots
p <- ggcyto::ggcyto(data = gs@data[[1]], aes(x=`FL1-H`)) + 
  #geom_hexagonal()
  geom_density()

p + scale_x_logicle(t=1e4)

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

### scale flowCore
library(ncdfFlow)
library(flowCore)
samp <- read.FCS(system.file("extdata","0877408774.B08", package="flowCore"))
rectGate <- rectangleGate(filterId="nonDebris", list("FSC-H"=c(200,Inf), "SSC-H"=c(200, Inf)) )
#fr <- filter(samp,rectGate)
#class(fr)
#summary(fr)

data(GvHD)
foo <- GvHD[1:3]
#fr2 <- filter(foo, rectGate)
#class(fr2)
#summary(fr2)

fs <- Subset(foo, rectGate)
gs <- GatingSet(fs)
p <- ggcyto::ggcyto(data = gs@data[1:3], aes(x=`FSC-H`, y=`SSC-H`)) + geom_hex()
p <- ggplot(data = gs@data[1:3], aes(x=`FSC-H`, y=`SSC-H`)) + geom_hex()


#p <- as.ggplot(p)

p$coordinates$limits$x <- c("min"=0, "max" = 100000)

p <- p + scale_x_continuous(trans = logicle_trans(), limits = c(0,100000))

p <- p + scale_y_continuous(trans = logicle_trans(), limits = c(0, 100000))

p + coord_trans(limx = c(0,100000), x = "logicle")

#does not work with plot_type = "hexagonal
p <- plot_gs_ggcyto(gs, 
                    plot_type = "dots",
                    plot_args = list(xvar = "FL1-H", yvar="FL2-H", size = 3),
                    options=list(
                      axis_limits=list("FL1-H" = c(100, 100000),
                                       "FL2-H" = c(100, 100000)),
                      transformation = list("FL1-H" = asinh_trans(),
                                            "FL2-H" = asinh_trans())))


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

### Bcells - test transform ####################################################################
library(flowAI)
library(scales)
library(flowCore)
data("Bcells")
data("GvHD")
gs <- GatingSet(Bcells)
gs <- GatingSet(GvHD)

sampleNames(gs)
colnames(gs)

# transformation <- lapply(colnames(gs), function(x){
#   el <- estimateLogicle(x = gs@data[[1]], x)
#   params <- as.list(environment(el@transforms[[x]]@f))
#   params <- params[c("w", "t", "m", "a")]
#   test_trans <- try(el@transforms[[x]]@f(1), silent = TRUE)
#   if(class(test_trans) == "try-error"){
#     #params_default <- list("w"=1, "t"=1e6, "m"=7, "a"=0)
#     #do.call(logicle_trans, params_default)
#     logicle_trans()
#   }else{
#     do.call(logicle_trans, params)
#   }
# })

transformation <- lapply(colnames(gs), function(x){flowWorkspace::flowjo_biexp_trans()})
names(transformation) <- colnames(gs)


transformation_parameters <- lapply(transformation, function(tr){
  as.list(environment(tr$transform))
})

rg <- rectangleGate(list("FSC-H" = c(500, 1000)), filterId = "rg")
gs_pop_add(gs, rg, parent= "root")

# p <- ggcyto(gs@data[[1]], aes_(x=as.name("FL1-H"), y=as.name("FL2-H"))) + 
#   geom_point() +
#   geom_gate(rg)
# 
# p <- as.ggplot(p)
# p <- p + scale_x_continuous(trans=asinh_trans())
# 
# p2 <- as.ggplot(p + geom_hex() + geom_gate(rg))
# p2 + scale_x_continuous(trans=log10_trans(), limits = c(10,1000))
# 
# p1 <- p + scale_x_continuous(trans=log10_trans(), limits = c(10,1000)) +
#    scale_y_continuous(trans=log10_trans(), limits = c(10,1000))
# p1 + stat_binhex() + geom_gate(rg)
# 
# transformation[["SSC-A"]] <- identity_trans()
# transformation[["FSC-A"]] <- identity_trans()

p <- plot_gs_ggcyto(gs, gate_name = "rg", sample = sampleNames(gs)[1:3], plot_type = "dots",
               plot_args = list("xvar"= "FSC-H", "yvar" = "SSC-H", 
                                "show_outliers" = TRUE, transform_function = "log",
                                size = 1, alpha = 0.2, use_pointdensity = TRUE, adjust = 10, max_nrow_to_plot = 10000),
               options = list(
                 #"facet_var" = "name",
                 option = "viridis",
  axis_limits=list("FSC-A"=c(1e4,3e5), "SSC-A" = c(1e3,3e5)),
  transformation = transformation))

p

p <- plot_gs(gs, plot_type = "dots", 
             Ncells = 100000,
               plot_args = list("xvar"= "FSC-H", "yvar" = "SSC-H", size = 1, alpha = 0.8, use_pointdensity = TRUE), 
               options = list(
                 option = "magma",
                 axis_limits=list("FL1-H"=c(1,100000)),
                 transformation = transformation))

#### test with flowWorkspaceData datasets

library(ggcyto)
library(flowWorkspaceData)
library(flowWorkspace)

dataDir <- system.file("extdata",package="flowWorkspaceData")
list.files(dataDir)
pattern <- "gs_DC_auto"
gs <- load_gs(list.files(dataDir, pattern = pattern, full = TRUE))
gs_get_pop_paths(gs)

transformation <- lapply(colnames(gs@data), logicle_trans)
names(transformation) <- colnames(gs@data)

options <- list(
  transformation = transformation,
  axis_limits = list("FSC-A" = c(5e4, 15e4))
)

plot_gate(gate_name = "lymph", gs = gs, plot_type = "dots", options = options)


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

#p <- as.ggplot(p)

#p1 <- format_plot(p, options = list(transformation = transformation))

gate <- gs_pop_get_gate(gs, "CD19")

p2 <- add_gate_to_plot(p, gate)

p3 <- as.ggplot(p2)
p1 <- format_plot(p3, options = list(transformation = transformation))

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
