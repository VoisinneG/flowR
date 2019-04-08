
BiocManager::install("flowCore", version = "3.8")
library(flowCore)


dir_name <- "~/Documents/Data/BMMlab/DATA_BMM_GV/CyTOF/2018-11-23-Itsn2-CRISPR-Long/Data/"

files = list.files(dir_name, pattern = "\\.fcs$")
dat <- read.flowSet( paste(dir_name, files, sep = ""), which.lines = 1:3000)

chnls <- rep("", length(dat))
for(i in 1:length(dat)){
  chnls[i] <- paste(parameters(dat[[i]])$name, collapse = "; ")
}


df_files <- data.frame(files = files, chanels = chnls)
