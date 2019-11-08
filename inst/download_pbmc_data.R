library(readxl)
url <- "http://imlspenticton.uzh.ch/robinson_lab/cytofWorkflow"
metadata_filename <- "PBMC8_metadata.xlsx"
download.file(paste0(url, "/", metadata_filename), destfile = paste0("./data/PBMC/",metadata_filename),
              mode = "wb")
md <- read_excel(paste0("./data/PBMC/",metadata_filename))

## Make sure condition variables are factors with the right levels
md$condition <- factor(md$condition, levels = c("Ref", "BCRXL"))
head(md)

fcs_filename <- "PBMC8_fcs_files.zip"
download.file(paste0(url, "/", fcs_filename), destfile = paste0("./data/PBMC/", fcs_filename), 
              mode = "wb")

