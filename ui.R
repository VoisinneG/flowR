library(shiny)
library(BiocManager)
library("flowR")

options(repos = BiocManager::repositories(), shiny.maxRequestSize = 1000*1024^2)
flowR_ui2()