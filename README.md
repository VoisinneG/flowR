R package: flowR
================
Guillaume Voisinne
2019 - 12 - 03


flowR
========

An interface for the analysis of flow cytometry data using R

Install
-------

### Rstudio Setup for Windows users (inside CML only)

To install flowR, you will first need to install R and Rstudio.
If you use windows 10, you can get and Rstudio using the 'Software Center':

- go to 'Software Center'
- download the latest version of R
- download Rstudio
- open Rstudio

Now we need to create a R library folder with read/write permissions.
To do that using R studio, type in the console :

    dir.create("C:/Temp/R_library")
    cat(".libPaths('C:/Temp/R_library')\n", file = paste0(getwd(), "/.Rprofile"))


Now close and restart Rstudio.
By default, all your packages will be saved in `C:/Temp/R_library`.

### Install from github

Install package `BiocManager` and `devtools` using

    install.packages(c('BiocManager', 'devtools'))
    
Make sure to set package repositories to their latest versions using:

    options(repos = BiocManager::repositories())

The package is available from github. Install it using the **devtools** package:

    devtools::install_github("VoisinneG/flowR")

Use
---

To run the graphical user interface, type the following command in the R console:

    library("flowR")
    flowR::run_flowR()

Demo app
---

The demo app (no installation required) can be accessed [here](https://voisinneg.shinyapps.io/flowR/)