.rs.api.documentSaveAll() # ferme et sauvegarde tous les fichiers ouverts
suppressWarnings(lapply(names(sessionInfo()$otherPkgs), function(x){if(!is.null(x)){ print(x); detach(paste('package:',x,sep = ""),character.only = TRUE, unload = TRUE)}} ))# detache tous les packages
rm(list = ls(all.names = TRUE))# vide l'environneent
devtools::document('.') # genere NAMESPACE et man
devtools::load_all('.') # charge le package
#options(app.prod = FALSE) # TRUE = production mode, FALSE = development mode
#run_flowR("zenith") # lance l'application
#runApp("./")
#run before deploying app
options(repos = BiocManager::repositories(), shiny.maxRequestSize = 100*1024^2)
getOption("repos")
run_flowR()
