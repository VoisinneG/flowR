.rs.api.documentSaveAll() # ferme et sauvegarde tous les fichiers ouvert
suppressWarnings(lapply(names(sessionInfo()$otherPkgs), function(x){if(!is.null(x)){ print(x); detach(paste('package:',x,sep = ""),character.only = TRUE, unload = TRUE)}} ))# detache tous les packages
rm(list = ls(all.names = TRUE))# vide l'environneent
devtools::document('.') # genere NAMESPACE et man
devtools::load_all('.') # charge le package
#options(app.prod = FALSE) # TRUE = production mode, FALSE = development mode
shiny::runApp('./') # lance l'application
