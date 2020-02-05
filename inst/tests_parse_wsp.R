library(CytoML)
library(openCyto)
library(xml2)


# Parse flowJO workspace

ws <- read_xml("../flowR_utils/demo-data/JL04BMVLG-Valentin/Tumor-testFlowR.wsp")

# parse spillover matrices from FlowJo wsp
ws_path <- "../flowR_utils/demo-data/JL04BMVLG-Valentin/Tumor-testFlowR.wsp"

ws <- read_xml(ws_path)

spill_list <- get_spillover_matrices_from_ws("../flowR_utils/demo-data/JL04BMVLG-Valentin/Tumor-testFlowR.wsp")

# parse gates
gates <- get_gates_from_ws(ws_path = "../flowR_utils/demo-data/JL04BMVLG-Valentin/Tumor-testFlowR.wsp", 
                           group = "All Samples")

gs <- parseWorkspace(ws,
                     name = "All Samples",
                     execute = TRUE,
                     isNcdf = TRUE,
                     sampNloc = "sampleNode")

# import diva worspace

ws <- openDiva("./data/FE03BMGV/FE03BMGV.xml")
getSampleGroups(ws)
getSamples(ws)
gs <- parseWorkspace(ws,
                     name = c("Ankrd13a", "Ablim1"),
                     execute = TRUE,
                     isNcdf = TRUE,
                     sampNloc = "sampleNode")

#Parse diva xml
xt <- read_xml("../flowR_utils/demo-data/JL04BMVLG-Valentin/JL04BMVLG.xml")
xt <- read_xml("~/test_diva_comp/JA22BMMVLG.xml")

settingsNodes <-  xml_find_first(xt, ".//instrument_settings")
parameters <- xml_find_all(settings, ".//parameter")

templates <-  xml_find_all(xt, ".//worksheet_template")
gatesNode <- xml_find_first(wtNode[[2]], "//gates")
gate_set <- xml_find_all(gatesNode, ".//gate")

spill_list <- get_spillover_matrices_from_ws_diva("~/test_diva_comp/JA22BMMVLG.xml")

