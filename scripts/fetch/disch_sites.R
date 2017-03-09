#' Get discharge sites for NWIS
#'
#'@param viz the vizlab object 
fetch.disch_sites<- function(viz){
  library(dataRetrieval)
  hucs <- stringr::str_pad(1:21, width = 2, pad = "0")
  sites <- readNWISdv(site.id, "00060","","")
  
  saveRDS(dailyData, viz[["location"]])
  
}