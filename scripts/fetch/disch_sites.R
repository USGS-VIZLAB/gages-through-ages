#' Get discharge sites for NWIS
#'
#'@param viz the vizlab object 
fetch.disch_sites <- function(viz){
  library(dataRetrieval)
  hucs <- stringr::str_pad(1:21, width = 2, pad = "0")
  
  sites <- data.frame(stringsAsFactors = FALSE)
  for (huc in hucs){
    resp <- dataRetrieval::whatNWISsites(huc = huc, parameterCd = "00060", outputDataTypeCd = "dv")
    sites <- rbind(sites, cbind(resp, data.frame(stringsAsFactors = FALSE, huc=huc)))
  }
  
  sites <- dplyr::filter(sites, dec_long_va < -65.4) # remove US virgin Islands and other things we won't plot
  saveRDS(sites, viz[["location"]])
  
}
