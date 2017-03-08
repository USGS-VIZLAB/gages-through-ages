#' Get NM daily data
#'
#'@param viz the vizlab object 
fetch.dv_data <- function(viz = getContentInfo(viz.id = "NM_data")){
  library(dataRetrieval)
  site.id <- viz[["siteID"]]
  
  dailyData <- readNWISdv(site.id, "00060","","")
  
  saveRDS(dailyData, viz[["location"]])
    
}