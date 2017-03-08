#' Get NM daily data
#'
#'@param viz the vizlab object 
fetcher.get_dv_data <- function(viz = getContentInfo(viz.id = "get_NM_data")){
  library(dataRetrieval)
  site.id <- viz[["siteID"]]
  
  dailyData <- readNWISdv(site.id, "00060","","")
  
  saveRDS(dailyData, viz[["location"]])
    
}