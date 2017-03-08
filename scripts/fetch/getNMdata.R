#' Get NM daily data
#'
#'@param viz the vizlab object 
fetcher.getNMdata <- function(viz = getContentInfo(viz.id = "getNMdata")){
  library(dataRetrieval)
  site.id <- viz[["siteID"]]
  
  dailyData <- readNWISdv(site.id, "00060","","")
  
  saveRDS(dailyData, viz[["location"]])
    
}