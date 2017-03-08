process.hydrographTotal <- function(viz, smooth.days){
  dischargeTS <- readData(viz[['depends']][['dischargeTS']])
  
  
  
  saveRDS(dischargeTS, file=viz[["location"]])
}

process.hydrographByYear <- function(viz){
  dischargeTS <- readData(viz[['depends']][['dischargeTS']])
  
  dischargeTS[['DayOfYear']] <- as.numeric(format(dischargeTS[['Date']], "%j"))
  dischargeTS[['Year']] <- as.numeric(format(dischargeTS[['Date']], "%Y"))
  
  saveRDS(dischargeTS, file=viz[["location"]])
}
