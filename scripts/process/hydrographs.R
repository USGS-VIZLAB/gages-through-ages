process.hydrographTotal <- function(viz, smooth.days){
  dischargeTS <- readData(viz[['depends']][['dischargeTS']])
  
  
  
  saveRDS(dischargeTS, file=viz[["location"]])
}

process.hydrographByYear <- function(viz){
  library(dataRetrieval)
  
  dailyData <- readData(viz[['depends']][['dailyData']])
  dailyData <- renameNWISColumns(dailyData)
  
  dailyData[['DayOfYear']] <- as.numeric(format(dailyData[['Date']], "%j"))
  dailyData[['Year']] <- as.numeric(format(dailyData[['Date']], "%Y"))
  
  saveRDS(dailyData, file=viz[["location"]])
}
