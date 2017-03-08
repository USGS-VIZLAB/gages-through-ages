process.hydrographTotal <- function(viz){
  library(dataRetrieval)
  library(caTools)
  
  dailyData <- readData(viz[['depends']][['dailyData']])
  smooth.days <- viz[['smooth.days']]

  dailyData <- renameNWISColumns(dailyData)
  
  dailyData[['Flow.smooth']] <- runmean(dailyData[['Flow']], smooth.days, endrule = "constant", align="left")
  
  saveRDS(dailyData, file=viz[["location"]])
}

process.hydrographByYear <- function(viz){
  library(dataRetrieval)
  
  dailyData <- readData(viz[['depends']][['dailyData']])
  dailyData <- renameNWISColumns(dailyData)
  
  dailyData[['DayOfYear']] <- as.numeric(format(dailyData[['Date']], "%j"))
  dailyData[['Year']] <- as.numeric(format(dailyData[['Date']], "%Y"))
  
  saveRDS(dailyData, file=viz[["location"]])
}
