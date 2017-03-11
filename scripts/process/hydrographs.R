process.hydrographData <- function(viz){
  library(dataRetrieval)
  library(dplyr)
  library(caTools)
  
  data <- readData(viz[['depends']][['dailyData']])
  smooth.days <- viz[['smooth.days']]
  
  data <- renameNWISColumns(data)
  data <- arrange(data, Date)
  
  data[['DayOfYear']] <- as.numeric(format(data[['Date']], "%j"))
  data[['Year']] <- as.numeric(format(data[['Date']], "%Y"))
  
  saveRDS(data, file=viz[["location"]])
}




