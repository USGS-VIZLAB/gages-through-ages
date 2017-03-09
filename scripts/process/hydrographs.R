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

process.hydrographRectangles <- function(viz){
  dailySmooth <- readData(viz[['depends']][['dailySmooth']])
  
  heightOfRect <- 50
  widthOfPlotRegion <- 366
  allYrs <- unique(as.numeric(format(dailySmooth[['Date']], "%Y")))
  firstYr <- min(allYrs)
  # lastYr <- max(allYrs)
  lastYr <- 2016
  totalNumYrs <- length(allYrs)
  widthOfRect <- round(widthOfPlotRegion/totalNumYrs, digits=3)
  
  xvals <- seq(0, by=widthOfRect, length.out=totalNumYrs)
  
  rect_specs <-lapply(xvals, function(x, widthOfRect, heightOfRect) {
    rect_args <- c(x=x, width=widthOfRect, height=heightOfRect)
    return(rect_args)
  }, widthOfRect, heightOfRect)
  names(rect_specs) <- paste0("y", allYrs)
  
  saveRDS(rect_specs, file=viz[["location"]])
}
