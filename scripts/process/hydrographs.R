process.hydrographTotal <- function(viz){
  library(dataRetrieval)
  library(dplyr)
  library(caTools)
  
  dailyData <- readData(viz[['depends']][['dailyData']])
  smooth.days <- viz[['smooth.days']]
  dailyData <- renameNWISColumns(dailyData)
  dailyData <- arrange(dailyData, Date)
  
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
  dailySmoothSVG <- readData(viz[['depends']][['dailySmoothSVG']])
  
  library(dplyr)
  data <- mutate(dailySmoothSVG, yr = format(Date, "%Y"))
  data <- group_by(data, yr)
  data <- summarize(data, 
                    x=min(DateSVG),
                    width=max(DateSVG) - min(DateSVG))
  data <- mutate(data, x=as.character(x), width=as.character(width),
                 height="100%")
  allYrs <- data$yr
  rect_specs <- by(data, 1:nrow(data), as.list)
  names(rect_specs) <- paste0("y", allYrs)
  
  # 
  # widthOfPlotRegion <- 366
  # allYrs <- unique(as.numeric(format(dailySmooth[['Date']], "%Y")))
  # firstYr <- min(allYrs)
  # # lastYr <- max(allYrs)
  # lastYr <- 2016
  # totalNumYrs <- length(allYrs)
  # widthOfRect <- round(widthOfPlotRegion/totalNumYrs, digits=3)
  # 
  # xvals <- seq(0, by=widthOfRect, length.out=totalNumYrs)
  
  # rect_specs <-lapply(xvals, function(x, widthOfRect) {
  #   rect_args <- list(x=as.character(x),
  #                     width=as.character(widthOfRect),
  #                     height="100%")
  #   return(rect_args)
  # }, widthOfRect)
  
  saveRDS(rect_specs, file=viz[["location"]])
}

process.hydrographSVGLand <- function(viz){
  data <- readData(viz[['depends']][['dailySmooth']])
  
  heightOfPlotRegion <- 50
  widthOfPlotRegion <- 366
  heightOfOneFlow <- round(heightOfPlotRegion/max(data[['Flow.smooth']]), 5)
  widthOfDay <- 366/as.numeric(max(data$Date) - min(data$Date))

  data <- dplyr::mutate(data, daysince=as.numeric(Date-min(Date)))
  
  data[['DateSVG']] <- data[['daysince']]*widthOfDay
  data[['FlowSVG']] <- data[['Flow.smooth']]*heightOfOneFlow

  saveRDS(data, file=viz[['location']])
}
