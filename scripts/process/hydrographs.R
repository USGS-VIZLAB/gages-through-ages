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
  data[['Flow.smooth']] <- runmean(data[['Flow']], smooth.days, endrule = "constant", align="left")
  
  saveRDS(data, file=viz[["location"]])
}


process.hydrographDataSVG <- function(viz=getContentInfo("processedNMdailySVG")){
  data <- readData(viz[['depends']][['dailyData']])
  pixelHeight <- viz[['pixelHeight']]
  pixelWidth <- viz[['pixelWidth']]
  
  pixelCfs <- round(pixelHeight/max(data[['Flow.smooth']]), 5)
  pixelDay <- getWidthOfDay(data$Date, pixelWidth)
  
  data <- dplyr::mutate(data, daysince=as.numeric(Date-min(Date)))
  
  data[['DateSVG']] <- data[['daysince']]*pixelDay
  data[['FlowSVG']] <- data[['Flow.smooth']]*pixelCfs
  
  dataList <- list(data = data, pixelDay = pixelDay)
  
  saveRDS(dataList, file=viz[['location']])
}

process.hydrographRectangles <- function(viz){
  dataList <- readData(viz[['depends']][['dailyDataSVG']])
  data <- dataList[['data']]
  
  library(dplyr)
  allYrs <- unique(data[['Year']])
  
  data <- group_by(data, Year)
  data <- summarize(data, x=min(DateSVG), width=max(DateSVG) - min(DateSVG))
  data <- mutate(data, x=as.character(x), width=as.character(width), height="100%")
  
  rect_specs <- by(data, 1:nrow(data), as.list)
  names(rect_specs) <- paste0("y", allYrs)
  
  saveRDS(rect_specs, file=viz[["location"]])
}

#' handle date converion to SVG coords
#' 
#' @param date_vec a vector of all dates to use
#' @param plot_width a single numeric value indicating the width
#' of the plot in pixels.
#' 
#' @return SVG coords
getWidthOfDay <- function(date_vec, plot_width){
  total_num_days <- as.numeric(max(date_vec) - min(date_vec))
  pixels_per_day <- plot_width/total_num_days
  return(pixels_per_day)
}
