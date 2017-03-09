process.doy <- function(viz = getContentInfo(viz.id = "doy-NM")){
  
  daily <- readData(viz[["depends"]][["daily"]])
  
  library(svglite)
  library(dplyr)
  library(xml2)

  doy_svg = svglite::xmlSVG({
    par(omi=c(0,0,0,0), mai=c(0.5,0.75,0,0),las=1)
    plot(1, type="n", xlab="", #frame.plot=FALSE,
         ylab="Cubic Feed Per Second", 
         xlim=c(0, 366), ylim=c(0, 16000),xaxt="n")
    axis(1, at=c(15,46,74,105,135,166,196,227,258,288,319,349),
         label = month.abb, tick = FALSE)
    axis(1, at = c(32,60,91,121,152,182,213,244,274,305,335), tick=TRUE, label=FALSE)
  }, height=0.2, width=2)
  
  
  grab_spark <- function(vals){
    
    x = svglite::xmlSVG({
      par(omi=c(0,0,0,0), mai=c(0,0,0,0))
      plot(vals, type='l', axes=F, ann=F)
    }, height=0.2, width=2)
    # x <- xml_attr(xml_find_first(x, '//*[local-name()="polyline"]'),'points')
    
  }
  

  for(i in unique(daily$Year)){
    year_data <- filter(daily, Year == i)
    #TODO: add NA to Feb.29
    x <- grab_spark(select(year_data, DayOfYear, Flow))
  }

  # 
  # polyline <- xml_children(x)[3]
  # xml_attr(polyline, "id") <- "year123"
  # xml_attr(polyline, "id")
  
  
  saveRDS(doy_svg, viz[["location"]])
  
}