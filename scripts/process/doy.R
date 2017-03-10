visualize.doy <- function(viz = getContentInfo(viz.id = "doy-NM")){
  
  daily <- readData(viz[["depends"]][["daily"]])
  
  library(svglite)
  library(dplyr)
  library(xml2)

  doy_svg = svglite::xmlSVG({
    par(omi=c(0,0,0,0), mai=c(0.5,0.75,0,0),las=1, xaxs = "i")
    plot(1, type="n", xlab="", frame.plot=FALSE,
         ylab="Cubic Feed Per Second", mgp=c(2.5,0.25,0), 
         xlim=c(0, 366), ylim=c(0, 16000),xaxt="n")
    axis(1, at=c(15,46,74,105,135,166,196,227,258,288,319,349),
         label = month.abb, tick = FALSE)
    axis(1, at = c(32,60,91,121,152,182,213,244,274,305,335), 
         tick=TRUE, label=FALSE)
  }, height=5, width=5.0833)
  
  doy_svg <- clean_up_svg(doy_svg, viz)
  
  grab_spark <- function(vals){
    
    x = svglite::xmlSVG({
      par(omi=c(0,0,0,0), mai=c(0.5,0.75,0,0),
          las=1, mgp=c(2.5,0.25,0), xaxs = "i")
      plot(vals, type='l', axes=F, ann=F,
           xlim=c(0, 366), ylim=c(0, 16000))
    }, height=5, width=5.0833)
    
  }
  
  g.doys <- xml_add_sibling(xml_children(doy_svg)[[length(xml_children(doy_svg))]], 'g', id='dayOfYear','class'='doy-polyline')
  
  for(i in unique(daily$Year)){
    year_data <- filter(daily, Year == i) 
    #TODO: add NA to Feb.29
    
    x <- grab_spark(select(year_data, DayOfYear, Flow))
    polyline <- xml_children(x)[4]
    xml_attr(polyline, "id") <- paste0("y",i)
    xml_attr(polyline, "class") <- "doy-lines-by-year"
    xml_attr(polyline, "clip-path") <- NULL
    xml_attr(polyline, "style") <- NULL
    xml_add_child(g.doys, polyline[[1]])
  }

  write_xml(doy_svg, viz[["location"]])
  
}