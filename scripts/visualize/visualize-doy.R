visualize.doy <- function(viz = getContentInfo(viz.id = "doy-NM")){

  library(svglite)
  library(dplyr)
  library(xml2)
    
  daily <- readData(viz[["depends"]][["daily"]])
  zoomer <- readData(viz[["depends"]][["zoomer"]])
  watermark <- readData(viz[["depends"]][['watermark']])
  
  zoomer.xml <- read_xml(zoomer)
  zoom.vb <- xml_attr(zoomer.xml, "viewBox")
  zoom.vb <- as.numeric(strsplit(zoom.vb, ' ')[[1]])
  xml_attr(zoomer.xml, "viewBox") <- NULL
  
  height <- viz[["height"]]
  width <- viz[["width"]]
  yMax <- max(daily$Flow, na.rm = TRUE)
  
  doy_svg = svglite::xmlSVG({
    par(omi=c(0,0,0,0), mai=c(0.5,0.55,0,0),las=1, xaxs = "i")
    plot(1, type="n", xlab="", frame.plot=FALSE,
         ylab= "",
         mgp=c(2.5,0.25,0), 
         xlim=c(0, 366), ylim=c(0, yMax),axes = FALSE)
    mtext(month.abb, 1, line = 0,
          at=c(15,46,74,105,135,166,196,227,258,288,319,349)) #midpoint of months
    axis(1, labels = FALSE, lwd.ticks = 0, at=c(0,366))
    axis(1, labels = FALSE, 
         at=c(32,60,91,121,152,182,213,244,274,305,335)) #first day of months
    axis(2, at=axTicks(2), labels=format(axTicks(2),  big.mark=',',scientific=FALSE), 
         lwd.ticks = 0, mgp=c(2.5,0.25,0))
    axis(2, labels = FALSE, lwd.ticks = 0, at=c(par("usr")[3:4]))
  }, height=height, width=width)
  
  #################
  r <- xml_find_all(doy_svg, '//*[local-name()="rect"]')
  .junk <- lapply(r, xml_remove)
  
  defs <- xml_find_all(doy_svg, '//*[local-name()="defs"]')
  .junk <- xml_remove(defs)
  
  text <- xml_find_all(doy_svg, '//*[local-name()="text"]')
  .junk <- xml_remove(text)
  
  xml_attr(text, "style") <- NULL
  xml_attr(text, "textLength") <- NULL
  xml_attr(text, "lengthAdjust") <- NULL

  g.labels <- xml_add_sibling(xml_children(doy_svg)[[length(xml_children(doy_svg))]], 
                                 'g', 'class'='axis-labels svg-text')
  
  for(i in text){
    xml_add_child(g.labels, i)
  }
  
  g.lines <- xml_add_sibling(xml_children(doy_svg)[[length(xml_children(doy_svg))]], 
                              'g', 'class'='axis-lines')
  
  axis_lines <- xml_find_all(doy_svg, '//*[local-name()="line"]')
  xml_remove(axis_lines)
  xml_attr(axis_lines, "style") <- NULL
  
  for(i in axis_lines){
    xml_add_child(g.lines, i)
  }
  
  #################
  xml_name(doy_svg, ns = character()) <- "g"
  
  vb_doy <- xml_attr(doy_svg, "viewBox")
  vb_doy <- as.numeric(strsplit(vb_doy, ' ')[[1]])
  
  xml_attr(doy_svg, "viewBox") <- NULL
  
  xml_attr(doy_svg, 'transform') <- sprintf("translate(0,%s)", zoom.vb[4])
  
  grab_spark <- function(vals, yMax, height, width){
    
    x = svglite::xmlSVG({
      par(omi=c(0,0,0,0), mai=c(0.5,0.55,0,0),
          las=1, mgp=c(2.5,0.25,0), xaxs = "i")
      plot(vals, type='l', axes=F, ann=F,
           xlim=c(0, 366), ylim=c(0, yMax))
    }, height=height, width=width)
    
  }
  
  g.doys <- xml_add_sibling(xml_children(doy_svg)[[length(xml_children(doy_svg))]], 'g', id='dayOfYear','class'='doy-polyline')
  
  yMax <- max(daily$Flow, na.rm = TRUE)
  maxYear <- viz[["maxYear"]]
  minYear <- viz[["minYear"]]
  
  years <- unique(daily$Year)
  years <- years[years <= maxYear]
  years <- years[years >= minYear]
  
  for(i in years){
    year_data <- filter(daily, Year == i) 
    
    if((i%%4 == 0) & ((i%%100 != 0) | (i%%400 == 0))){
      year_data$DayOfYear[year_data$DayOfYear>=60] <- year_data$DayOfYear[year_data$DayOfYear>=60]+1
    }
    
    sub_data <- data.frame(approx(year_data$DayOfYear, 
                                  year_data$Flow, n = 366/viz[["full-desample"]]))
    
    x <- grab_spark(sub_data, yMax, height, width)
    polyline <- xml_find_first(x, '//*[local-name()="polyline"]')
    xml_attr(polyline, "id") <- paste0("doy_",i)
    xml_attr(polyline, "class") <- "doy-lines-by-year"
    xml_attr(polyline, "clip-path") <- NULL
    xml_attr(polyline, "style") <- NULL
    xml_attr(polyline, "viewBox") <- NULL
    xml_add_child(g.doys, polyline)
  }
  
  
  ################
  root <- xml_new_document() %>% xml_add_child("svg")
  xml_add_child(root, doy_svg)
  
  xml_attr(root, "preserveAspectRatio") <- "xMidYMid meet"
  xml_attr(root, "xmlns") <- 'http://www.w3.org/2000/svg'
  xml_attr(root, "xmlns:xlink") <- 'http://www.w3.org/1999/xlink'
  xml_attr(root, "id") <- viz[["id"]]
  
  xml_add_sibling(xml_children(root)[[1]], 'desc', .where='before', viz[["alttext"]])
  xml_add_sibling(xml_children(root)[[1]], 'title', .where='before', viz[["title"]])
  ################
  
  xml_add_child(root, zoomer.xml)

  vb_overall <- zoom.vb
  vb_overall[4] <- vb_doy[4] + zoom.vb[4]
  
  xml_attr(root, "viewBox") <- paste(vb_overall, collapse=' ')
  
  g.watermark <- xml_add_child(root, 'g', id='usgs-watermark', 
                               transform = sprintf('translate(50,%s)scale(0.20)', 
                                                   as.character(zoom.vb[4]-3)))
  xml_add_child(g.watermark,'path', d=watermark[['usgs']], onclick="window.open('https://www2.usgs.gov/water/','_blank')", 'class'='watermark')
  xml_add_child(g.watermark,'path', d=watermark[['wave']], onclick="window.open('https://www2.usgs.gov/water/','_blank')", 'class'='watermark')
  
  write_xml(root, viz[["location"]])
  
}