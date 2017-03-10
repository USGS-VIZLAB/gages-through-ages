visualize.plot_hydrographTotal <- function(viz){
  dataList <- readData(viz[['depends']][['dailySmoothSVG']])
  data <- dataList[['data']]
  pixelDay <- dataList[['pixelDay']]
  rects <- readData(viz[['depends']][['rectangles']])
  pixelHeight <- viz[['pixelHeight']]
  pixelWidth <- viz[['pixelWidth']]
  
  library(dplyr)
  library(svglite)
  library(xml2)
  
  data_hydrograph <- dplyr::select(data, DateSVG, FlowSVG)
  
  # format axis labels
  labels <- seq(1890, 2016, by=10)
  labels_dates <- as.Date(paste0(labels, "-01-01"))
  dayssince <- labels_dates - as.Date("1889-01-01")
  at <- dayssince*pixelDay
  
  total_svg <- svglite::xmlSVG({
    par(omi=c(0,0,0,0), mai=c(0.3,0,0,0),las=1,xaxs="i")
    plot(data_hydrograph, type='l', axes=F, ann=F, xaxt="n")
    axis(side=1, at=at, labels=labels, cex.axis=0.6)
  }, height=pixelHeight/72, width=pixelWidth/72)
  total_svg <- clean_up_svg(total_svg, viz)
  
  svgaxis <- xml_children(total_svg)
  
  hydrograph <- xml_children(total_svg)[3]
  xml_set_attr(total_svg, 'fill', "none")
  xml_set_attr(total_svg, 'stroke', "black")

  for(yr in names(rects)){
    rect <- xml_add_child(total_svg, 'rect', 
                          x=rects[[yr]][['x']], 
                          width=rects[[yr]][['width']], 
                          height=rects[[yr]][['height']], 
                          id=yr)
    xml_set_attr(rect, 'fill', "blue")
    xml_set_attr(rect, 'stroke', "blue")
    xml_set_attr(rect, 'stroke-width', "0.5")
    xml_set_attr(rect, 'opacity', "0")
    xml_set_attr(rect, 'onmouseover', "evt.target.setAttribute('opacity', '0.5');")
    xml_set_attr(rect, 'onmouseout', "evt.target.setAttribute('opacity', '0');")
  }
  
  write_xml(total_svg, viz[["location"]])
}
