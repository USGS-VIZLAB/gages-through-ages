visualize.plot_hydrographTotal <- function(viz){
  dataList <- readData(viz[['depends']][['dailySmoothSVG']])
  data <- dataList[['data']]
  pixelDay <- dataList[['pixelDay']]
  rectangles <- readData(viz[['depends']][['rectangles']])
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
    plot(data_hydrograph, type='l', axes=F, ann=F, xaxt="n",
         xlim=c(0, pixelWidth), ylim=c(0, pixelHeight))
    axis(side=1, at=at, labels=labels, cex.axis=0.6)
  }, height=pixelHeight/72, width=pixelWidth/72)
  total_svg <- clean_up_svg(total_svg, viz)
  
  xml_set_attr(total_svg, 'fill', "none")
  xml_set_attr(total_svg, 'stroke', "black")

  # for(yr in names(rectangles)){
  for(yr in names(rectangles)[1]){
    rect_svg_all <- svglite::xmlSVG({
      par(omi=c(0,0,0,0), mai=c(0.3,0,0,0),las=1,xaxs="i")
      plot.new()
      rect(xleft=rectangles[[yr]][['xleft']],
         xright=rectangles[[yr]][['xright']],
         ybottom=0, ytop=pixelHeight, 
         border="blue", col="blue",
         ann=F, xaxt="n",
         xlim=c(0, pixelWidth), ylim=c(0, pixelHeight))
    },  height=pixelHeight/72, width=pixelWidth/72)
  
    rect_svg <- xml_children(rect_svg_all)[4]
    
    xml_set_attr(rect_svg, 'id', yr)
    xml_set_attr(rect_svg, 'stroke-width', "0.5")
    xml_set_attr(rect_svg, 'opacity', "0")
    xml_set_attr(rect_svg, 'onmouseover', "evt.target.setAttribute('opacity', '0.5');")
    xml_set_attr(rect_svg, 'onmouseout', "evt.target.setAttribute('opacity', '0');")

    xml_add_child(total_svg, rect_svg[[1]])
  }
  
  write_xml(total_svg, viz[["location"]])
}
