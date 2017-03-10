visualize.plot_hydrographTotal <- function(viz=getContentInfo("NMHydrograhTotal-svg")){
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
  
  data_hydrograph <- data.frame(approx(data_hydrograph$DateSVG, 
                                       data_hydrograph$FlowSVG, 
                                       n = 5441))
  
  # format axis labels
  labels <- seq(1890, 2016, by=10)
  labels_dates <- as.Date(paste0(labels, "-01-01"))
  dayssince <- labels_dates - as.Date("1889-01-01")
  at <- dayssince*pixelDay
  
  total_svg <- svglite::xmlSVG({

    par(omi=c(0,0,0,0), mai=c(0.5,0.75,0,0),las=1, xaxs = "i",mgp=c(2.5,0.25,0))
    plot(data_hydrograph, type='l', axes=F, ann=F, xaxt="n",
         xlim=c(0, pixelWidth), ylim=c(0, pixelHeight))
    axis(side=1, at=at, labels=labels, cex.axis=0.6)
  }, height=pixelHeight/72, width=pixelWidth/72)
  
  total_svg <- clean_up_svg(total_svg, viz)
  pline <- xml_find_first(total_svg, '//*[local-name()="polyline"]')
  xml_remove(pline)
  xml_attr(pline,"id") <- "total-hydrograph"
  xml_attr(pline,"clip-path") <- NULL
  xml_attr(pline,"style") <- NULL
  
  xml_add_child(total_svg, pline)

  g.year_rects <- xml_add_sibling(xml_children(total_svg)[[length(xml_children(total_svg))]], 'g', id='rectYears','class'='years-rect-all')

  for(yr in names(rectangles)){
    rect_svg_all <- svglite::xmlSVG({
      par(omi=c(0,0,0,0), mai=c(0.5,0.75,0,0),las=1, xaxs = "i")
      plot(1, type="n", axes=F, ann=F, xaxt="n",
           xlim=c(0, pixelWidth), ylim=c(0, pixelHeight))
      rect(xleft=rectangles[[yr]][['xleft']],
         xright=rectangles[[yr]][['xright']],
         ybottom=0, ytop=pixelHeight, 
         border="blue", col="blue",
         ann=F, xaxt="n",
         xlim=c(0, pixelWidth), ylim=c(0, pixelHeight))
    },  height=pixelHeight/72, width=pixelWidth/72)
  

    rect_svg <- xml_children(rect_svg_all)[4]
    
    xml_set_attr(rect_svg, 'id', yr)
    xml_attr(rect_svg, "style") <- NULL
    xml_attr(rect_svg, "clip-path") <- NULL
    xml_attr(rect_svg, "class") <- "years-rect"
    # xml_set_attr(rect_svg, 'onmouseover', "evt.target.setAttribute('opacity', '0.5');")
    # xml_set_attr(rect_svg, 'onmouseout', "evt.target.setAttribute('opacity', '0');")

    xml_add_child(g.year_rects, rect_svg[[1]])
  }
  
  write_xml(total_svg, viz[["location"]])
}
