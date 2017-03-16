process.plot_hydrographTotal <- function(viz=getContentInfo("NMHydrograhTotal-svg")){
  
  library(dplyr)
  library(svglite)
  library(xml2)
  
  rawData <- readData(viz[['depends']][['data']])
  height <- viz[['height']]
  width <- viz[['width']]
  maxYear <- viz[["maxYear"]]
  minYear <- viz[["minYear"]]
  
  date_chunks <- split(rawData$Date, cumsum(c(0, diff(rawData$Date)>=2)))
  flow_chunks <- split(rawData$Flow, cumsum(c(0, diff(rawData$Date)>=2)))
  
  yearRange <- c(minYear, maxYear)
  
  yMax <- 1.2*max(rawData$Flow, na.rm = TRUE)
  
  total_svg <- svglite::xmlSVG({

    par(omi=c(0,0,0,0), mai=c(0.5,0.75,0,0),
        las=1, xaxs = "i",mgp=c(2.5,0.25,0))
    plot(1, type='n', axes=F, ann=F, xaxt="n",
         xlim=yearRange, 
         ylim=c(0, yMax))
    axis(side=1, cex.axis=0.6)
  }, height=height, width=width)
  
  r <- xml_find_all(total_svg, '//*[local-name()="rect"]')
  .junk <- lapply(r, xml_remove)
  
  defs <- xml_find_all(total_svg, '//*[local-name()="defs"]')
  xml_remove(defs)
  
  text <- xml_find_all(total_svg, '//*[local-name()="text"]')
  xml_remove(text)
  xml_attr(text, "style") <- NULL
  xml_attr(text, "textLength") <- NULL
  xml_attr(text, "lengthAdjust") <- NULL
  
  g.labels <- xml_add_sibling(xml_children(total_svg)[[length(xml_children(total_svg))]], 
                              'g', 'class'='axis-labels svg-text')
  
  for(i in text){
    xml_add_child(g.labels, i)
  }

  lines <- xml_find_all(total_svg, '//*[local-name()="line"]')
  .junk <- xml_remove(lines)
  
  g.year_rects <- xml_add_sibling(xml_children(total_svg)[[length(xml_children(total_svg))]], 'g', id='rectYears','class'='years-rect-all')

  for(yr in c(yearRange[1]:yearRange[2])){
    rect_svg_all <- svglite::xmlSVG({
      par(omi=c(0,0,0,0), mai=c(0.5,0.75,0,0),
          las=1, xaxs = "i")
      plot(1, type="n", axes=F, ann=F, xaxt="n",
           xlim=yearRange, ylim=c(0, yMax))
      
      rect(xleft=yr,
         xright=yr+1,
         ybottom=0, ytop=yMax)
    },  height=height, width=width)
  
    rects <- xml_find_all(rect_svg_all, '//*[local-name()="rect"]')
    rect_svg <- rects[length(rects)]
    
    xml_set_attr(rect_svg, 'id', paste0("r_",yr))
    xml_attr(rect_svg, "style") <- NULL
    xml_attr(rect_svg, "clip-path") <- NULL
    xml_attr(rect_svg, "class") <- "years-rect"

    xml_add_child(g.year_rects, rect_svg[[1]])
  }
  
  g.totalPoly <- xml_add_sibling(xml_children(total_svg)[[length(xml_children(total_svg))]], 
                                 'g', id='totalHydro')
  
  full_range <- as.Date(c(paste0(yearRange[1],"-01-01"),
                          paste0(yearRange[2],"-12-31")))
  
  for(i in 1:length(date_chunks)){
    x <- date_chunks[[i]]
    y <- flow_chunks[[i]]
    
    chunk_data <- data.frame(approx(x, y, 
                                   n = length(x)/viz[["full-desample"]]))
    chunk_data$x <- as.Date(chunk_data$x, origin="1970-01-01")
    
    hydro_lines <- svglite::xmlSVG({
  
      par(omi=c(0,0,0,0), mai=c(0.5,0.75,0,0),las=1, xaxs = "i")
      plot(chunk_data, xlim=full_range,ylim=c(0, yMax),
           type="l", axes=F, ann=F, xaxt="n")
    }, height=height, width=width)
    
    pline <- xml_find_first(hydro_lines, '//*[local-name()="polyline"]')
    xml_remove(pline)
    xml_attr(pline,"clip-path") <- NULL
    xml_attr(pline,"style") <- NULL
    
    xml_add_child(g.totalPoly, pline)
  }
  
  xml_name(total_svg, ns = character()) <- "g"
  xml_attr(total_svg, "xmlns") <- NULL
  xml_attr(total_svg, "preserveAspectRatio") <- NULL
  xml_attr(total_svg, "xmlns:xlink") <- NULL
  xml_attr(total_svg, "class") <- 'total-hydrograph'
  write_xml(total_svg, viz[["location"]])
}
