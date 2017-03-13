process.plot_hydrographTotal <- function(viz=getContentInfo("NMHydrograhTotal-svg")){
  
  library(dplyr)
  library(svglite)
  library(xml2)
  
  rawData <- readData(viz[['depends']][['data']])
  height <- viz[['height']]
  width <- viz[['width']]
  maxYear <- viz[["maxYear"]]
  minYear <- viz[["minYear"]]
  
  rawData <- data.frame(approx(rawData$Date, 
                               rawData$Flow, 
                               n = nrow(rawData)/viz[["full-desample"]]))

  yearRange <- c(minYear, maxYear)
  
  yMax <- 1.2*max(rawData$x, na.rm = TRUE)
  
  total_svg <- svglite::xmlSVG({

    par(omi=c(0,0,0,0), mai=c(0.5,0.75,0,0),
        las=1, xaxs = "i",mgp=c(2.5,0.25,0))
    plot(1, type='n', axes=F, ann=F, xaxt="n",
         xlim=yearRange, 
         ylim=c(0, yMax))
    axis(side=1, cex.axis=0.6)
  }, height=height, width=width)
  
  r <- xml_find_all(total_svg, '//*[local-name()="rect"]')
  defs <- xml_find_all(total_svg, '//*[local-name()="defs"]')
  xml_remove(defs)
  
  text <- xml_find_all(total_svg, '//*[local-name()="text"]')
  xml_attr(text, "style") <- NULL
  xml_attr(text, "textLength") <- NULL
  xml_attr(text, "lengthAdjust") <- NULL
  xml_attr(text, "class") <- "axis-labels"
  
  # clean up junk that svglite adds:
  .junk <- lapply(r, xml_remove)

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
    
    xml_set_attr(rect_svg, 'id', paste0("b",yr))
    xml_attr(rect_svg, "style") <- NULL
    xml_attr(rect_svg, "clip-path") <- NULL
    xml_attr(rect_svg, "class") <- "years-rect"
    xml_attr(rect_svg, "onmouseover") <- paste0("vizlab.showline('",yr,"')")

    xml_add_child(g.year_rects, rect_svg[[1]])
  }
  
  g.totalPoly <- xml_add_sibling(xml_children(total_svg)[[length(xml_children(total_svg))]], 
                                 'g', id='totalHydro','class'='total-hydrograph')
  
  hydro_lines <- svglite::xmlSVG({

    par(omi=c(0,0,0,0), mai=c(0.5,0.75,0,0),las=1, xaxs = "i")
    plot(rawData, 
         type="l", axes=F, ann=F, xaxt="n")
  }, height=height, width=width)
  
  pline <- xml_find_first(hydro_lines, '//*[local-name()="polyline"]')
  xml_remove(pline)
  xml_attr(pline,"class") <- "total-hydrograph"
  xml_attr(pline,"clip-path") <- NULL
  xml_attr(pline,"style") <- NULL
  
  xml_add_child(g.totalPoly, pline)
  
  xml_name(total_svg, ns = character()) <- "g"
  xml_attr(total_svg, "xmlns") <- NULL
  xml_attr(total_svg, "viewBox") <- NULL
  xml_attr(total_svg, "preserveAspectRatio") <- NULL
  xml_attr(total_svg, "xmlns:xlink") <- NULL
  
  write_xml(total_svg, viz[["location"]])
}
