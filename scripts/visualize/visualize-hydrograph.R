visualize.plot_hydrographTotal <- function(viz){
  data <- readData(viz[['depends']][['dailySmoothSVG']])
  rects <- readData(viz[['depends']][['rectangles']])
  
  library(svglite)
  library(xml2)
  
  data_hydrograph <- dplyr::select(data, DateSVG, FlowSVG)
  
  total_svg <- svglite::xmlSVG({
    par(omi=c(0,0,0,0), mai=c(0.5,0.75,0,0),las=1)
    plot(data_hydrograph, type='l', axes=F, ann=F,
         xlim=c(0, 366), ylim=c(0, 50), xaxt="n")
  }, height=3, width=10)
  total_svg <- clean_up_svg(total_svg, viz)
  
  n1 <- names(rects)[1]
  r1 <- rects[[1]]
  
  xml_add_child(total_svg, 'rect', x=r1[['x']], width=r1[['width']], 
                height=r1[['height']], id=n1)
  
  # polyline <- xml2::xml_children(hydrographTotal)[4]
    
  
  write_xml(total_svg, viz[["location"]])
}
