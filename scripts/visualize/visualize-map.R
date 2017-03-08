

visualize.states_svg <- function(viz = getContentInfo(viz.id = "states.svg")){
  data <- readDepends(viz)
  states <- data[['state-map']][['states']]
  
  watermark <- data[['watermark']]
  state.name <- as.character(row.names(states)[states@plotOrder])
  library(svglite)
  library(sp)
  size <- apply(bbox(states), 1, diff)/500000
  svg <- svglite::xmlSVG({
    par(mai=c(0,0,0,0), omi=c(0,0,0,0))
    sp::plot(states, ylim=bbox(states)[2,], xlim=bbox(states)[1,], setParUsrBB = TRUE)
  }, width = size[1], height = size[2])
  
  library(xml2)
  vb.num <- as.numeric(strsplit(xml_attr(svg, 'viewBox'),'[ ]')[[1]])
  p <- xml_find_all(svg, '//*[local-name()="path"]')
  if (length(p) != (length(states))){
    stop('something is wrong, the number of states and polys is different')
  }
  
  defs <- xml_add_child(svg, 'defs')
  
  g.states <- xml_add_child(svg, 'g', 'id' = 'state-polygons')
  g.watermark <- xml_add_child(svg, 'g', id='usgs-watermark',transform=sprintf('translate(2,%s)scale(0.20)', as.character(vb.num[4]-30)))


  for (i in 1:length(state.name)){
    id.name <- gsub(state.name[i], pattern = '[ ]', replacement = '_')
    xml_add_child(g.states, 'path', d = xml_attr(p[i], 'd'), id=id.name)

  }
  xml_add_child(g.watermark,'path', d=watermark[['usgs']], onclick="window.open('https://www2.usgs.gov/water/','_blank')", 'class'='watermark')
  xml_add_child(g.watermark,'path', d=watermark[['wave']], onclick="window.open('https://www2.usgs.gov/water/','_blank')", 'class'='watermark')
  
  xml_remove(p)
  
  write_xml(svg, viz[['location']])
  
}

#' do the things to the svg that we need to do every time if they come from svglite:
#' 
#' @param svg the svglite xml object from svglite
#' @param viz the vizlab object
#' 
#' @return a modified version of svg
clean_up_svg <- function(svg, viz){
  # let this thing scale:
  xml_attr(svg, "preserveAspectRatio") <- "xMidYMid meet"
  xml_attr(svg, "xmlns") <- 'http://www.w3.org/2000/svg'
  xml_attr(svg, "xmlns:xlink") <- 'http://www.w3.org/1999/xlink'
  xml_attr(svg, "id") <- "map-svg"
  
  r <- xml_find_all(svg, '//*[local-name()="rect"]')
  
  xml_add_sibling(xml_children(svg)[[1]], 'desc', .where='before', viz[["alttext"]])
  xml_add_sibling(xml_children(svg)[[1]], 'title', .where='before', viz[["title"]])
  
  defs <- xml_find_all(svg, '//*[local-name()="defs"]')
  # !!---- use these lines when we have css for the svg ---!!
  xml_remove(defs)
  
  # clean up junk that svglite adds:
  .junk <- lapply(r, xml_remove)
  return(svg)
}