

site.chunk <- 1000
group.names <- 'sites-group-%s'

size_map_svg <- function(sp){
  apply(sp::bbox(sp), 1, diff)/500000
}

visualize.map_thumbnail <- function(viz){
  library(dplyr)
  
  data <- readDepends(viz)
  states <- data[['state-map']]
  sites <- data[['site-map']]
  bars <- data[['bar-data']]
  height <- viz[['fig-height']]
  width <- viz[['fig-width']]
  
  png(filename = viz[['location']], width = width, height = height, units = 'px')
  createThumbnailPlot(states, sites, bars)
  dev.off()
}

visualize.states_svg <- function(viz){
  data <- readDepends(viz)
  states <- data[['state-map']]
  sites <- data[['site-map']]
  watermark <- data[['watermark']]
  bars <- data[['bar-data']]
  state.name <- as.character(row.names(states)[states@plotOrder])
  site.num <- sites$site_no # note that points sp objects don't have `plotOrder`, so we need to verify this
  
  library(svglite)
  library(sp)
  size <- size_map_svg(states)
  svg <- svglite::xmlSVG({
    par(mai=c(0,0,0,0), omi=c(0,0,0,0))
    sp::plot(states, ylim=bbox(states)[2,], xlim=bbox(states)[1,], setParUsrBB = TRUE)
    sp::plot(sites, add=TRUE, pch = 20, col='red')
  }, width = size[1], height = size[2])
  
  library(xml2)
  svg <- clean_up_svg(svg, viz)
  vb.num <- as.numeric(strsplit(xml_attr(svg, 'viewBox'),'[ ]')[[1]])
  p <- xml_find_all(svg, '//*[local-name()="path"]')
  c <- xml_find_all(svg, '//*[local-name()="circle"]')
  xml_remove(p)
  xml_remove(c)
  if (length(p) != (length(states))){
    stop('something is wrong, the number of states and polys is different')
  }
  if (length(c) != (length(sites))){
    stop('something is wrong, the number of sites and circles is different')
  }
  
  defs <- xml_add_child(svg, 'defs')
  
  g.states <- xml_add_child(svg, 'g', 'id' = 'state-polygons')
  g.sites <- xml_add_child(svg, 'g', 'id' = 'site-dots')
  g.watermark <- xml_add_child(svg, 'g', id='usgs-watermark', 
                               transform = sprintf('translate(2,%s)scale(0.20)', as.character(vb.num[4]+15)))

  
  for (i in 1:length(state.name)){
    id.name <- gsub(state.name[i], pattern = '[ ]', replacement = '_')
    class <- ifelse(state.name[i] %in% c('AK','HI','PR'), 'exterior-state','interior-state')
    xml_add_child(g.states, 'path', d = xml_attr(p[i], 'd'), id=id.name, class=class)
  }
  rm(p)
  
  library(dplyr)
  
  cxs <- as.numeric(xml_attr(c, 'cx')) %>% round(0) %>% as.character()
  cys <- as.numeric(xml_attr(c, 'cy')) %>% round(0) %>% as.character()

  chunk.s <- seq(1,by=site.chunk, to=length(cxs))
  chunk.e <- c(tail(chunk.s, -1L), length(cxs))
  if (tail(chunk.e,1) == tail(chunk.s,1)) stop("can't handle this case")
  
  for (i in 1:length(chunk.s)){
    xml_add_child(g.sites, 'path', 
                  d = paste("M",cxs[chunk.s[i]:chunk.e[i]], " ",  cys[chunk.s[i]:chunk.e[i]], "v0", collapse="", sep=''), 
                  id=sprintf(group.names, i), class='site-dot')
  }
  
  rm(c)
  
  xml_add_child(g.watermark,'path', d=watermark[['usgs']], onclick="window.open('https://www2.usgs.gov/water/','_blank')", 'class'='watermark')
  xml_add_child(g.watermark,'path', d=watermark[['wave']], onclick="window.open('https://www2.usgs.gov/water/','_blank')", 'class'='watermark')
  
  bars.xml <- read_xml(bars)
  
  svg <- add_bar_chart(svg, bars.xml)
  write_xml(svg, viz[['location']])
  
}

add_bar_chart <- function(svg, bars){
  vb <- as.numeric(strsplit(xml_attr(svg, "viewBox"), '[ ]')[[1]])
  xml_attr(bars, 'transform') <- sprintf("translate(0,%s)", vb[4])
  
  h <- xml_find_all(xml_children(bars)[1], '//*[local-name()="rect"]') %>% xml_attr('height') %>% as.numeric() %>% max
  vb[4] <- vb[4] + h

  xml_attr(svg, "viewBox") <- paste(vb, collapse=' ')
  
  xml_add_child(svg, bars)
  return(svg)
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
  xml_attr(svg, "id") <- viz[["id"]]
  
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

#' Create the actual map + bars separately from saving image
#' 
#' @param states spatial polygon of state outlines
#' @param sites spatial polygon of site locations
#' @param bars filepath to the xml file with bar data
#' 
#' @result a plot
createThumbnailPlot <- function(states, sites, bars){
  library(xml2)
  par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  sp::plot(states, expandBB = c(0.8,0,0,1.5))
  sp::plot(sites, add=TRUE, pch = 20, cex=0.05)
  
  bars.xml <- xml2::read_xml(bars) %>% xml_child()
  rects <- xml_children(bars.xml)
  xleft <- xml_attr(rects, 'x') %>% as.numeric()
  ys <- xml_attr(rects, 'y') %>% as.numeric()
  ytop <- max(ys) - ys
  ybottom <- 0
  xright <- xml_attr(rects, 'width') %>% as.numeric %>%  + xleft

  par(new=TRUE, mar=c(0,0,0,0), oma=c(3,0,0,0))
  plot(0,NA, xlim = c(0,tail(xright,1)), ylim = c(0, max(ys)), axes=FALSE , ylab="", xlab="")
  rect(xleft, ybottom, xright, ytop, col='dodgerblue', border = NA)
  
}
