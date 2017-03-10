

site.chunk <- 1000
group.names <- 'sites-group-%s'

visualize.states_svg <- function(viz){
  data <- readDepends(viz)
  states <- data[['state-map']]
  sites <- data[['site-map']]
  watermark <- data[['watermark']]
  state.name <- as.character(row.names(states)[states@plotOrder])
  site.num <- sites$site_no # note that points sp objects don't have `plotOrder`, so we need to verify this
  
  library(svglite)
  library(sp)
  size <- apply(bbox(states), 1, diff)/500000
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
  g.sites <- xml_add_child(svg, 'g', 'id' = 'site-dots',stroke="green", 'stroke-linecap'="round", 'stroke-width'="3")
  g.watermark <- xml_add_child(svg, 'g', id='usgs-watermark', 
                               transform = sprintf('translate(2,%s)scale(0.20)', as.character(vb.num[4]-30)))


  for (i in 1:length(state.name)){
    id.name <- gsub(state.name[i], pattern = '[ ]', replacement = '_')
    xml_add_child(g.states, 'path', d = xml_attr(p[i], 'd'), id=id.name)
  }
  rm(p)
  
  library(dplyr)
  
  cxs <- as.numeric(xml_attr(c, 'cx')) %>% round(0) %>% as.character()
  cys <- as.numeric(xml_attr(c, 'cy')) %>% round(0) %>% as.character()

  chunk.s <- seq(1,by=site.chunk, to=length(cxs))
  chunk.e <- c(tail(chunk.s, -1L), length(cxs))
  if (tail(chunk.e,1) == tail(chunk.s,1)) stop("can't handle this case")
  
  for (i in 1:length(chunk.s)){
    xml_add_child(g.sites, 'path', d = paste("M",cxs[chunk.s[i]:chunk.e[i]], " ",  cys[chunk.s[i]:chunk.e[i]], "v0", collapse="", sep=''), id=sprintf(group.names, i))
  }
  
  rm(c)
  
  xml_add_child(g.watermark,'path', d=watermark[['usgs']], onclick="window.open('https://www2.usgs.gov/water/','_blank')", 'class'='watermark')
  xml_add_child(g.watermark,'path', d=watermark[['wave']], onclick="window.open('https://www2.usgs.gov/water/','_blank')", 'class'='watermark')
  
  write_xml(svg, viz[['location']])
  
}

process.time_json <- function(viz){
  data <- readDepends(viz)
  library(dplyr)
  sites <- data[['site-map']]
  sites.w.data <- readr::read_csv('cache/allSitesYears_355.csv')
  chunk.s <- seq(1,by=site.chunk, to=length(sites))
  chunk.e <- c(tail(chunk.s, -1L), length(sites))
  json.out <- list()
  for (i in 1:length(chunk.s)){
    chunk <- list()
    for (yr in viz[['min-year']]:viz[['max-year']]){
      sites.n.chunk <- sites$site_no[chunk.s[i]:chunk.e[i]]
      sites.yr <- sites.w.data %>% filter(year == yr ) %>% .$site_no
      tmp <- list(which(sites.n.chunk %in% sites.yr)) #which of the sites 
      names(tmp) <- yr
      chunk <- append(chunk, tmp)
    }
    tmp <- list(chunk)
    names(tmp) <- sprintf(group.names, i)
    json.out <- append(json.out, tmp)
  }
  json.text <- jsonlite::toJSON(json.out)
  cat(json.text, file = viz[['location']])
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