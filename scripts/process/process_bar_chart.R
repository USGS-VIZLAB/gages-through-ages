process.bar_chart <- function(viz){
  data.in <- readDepends(viz)
  
  sites <- data.in$`disch-data`
  library(dplyr)
  bars <- filter(sites, year >= viz[['min-year']], year <= viz[['max-year']]) %>% 
    group_by(year) %>% tally %>% data.frame
  
  max.sites <- max(bars$n)
  library(xml2)
  g.bars <- read_xml("<g id='year-bars'/>")
  # since all of the style and formatting stuff will happen in visualize, this will assume a 100 x 100 px plot that can be scaled and fit elsewhere. 
  w <- 100
  h <- 100
  spc <- 0.2
  bin.w <- round((w-(length(bars$n)-1)*spc)/length(bars$n),3)
  bin.h <- round(bars$n/max.sites*h, 3)
  warning('just starting on this')
  for (i in 1:length(bars$year)){
    xml_add_child(g.bars, 'rect', 
                  x = as.character((i-1)*(bin.w+spc)), 
                  height = as.character(bin.h[i]), 
                  width = as.character(bin.w), y = as.character(h - bin.h[i]), id = paste0('yr', bars$year[i]))
  }
  write_xml(x = g.bars, file = viz[['location']])
  
}