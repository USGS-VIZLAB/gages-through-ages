process.bar_chart <- function(viz){
  data.in <- readDepends(viz)
  
  sites <- data.in$`disch-data`
  library(dplyr)
  bars <- filter(sites, year >= viz[['min-year']], year <= viz[['max-year']]) %>% 
    group_by(year) %>% tally %>% data.frame
  
  # max.sites <- max(bars$n)
  # library(xml2)
  # g.bars <- read_xml("<g id='year-bars'/>")
  # warning('just starting on this')
  # for (bar in bars$year){
  #   xml_add_child(g.bars, 'rect', x)
  # }
  saveRDS(bars, file = viz[['location']])
  
}