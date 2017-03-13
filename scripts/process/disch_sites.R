#' Get discharge sites for NWIS
#'
#'@param viz the vizlab object 
process.disch_sites <- function(viz){
  data.in <- readDepends(viz)
  
  library(dplyr)
  sites <- data.in[['disch-data']] %>% group_by(site_no) %>% 
    summarize(huc = stringr::str_sub(unique(huc_cd), 1L, 2L), 
              dec_lat_va = mean(dec_lat_va), dec_long_va = mean(dec_long_va)) %>% 
    filter(dec_long_va < -65.4) # remove US virgin Islands and other things we won't plot
  saveRDS(sites, viz[["location"]])
  
}
