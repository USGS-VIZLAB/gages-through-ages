process.year_json <- function(viz){
  data.in <- readDepends(viz)
  library(dplyr)
  sites <- data.in[['site-map']]
  sites.w.data <- data.in[["disch-data"]]
  
  chunk.s <- seq(1,by=site.chunk, to=length(sites))
  chunk.e <- c(tail(chunk.s, -1L), length(sites))
  json.out <- list()
  for (i in 1:length(chunk.s)){
    chunk <- list()
    for (yr in viz[['min-year']]:viz[['max-year']]){
      sites.n.chunk <- sites$site_no[chunk.s[i]:chunk.e[i]]
      sites.yr <- sites.w.data %>% filter(year == yr ) %>% .$site_no
      now.i <- which(sites.n.chunk %in% sites.yr)
      if (yr == viz[['min-year']]){
        tmp <- list(list(gn = now.i, ls = numeric()))
      } else {
        #last year's data:
        last.yr <- sites.w.data %>% filter(year == yr-1 ) %>% .$site_no
        last.i <- which(sites.n.chunk %in% last.yr)
        gained <- now.i[!now.i %in% last.i]
        lost <- last.i[!last.i %in% now.i]
        tmp <- list(list(gn = gained, ls = lost))
      }
      
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