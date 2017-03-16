#Run this independent of vizlab - will require 8 + hours of DL time
#generates csv output currently retrieved from sciencebase
#one part is parellized

#get # years of record for every site
library(dplyr)
library(stringr)
library(data.table)
library(dataRetrieval)
library(lubridate)
library(parallel)

source('scripts/fetch/helperFunctions.R')

dir.create('chunks',showWarnings = FALSE)

#loop over HUC regions
hucs <- str_pad(1:21, width = 2, pad = "0")
setAccess("internal")
allDF <- data.frame()
for(h in hucs) {
  #dataRet call
  hucDF <- select(readNWISdata(huc = h, hasDataTypeCd = "dv", service = "site",
                        seriesCatalogOutput = TRUE, parameterCd = "00060"),
                  agency_cd, site_no, site_tp_cd, station_nm, dec_lat_va, dec_long_va,
                  huc_cd, data_type_cd, parm_cd, begin_date, end_date, count_nu) 
                  
  hucDF <- filter(hucDF, data_type_cd == "dv", parm_cd == "00060")
  
  hucDF <- mutate(hucDF, begin_date = as.character(begin_date), 
                  end_date=as.character(end_date))
  #filter & append
  allDF <- bind_rows(allDF, hucDF)
}

#convert to dates
allDF <- mutate(allDF, begin_date = as.Date(begin_date), 
                end_date = as.Date(end_date),
                dayRange = as.numeric(end_date - begin_date),
                intDaysRecord  = count_nu - 2, #assuming record exists on first and last days
                intDaysAll = end_date - begin_date - 1,
                diff = intDaysAll - intDaysRecord)
# x <- allDF[c(duplicated(allDF$site_no),duplicated(allDF$site_no, fromLast = TRUE)),]
#get sites where ratio days/ years is off
saveRDS(allDF, "allDF.rds")
allDF <- readRDS("allDF.rds")

completeSiteDF <- filter(allDF, 
                         diff <= 10,
                         count_nu > 355) #can't have less than 355 days in any non-start/end year
incompleteSiteDF <- filter(allDF, diff > 10)
incompleteSiteDF <- filter(incompleteSiteDF, count_nu >= 355)


#want long df with row for each site/year
#For complete sites, check if start and end years should be counted
longOK <- checkCompleteYears(completeSiteDF)
longOK <- longOK[!duplicated(longOK),]
# fwrite(longOK, file = 'sitesYearsComplete_latLon_355_NEW.csv')

# #need to deal with multiple lines for some sites in allDF
# allDF_oneLineSite <- allDF[!duplicated(allDF$site_no),]
# longOK_join <- left_join(longOK, allDF_oneLineSite, by = "site_no")
# 
# fwrite(longOK_join, file = 'sitesYearsComplete_latLon_355.csv')

#get data for incomplete sites
#check what sites I already downloaded - save 6000 some sites
dlSites <- checkDownloadedSites('old_chunks')
toDownloadDF <- filter(incompleteSiteDF, !site_no %in% dlSites)
#don't repeat - there are sites with multiple measurement points
toDownloadSites <- unique(toDownloadDF$site_no)

#chunk by 200 sites
reqBks <- seq(1,length(toDownloadSites),by=200)

for(i in reqBks) {
  sites <- na.omit(toDownloadSites[i:(i+199)])
  print(paste('starting', i))
  
  all_sites <- tryCatch({
    currentSitesDF <- readNWISdv(siteNumber = sites, parameterCd = "00060")
    fwrite(currentSitesDF, file = file.path('chunks', paste0('newChunk', i)))
  },
  error=function(cond) {
    message("***************Errored on",i,"***********\n")
    return(all_sites)
  })
  print(paste("Finished sites", i, "through", i+199))
}

completeFromIncomplete <- data.frame()
for(i in files){
  complete <- yearsFunc(i)
  completeFromIncomplete <- bind_rows(completeFromIncomplete, complete)
}

cl <- makeCluster(4)

files <- list.files(c('chunks','old_chunks'), full.names = TRUE)
allIncompleteYears <- clusterApply(cl, fun = yearsFunc, x = files)
stopCluster(cl)

#reassemble to DF, write
allIncompleteDF <- do.call("bind_rows", allIncompleteYears)
incomplete_lat_lon <- left_join(allIncompleteDF, allDF_oneLineSite, by = "site_no")

fwrite(incomplete_lat_lon, file = "incomplete_lat_lon_355.csv")
allSites_355 <- bind_rows(longOK_join, incomplete_lat_lon)
fwrite(allSites_355, file = "allSitesYears_355.csv", quote = TRUE)

