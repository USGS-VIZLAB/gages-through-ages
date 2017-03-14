#check if start or end years should be counted for complete record sites
checkCompleteYears <- function(df) {
  #get num days in first year  - to drop (unless started in Jan 1-10)
  startYearDays <- 365 - yday(df$begin_date)
  #get num days in last year - to drop (unless ended in Dec 21-31)
  endYearDays <- yday(df$end_date)
  countStartYear <- startYearDays >= 355
  countEndYear <- endYearDays >= 355
  
  library(tidyr)
  library(dplyr)

  yearRange <- c(as.POSIXlt(min(df$begin_date, na.rm = TRUE))$year,
                 as.POSIXlt(max(df$end_date, na.rm = TRUE))$year)+1900

  df$startYear <- as.POSIXlt(df$begin_date)$year + 1900
  df$endYear <- as.POSIXlt(df$end_date)$year + 1900

  df$startYear[!countStartYear] <- df$startYear[!countStartYear] + 1
  df$endYear[!countEndYear] <- df$endYear[!countEndYear] - 1

  longOK <- data.frame()

  for(i in yearRange[1]:yearRange[2]){
    sub_year <- filter(df,
                       startYear <= i &
                       endYear >= i)
    if(nrow(sub_year) > 0){
        siteDF <- data.frame(site_no=sub_year$site_no,
                             year = rep(i, nrow(sub_year)),
                             stringsAsFactors = FALSE)
        longOK <- bind_rows(longOK, siteDF)
    }
  }
  
  #make long DF of site and year
  # longOK <- data.frame()
  # for(i in 1:nrow(df)) {
  #   endYear <- year(df$end_date[i])
  #   beginYear <- year(df$begin_date[i])
  #   years <- seq(beginYear + 1, endYear - 1) #exclude start and end
  #   if(countStartYear[i]) {years <- append(years, beginYear)}
  #   if(countEndYear[i]) {years <-  append(years, endYear)}
  # 
  #   siteDF <- data.frame(site_no=rep(df$site_no[i], length(years)), year = years,
  #                        stringsAsFactors = FALSE)
  #   longOK <- bind_rows(longOK, siteDF)
  # }
  return(longOK)
  
}

#find which sites already downloaded
checkDownloadedSites <- function(dir) {
  dlSites <- character(0)
  #check every file in directory
  for(f in list.files(dir, full.names = TRUE)) {
    fileDF <- fread(f, select = c("site_no", "Date"), 
                colClasses = c("site_no"="character", "Date"="character"))
    fileSites <- unique(fileDF$site_no)
    dlSites <- append(dlSites, fileSites)
  }
  return(dlSites)
}


#look at a complete site DV record, get years
yearsFunc <- function(files) {
  library(data.table)
  library(dplyr)
  library(lubridate)
  allIncompleteYears <- data.frame()
  for(f in files) {
    print(paste("starting", f))
    chunk <- fread(f, select = c("site_no", "Date"), 
                   colClasses = c("site_no"="character", "Date"="character"))
    incompleteYears <- data.frame()
    #for(s in unique(chunk$site_no)) {
      #siteDF <- filter(chunk, site_no == s)
      #need to actually check number of days
      #unique days of each year - then summarize with n()
      uniqueDF <- chunk[!duplicated(chunk),]#old chunks might have downloaded sites > once
      uniqueDF$year <- year(uniqueDF$Date)
      uniqueGrp <- group_by(uniqueDF, site_no, year)
      summaryDF <- filter(summarize(uniqueGrp, nDays = n()), nDays >= 355)
      if(any(summaryDF$nDays > 366)){cat(paste('ERRROR > 366', f), file = "ERRORLOG.txt")}
      #out <- data.frame(site_no = s, year = unique(year(siteDF$Date)))
      #incompleteYears <- bind_rows(incompleteYears, out)
    #}
    allIncompleteYears <- bind_rows(allIncompleteYears, summaryDF)
    print(paste("finished", f))
  }
  return(allIncompleteYears)
}