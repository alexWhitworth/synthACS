holiday_scrape <- function(beg_year, end_year) {
  require(scrapeR) 
  require(stringr)
  
  # 01. pre-allocate data structures
  #----------------------------------------
  months <- data.frame(month= c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                       month_number= 1:12)

  # 02. scrape holidays
  #----------------------------------------
  for(i in beg_year:end_year) {
    # pull in holidays for a given year
    cal <- as.data.frame(readHTMLTable(scrape(paste0("http://www.timeanddate.com/holidays/us/", i), 
                          parse=TRUE)[[1]], stringsAsFactors= FALSE))
    cal$year <- i
    
    if (i == beg_year) {
      holidays <- cal
    } else {
      holidays <- rbind(holidays, cal)
    } 
  }
    
  # Munging - dates
  names(holidays) <- c("date", "week_day", "holiday", "holiday_cat", "states_celebrated", "year")
  holidays$month <- str_sub(holidays$date, 1, 3)
  holidays$day <- as.numeric(str_sub(holidays$date, 4))
  holidays <- merge(holidays, months, all.x= TRUE)
  holidays$date2 <- as.Date(paste(holidays$year, holidays$month_number, holidays$day, sep= "-"), format= "%Y-%m-%d")
  
  
  # 03. Subset specific holiday categories
  #----------------------------------------
  # avoiding duplicates
  holidays_sub <- holidays[holidays$holiday_cat %in% c("National holiday", "National holiday, Christian") |
                          (holidays$holiday == "Christmas Eve" & holidays$holiday_cat == "Observance, Christian") | 
                          (holidays$holiday == "New Year's Eve" & holidays$holiday_cat == "Observance, Christian") |
                          (holidays$holiday == "Election Day"), c("date2", "year", "month", "week_day", "holiday")]
  
  # return
  return(holidays_sub[!duplicated(holidays_sub), ])
}


num_wks <- function(days, sun_indices) {
  if((length(days) - (sun_indices[1] -1)) %% 7 == 0 ) {
    wks <- (length(days) - (sun_indices[1] -1)) / 7 + 1
    week_num <- c(rep(1, (sun_indices[1]-1)), rep(2:wks, each= 7))
  } else {
    wks <- floor((length(days) - (sun_indices[1] -1)) / 7) + 2
    week_num <- c(rep(1, (sun_indices[1]-1)), rep(2:(wks-1), each= 7), 
                  rep(wks, length(days) - (wks - 1) * 7 + sun_indices[1] - 2))
  }
  return(week_num)
}
