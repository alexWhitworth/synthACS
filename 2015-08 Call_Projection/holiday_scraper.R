holiday_scrape <- function(beg_year, end_year) {
  require(scrapeR) 
  require(stringr)
  
  # 01. pre-allocate data structures
  #----------------------------------------
  months <- data.frame(month= c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                       month_number= 1:12)
  cal <- vector(mode= "list")
  
  # 02. scrape holidays
  #----------------------------------------
  for(i in beg_year:end_year) {
    # pull in holidays for a given year
    cal[[i - beg_year + 1]] <- as.data.frame(readHTMLTable(scrape(paste0("http://www.timeanddate.com/holidays/us/", i), 
                          parse=TRUE)[[1]], stringsAsFactors= FALSE))
    # mung dates
    cal[[i - beg_year + 1]]$year  <- i
    cal[[i - beg_year + 1]]$month <- str_sub(cal[[i - beg_year + 1]][,1], 1, 3)
    cal[[i - beg_year + 1]]$day   <- as.numeric(str_sub(cal[[i - beg_year + 1]][,1], 4))
    
    # add black friday
    cal[[i - beg_year + 1]] <- add_black_fri(cal[[i - beg_year + 1]]) 
    names(cal[[i - beg_year + 1]]) <- c("date", "week_day", "holiday", "holiday_cat", "states_celebrated", "year", "month", "day")
  }
  holidays <- data.table::rbindlist(cal)
    
  # Munging - dates
  holidays <- merge(holidays, months, by= "month", all.x= TRUE)
  holidays$date2 <- as.Date(paste(holidays$year, holidays$month_number, holidays$day, sep= "-"), format= "%Y-%m-%d")
  
  
  # 03. Subset specific holiday categories
  #----------------------------------------
  # avoiding duplicates
  holidays <- as.data.frame(holidays)
  holidays_sub <- holidays[holidays$holiday_cat %in% c("National holiday", "National holiday, Christian", "Consumerism") |
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


add_black_fri <- function(x) {
  tg <- x[which(x[,3] == "Thanksgiving Day"), ]
  tg_day <- tg$day
  yr <- tg$year
  black_fri <- c(paste("Nov", tg_day + 1), "Friday", "Black Friday", "Consumerism", "", yr, "Nov", tg_day + 1)
  return(data.frame(rbind(x, black_fri)))
}