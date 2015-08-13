Holiday_normalization <- function(prediction_month, prediction_year, calls) {
  # [AW 7/31] Replace all calls to aggregate w/ dplyr()
  require(dplyr)
  require(lubridate)
  
  # 02. Basic munging -- dates, pull in holidays, do some aggregation
  #--------------------------------------------
  calls_daily <- calls %>% group_by(call_date) %>%
    summarize(Called= sum(mktg_call_count, na.rm=TRUE))
  
  calls_daily$call_date <- as.Date(calls_daily$call_date)
  calls_daily$Week <- week(calls_daily$call_date)
  calls_daily$weekday <- weekdays(calls_daily$call_date)
  
  #Restrict calls to only calls over the training period to learn
  calls_daily <- calls_daily[(month(calls_daily$call_date) <  prediction_month & 
                                year(calls_daily$call_date) ==  prediction_year) | 
                               (year(calls_daily$call_date) <  prediction_year), ]
  
  
  # merge in holidays -- [AW 7/31] replaced code with holiday_scrape()  -- pull in with package
  calls_daily <- merge(calls_daily, holiday_scrape(beg_year= 2012, end_year= (year(Sys.Date()) + 1)), 
                         by.x="call_date", by.y="date", all.x="TRUE")
  
  calls_daily$holiday <- as.factor(calls_daily$holiday)
    
  #Aggregate calls per week and merge back to original
  calls_each_week <- calls_daily %>% group_by(year, Week) %>%
    summarize(weekly_calls= sum(Called, na.rm= TRUE))
  
  calls_daily <- merge(calls_daily, calls_each_week, by=c("year", "Week"), na.rm="TRUE")
  calls_daily$perc_of_week <- calls_daily$Called / calls_daily$weekly_calls

  # 03. Compensate for NYE issues 
  #-------------------------------------------------  
  #For New Year's Eve (Week 53) get perc_of_week based of total weekly_calls in weeks 1-52
  new_years_eve_days <- calls_daily[calls_daily$Week == 53 | calls_daily$Week == 1 | calls_daily$Week == 52, ]
  new_years_eve_days <- new_years_eve_days[new_years_eve_days$Year < year(Sys.Date()) | 
                          (new_years_eve_days$Year == year(Sys.Date()) & new_years_eve_days$Week == 1), ]
  
  # merge in key for day of week 
  weekday_key <- data.frame(day_of_week= c("Monday", "Tuesday", "Wednesday", "Thursday", 
                              "Friday", "Saturday", "Sunday"), weekday_number= 1:7)
  new_years_eve_days <- merge(new_years_eve_days, weekday_key, by.x="weekday", by.y="day_of_week", all.x="TRUE")
  new_years_eve_days <- new_years_eve_days[order(new_years_eve_days$call_date), ]
  

  # Number weeks --- [AW 7/31] put function in package
  new_years_eve_days$week_num <- num_wks(new_years_eve_days$weekday_number, 
                                           which(new_years_eve_days$weekday_number == 1))
  
  # subset to NYE
  new_years_eve_only <- new_years_eve_days[as.character(new_years_eve_days$Holiday) == "New Year's Eve" &
                                               !is.na(new_years_eve_days$call_date), ]
  
  ####### [AW 7/31] Where I am
  new_years_eve_final <- new_years_eve_days[new_years_eve_days$week_num == new_years_eve_only$week_num[1], ]
  for(i in 2:nrow(new_years_eve_only)) {
    x <- new_years_eve_days[new_years_eve_days$week_num == new_years_eve_only$week_num[i], ]
    new_years_eve_final <- rbind(x, new_years_eve_final)
  }
  
  new_years_calls <- aggregate(new_years_eve_final$Called, by=list(new_years_eve_final$week_num), FUN=sum, na.rm="TRUE")
  
  new_years_dates <- new_years_eve_final[(new_years_eve_final$Holiday == "New Year's Eve" | new_years_eve_final$Holiday == "New Year's Day"), ]
  new_years_dates <- new_years_dates[is.na(new_years_dates$Called) == "FALSE", ]                                  
  
  new_years_calls <- merge(new_years_dates, new_years_calls, by.x="new_week", by.y="Group.1", all.x="TRUE")
  
  new_years_calls_1 <- as.data.frame(cbind(as.Date(new_years_calls$call_date), new_years_calls$Week, new_years_calls$x))
  names(new_years_calls_1) <- c("call_date", "Week", "weekly_called")
  
  #Merge new years call number to calls
  calls_daily_2 <- merge(calls_daily_2, new_years_calls_1, by="call_date", all.x="TRUE")
  calls_daily_2$weekly_calls <- ifelse(is.na(calls_daily_2$weekly_called) == "TRUE", calls_daily_2$weekly_calls, calls_daily_2$weekly_called)  
  
  #Now, make perc again with weekly_calls 
  calls_daily_2$perc_of_week <- calls_daily_2$Called / calls_daily_2$weekly_calls
  calls_daily_2$Week.y <- NULL
  calls_daily_2$Week <- calls_daily_2$Week.x
  calls_daily_2$Week.x <- NULL

  # 04. XXX
  #--------------------------------------------------------
  
  #Name NA days 'regular'
  calls_daily_2$Holiday <- ifelse(is.na(calls_daily_2$Holiday) == "TRUE", "regular", as.character(calls_daily_2$Holiday))
  
  #Now, get call curve for all Holidays and regular days 
  call_perc_avg <- aggregate(calls_daily_2$perc_of_week, by=list(calls_daily_2$Holiday, calls_daily_2$weekday), FUN=mean, na.rm="TRUE")
  call_perc_avg <- call_perc_avg[order(call_perc_avg$Group.1, call_perc_avg$Group.2), ]
  
  #Rename call_perc_avg groups
  names(call_perc_avg) <- c("Holiday", "day_of_week", "perc_of_weekly_total")
  
  #Merge numeric day_of_week to weekly call curve by Holiday
  call_perc_avg_1 <- merge(call_perc_avg, weekday_key, by="day_of_week", na.rm="TRUE")
  
  #Regular days only
  reg_days <- call_perc_avg_1[as.character(call_perc_avg_1$Holiday) == "regular", ]
  reg_days <- reg_days[order(reg_days$weekday_number), ]
  
  #Order Holidays
  call_perc_avg_1 <- call_perc_avg_1[order(call_perc_avg_1$Holiday), ]
  
  #Ratio of calls on holidays vs. same weekdays
  call_perc_holidays_only <- call_perc_avg_1[as.character(call_perc_avg_1$Holiday) != "regular", ]
  calls_raw_reg <- call_perc_avg_1[as.character(call_perc_avg_1$Holiday) == "regular", ]
  
  #Merge regular call volumes to holidays
  call_perc_avg_2 <- merge(call_perc_holidays_only, calls_raw_reg, by=c("weekday_number"), all.x="TRUE")
  call_perc_avg_2$day_of_week.y <- NULL
  call_perc_avg_2$Holiday.y <- NULL
  call_perc_avg_2$perc_of_weekly_total_reg <- call_perc_avg_2$perc_of_weekly_total.y 
  call_perc_avg_2$perc_of_weekly_total.y <- NULL
  names(call_perc_avg_2) <- c("weekday_number", "day_of_week", "Holiday", "perc_of_weekly_total_Holiday", "perc_of_weekly_total_reg")
  
  #Get average Holiday vs. regular
  mean_perc_holidays <- aggregate(call_perc_avg_2$perc_of_weekly_total_Holiday, by=list(call_perc_avg_2$Holiday), FUN=mean, na.rm="TRUE")
  mean_perc_reg <- aggregate(call_perc_avg_2$perc_of_weekly_total_reg, by=list(call_perc_avg_2$Holiday), FUN=mean, na.rm="TRUE") 
  
  #Merge to get holiday adjustment 
  holiday_adjustment <- merge(mean_perc_holidays, mean_perc_reg, by=c("Group.1"), na.rm="TRUE")
  
  #Merge back call dates
  Holidays_only <- calls_daily_2[as.character(calls_daily_2$Holiday) != "regular", ]
  Holidays_only$Year <- NULL
  Holidays_only$Month <- NULL
  Holidays_only$Called <- NULL
  Holidays_only$weekday <- NULL
  Holidays_only$weekly_calls <- NULL
  Holidays_only$perc_of_week <- NULL
  Holidays_only$Week <- NULL
  Holidays_only$Year <- NULL
  holiday_adjustment$holiday_adjustment <- holiday_adjustment$x.x / holiday_adjustment$x.y 
  holiday_adjustment$x.x <- NULL
  holiday_adjustment$x.y <- NULL
  names(holiday_adjustment) <- c("Holiday", names(holiday_adjustment[2]))
  
  Holiday_adjustment <- merge(Holidays_only, holiday_adjustment, by="Holiday", all.x="TRUE")
  Holiday_adjustment <- Holiday_adjustment[!duplicated(Holiday_adjustment$Holiday), ]
  Holiday_adjustment <- as.data.frame(cbind(Holiday_adjustment$Holiday, Holiday_adjustment$holiday_adjustment))
  names(Holiday_adjustment) <- c("Holiday", "holiday_adjustment")
  
  #Finally enumerate holiday months
  month_names_1 <- substr(month.name, 1, 3)
  month_names_2 <- seq(1, 12, 1)
  
  month_names <- as.data.frame(cbind(month_names_1, month_names_2))
  
  #Merge month numbers into All_holidays
  All_holidays_1 <- merge(All_holidays, month_names, by.x="Month_char", by.y="month_names_1", all.x="TRUE")
  
  All_holidays_1 <- as.data.frame(Holiday= as.character(All_holidays_1$V2), Year= All_holidays_1$Year, 
                                  Month= as.numeric(as.character(All_holidays_1$month_names_2)), Day= All_holidays_1$Day)
  
  return(list("Holiday_adjustment" = Holiday_adjustment, 
              "calls_daily" = calls_daily, "All_holidays" = All_holidays, 
              "month_names" = month_names, "All_holidays_1" = All_holidays_1))
}