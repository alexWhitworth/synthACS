load("/Users/ashelton/Documents/The Call Projection Model/R_workspace_holiday")

#Get full calls
full_calls <- call_query_all()

#Order calls by date
calls <- full_calls$called[order(full_calls$called$call_date), ]

#Next, aggregate for called by date
calls_daily <- aggregate(calls$mktg_call_count, by=list(calls$call_date), FUN=sum, na.rm="TRUE")

#Rename
calls_daily$call_date <- calls_daily$Group.1
calls_daily$Group.1 <- NULL

calls_daily$Called <- calls_daily$x
calls_daily$x <- NULL

#Coerce call_date to data
library(lubridate)
calls_daily$call_date <- as.Date(calls_daily$call_date)

#Now, insert week and weekday
calls_daily$Week <- week(calls_daily$call_date)
calls_daily$weekday <- weekdays(calls_daily$call_date)

#First, merge in holidays 
##Get holiday dates from the website 'timeanddate'
library(scrapeR)

#Make Holidays a list
Holidays <- list()

for(i in 2012:year(Sys.Date())) {
  url <- paste("http://www.timeanddate.com/calendar/?year=", i, "&country=1", sep="")

  #Pull holiday dates for every year
  calendar <- scrape(url, parse = "TRUE")

  #A list of length 1 (HTML code) is output to calendar above. readHTMLTable conveniently converts this to a list sorted by the different classes 
  #in the HTML code
  calendar_1 <- readHTMLTable(calendar[[1]])

  Holidays[[i]] <- rbind(calendar_1[[17]], calendar_1[[18]], calendar_1[[19]])
  Holidays[[i]]$Year <- i
  
  if(i == 2012) {
    All_holidays <- Holidays[[i]]
  }
  
  else {
    All_holidays <- rbind(All_holidays, Holidays[[i]])
  }
}

#Get month and day
All_holidays$V1  <- as.character(All_holidays$V1)
All_holidays$Day <- substr(as.character(All_holidays$V1), nchar(as.character(All_holidays$V1)) - 1, nchar(as.character(All_holidays$V1))) 
All_holidays$Month_char <- substr(All_holidays$V1, 1, 3)

#Name months
months <- 1:12
names(months) <- month.abb

#Create month index
month_index <- matrix(0, 12, 2)
month_index[ ,1] <- names(months)
month_index[ ,2] <- as.numeric(months)
month_index <- as.data.frame(month_index)
names(month_index) <- c("Month_char", "Month_numeric")

#Merge numeric months to All_holidays
All_holidays_1 <- merge(All_holidays, month_index, by="Month_char", all.x="TRUE")

#Create date column
All_holidays_1$Date <- paste(All_holidays_1$Year, "-", All_holidays_1$Month_numeric, "-", All_holidays_1$Day, sep="")
All_holidays_1$Date <- ifelse(substr(All_holidays_1$Date, nchar(All_holidays_1$Date) - 1, nchar(All_holidays_1$Date) - 1) != " ", All_holidays_1$Date,  
                       paste(All_holidays_1$Year, "-", All_holidays_1$Month_numeric, "-0", substr(All_holidays_1$Day, nchar(All_holidays_1$Day), nchar(All_holidays_1$Day)),
                       sep=""))

#Coerce to date
All_holidays_1$Date <- as.Date(All_holidays_1$Date)

#Keep only important variables
attach(All_holidays_1)
All_holidays_2 <- as.data.frame(cbind(as.Date(Date), as.character(V2)))
names(All_holidays_2) <- c("Date", "Holiday")
detach(All_holidays_1)

#Merge back to calls daily 
calls_daily_1 <- merge(calls_daily, All_holidays_2, by.x="call_date", by.y="Date", all.x="TRUE")

#Coerce calls_daily_1 Holiday to factor
calls_daily_1$Holiday <- as.factor(calls_daily_1$Holiday)

#Year, month
calls_daily_1$Year <- year(calls_daily_1$call_date)
calls_daily_1$Month <- month(calls_daily_1$call_date)

#Aggregate calls per week by holiday (remmeber an 'NA' is just any regular day)
calls_each_week <- aggregate(calls_daily_1$Called, by=list(calls_daily_1$Year, calls_daily_1$Week), FUN=sum, na.rm="TRUE")
names(calls_each_week) <- c("Year", "Week", "weekly_calls")
      
#Merge calls each week back to calls_daily
calls_daily_2 <- merge(calls_daily_1, calls_each_week, by=c("Year", "Week"), na.rm="TRUE")

#Get perc_of_week
calls_daily_2$perc_of_week <- calls_daily_2$Called / calls_daily_2$weekly_calls

#New years issues

#For New Year's Eve (Week 53) get perc_of_week based of total weekly_calls in weeks 1-52
new_years_eve_days <- calls_daily_2[calls_daily_2$Week == 53 | calls_daily_2$Week == 1 | calls_daily_2$Week == 52,  ]
new_years_eve_days <- new_years_eve_days[new_years_eve_days$Year < year(Sys.Date()) | (new_years_eve_days$Year == year(Sys.Date()) & 
                      new_years_eve_days$Week == 1), ]

#Numeric day of week 
weekday_number <- seq(1, 7, 1)
weekday_number_days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
weekday_key <- as.data.frame(cbind(weekday_number_days, as.numeric(as.character(weekday_number))))
names(weekday_key) <- c("day_of_week", "weekday_number")

new_years_eve_days_1 <- merge(new_years_eve_days, weekday_key, by.x="weekday", by.y="day_of_week", all.x="TRUE")
new_years_eve_days_1 <- new_years_eve_days_1[order(new_years_eve_days_1$call_date), ]

new_years_eve_days_1$new_week[1] <- 1
for(i in 2:nrow(new_years_eve_days_1)) { 
  
  if(new_years_eve_days_1$weekday_number[i] == 1) {
    new_years_eve_days_1$new_week[i] <- new_years_eve_days_1$new_week[i-1] + 1
  }
  
  else {
    new_years_eve_days_1$new_week[i] <- new_years_eve_days_1$new_week[i-1]
  }

}

new_years_eve_only <- new_years_eve_days_1[as.character(new_years_eve_days_1$Holiday) == "New Year's Eve", ]
new_years_eve_only <- new_years_eve_only[is.na(new_years_eve_only$call_date) == "FALSE", ]

new_years_eve_final <- new_years_eve_days_1[new_years_eve_days_1$new_week == new_years_eve_only$new_week[1], ]
for(i in 2:nrow(new_years_eve_only)) {
  x <- new_years_eve_days_1[new_years_eve_days_1$new_week == new_years_eve_only$new_week[i], ]
  new_years_eve_final <- rbind(x, new_years_eve_final)
}

new_years_calls <- aggregate(new_years_eve_final$Called, by=list(new_years_eve_final$new_week), FUN=sum, na.rm="TRUE")

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

Holidays_full <- merge(Holidays_only, holiday_adjustment, by="Holiday", all.x="TRUE")

#Keep only columns of interest
Holidays_full_1 <- as.data.frame(cbind(Holidays_full$call_date, Holidays_full$Holiday, Holidays_full$holiday_adjustment))


