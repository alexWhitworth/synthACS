#Get holidays
library(scrapeR)

Holidays <- list()

for(i in 2012:2019) {
  url <- paste("http://www.timeanddate.com/holidays/us/", i, sep="")
  
  #Pull holiday dates for every year
  calendar <- scrape(url, parse = "TRUE")
  
  # A list of length 1 (HTML code) is output to calendar above. readHTMLTable 
  # conveniently converts this to a list sorted by the different classes 
  # in the HTML code
  holidays <- as.data.frame(readHTMLTable(calendar[[1]]))
  holidays$Year <- i
  
  if(i == 2012) {
    All_holidays <- holidays
  }
  
  else {
    All_holidays <- rbind(All_holidays, holidays)
  }
}

#Names All_holidays
names(All_holidays) <- c("day_of_week", "month_and_day", "Holiday", "holiday_category", "states_celebrated", "Year")

#months 
months <- matrix(0, 12, 2)
months[1:12, 1] <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
months[1:12, 2] <- as.numeric(rep(1:12))
months <- as.data.frame(months)
names(months) <- c("Month", "month_number")
  
#Create month
All_holidays$Month <- substr(All_holidays$month_and_day, 1, 3)

#Create Day
All_holidays$Day <- as.numeric(as.character(substr(All_holidays$month_and_day, 5, 6)))

#Merge in month_number
All_holidays <- merge(All_holidays, months, by="Month", all.x="TRUE")

#Turn into date
All_holidays$Date <-  as.Date(paste(All_holidays$Year, All_holidays$month_number, All_holidays$Day, sep="-"))

All_holidays <- All_holidays[as.character(All_holidays$holiday_category) == "National holiday" | 
                 as.character(All_holidays$holiday_category) == "National holiday, Christian" |
                 (as.character(All_holidays$Holiday) == "Christmas Eve" & 
                    as.character(All_holidays$holiday_category) == "Observance, Christian") |
                 (as.character(All_holidays$Holiday) == "New Year's Eve" & 
                    as.character(All_holidays$holiday_category) == "Observance") | 
                 (as.character(All_holidays$Holiday) == "Election Day" & as.character(All_holidays$holiday_category) == "Observance"), ]

All_holidays_1 <- as.data.frame(cbind(as.character(All_holidays$Date), as.character(All_holidays$Holiday)))
names(All_holidays_1) <- c("Date", "Holiday")
All_holidays_1$Date <- as.Date(All_holidays_1$Date)

rm(All_holidays, calendar, holidays, Holidays, i, months, url)


#Limit to Uni

