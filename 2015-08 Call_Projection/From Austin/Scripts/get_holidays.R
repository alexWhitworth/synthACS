##Example to get 2014 holiday dates from the website 'timeanddate'
library(scrapeR)

for(i in 2013:2012) {
  url <- paste("http://www.timeanddate.com/calendar/?year=", 2012, "&country=1", sep="")
}

#Pull holiday dates for every year
calendar <- scrape(url="http://www.timeanddate.com/calendar/", parse = "TRUE")

#A list of length 1 (HTML code) is output to calendar above. readHTMLTable conveniently converts this to a list sorted by the different classes 
#in the HTML code
calendar_1 <- readHTMLTable(calendar)

Holidays <- rbind(calendar_1[[17]], calendar_1[[18]], calendar_1[[19]])