###Pull campaign response data and clean as input for DM call projection
###Assumed that we want to use data back through March 2014 only
###To be used to fill in new base dataset

#Load 'RODBC' package to load from server
library(RODBC)

#Connect to Phoenix
# ch <- odbcConnect("phoenix1")
ch <- odbcConnect("c2g")

#Pull campaign response, all variables
campaign_response <- sqlQuery(ch, "SELECT * FROM [dbo].[c2g_campaign_response]", stringsAsFactors= FALSE)

close(ch)

#Pull Called data
#Must be pulled MANUALLY from SQL server
called <- read.csv("/Users/ashelton/Documents/The Call Projection Model/called_10_26_14.csv")

#Create response_date variables and projected and called
campaign_response$year_response_date <- as.numeric(substr(as.character(campaign_response$response_date), 1, 4))
campaign_response$month_response_date <- as.numeric(substr(as.character(campaign_response$response_date), 6, 7))
campaign_response$day_response_date <- as.numeric(substr(as.character(campaign_response$response_date), 9, 10))

campaign_response$projected <- "NA"

#Sum called by day
called_by_day <- aggregate(called$mktg_call_count, by=list(called$call_date), FUN=sum, na.rm="TRUE")

#Attach response month, day, year to called_by_day
called_by_day$month_response_date <- 
  ifelse(substr(as.character(called_by_day$Group.1), 2, 2) == "/", 
    as.numeric(substr(as.character(called_by_day$Group.1), 1, 1)), 
    as.numeric(substr(as.character(called_by_day$Group.1), 1, 2)))

called_by_day$day_response_date <- 
  ifelse(substr(as.character(called_by_day$Group.1), 2, 2) == "/", 
    ifelse(is.na(as.numeric(substr(as.character(called_by_day$Group.1), 3, 4))) == "FALSE", 
           as.numeric(substr(as.character(called_by_day$Group.1), 3, 4)), 
           as.numeric(substr(as.character(called_by_day$Group.1), 3, 3))), 
    ifelse(is.na(as.numeric(substr(as.character(called_by_day$Group.1), 4, 5))) == "FALSE", 
           as.numeric(substr(as.character(called_by_day$Group.1), 4, 5)), 
           as.numeric(substr(as.character(called_by_day$Group.1), 4, 4))))

called_by_day$year_response_date <- as.numeric(substr(as.character(called_by_day$Group.1), 
              nchar(as.character(called_by_day$Group.1)) - 3, nchar(as.character(called_by_day$Group.1))))

#Merge called_by_day to campaign response
campaign_response_1 <- merge(campaign_response, called_by_day, 
                             by=c("year_response_date", "month_response_date", "day_response_date"), all.x="TRUE")
campaign_response_1$Called <- campaign_response_1$x
campaign_response_1$x <- NULL

#Now, limit to years and months we want only 
campaign_response_2 <- campaign_response_1[campaign_response_1$year_response_date > 2014 | 
                                             campaign_response_1$year_response_date == 2014 & 
                                             campaign_response_1$month_response_date >= 3, ]

#Eliminate NULL response dates
campaign_response_2 <- campaign_response_2[is.na(campaign_response_2$year_response_date) == "FALSE", ]

#Get rid of A/B Test
campaign_response_2 <- campaign_response_2[as.character(campaign_response_2$campaign_group) != "A/B Test", ]

#Get rid of NA campaign groups
campaign_response_2 <- campaign_response_2[is.na(campaign_response_2$campaign_group) == "FALSE", ]

#Cleaned data!!
campaign_response_final <- campaign_response_2
campaign_response_final <- campaign_response_final[
  order(campaign_response_final$year_response_date, campaign_response_final$month_response_date, 
        campaign_response_final$day_response_date), ]
rm(campaign_response_2, campaign_response_1, campaign_response, called_by_day, called, ch)



