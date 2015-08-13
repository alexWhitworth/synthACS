DM_call_model_data_load_and_clean <- function(called_data, current_date) {
  ##load 'lubridate' for working with dates
  library(lubridate)
  
  ##'current_date' needs to be loaded in format "year-month-day", ex. "2014-09-20"
  current_date_numeric <- as.numeric(as.Date(current_date))
  
  ###Pull campaign response data and clean as input for DM call projection
  ###Assumed that we want to use data back through March 2014 only
  
  #Load 'RODBC' package to load from server
  library(RODBC)
  
  #Connect to Phoenix
  ch <- odbcConnect("c2g")
  
  #Pull campaign response, all variables
  campaign_response <- sqlQuery(ch, "SELECT * FROM [c2g].[dbo].[c2g_campaign_response]")
  
  #Pull Called data
  #Must be pulled MANUALLY from SQL server
  called <- called_data
  
  #Create response_date variables and projected and called
  campaign_response$year_response_date <- as.numeric(substr(as.character(campaign_response$response_date), 1, 4))
  campaign_response$month_response_date <- as.numeric(substr(as.character(campaign_response$response_date), 6, 7))
  campaign_response$day_response_date <- as.numeric(substr(as.character(campaign_response$response_date), 9, 10))
  
  campaign_response$projected <- "NA"
  
  #Coerce campaign group to character
  campaign_response$campaign_group <- as.character(campaign_response$campaign_group)
  
  #Re-label NA, NULL campaign response groups as "Not known"
  campaign_response$campaign_group <- ifelse(is.na(campaign_response$campaign_group) | is.null(campaign_response$campaign_group), "Not known", campaign_response$campaign_group)
  
  #Sum called by day
  called_by_day <- aggregate(called$mktg_call_count, by=list(called$call_date), FUN=sum, na.rm="TRUE")
  
  #Attach response month, day, year to called_by_day
  #called_by_day$month_response_date <- ifelse(substr(as.character(called_by_day$Group.1), 2, 2) == "/", 
  #                                            as.numeric(substr(as.character(called_by_day$Group.1), 1, 1)), as.numeric(substr(as.character(called_by_day$Group.1), 1, 2)))
  
  #called_by_day$day_response_date <- ifelse(substr(as.character(called_by_day$Group.1), 2, 2) == "/", 
  #                                        ifelse(is.na(as.numeric(substr(as.character(called_by_day$Group.1), 3, 4))) == "FALSE", as.numeric(substr(as.character(called_by_day$Group.1), 3, 4)), 
  #                                               as.numeric(substr(as.character(called_by_day$Group.1), 3, 3))), ifelse(is.na(as.numeric(substr(as.character(called_by_day$Group.1), 4, 5))) == "FALSE", 
  #                                                                                                                      as.numeric(substr(as.character(called_by_day$Group.1), 4, 5)), as.numeric(substr(as.character(called_by_day$Group.1), 4, 4))))
  
  #called_by_day$year_response_date <- as.numeric(substr(as.character(called_by_day$Group.1), nchar(as.character(called_by_day$Group.1)) - 3, nchar(as.character(called_by_day$Group.1))))
  called_by_day$month_response_date <- month(called_by_day$Group.1)
  called_by_day$day_response_date <- day(called_by_day$Group.1)
  called_by_day$year_response_date <- year(called_by_day$Group.1)
  
  #Merge called_by_day to campaign response
  campaign_response_1 <- merge(campaign_response, called_by_day, by=c("year_response_date", "month_response_date", "day_response_date"), all.x="TRUE")
  campaign_response_1$Called <- campaign_response_1$x
  campaign_response_1$x <- NULL
  
  ##Calculate 'first_future_date', first day without actual responses
  first_future_date <- max(as.numeric(as.Date(campaign_response_1$response_date)), na.rm="TRUE") + 1
  
  #Now, limit to years and months we want only 
  campaign_response_2 <- campaign_response_1[campaign_response_1$year_response_date > 2014 | campaign_response_1$year_response_date == 2014 &
                                               campaign_response_1$month_response_date >= 3, ]
  
  ###TRY ONLY > first future date so that we don't double count calls on future date
  campaign_future <- campaign_response_1[is.na(campaign_response_1$year_response_date) == "TRUE" & as.numeric(as.Date(campaign_response_1$date)) > first_future_date 
                                         & as.numeric(as.Date(campaign_response_1$date)) <= current_date_numeric, ]
  
  #Get rid of A/B Test in campaign future
  campaign_future <- campaign_future[as.character(campaign_future$campaign_group) != "A/B Test", ]
  
  #Change NA campaign future to Not known
  campaign_future$campaign_group <- ifelse(is.na(campaign_future$campaign_group), "Not known", campaign_future$campaign_group)
  
  #Get rid of A/B Test
  campaign_response_2 <- campaign_response_2[as.character(campaign_response_2$campaign_group) != "A/B Test", ]
  
  #Change NA campaign future to Not known
  campaign_response_2$campaign_group <- ifelse(is.na(campaign_response_2$campaign_group), "Not known", campaign_response_2$campaign_group)
  
  #Get rid of NA dates
  campaign_response_2 <- campaign_response_2[is.na(campaign_response_2$date) == "FALSE", ]
  
  #Outstanding campaigns
  campaigns_outstanding <- campaign_response_2[(current_date_numeric - as.numeric(as.Date(campaign_response_2$date))) <= (90 + (current_date_numeric - first_future_date)), ]
  
  #Cleaned data!!
  campaign_response_final <- campaign_response_2
  campaign_response_final <- campaign_response_final[order(campaign_response_final$year_response_date, campaign_response_final$month_response_date, campaign_response_final$day_response_date), ]
  rm(ch)
  
  #Eliminate any dates in campaign_respose_final > current_date
  campaign_response_final <- campaign_response_final[as.numeric(as.Date(campaign_response_final$date)) <= current_date_numeric, ]
  
  #Create campaign response where all days have already happened (response date exists)
  campaign_response_old <- campaign_response_final[is.na(campaign_response_final$response_date) == "FALSE" & as.character(campaign_response_final$response_date) != "NULL", ]
  
  #Bind campaigns_outstanding and campaign_future to see all current possible campaigns
  current_campaigns <- rbind(campaign_future, campaigns_outstanding)
  
  #Unique campaigns only
  current_campaigns <- current_campaigns[!duplicated(current_campaigns$cell_code), ]
  
  #Now, vector of days that we are projecting out into the future
  future_days <- seq(first_future_date, current_date_numeric, 1)
  class(future_days) <- "Date"
  
  #Loop through current_campaigns by date to assign the campaigns outstanding for each date and create forward looking campaign data
  for(i in 1:length(future_days)) {
    
    campaigns_by_day <- current_campaigns[as.numeric(as.Date(current_campaigns$date)) <= as.numeric(future_days[i]), ]
    campaigns_by_day$response_date <- future_days[i]
    campaigns_by_day$days_to_response <- as.numeric(as.Date(campaigns_by_day$response_date)) - as.numeric(as.Date(campaigns_by_day$date)) 
    campaigns_by_day <- campaigns_by_day[campaigns_by_day$days_to_response <= 90, ]
    
    if(i == 1) {
      new_campaigns <- campaigns_by_day  
    }
    
    else {
      new_campaigns <- rbind(new_campaigns, campaigns_by_day)
    }
  }
  
  #Define both new_campaigns projected and called
  new_campaigns$projected <- "NA"
  new_campaigns$year_response_date <- as.numeric(substr(as.character(new_campaigns$response_date), 1, 4))
  new_campaigns$month_response_date <- as.numeric(substr(as.character(new_campaigns$response_date), 6, 7))
  new_campaigns$day_response_date <- as.numeric(substr(as.character(new_campaigns$response_date), 9, 10))
  new_campaigns_1 <- merge(new_campaigns, called_by_day, by=c("year_response_date", "month_response_date", "day_response_date"), all.x="TRUE")
  new_campaigns <- new_campaigns_1
  new_campaigns$Group.1 <- NULL
  new_campaigns$Called <- new_campaigns$x
  new_campaigns$x <- NULL
  new_campaigns$Group.1.x <- NULL
  new_campaigns$Group.1.y <- NULL
  
  #Remove 'Group.1' from old campaigns
  campaign_response_old$Group.1 <- NULL
  
  #Fix new campaigns response data
  new_campaigns$year_response_date <- year(new_campaigns$response_date)
  new_campaigns$month_response_date <- month(new_campaigns$response_date)
  new_campaigns$day_response_date <- day(new_campaigns$response_date) 
  # campaigns_by_day$response_day_of_week <- #ifelse(wday(campaigns_by_day$response_date) == 1, "Sunday", ifelse(wday(campaigns_by_day$response_date) == 2, "Monday", 
  #ifelse(wday(campaigns_by_day$response_date) == 3, "Tuesday", ifelse(wday(campaigns_by_day$response_date) == 4,
  #"Wednesday", ifelse(wday(campaigns_by_day$response_date) == 5, "Thursday", ifelse(wday(campaigns_by_day$response_date) == 6, 
  #"Friday", ifelse(wday(campaigns_by_day$response_date) == 7, "Saturday", "NA")))))))
  
  #Change day_of_week to response_day_of_week
  new_campaigns$response_day_of_week <-    wday(new_campaigns$response_date)
  new_campaigns$response_day_of_week <- ifelse(new_campaigns$response_day_of_week == 1, "Sunday", ifelse(new_campaigns$response_day_of_week == 2, "Monday", ifelse(new_campaigns$response_day_of_week == 3,
                                                                                                                                                                   "Tuesday", ifelse(new_campaigns$response_day_of_week == 4, "Wednesday", ifelse(new_campaigns$response_day_of_week == 5, "Thursday", ifelse(new_campaigns$response_day_of_week == 6, "Friday", 
                                                                                                                                                                                                                                                                                                              "Saturday"))))))
  
  #Form final dataset, campaign_response
  campaign_response <- rbind(new_campaigns, campaign_response_old)
  campaign_response <- campaign_response[order(campaign_response$year_response_date, campaign_response$month_response_date, campaign_response$day_response_date), ]
  
  #Coerce Called to numeric
  campaign_response$Called <- as.numeric(as.character(campaign_response$Called))
  
  output <- list("campaign_response_final" = campaign_response_final, "current_date_numeric" = current_date_numeric, "current_date" = current_date, 
                 "campaign_response_1" = campaign_response_1, "campaign_response_2" = campaign_response_2, "campaign_response_old" = campaign_response_old, "first_future_date" = first_future_date, 
                 "campaign_future" = campaign_future, "campaigns_outstanding" = campaigns_outstanding, "current_campaigns" = current_campaigns, "future_days" = future_days,
                 "campaigns_by_day" = campaigns_by_day, "new_campaigns" = new_campaigns, "called_by_day" = called_by_day, "new_campaigns_1" = new_campaigns_1, "campaign_response" = campaign_response)
  return(output)
  
}