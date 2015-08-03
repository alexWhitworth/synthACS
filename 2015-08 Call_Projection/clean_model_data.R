#' @title Clean Model data
#' @description ?????
#' @param a \code{data.frame} with call data from \code{data_pull()}.
#' @param channel A character string corresponding to the appropriate ODBC connection. Defaults to "c2g"
clean_model_data <- function(called_data, channel= "c2g") {
  require(lubridate)
  require(RODBC)
  require(dplyr)
  
  # 01. Pull campaign response data and clean
  #     Assumed that we want to use data back through March 2014 only  
  #---------------------------------------------------
  #Connect to Phoenix; Pull campaign response, all variables
  ch <- odbcConnect(channel)
  campaign_response <- sqlQuery(ch, "SELECT * FROM [c2g].[dbo].[c2g_campaign_response]", stringsAsFactors= FALSE)
  close(ch); rm(ch)
  
  # do some munging on response data
  campaign_response$year_response_date <- year(campaign_response$response_date)
  campaign_response$month_response_date <- month(campaign_response$response_date)
  campaign_response$day_response_date <- day(campaign_response$response_date)
  campaign_response$projected <- NA
  campaign_response$campaign_group <- ifelse(is.na(campaign_response$campaign_group) | 
           is.null(campaign_response$campaign_group), "Not known", campaign_response$campaign_group)
  
  # Aggregate daily call volume, do some basic munging
  called_by_day <- called_data %>% group_by(call_date) %>% summarize(Called= sum(mktg_call_count, na.rm= TRUE))
  called_by_day$month_response_date <- month(called_by_day$call_date)
  called_by_day$day_response_date <- day(called_by_day$call_date)
  called_by_day$year_response_date <- year(called_by_day$call_date)
  
  # Merge called_by_day to campaign response
  campaign_response <- merge(campaign_response, called_by_day, 
                               by=c("year_response_date", "month_response_date", "day_response_date"), all.x="TRUE")  
  
  # 01b. Calculate some dates
  #---------------------------------------------------
  # Find last day of month
  last_day <- 
    ifelse(is.Date(try(as.Date(paste(year(Sys.Date()), "-", month(Sys.Date()), "-", 31, sep="")), TRUE)) == TRUE, 
      paste(year(Sys.Date()), "-", month(Sys.Date()), "-", 31, sep=""), 
      ifelse(is.Date(try(as.Date(paste(year(Sys.Date()), "-", month(Sys.Date()), "-", 30, sep="")), TRUE)) == TRUE, 
        paste(year(Sys.Date()), "-", month(Sys.Date()), "-", 30, sep=""), 
        ifelse(is.Date(try(as.Date(paste(year(Sys.Date()), "-", month(Sys.Date()), "-", 29, sep="")), TRUE)) == TRUE, 
          paste(year(Sys.Date()), "-", month(Sys.Date()), "-", 29, sep=""),
          ifelse(is.Date(try(as.Date(paste(year(Sys.Date()), "-", month(Sys.Date()), "-", 28, sep="")), TRUE)) == TRUE, 
                 paste(year(Sys.Date()), "-", month(Sys.Date()), "-", 28, sep="")))))
  
  current_date_numeric <- as.numeric(as.Date(last_day))
  # calc first day without actual responses
  first_future_date <- max(as.numeric(campaign_response$response_date), na.rm=TRUE) + 1 
  
  # 01c. subset data and do some final munging
  #---------------------------------------------------  
  # Subset March-2014 or >>
  campaign_response_final <- campaign_response[campaign_response$year_response_date > 2014 | 
     (campaign_response$year_response_date == 2014 & campaign_response$month_response_date >= 3) &
     as.character(campaign_response$campaign_group) != "A/B Test" & !is.na(campaign_response$date), ]
  
  campaign_response_final <- campaign_response_final[order(campaign_response_final$year_response_date, 
                                                   campaign_response_final$month_response_date, 
                                                   campaign_response_final$day_response_date), ]
  
  campaign_response_final <- campaign_response_final[as.numeric(as.Date(campaign_response_final$date)) <= current_date_numeric, ]
  
  # 02. Note outstanding, completed, and future campaigns
  #---------------------------------------------------  
  #Outstanding campaigns
  campaign_outstanding <- campaign_response_final[
    (current_date_numeric - as.numeric(as.Date(campaign_response_final$date))) <= (90 + (current_date_numeric - first_future_date)), ]
  
  ###TRY ONLY > first future date so that we don't double count calls on future date
  campaign_future <- campaign_response[!is.na(campaign_response$year_response_date) & 
                                         as.numeric(as.Date(campaign_response$date)) > first_future_date &
                                         as.numeric(as.Date(campaign_response$date)) <= current_date_numeric &
                                         as.character(campaign_response$campaign_group) != "A/B Test", ]
  
  #Create campaign response where all days have already happened (response date exists)
  campaign_response_old <- campaign_response_final[!is.na(campaign_response_final$response_date) & 
                                                     !is.null(campaign_response_final$response_date), ]
  
  #Bind campaigns_outstanding and campaign_future to see all current possible campaigns and dedup
  current_campaigns <- rbind(campaign_future, campaign_outstanding)
  current_campaigns <- current_campaigns[!duplicated(current_campaigns$cell_code), ]
  
  rm(campaign_future, campaign_oustanding)
  
  # 03. Create new_campaigns, merge, and return
  #---------------------------------------------------  
  new_campaigns <- new_campaign_proj(current_date_numeric, first_future_date,
                                     current_campaigns, called_by_day) ## needs to be loaded (create R package)
  
  #Form final dataset, campaign_response
  campaign_response <- rbind(new_campaigns, campaign_response_old)
  campaign_response <- campaign_response[order(campaign_response$year_response_date, 
                                               campaign_response$month_response_date, 
                                               campaign_response$day_response_date), ]  
  
  return(list(called_by_day= called_by_day, campaign_response= campaign_response, 
              campaign_response_final= campaign_response_final))
}