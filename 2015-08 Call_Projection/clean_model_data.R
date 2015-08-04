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
  # Pull campaign response data, all variables
  ch <- odbcConnect(channel)
  camp_resp <- sqlQuery(ch, "SELECT * FROM [c2g].[dbo].[c2g_campaign_response] 
                                where year(date) >= 2014", stringsAsFactors= FALSE)
  close(ch); rm(ch)
  
  # do some munging on response data
  camp_resp$year_response_date <- year(camp_resp$response_date)
  camp_resp$month_response_date <- month(camp_resp$response_date)
  camp_resp$day_response_date <- day(camp_resp$response_date)
  # camp_resp$projected <- NA
  camp_resp$campaign_group <- ifelse(is.na(camp_resp$campaign_group) | 
           is.null(camp_resp$campaign_group), "Not known", camp_resp$campaign_group)
  
  # Aggregate daily call volume, do some basic munging
  called_by_day <- called_data %>% group_by(call_date) %>% summarize(Called= sum(mktg_call_count, na.rm= TRUE))
  called_by_day$month_response_date <- month(called_by_day$call_date)
  called_by_day$day_response_date <- day(called_by_day$call_date)
  called_by_day$year_response_date <- year(called_by_day$call_date)
  
  # Merge called_by_day to campaign response
  camp_resp <- merge(camp_resp, called_by_day, 
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
  
  # coerce to numeric
  last_day_num <- as.numeric(as.Date(last_day))
  # calc first day for future responses
  future_date1 <- max(as.numeric(camp_resp$response_date), na.rm=TRUE) + 1 
  
  # 01c. subset data and do some final munging
  #---------------------------------------------------  
  # Subset March-2014 or >>
  camp_resp_final <- camp_resp[camp_resp$year_response_date > 2014 | 
     (camp_resp$year_response_date == 2014 & camp_resp$month_response_date >= 3) &
     as.character(camp_resp$campaign_group) != "A/B Test" & !is.na(camp_resp$date), ]
  
  camp_resp_final <- camp_resp_final[order(camp_resp_final$year_response_date, 
                                                   camp_resp_final$month_response_date, 
                                                   camp_resp_final$day_response_date), ]
  
  camp_resp_final <- camp_resp_final[as.numeric(as.Date(camp_resp_final$date)) <= last_day_num, ]
  
  # 02. Note outstanding, completed, and future campaigns
  #---------------------------------------------------  
  # Outstanding campaigns -- ie haven't completed yet
  campaign_outstanding <- camp_resp_final[
    (last_day_num - as.numeric(as.Date(camp_resp_final$date))) <= (90 + (last_day_num - future_date1)), ]
  
  ###TRY ONLY > first future date so that we don't double count calls on future date
  campaign_future <- camp_resp[!is.na(camp_resp$year_response_date) & 
                                         as.numeric(as.Date(camp_resp$date)) > future_date1 &
                                         as.numeric(as.Date(camp_resp$date)) <= last_day_num &
                                         as.character(camp_resp$campaign_group) != "A/B Test", ]
  
  # Complete campaigns: all response days have already happened (response date exists)
  camp_resp_comp <- camp_resp_final[!is.na(camp_resp_final$response_date) & 
                                                     !is.null(camp_resp_final$response_date), ]
  
  #Bind campaigns_outstanding and campaign_future to see all current possible campaigns and dedup
  current_campaigns <- rbind(campaign_future, campaign_outstanding)
  current_campaigns <- current_campaigns[!duplicated(current_campaigns$cell_code), ]
  
  rm(campaign_future, campaign_outstanding)
  
  # 03. Create new_campaigns -- ie projection range, merge, and return
  #---------------------------------------------------  
  new_campaigns <- new_campaign_proj(last_day_num, future_date1,
                                     current_campaigns, called_by_day) ## needs to be loaded (create R package)
  
  # Form final dataset, camp_resp
  camp_resp <- rbind(new_campaigns, camp_resp_comp)
  camp_resp <- camp_resp[order(camp_resp$year_response_date, 
                               camp_resp$month_response_date, 
                               camp_resp$day_response_date), ]  
  
  return(list(called_by_day= called_by_day, camp_resp= camp_resp, 
              camp_resp_final= camp_resp_final)) # not sure that I need camp_resp_final
}