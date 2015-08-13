#' @title Clean Model data
#' @description Input IB call data, pull campaign response data. Do aggregations, data cleaning,
#' and find all future dates for oustanding and future campaigns where we expect responses within
#' the upcoming month.
#' @param called_data A \code{data.frame} with call data from \code{data_pull()}.
#' @param channel A character string corresponding to the appropriate ODBC connection. Defaults to "c2g"
#' @return \code{list} of four \code{data.frame}s: (1) Aggregated call-by-day data, (2) completed
#' campaign data, (3) oustanding campaign data, (4) all future response days for upcoming campaigns.
#' @export
get_model_data <- function(called_data, channel= "c2g") {
  require(lubridate)
  require(RODBC)
  require(data.table)
  
  # 01. Pull campaign response data and clean
  #     Assumed that we want to use data back through March 2014 only  
  #---------------------------------------------------
  # Pull campaign response data, all variables
  ch <- odbcConnect(channel)
  camp_resp <- data.table(sqlQuery(ch, "SELECT * FROM [c2g].[dbo].[c2g_campaign_response] 
                                where year(date) >= 2014", stringsAsFactors= FALSE), 
                          key= c("cell_code", "response_date"))
  close(ch); rm(ch)
  
  # do some munging on response data
  camp_resp$response_day_of_week <- NULL
  camp_resp$year_response_date <- year(camp_resp$response_date)
  camp_resp$month_response_date <- month(camp_resp$response_date)
  camp_resp$day_response_date <- day(camp_resp$response_date)
  
  camp_resp$responders <- as.double(camp_resp$responders)
  
  # Aggregate daily call volume, do some basic munging
  called_by_day <- data.table(called)[, .(Called= sum(call_count, na.rm= TRUE)), keyby= call_date]
  called_by_day$month_response_date <- month(called_by_day$call_date)
  called_by_day$day_response_date <- day(called_by_day$call_date)
  called_by_day$year_response_date <- year(called_by_day$call_date)
  called_by_day$resp_day_of_week <- wday(called_by_day$call_date)
  
  
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
  # Remove A/B Test and any missing dates (errors)
  camp_resp_final <- camp_resp[tolower(campaign_type) %in% c("ad hoc", "bau", "latest inquiry") &
                                 !is.na(date), ][,
                               ':='(campaign_type= tolower(campaign_type))] 
  
  # only look to last day of current month
  camp_resp_final <- camp_resp_final[as.numeric(as.Date(camp_resp_final$date)) <= last_day_num, ]
  
  # 02. Subset outstanding, future and completed campaigns
  #---------------------------------------------------  
  # Outstanding campaigns -- ie haven't completed yet
  camp_outstanding <- camp_resp_final[camp_resp_final$days_of_tracking < 90, ]
  
  # Complete campaigns: full 90 days of tracking
  camp_resp_comp <- camp_resp_final[!is.na(camp_resp_final$response_date) & 
                                    !is.null(camp_resp_final$response_date) &
                                    camp_resp_final$days_of_tracking == 90, ]
  
  # future campaigns: days of tracking < 0
  camp_future <- camp_resp_final[!is.na(camp_resp_final$response_date) & 
                                   !is.null(camp_resp_final$response_date) &
                                   camp_resp_final$days_of_tracking < 0, ]
  
  # current campaigns': outstanding or future with < 90 days tracking
  # double check for errors
  cur_campaigns <- rbind(camp_outstanding, camp_future)
  cur_campaigns <- cur_campaigns[!duplicated(cur_campaigns),] 
  
  sel_last <- function(x) {return(x[order(days_to_response)][nrow(x),])}
  
  cur_campaigns <- cur_campaigns[, sel_last(.SD), by= cell_code]
  
  # 03. Create new_campaigns -- ie dates to be projected -- and return
  #---------------------------------------------------  
  new_campaigns <- data.table(new_campaign_proj(last_day_num, future_date1,
                                     cur_campaigns, called_by_day),
                              key= c("cell_code", "response_date"))  ## needs to be loaded (create R package)
  
  return(list(called_by_day= called_by_day, # not sure that I need this; perhaps for historical analysis
              camp_complete= camp_resp_comp,
              camp_outstanding= camp_outstanding,
              camp_proj= new_campaigns)) 
}