#' @title Clean Model data
#' @description Input IB call data, pull campaign response data. Do aggregations, data cleaning,
#' and find all future dates for oustanding and future campaigns where we expect responses within
#' the upcoming month.
#' @param called_data A \code{data.frame} with call data from \code{data_pull()}.
#' @param channel A character string corresponding to the appropriate ODBC connection. Defaults to "c2g"
#' @param historical Logical. Do you want to forecast historical projections for testing? 
#' Defaults to FALSE
#' @param hist_yr An integer corresponding to a historical year. Must be $>= 2012$.
#' @param hist_mo An integer in $[1,12]$ corresponding to a historical month.
#' @return \code{list} of four \code{data.frame}s: (1) Aggregated call-by-day data, (2) completed
#' campaign data, (3) oustanding campaign data, (4) all future response days for upcoming campaigns.
#' @export
get_model_data <- function(called_data, channel= "c2g", historical= FALSE, hist_yr, hist_mo) {
  if (historical == TRUE) {
    if (!hist_mo %in% seq(1,12,1)) {
      stop("Input integer in [1,12] for hist_mo.")
    }
    if (!hist_yr %in% seq(2012, lubridate::year(Sys.Date()), 1)) {
      stop("Input integer in [2012, current year] for hist_yr.")
    }
  }
  
  # 01. Pull campaign response data and clean
  #     Assumed that we want to use data back through March 2014 only  
  #---------------------------------------------------
  # Pull campaign response data, all variables
  ch <- odbcConnect(channel)
  if (!historical) {
    camp_resp <- data.table(sqlQuery(ch, "SELECT * FROM [c2g].[dbo].[c2g_campaign_response] 
                                where year(date) >= 2014", stringsAsFactors= FALSE), 
                          key= c("cell_code", "response_date"))
  } else {
    hist_date <- paste(hist_yr, hist_mo, 1, sep= "-")
    query_txt <- paste(
      "SELECT * FROM [c2g].[dbo].[c2g_campaign_response] where year(date) >= 2014 and response_date < '", 
      hist_date, "'")
    
    camp_resp <- data.table(sqlQuery(ch, query_txt, stringsAsFactors= FALSE), 
                            key= c("cell_code", "response_date"))
  }
  close(ch); rm(ch, hist_date, query_txt)
  
  # do some munging on response data
  camp_resp$response_day_of_week <- NULL
  camp_resp$year_response_date <- year(camp_resp$response_date)
  camp_resp$month_response_date <- month(camp_resp$response_date)
  camp_resp$day_response_date <- day(camp_resp$response_date)
  
  camp_resp$responders <- as.double(camp_resp$responders)
  
  # Aggregate daily call volume, do some basic munging
  called_by_day <- data.table(called_data)[, .(Called= sum(call_count, na.rm= TRUE)), keyby= call_date]
  called_by_day$month_response_date <- month(called_by_day$call_date)
  called_by_day$day_response_date <- day(called_by_day$call_date)
  called_by_day$year_response_date <- year(called_by_day$call_date)
  called_by_day$resp_day_of_week <- wday(called_by_day$call_date)
  
  
  # 01b. Calculate some dates
  #---------------------------------------------------
  # Find last day of month
  if (!historical) {
    cur_yr <- year(Sys.Date())
    cur_mo <- month(Sys.Date())
    
    last_day <- ifelse(is.Date(try(as.Date(paste(cur_yr, "-", cur_mo, "-", 31, sep="")), TRUE)), 
          paste(cur_yr, "-", cur_mo, "-", 31, sep=""), 
        ifelse(is.Date(try(as.Date(paste(cur_yr, "-", cur_mo, "-", 30, sep="")), TRUE)), 
          paste(cur_yr, "-", cur_mo, "-", 30, sep=""), 
          ifelse(is.Date(try(as.Date(paste(cur_yr, "-", month(cur_mo, "-", 29, sep="")), TRUE)), 
            paste(cur_yr, "-", cur_mo, "-", 29, sep=""),
            ifelse(is.Date(try(as.Date(paste(cur_yr, "-", cur_mo, "-", 28, sep="")), TRUE)), 
                   paste(cur_yr, "-", cur_mo, "-", 28, sep=""))))))
  } else {
    last_day <- ifelse(is.Date(try(as.Date(paste(hist_yr, "-", hist_mo, "-", 31, sep="")), TRUE)), 
          paste(hist_yr, "-", hist_mo, "-", 31, sep=""), 
        ifelse(is.Date(try(as.Date(paste(hist_yr, "-", hist_mo, "-", 30, sep="")), TRUE)), 
          paste(hist_yr, "-", hist_mo, "-", 30, sep=""), 
          ifelse(is.Date(try(as.Date(paste(hist_yr, "-", hist_mo, "-", 29, sep="")), TRUE)), 
            paste(hist_yr, "-", hist_mo, "-", 29, sep=""),
            ifelse(is.Date(try(as.Date(paste(hist_yr, "-", hist_mo, "-", 28, sep="")), TRUE)), 
                   paste(hist_hr, "-", month(Sys.Date()), "-", 28, sep="")))))
  }
  
  # coerce to numeric
  last_day_num <- as.numeric(as.Date(last_day))
  # calc first day for future responses
  first_future_resp_dt <- max(as.numeric(camp_resp$response_date), na.rm=TRUE) + 1 
  
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
  if (!historical) {
    camp_outstanding <- camp_resp_final[camp_resp_final$days_of_tracking < 90, ]
  } else {
    sum_tracking <- camp_resp %>% group_by(cell_code) %>% 
      summarize(max_resp = max(response_date), 
          days_to_response= days_to_response[which(response_date == max(response_date))])
    camp_outstanding <- camp_resp_final[cell_code %in% sum_tracking[sum_tracking$max_resp >= 
          as.Date("2014-07-15", format= "%Y-%m-%d") & sum_tracking$days_to_response < 90,]$cell_code,]
  }
  
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
  new_campaigns <- data.table(new_campaign_proj(last_day_num, first_future_resp_dt,
                                     cur_campaigns),
                              key= c("cell_code", "response_date")) 
  
  return(list(called_by_day= called_by_day, # not sure that I need this; perhaps for historical analysis
              camp_complete= camp_resp_comp,
              camp_outstanding= camp_outstanding,
              camp_proj= new_campaigns)) 
}
