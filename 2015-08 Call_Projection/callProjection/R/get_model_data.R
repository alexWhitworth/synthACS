#' @title Clean Model data
#' @description Pull campaign response data. Do aggregations, data cleaning,
#' and find all future dates for oustanding and future campaigns where we expect responses within
#' the upcoming month.
#' @param channel A character string corresponding to the appropriate ODBC connection. Defaults to "c2g"
#' @param historical Logical. Do you want to forecast historical projections for testing? 
#' Defaults to FALSE
#' @param hist_yr An integer corresponding to a historical year. Must be $>= 2012$.
#' @param hist_mo An integer in $[1,12]$ corresponding to a historical month.
#' @return \code{list} of three \code{data.frame}s: (21 completed
#' campaign data, (2) oustanding campaign data, (3) all future response days for upcoming campaigns.
#' @export
get_model_data <- function(channel= "c2g", historical= FALSE, hist_yr, hist_mo) {
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
  camp_resp$responders <- as.double(camp_resp$responders) # needed for data.table
  
  
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
                   paste(hist_yr, "-", month(Sys.Date()), "-", 28, sep="")))))
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
  if (!historical) {
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
    
  } else {
    camp_dates <- camp_resp %>% group_by(cell_code) %>% summarize(date= max(date), last_date= max(date) + 90)
    # Outstanding: final campaign tracking day > max of response date
    # completed: final campaign tracking day <= max of response date
    # future: mail date hasn't occured yet
    camp_dates$outstand <- ifelse(camp_dates$last_date > max(camp_resp$response_date), TRUE, FALSE)
    camp_dates$comp     <- ifelse(camp_dates$last_date <= max(camp_resp$response_date), TRUE, FALSE)
    camp_dates$future   <- ifelse(camp_dates$date > max(camp_resp$response_date), TRUE, FALSE)
    
    camp_outstanding <- camp_resp_final[cell_code %in% camp_dates[camp_dates$outstand == TRUE, ]$cell_code, ]
    
    camp_resp_comp <- camp_resp_final[!is.na(camp_resp_final$response_date) & 
                                        !is.null(camp_resp_final$response_date) & 
                                      cell_code %in% camp_dates[camp_dates$comp == TRUE, ]$cell_code, ]
    camp_future <- camp_resp_final[!is.na(camp_resp_final$response_date) & 
                                     !is.null(camp_resp_final$response_date) &
                                     cell_code %in% camp_dates[camp_dates$future == TRUE, ]$cell_code, ]
  }
  
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
  
  return(list(camp_complete= camp_resp_comp,
              camp_outstanding= camp_outstanding,
              camp_proj= new_campaigns)) 
}
