#' @title Create backbone for future forecasts
#' @description Internal function called by \code{clean_model_data()}. Calculates all future dates
#' where we expect responses for each current campaign for use in projection.
#' @param cur_date \code{Numeric}. Coerced date corresponding to the last day of the current month.
#' @param future_date \code{Numeric}. Corresponds to first day after max response date to date.
#' @param cur_campaigns \code{data.frame}. Defined in \code{get_model_data()}.
#' @return A \code{data.frame} with all future dates where we expect responses for 
#' each current campaign for use in projection.
#' @export

new_campaign_proj <- function(cur_date, future_date, cur_campaigns) {
  require(lubridate)
  require(data.table) 
  
  future_days <- seq(future_date, cur_date, 1); class(future_days) <- "Date"
  
  # preallocate 
  p <- length(future_days)
  new_camp <- vector(mode= "list", length= p)
  
  # Loop through cur_campaigns by date to assign upcoming days in which we will project responses
  for(i in 1:p) {
    future_day_resp <- within(cur_campaigns[cur_campaigns$date <= future_days[i], ], {
      response_date= future_days[i]
      days_to_response= as.numeric(as.Date(response_date) - as.Date(date))
      responders= NA
      pct_of_leads= NA
      pct_of_responders= NA
    })
    
    # only keep 90 days of tracking
    future_day_resp <- future_day_resp[days_to_response <= 90, ]
    new_camp[[i]] <- future_day_resp
  }
  new_campaigns <- rbindlist(new_camp)
  
  #Define both new_campaigns projected and called
  new_campaigns$year_response_date <- year(new_campaigns$response_date)
  new_campaigns$month_response_date <- month(new_campaigns$response_date)
  new_campaigns$day_response_date <- day(new_campaigns$response_date)
  
  #Change day_of_week to response_day_of_week
  new_campaigns$response_day_of_week <- wday(new_campaigns$response_date)
  new_campaigns$response_day_of_week <- ifelse(new_campaigns$response_day_of_week == 1, "Sunday", 
                                        ifelse(new_campaigns$response_day_of_week == 2, "Monday", 
                                        ifelse(new_campaigns$response_day_of_week == 3, "Tuesday", 
                                        ifelse(new_campaigns$response_day_of_week == 4, "Wednesday", 
                                        ifelse(new_campaigns$response_day_of_week == 5, "Thursday", 
                                        ifelse(new_campaigns$response_day_of_week == 6, "Friday", "Saturday"))))))
  
  return(new_campaigns)
}










