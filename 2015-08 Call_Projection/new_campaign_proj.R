#' @description Internal function called by \code{clean_model_data()}. Calculates future dates
#' for response projection for each current campaign.
#' @param cur_date \code{Numeric}. Coerced date corresponding to the last day of the current month.
#' @param future_date \code{Numeric}. Corresponds to first day after max response date to date.
#' @param current_campaigns \code{data.frame}. Defined in \code{clean_model_data()}.
#' @param called_by_day \code{data.frame}. Aggregate sum of calls by day
#' @return A \code{data.frame}.
#' @export

new_campaign_proj <- function(cur_date, future_date, current_campaigns, called_by_day) {
  require(lubridate)
  require(data.table) # for rbindlist()
  
  future_days <- seq(future_date, cur_date, 1); class(future_days) <- "Date"
  # preallocate 
  
  p <- length(future_days)
  new_camp <- vector(mode= "list", length= p)
  
  # Loop through current_campaigns by date to assign the campaigns outstanding 
  # for each date and create forward looking campaign data
  for(i in 1:p) {
    daily_campaigns <- current_campaigns[as.numeric(as.Date(current_campaigns$date)) <= as.numeric(future_days[i]), ]
    daily_campaigns$response_date <- future_days[i]
    daily_campaigns$days_to_response <- as.numeric(as.Date(daily_campaigns$response_date)) - as.numeric(as.Date(daily_campaigns$date)) 
    daily_campaigns <- daily_campaigns[daily_campaigns$days_to_response <= 90, ]
    
    new_camp[[i]] <- daily_campaigns[, -c(29:30)]
  }
  new_campaigns <- rbindlist(new_camp)
  
  #Define both new_campaigns projected and called
  new_campaigns$projected <- "NA"
  new_campaigns$year_response_date <- year(new_campaigns$response_date)
  new_campaigns$month_response_date <- month(new_campaigns$response_date)
  new_campaigns$day_response_date <- day(new_campaigns$response_date)
  new_campaigns <- merge(new_campaigns, called_by_day, 
                         by=c("year_response_date", "month_response_date", "day_response_date"), all.x="TRUE")
  
  
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










