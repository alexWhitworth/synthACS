all_call_estimates <- function(all_calls) {
  
  require(lubridate)
  require(dplyr) #[AW 7/31] for replacing aggregate
  require(reshape2) # [AW 7/31] switch to require(tidyr) on R v 3.2.X
  
  # replace w/ dplyr
  calls_by_date <- all_calls %>% group_by(call_date) %>%
    summarize(all_calls= sum(mktg_call_count, na.rm=TRUE))

  calls_by_date_cat <- all_calls %>% group_by(call_date, category) %>%
    summarize(call_cnt= sum(mktg_call_count, na.rm= TRUE))

  calls <- merge(calls_by_date, dcast(calls_by_date_cat, formula= call_date ~ category), 
                 by= "call_date", all= TRUE)
  
#   calls$other <- calls$all_calls - (calls$mktg_direct + calls$customer_service + calls$DB_IVR + 
#                                     calls$DB_com + calls$Direct_Extension + calls$iUpdate + 
#                                     calls$Organic + calls$Paid_web)
  
  #Insert day of week
  calls$day_of_week <- wday(calls$call_date) # lubridate::wday() - returns numeric 1== Sunday, 7 == Saturday
  
  #Coerce call_date to Date
  calls$call_date <- as.Date(calls$call_date)
  
  return(calls)
}