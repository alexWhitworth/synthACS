adjust_lagged_error <- function(full_estimates, lags, maxadjust){
  
  #quantmod for lags, lubdridate for dates
  library(quantmod)
  library(lubridate)
  
  #Now, average lagged error for all non-weekend call estimates 
  monday <- full_estimates[wday(full_estimates$call_date) == 2 & is.na(full_estimates$Holiday), ]
  tuesday <- full_estimates[wday(full_estimates$call_date) == 3 & is.na(full_estimates$Holiday), ]
  wednesday <- full_estimates[wday(full_estimates$call_date) == 4 & is.na(full_estimates$Holiday), ]
  thursday <- full_estimates[wday(full_estimates$call_date) == 5 & is.na(full_estimates$Holiday), ]
  friday <- full_estimates[wday(full_estimates$call_date) == 6 & is.na(full_estimates$Holiday), ]
  
  monday_lagged_error <- rowMeans(Lag(monday$Error, 1:lags), na.rm="TRUE")
  tuesday_lagged_error <- rowMeans(Lag(tuesday$Error, 1:lags), na.rm="TRUE")
  wednesday_lagged_error <- rowMeans(Lag(wednesday$Error, 1:lags), na.rm="TRUE")
  thursday_lagged_error <- rowMeans(Lag(thursday$Error, 1:lags), na.rm="TRUE")
  friday_lagged_error <- rowMeans(Lag(friday$Error, 1:lags), na.rm="TRUE")
  
  monday <- cbind(monday$call_date, monday_lagged_error)
  tuesday <- cbind(tuesday$call_date, tuesday_lagged_error)
  wednesday <- cbind(wednesday$call_date, wednesday_lagged_error)
  thursday <- cbind(thursday$call_date, thursday_lagged_error)
  friday <- cbind(friday$call_date, friday_lagged_error)
  all_lagged_error <- as.data.frame(rbind(monday, tuesday, wednesday, thursday, friday))
  names(all_lagged_error) <- c("call_date", "lagged_error")
  
  # add back to full_estimates data
  full_estimates_1 <- merge(full_estimates, all_lagged_error, by="call_date", all.x="TRUE")
  
  # add new projection ???
  full_estimates_1$total_projection_new <- 
    ifelse((wday(full_estimates_1$call_date) != 1 & wday(full_estimates_1$call_date) != 7 &
      is.na(full_estimates_1$Holiday)), 
      ifelse(full_estimates_1$lagged_error > (maxadjust * full_estimates_1$total_projection), 
         (1+maxadjust) * full_estimates_1$total_projection,  
         ifelse(full_estimates_1$lagged_error < -(maxadjust * full_estimates_1$total_projection), 
                (1-maxadjust) * full_estimates_1$total_projection,
                full_estimates_1$total_projection + full_estimates_1$lagged_error)), full_estimates_1$total_projection)
  
  full_estimates_1$total_projection_new <- 
    ifelse(is.nan(full_estimates_1$total_projection_new) == "FALSE", 
           full_estimates_1$total_projection_new, full_estimates_1$total_projection)
  
  #If projection has been changed 
  adjustment <- full_estimates_1$total_projection_new / full_estimates_1$total_projection
  full_estimates_1[ ,4:12] <- full_estimates_1[ ,4:12] * adjustment
  
  #Change Paid and Digital Media
  paid_web_projection <- lm(full_estimates_1$Paid_web[(nrow(full_estimates_1) - 120):nrow(full_estimates_1)] ~ 
                              0 + full_estimates_1$Paid_web_projection[(nrow(full_estimates_1) - 120):nrow(full_estimates_1)])
  
  #Now, just round categories
  full_estimates_1[ ,4:12] <- round(full_estimates_1[ ,4:12], 0)
  
  #And re-sum projections
  full_estimates_1$total_projection_new <- rowSums(full_estimates_1[ ,4:12])
  
  full_estimates_1$new_error <- full_estimates_1$all_calls - full_estimates_1$total_projection_new
  
  full_estimates_1$total_projection <- full_estimates_1$total_projection_new
  full_estimates_1$Error <- full_estimates_1$new_error
  
  full_estimates_1$total_projection_new <- full_estimates_1$lagged_error <- full_estimates_1$new_error <- NULL
  full_estimates_1$Holiday <- ifelse(is.na(full_estimates_1$Holiday), "NO", as.character(full_estimates_1$Holiday))
  full_estimates_1$Year <- year(full_estimates_1$call_date)
  full_estimates_1$Month <- month(full_estimates_1$call_date)
  full_estimates_1$Day <- day(full_estimates_1$call_date)
  
  RMSE_monthly <- aggregate(full_estimates_1$Error, by=list(full_estimates_1$Year, full_estimates_1$Month), FUN=sd, na.rm="TRUE")
  names(RMSE_monthly) <- c("Year", "Month", "sd_Error")
  
  bias_monthly <- aggregate(full_estimates_1$Error, by=list(full_estimates_1$Year, full_estimates_1$Month), FUN=mean, na.rm="TRUE")
  names(bias_monthly) <- c("Year", "Month", "Average_Miss")
  
  full_estimates_1 <- merge(full_estimates_1, RMSE_monthly, by=c("Year","Month"), all.x="TRUE")
  full_estimates_1 <- merge(full_estimates_1, bias_monthly, by=c("Year","Month"), all.x="TRUE")
  
  full_estimates_1$call_date <- as.character(full_estimates_1$call_date)
  
  
  #Order by call_date
  full_estimates_1 <- full_estimates_1[order(full_estimates_1$call_date), ]
  
  #Get rid of holiday adjustment
  full_estimates_1$holiday_adjustment <- NULL
  
  full_estimates_1 <- full_estimates_1[c("Year", "Month", "Day", "day_of_week", "call_date", "Holiday", "mktg_direct_projection",
                                         "customer_service_projection", "DB_IVR_projection", "DB_com_projection", 
                                         "Direct_Extension_projection", "iUpdate_projection", "Organic_projection",
                                         "Paid_web_projection", "other_projection", 
                                         "mktg_direct", "customer_service", "DB_IVR", "DB_com", 
                                         "Direct_Extension", "iUpdate", "Organic",
                                         "Paid_web", "other", "total_projection", "all_calls", "Error", "sd_Error", "Average_Miss")]
  
  return(list("friday" = friday, "monday_lagged_error" = monday_lagged_error, 
              "all_lagged_error" = all_lagged_error, "full_estimates_1" = full_estimates_1, 
              "RMSE_monthly" = RMSE_monthly, "bias_monthly" = bias_monthly,
              "paid_web_projection" = paid_web_projection))    
}  
  
  