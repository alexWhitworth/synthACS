monthly_adjustment <- function(all_calls, control_year_min, control_year_max, alpha= 0.1) {
  
  require(lubridate) # [AW 7/31] Needed for year(), etc
  require(dplyr)     # Needed to replace aggregate()
  
  #Add year to all_calls
  all_calls$year <- year(all_calls$call_date)
  
  # Subset to control years
  all_calls <- all_calls[all_calls$year >= control_year_min & all_calls$year <= control_year_max, ]
  
  ### 01. Do some aggregation on call data
  #--------------------------------------------
  #monthly_calls
  calls_by_month <- all_calls %>% group_by(year, month) %>%
    summarize(calls= sum(mktg_call_count, na.rm=TRUE))
  
  avg_calls_by_month <- calls_by_month %>% group_by(month) %>%
    summarize(calls= sum(calls, na.rm=TRUE))
  
  ### 02. Normalize monthly calls, only if monthly calls are statistically different by month
  #-------------------------------------------
  mo_avg_calls <- mean(avg_calls_by_month$calls) # [AW 7/31] Does this need to be stored as a vector? Used as scalar... Answer == NO
  # Norm
  avg_calls_by_month$monthly_norm <- avg_calls_by_month$calls / mo_avg_calls
  
  #Test Stat. siginificant difference (alpha = 0.1) ??
  avg_calls_by_month$p.value <- dnorm((avg_calls_by_month$monthly_norm - 1) / (sd(avg_calls_by_month$monthly_norm) / sqrt(12)))
  avg_calls_by_month$monthly_adjustment <- ifelse(avg_calls_by_month$p.value <= alpha, avg_calls_by_month$monthly_norm, 1)
  
  ##Monthly adjustment
  monthly_adjustment <- as.data.frame(month= avg_calls_by_month$month, 
                                      monthly_adjustment= avg_calls_by_month$monthly_adjustment)
  
  # return
  # [AW 7/31] Need to return all of this?
  return(list(calls_by_month= calls_by_month, 
              avg_calls_by_month= avg_calls_by_month, avg_calls= mo_avg_calls, 
              monthly_adjustment= monthly_adjustment))
  
}


#----------------------------------------------------------------------------------------
# original
#----------------------------------------------------------------------------------------
monthly_adjustment <- function(all_calls, control_year_min, control_year_max) {
  
  #Add year to all_calls
  all_calls$year <- year(all_calls$call_date)
  
  #Limit to control years
  all_calls <- all_calls[all_calls$year <= control_year_max & all_calls$year >= control_year_min, ]
  
  #monthly_calls
  calls_by_month <- aggregate(all_calls$mktg_call_count, by=list(all_calls$year, all_calls$month), FUN=sum, na.rm="TRUE")    
  names(calls_by_month) <- c("year", "month", "calls")
  
  avg_calls_by_month <- aggregate(calls_by_month$calls, by=list(calls_by_month$month), FUN=mean, na.rm="TRUE")    
  names(avg_calls_by_month) <- c("month", "calls")
  
  avg_calls <- mean(avg_calls_by_month$calls)
  avg_calls_by_month$avg_calls <- avg_calls
  
  #Norm
  avg_calls_by_month$monthly_norm <- avg_calls_by_month$calls / avg_calls_by_month$avg_calls
  
  #Stat. siginificant at 90% level??
  avg_calls_by_month$p.value <- dnorm((avg_calls_by_month$monthly_norm - 1) / (sd(avg_calls_by_month$monthly_norm) / sqrt(12)))
  
  avg_calls_by_month$monthly_adjustment <- ifelse(avg_calls_by_month$p.value <= .10, avg_calls_by_month$monthly_norm, 1)
  
  ##Monthly adjustment
  monthly_adjustment <- as.data.frame(cbind(avg_calls_by_month$month, avg_calls_by_month$monthly_adjustment))
  names(monthly_adjustment) <- c("month", "monthly_adjustment")
  
  output <- list("calls_by_month" = calls_by_month, "avg_calls_by_month" = avg_calls_by_month, "avg_calls" = avg_calls, 
                 "monthly_adjustment" = monthly_adjustment)
  return(output)
  
}