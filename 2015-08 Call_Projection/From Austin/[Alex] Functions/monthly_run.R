
#' @param DM_data A \code{data.frame}. Returned \code{campaign_response} from \code{clean_model_data()}.
#' @param start_month An integer $\in$ [1,12] corresponding to the ???
#' @param end_month An integer $\in$ [1,12] corresponding to the ???
#' @param start_year An integer corresponding to ???
#' @param end_year An integer corresponding to ???
#' @param min_days ???
#' @param estimation_limit_called_proj ???
#' @param momentum_limit ???
monthly_run <- function(DM_data, start_month, end_month, start_year, end_year, 
                        min_days, estimation_limit_called_proj, momentum_limit) {
  library(RODBC)
  library(lubridate)
  
  #First, add 'Holiday' variable to DM_data
  #Scrape all holiays from the web (timeanddate.com)    
  holiday <- Holiday_normalization(12, (end_year + 1), calls_holiday$called)
  holiday$All_holidays_1$response_date <- as.Date(paste(holiday$All_holidays_1$Year, 
                                                        holiday$All_holidays_1$Month, holiday$All_holidays_1$Day, sep="-"))
  holiday$All_holidays_1$Year <- NULL
  holiday$All_holidays_1$Month <- NULL
  holiday$All_holidays_1$Day <- NULL
  
  holidays <- holiday$All_holidays_1
  
  DM_data <- merge(DM_data, holidays, by="response_date", all.x = "TRUE")
  
  #Initialize training and testing data
  training_data <- DM_data[(DM_data$month_response_date < start_month & DM_data$year_response_date == start_year) |
                             DM_data$year_response_date < start_year, ]    
  
  OOS_data <- DM_data[(DM_data$month_response_date == start_month & DM_data$year_response_date == start_year), ]    
  
  #Make month output which outputs all statistics for every month
  #----------------------------------------------------------------------------------------
  monthly_output <- list()
  
  #Initialize k to 1
  k <- 1
  #Loop over months
  for(j in start_year:end_year) {
    for(i in 1:12) {
      
      current_month <- i
      current_year <- j
      
      #Conditions needed to run 
      if((i >= start_month & j == start_year & start_year != end_year) | # eg.  2014-05 <= MM-YYYY <= 2014-12
         (j > start_year & j < end_year & start_year != end_year) |      # eg. 2014 < YYYY < 2015 -- impossible
         (j == end_year & i <= end_month & start_year != end_year) |     # eg. 2015-01 -- 2015-09
         (i >= start_month & i <= end_month & start_year == end_year)) { # eg. start_year != end_year -- impossible
        #Start_month run logic is different (past_proj == FALSE)
        if(i == start_month & j == start_year) {
          x <- DM_call_estimate(full_data= DM_data, training_data= training_data, OOS_data= OOS_data, 
                                min_days= min_days, estimation_limit_called_proj, momentum_limit, 
                                past_proj= "FALSE", holiday_adj= "FALSE", current_month, current_year) 
          
          x$year <- j 
          x$month <- i
          monthly_output[[k]] <- x
        }
        else {
          #Assign new DM data
          DM_data <- x$full_data
          
          #New training and testing data
          training_data <- DM_data[(DM_data$month_response_date < i & DM_data$year_response_date == j) | 
                                   DM_data$year_response_date < j, ]
          OOS_data <- DM_data[DM_data$month_response_date == i & DM_data$year_response_date == j, ]
          
          #Run DM_call_estimate 
          x <- DM_call_estimate(DM_data, training_data, OOS_data, min_days, estimation_limit_called_proj, momentum_limit, 
                                past_proj= "TRUE", holiday_adj= "TRUE", current_month, current_year)
          x$year <- j 
          x$month <- i
          monthly_output[[k]] <- x
          
        }
        
        #Increment count by 1
        k <- k + 1
        DM_output <- x 
        
      }
    }
  }
  
  #Bind together total_projected_responses into one dataset
  #Initialize y
  y <- monthly_output[[1]]$total_projected_responses_1
  for(i in 2:length(monthly_output)) {
    y <- rbind(y, monthly_output[[i]]$total_projected_responses_1)
  }
  
  full_projections <- y
  
  # [AW] Calculat some error statistics and return
  #----------------------------------------------------------------------------------------
  
  #Error
  full_projections$Error <- full_projections$projected - full_projections$Called
  
  #monthly_miss_standard_deviation
  library(lubridate)
  
  #Year, month
  full_projections$Year <- year(full_projections$response_date)
  full_projections$Month <- month(full_projections$response_date)
  
  error <- aggregate(full_projections$Error, 
                     by=list(full_projections$Year, full_projections$Month), FUN=sd, na.rm="TRUE")
  
  full_projections <- merge(full_projections, error, by.x=c("Year", "Month"), by.y=c("Group.1", "Group.2"), all.x="TRUE")
  full_projections$RMSE_monthly <- full_projections$x
  full_projections$x <- NULL
  
  full_projections$projected <- round(full_projections$projected, 2)
  full_projections$Error <- round(full_projections$Error, 2)
  full_projections$RMSE_monthly <- round(full_projections$RMSE_monthly, 2)
  
  #Order
  full_projections <- full_projections[order(full_projections$response_date), ]
  
  
  # [AW] NOTES: 
  # x = most recent month of predictions
  # monthly output = the list of projections -- this is redundant
  # full projections = a single data frame that is a copy of monthly output (plus some stats)
  # error = aggregated error by year/month -- redundant (in full projections)
  # dm_output = x -- redundant
  # holidays = list of holiday information
  # dm_data = one of the inputs -- redundant
  #----------------------------------------------------------------------------------------
  return(list("x" = x, "monthly_output" = monthly_output, "full_projections" = full_projections, 
              "error" = error, "DM_output" = DM_output, 
              "holidays" = holidays, "DM_data" = DM_data))      
}