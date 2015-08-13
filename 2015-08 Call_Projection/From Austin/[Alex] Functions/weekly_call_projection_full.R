weekly_call_projection_full <- function() { 
  library(lubridate)
  
  called_data <- pull_call_data(channel= channel, call_date= '1/01/2014') ## needs to be loaded (create R package)
  
  #Then, compose full data
  model_data <- clean_model_data(called_data, last_day) 
  
  #Run projections
  y <- monthly_run(DM_data= model_data$campaign_response, start_month= 5, end_month= month(Sys.Date()), 
                   start_year= 2014, end_year= year(Sys.Date()), min_days= 30, 
                   estimation_limit_called_proj= 10, momentum_limit= 0)
  
  return(list("called" = called_data, "campaign_response" = model_data$campaign_response, 
              "end_of_month_date" = last_day, "called_by_day" = model_data$called_by_day, 
              "full_projections" = y$full_projections, "y" = y))
}


#----------------------------------------------------------------------------------------
# original
#----------------------------------------------------------------------------------------

weekly_call_projection_full <- function() { 
  #Load 'lubridate' to work with dates
  library(lubridate)
  
  #First, query calls (mktg calls only)
  x <- call_query()
  
  #Find last day of month
  last_day <- ifelse(is.Date(try(as.Date(paste(year(Sys.Date()), "-", month(Sys.Date())+1, "-", 31, sep="")), TRUE)) == "TRUE", paste(year(Sys.Date()), "-", month(Sys.Date())+1, "-", 31, sep=""), 
                     ifelse(is.Date(try(as.Date(paste(year(Sys.Date()), "-", month(Sys.Date())+1, "-", 30, sep="")), TRUE)) == "TRUE", paste(year(Sys.Date()), "-", month(Sys.Date())+1, "-", 30, sep=""), 
                            ifelse(is.Date(try(as.Date(paste(year(Sys.Date()), "-", month(Sys.Date())+1, "-", 29, sep="")), TRUE)) == "TRUE", paste(year(Sys.Date()), "-", month(Sys.Date())+1, "-", 29, sep=""),
                                   ifelse(is.Date(try(as.Date(paste(year(Sys.Date()), "-", month(Sys.Date())+1, "-", 28, sep="")), TRUE)) == "TRUE", paste(year(Sys.Date()), "-", month(Sys.Date())+1, "-", 28, sep=""))))) 
  
  #Then, compose full data (mktg only)
  a <- DM_call_model_data_load_and_clean(x$called, last_day)
  
  #Run projections
  #month(Sys.Date())
  y <- monthly_run(a$campaign_response, 5, month(Sys.Date())+1, 2014, year(Sys.Date()), 30, 10, 0, All_holidays_1)
  
  full_estimates <- y$full_estimates
  
  
  #output <- list("called" = x$called, "campaign_response" = a$campaign_response, "end_of_month_date" = last_day,
  #               "called_by_day" = a$called_by_day, "full_projections" = y$full_projections, "y" = y)
  
  output <- list("a" = a, "x" = x, "y" = y)
  return(output)
}
