call_regressions <- function(calls, projections, current_date, lagback, perc_lower, 
                             perc_upper, perc_lower_mktg, perc_upper_mktg, holiday_calls) {
  #lagback is the number of weeks to lagback to get percentiles on call counts
  
  #Bring in 'quantmod' to lag time-series
  library(quantmod)
  
  #Bring in 'lubridate' for dates
  library(lubridate)
  
  #Hold total_calls
  total_calls <- calls
  
  #First order calls correctly (oldest to newest)
  calls <- calls[order(calls$call_date), ]
  
  #Limit calls to those in which date < current_date
  calls <- calls[(calls$call_date < current_date), ]
  
  #Coerce projections to a data frame
  projections <- as.data.frame(projections)
  names(projections) <- c("call_date", "Called", "mktg_direct")
  projections$Called <- NULL
  
  #Holiday normalization
  holiday_norm <- Holiday_norm_new(All_holidays_1, month(max(projections$call_date)), year(max(projections$call_date)), holiday_calls)
  All_holidays <- holiday_norm$All_holidays_2
  
  #Holiday date
  All_holidays$call_date <- All_holidays$response_date
  All_holidays$response_date <- NULL
  
  #Now, merge All_holidays to calls AND projections
  calls <- merge(calls, All_holidays, by="call_date", all.x="TRUE")
  calls <- merge(calls, holiday_norm$Holiday_adjustment, by="Holiday", all.x="TRUE")
  
  projections <- merge(projections, All_holidays, by="call_date", all.x="TRUE")
  projections <- merge(projections, holiday_norm$Holiday_adjustment, by="Holiday", all.x="TRUE")
  
  #Limit calls to Holiday and non-Holidays 
  calls_holiday <- calls[is.na(calls$Holiday) == "FALSE", ]
  calls <- calls[is.na(calls$Holiday), ]
  
  #order projections and calls by date
  calls <- calls[order(calls$call_date), ]
  projections <- projections[order(projections$call_date), ]
  
  ###Average 30-60 day same day of week lag for each call type)
  projections_1 <- projections                              
  
  projections_1$customer_service <- NA
  projections_1$DB_IVR <- NA
  projections_1$DB_com <- NA
  projections_1$Direct_Extension <- NA
  projections_1$iUpdate <- NA
  projections_1$Organic <- NA
  projections_1$Paid_web <- NA
  projections_1$all_calls <- NA
  projections_1$other <- NA
  projections_1$day_of_week <- wday(projections_1$call_date)
  
  #full calls, mean 1-month trailing average all call types
  full_calls <- rbind(calls, projections_1)
  
  #Order full_calls by call_date
  full_calls <- full_calls[order(full_calls$call_date), ]
  
  #Sunday
  full_calls_sunday <- full_calls[full_calls$day_of_week == 1, ]
  lag_mktg_direct_sunday <- rowMeans(Lag(full_calls_sunday$mktg_direct, 1:5), na.rm="TRUE")
  lag_customer_service_sunday <- rowMeans(Lag(full_calls_sunday$customer_service, 1:5), na.rm="TRUE")
  lag_DB_IVR_sunday <- rowMeans(Lag(full_calls_sunday$DB_IVR, 1:5), na.rm="TRUE")
  lag_DB_com_sunday <- rowMeans(Lag(full_calls_sunday$DB_com, 1:5), na.rm="TRUE")
  lag_iUpdate_sunday <- rowMeans(Lag(full_calls_sunday$iUpdate, 1:5), na.rm="TRUE")
  lag_Direct_Extension_sunday <- rowMeans(Lag(full_calls_sunday$Direct_Extension, 1:5), na.rm="TRUE")
  lag_Organic_sunday <- rowMeans(Lag(full_calls_sunday$Organic, 1:5), na.rm="TRUE")
  lag_Paid_web_sunday <- rowMeans(Lag(full_calls_sunday$Paid_web, 1:5), na.rm="TRUE")
  lag_other_sunday <- rowMeans(Lag(full_calls_sunday$other, 1:5), na.rm="TRUE")
  
  full_sunday <- cbind(full_calls_sunday, lag_mktg_direct_sunday, lag_customer_service_sunday, lag_DB_IVR_sunday, lag_DB_com_sunday,
                       lag_iUpdate_sunday, lag_Direct_Extension_sunday, lag_Organic_sunday, lag_Paid_web_sunday, lag_other_sunday)
  
  names(full_sunday) <-c("Holiday", "call_date", "mktg_direct", "customer_service", "DB_IVR", "DB_com", "Direct_Extension", "iUpdate",                    
                         "Organic", "Paid_web", "all_calls", "other", "day_of_week", "holiday_adjustment", "lag_mktg_direct",
                         "lag_customer_service", "lag_DB_IVR", "lag_DB_com", "lag_iUpdate", "lag_Direct_Extension", "lag_Organic", "lag_Paid_web",
                         "lag_other")
  
  #Monday
  full_calls_monday <- full_calls[full_calls$day_of_week == 2, ]
  lag_mktg_direct_monday <- rowMeans(Lag(full_calls_monday$mktg_direct, 1:5), na.rm="TRUE")
  lag_customer_service_monday <- rowMeans(Lag(full_calls_monday$customer_service, 1:5), na.rm="TRUE")
  lag_DB_IVR_monday <- rowMeans(Lag(full_calls_monday$DB_IVR, 1:5), na.rm="TRUE")
  lag_DB_com_monday <- rowMeans(Lag(full_calls_monday$DB_com, 1:5), na.rm="TRUE")
  lag_iUpdate_monday <- rowMeans(Lag(full_calls_monday$iUpdate, 1:5), na.rm="TRUE")
  lag_Direct_Extension_monday <- rowMeans(Lag(full_calls_monday$Direct_Extension, 1:5), na.rm="TRUE")
  lag_Organic_monday <- rowMeans(Lag(full_calls_monday$Organic, 1:5), na.rm="TRUE")
  lag_Paid_web_monday <- rowMeans(Lag(full_calls_monday$Paid_web, 1:5), na.rm="TRUE")
  lag_other_monday <- rowMeans(Lag(full_calls_monday$other, 1:5), na.rm="TRUE")
  
  full_monday <- cbind(full_calls_monday, lag_mktg_direct_monday, lag_customer_service_monday, lag_DB_IVR_monday, lag_DB_com_monday,
                       lag_iUpdate_monday, lag_Direct_Extension_monday, lag_Organic_monday, lag_Paid_web_monday, lag_other_monday)
  
  names(full_monday) <-c("Holiday", "call_date", "mktg_direct", "customer_service", "DB_IVR", "DB_com", "Direct_Extension", "iUpdate",                    
                         "Organic", "Paid_web", "all_calls", "other", "day_of_week", "holiday_adjustment", "lag_mktg_direct",
                         "lag_customer_service", "lag_DB_IVR", "lag_DB_com", "lag_iUpdate", "lag_Direct_Extension", "lag_Organic", "lag_Paid_web",
                         "lag_other")
  
  #Tuesday
  full_calls_tuesday <- full_calls[full_calls$day_of_week == 3, ]
  lag_mktg_direct_tuesday <- rowMeans(Lag(full_calls_tuesday$mktg_direct, 1:5), na.rm="TRUE")
  lag_customer_service_tuesday <- rowMeans(Lag(full_calls_tuesday$customer_service, 1:5), na.rm="TRUE")
  lag_DB_IVR_tuesday <- rowMeans(Lag(full_calls_tuesday$DB_IVR, 1:5), na.rm="TRUE")
  lag_DB_com_tuesday <- rowMeans(Lag(full_calls_tuesday$DB_com, 1:5), na.rm="TRUE")
  lag_iUpdate_tuesday <- rowMeans(Lag(full_calls_tuesday$iUpdate, 1:5), na.rm="TRUE")
  lag_Direct_Extension_tuesday <- rowMeans(Lag(full_calls_tuesday$Direct_Extension, 1:5), na.rm="TRUE")
  lag_Organic_tuesday <- rowMeans(Lag(full_calls_tuesday$Organic, 1:5), na.rm="TRUE")
  lag_Paid_web_tuesday <- rowMeans(Lag(full_calls_tuesday$Paid_web, 1:5), na.rm="TRUE")
  lag_other_tuesday <- rowMeans(Lag(full_calls_tuesday$other, 1:5), na.rm="TRUE")
  
  full_tuesday <- cbind(full_calls_tuesday, lag_mktg_direct_tuesday, lag_customer_service_tuesday, lag_DB_IVR_tuesday, lag_DB_com_tuesday,
                        lag_iUpdate_tuesday, lag_Direct_Extension_tuesday, lag_Organic_tuesday, lag_Paid_web_tuesday, lag_other_tuesday)
  
  names(full_tuesday) <-c("Holiday", "call_date", "mktg_direct", "customer_service", "DB_IVR", "DB_com", "Direct_Extension", "iUpdate",                    
                          "Organic", "Paid_web", "all_calls", "other", "day_of_week", "holiday_adjustment", "lag_mktg_direct",
                          "lag_customer_service", "lag_DB_IVR", "lag_DB_com", "lag_iUpdate", "lag_Direct_Extension", "lag_Organic", "lag_Paid_web",
                          "lag_other")
  
  #wednesday
  full_calls_wednesday <- full_calls[full_calls$day_of_week == 4, ]
  lag_mktg_direct_wednesday <- rowMeans(Lag(full_calls_wednesday$mktg_direct, 1:5), na.rm="TRUE")
  lag_customer_service_wednesday <- rowMeans(Lag(full_calls_wednesday$customer_service, 1:5), na.rm="TRUE")
  lag_DB_IVR_wednesday <- rowMeans(Lag(full_calls_wednesday$DB_IVR, 1:5), na.rm="TRUE")
  lag_DB_com_wednesday <- rowMeans(Lag(full_calls_wednesday$DB_com, 1:5), na.rm="TRUE")
  lag_iUpdate_wednesday <- rowMeans(Lag(full_calls_wednesday$iUpdate, 1:5), na.rm="TRUE")
  lag_Direct_Extension_wednesday <- rowMeans(Lag(full_calls_wednesday$Direct_Extension, 1:5), na.rm="TRUE")
  lag_Organic_wednesday <- rowMeans(Lag(full_calls_wednesday$Organic, 1:5), na.rm="TRUE")
  lag_Paid_web_wednesday <- rowMeans(Lag(full_calls_wednesday$Paid_web, 1:5), na.rm="TRUE")
  lag_other_wednesday <- rowMeans(Lag(full_calls_wednesday$other, 1:5), na.rm="TRUE")
  
  full_wednesday <- cbind(full_calls_wednesday, lag_mktg_direct_wednesday, lag_customer_service_wednesday, lag_DB_IVR_wednesday, lag_DB_com_wednesday,
                          lag_iUpdate_wednesday, lag_Direct_Extension_wednesday, lag_Organic_wednesday, lag_Paid_web_wednesday, lag_other_wednesday)
  
  names(full_wednesday) <-c("Holiday", "call_date", "mktg_direct", "customer_service", "DB_IVR", "DB_com", "Direct_Extension", "iUpdate",                    
                            "Organic", "Paid_web", "all_calls", "other", "day_of_week", "holiday_adjustment", "lag_mktg_direct",
                            "lag_customer_service", "lag_DB_IVR", "lag_DB_com", "lag_iUpdate", "lag_Direct_Extension", "lag_Organic", "lag_Paid_web",
                            "lag_other")
  
  #thursday
  full_calls_thursday <- full_calls[full_calls$day_of_week == 5, ]
  lag_mktg_direct_thursday <- rowMeans(Lag(full_calls_thursday$mktg_direct, 1:5), na.rm="TRUE")
  lag_customer_service_thursday <- rowMeans(Lag(full_calls_thursday$customer_service, 1:5), na.rm="TRUE")
  lag_DB_IVR_thursday <- rowMeans(Lag(full_calls_thursday$DB_IVR, 1:5), na.rm="TRUE")
  lag_DB_com_thursday <- rowMeans(Lag(full_calls_thursday$DB_com, 1:5), na.rm="TRUE")
  lag_iUpdate_thursday <- rowMeans(Lag(full_calls_thursday$iUpdate, 1:5), na.rm="TRUE")
  lag_Direct_Extension_thursday <- rowMeans(Lag(full_calls_thursday$Direct_Extension, 1:5), na.rm="TRUE")
  lag_Organic_thursday <- rowMeans(Lag(full_calls_thursday$Organic, 1:5), na.rm="TRUE")
  lag_Paid_web_thursday <- rowMeans(Lag(full_calls_thursday$Paid_web, 1:5), na.rm="TRUE")
  lag_other_thursday <- rowMeans(Lag(full_calls_thursday$other, 1:5), na.rm="TRUE")
  
  full_thursday <- cbind(full_calls_thursday, lag_mktg_direct_thursday, lag_customer_service_thursday, lag_DB_IVR_thursday, lag_DB_com_thursday,
                         lag_iUpdate_thursday, lag_Direct_Extension_thursday, lag_Organic_thursday, lag_Paid_web_thursday, lag_other_thursday)
  
  names(full_thursday) <-c("Holiday", "call_date", "mktg_direct", "customer_service", "DB_IVR", "DB_com", "Direct_Extension", "iUpdate",                    
                           "Organic", "Paid_web", "all_calls", "other", "day_of_week", "holiday_adjustment", "lag_mktg_direct",
                           "lag_customer_service", "lag_DB_IVR", "lag_DB_com", "lag_iUpdate", "lag_Direct_Extension", "lag_Organic", "lag_Paid_web",
                           "lag_other")
  
  #friday
  full_calls_friday <- full_calls[full_calls$day_of_week == 6, ]
  lag_mktg_direct_friday <- rowMeans(Lag(full_calls_friday$mktg_direct, 1:5), na.rm="TRUE")
  lag_customer_service_friday <- rowMeans(Lag(full_calls_friday$customer_service, 1:5), na.rm="TRUE")
  lag_DB_IVR_friday <- rowMeans(Lag(full_calls_friday$DB_IVR, 1:5), na.rm="TRUE")
  lag_DB_com_friday <- rowMeans(Lag(full_calls_friday$DB_com, 1:5), na.rm="TRUE")
  lag_iUpdate_friday <- rowMeans(Lag(full_calls_friday$iUpdate, 1:5), na.rm="TRUE")
  lag_Direct_Extension_friday <- rowMeans(Lag(full_calls_friday$Direct_Extension, 1:5), na.rm="TRUE")
  lag_Organic_friday <- rowMeans(Lag(full_calls_friday$Organic, 1:5), na.rm="TRUE")
  lag_Paid_web_friday <- rowMeans(Lag(full_calls_friday$Paid_web, 1:5), na.rm="TRUE")
  lag_other_friday <- rowMeans(Lag(full_calls_friday$other, 1:5), na.rm="TRUE")
  
  full_friday <- cbind(full_calls_friday, lag_mktg_direct_friday, lag_customer_service_friday, lag_DB_IVR_friday, lag_DB_com_friday,
                       lag_iUpdate_friday, lag_Direct_Extension_friday, lag_Organic_friday, lag_Paid_web_friday, lag_other_friday)
  
  names(full_friday) <-c("Holiday", "call_date", "mktg_direct", "customer_service", "DB_IVR", "DB_com", "Direct_Extension", "iUpdate",                    
                         "Organic", "Paid_web", "all_calls", "other", "day_of_week", "holiday_adjustment", "lag_mktg_direct",
                         "lag_customer_service", "lag_DB_IVR", "lag_DB_com", "lag_iUpdate", "lag_Direct_Extension", "lag_Organic", "lag_Paid_web",
                         "lag_other")
  
  #saturday
  full_calls_saturday <- full_calls[full_calls$day_of_week == 7, ]
  lag_mktg_direct_saturday <- rowMeans(Lag(full_calls_saturday$mktg_direct, 1:5), na.rm="TRUE")
  lag_customer_service_saturday <- rowMeans(Lag(full_calls_saturday$customer_service, 1:5), na.rm="TRUE")
  lag_DB_IVR_saturday <- rowMeans(Lag(full_calls_saturday$DB_IVR, 1:5), na.rm="TRUE")
  lag_DB_com_saturday <- rowMeans(Lag(full_calls_saturday$DB_com, 1:5), na.rm="TRUE")
  lag_iUpdate_saturday <- rowMeans(Lag(full_calls_saturday$iUpdate, 1:5), na.rm="TRUE")
  lag_Direct_Extension_saturday <- rowMeans(Lag(full_calls_saturday$Direct_Extension, 1:5), na.rm="TRUE")
  lag_Organic_saturday <- rowMeans(Lag(full_calls_saturday$Organic, 1:5), na.rm="TRUE")
  lag_Paid_web_saturday <- rowMeans(Lag(full_calls_saturday$Paid_web, 1:5), na.rm="TRUE")
  lag_other_saturday <- rowMeans(Lag(full_calls_saturday$other, 1:5), na.rm="TRUE")
  
  full_saturday <- cbind(full_calls_saturday, lag_mktg_direct_saturday, lag_customer_service_saturday, lag_DB_IVR_saturday, lag_DB_com_saturday,
                         lag_iUpdate_saturday, lag_Direct_Extension_saturday, lag_Organic_saturday, lag_Paid_web_saturday, lag_other_saturday)
  
  names(full_saturday) <-c("Holiday", "call_date", "mktg_direct", "customer_service", "DB_IVR", "DB_com", "Direct_Extension", "iUpdate",                    
                           "Organic", "Paid_web", "all_calls", "other", "day_of_week", "holiday_adjustment", "lag_mktg_direct",
                           "lag_customer_service", "lag_DB_IVR", "lag_DB_com", "lag_iUpdate", "lag_Direct_Extension", "lag_Organic", "lag_Paid_web",
                           "lag_other")
  
  all_calls <- rbind(full_sunday, full_monday, full_tuesday, full_wednesday, full_thursday, full_friday, full_saturday)
  
  #Order all calls
  all_calls <- all_calls[order(all_calls$call_date), ]
  
  #Assign all_calls to calls and projections
  calls <- all_calls[all_calls$call_date < current_date, ]
  projections <- all_calls[all_calls$call_date >= current_date, ]
  
  #Regression of Customer Service on log(mktg_direct)
  attach(calls)
  
  #Calls_Sunday
  calls_Sunday <- calls[calls$day_of_week == 1, ]
  calls_Sunday <- calls_Sunday[(nrow(calls_Sunday)-(lagback-1)):nrow(calls_Sunday), ]
  
  #Calls_Monday
  calls_Monday <- calls[calls$day_of_week == 2, ]
  calls_Monday <- calls_Monday[(nrow(calls_Monday)-(lagback-1)):nrow(calls_Monday), ]
  
  #Calls_Tuesday
  calls_Tuesday <- calls[calls$day_of_week == 3, ]
  calls_Tuesday <- calls_Tuesday[(nrow(calls_Tuesday)-(lagback-1)):nrow(calls_Tuesday), ]
  
  #Calls_Wednesday
  calls_Wednesday<- calls[calls$day_of_week == 4, ]
  calls_Wednesday <- calls_Wednesday[(nrow(calls_Wednesday)-(lagback-1)):nrow(calls_Wednesday), ]
  
  #Calls_Thursday
  calls_Thursday <- calls[calls$day_of_week == 5, ]
  calls_Thursday <- calls_Thursday[(nrow(calls_Thursday)-(lagback-1)):nrow(calls_Thursday), ]
  
  #Calls_Friday
  calls_Friday <- calls[calls$day_of_week == 6, ]
  calls_Friday <- calls_Friday[(nrow(calls_Friday)-(lagback-1)):nrow(calls_Friday), ]
  
  #Calls_Friday
  calls_Saturday <- calls[calls$day_of_week == 7, ]
  calls_Saturday <- calls_Saturday[(nrow(calls_Saturday)-(lagback-1)):nrow(calls_Saturday), ]
  
  #Stepwise regression, customer_service
  a <- step(lm(customer_service ~ mktg_direct + log(mktg_direct) + lag_mktg_direct), subset = day_of_week == 3)
  
  #cust_service
  cust_service_reg_1 <- lm(a$terms, subset = day_of_week == 1)
  cust_service_reg_2 <- lm(a$terms, subset = day_of_week == 2)
  cust_service_reg_3 <- lm(a$terms, subset = day_of_week == 3)
  cust_service_reg_4 <- lm(a$terms, subset = day_of_week == 4)
  cust_service_reg_5 <- lm(a$terms, subset = day_of_week == 5)
  cust_service_reg_6 <- lm(a$terms, subset = day_of_week == 6)
  cust_service_reg_7 <- lm(a$terms, subset = day_of_week == 7)
  
  cust_service_predict_1 <- predict(cust_service_reg_1, projections)
  cust_service_predict_2 <- predict(cust_service_reg_2, projections)
  cust_service_predict_3 <- predict(cust_service_reg_3, projections)
  cust_service_predict_4 <- predict(cust_service_reg_4, projections)
  cust_service_predict_5 <- predict(cust_service_reg_5, projections)
  cust_service_predict_6 <- predict(cust_service_reg_6, projections)
  cust_service_predict_7 <- predict(cust_service_reg_7, projections)
  
  cust_service_predict <- cbind(cust_service_predict_1, cust_service_predict_2, cust_service_predict_3, cust_service_predict_4, 
                                cust_service_predict_5, cust_service_predict_6, cust_service_predict_7)
  
  #Coerce to dataframe and name
  cust_service_predict <- as.data.frame(cbind(projections$call_date, cust_service_predict))
  names(cust_service_predict) <- c("call_date", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  cust_service_predict$call_date <- as.Date(cust_service_predict$call_date, origin="1970-01-01")
  
  cust_service_predict$customer_service_unbounded <- ifelse(wday(cust_service_predict$call_date) == 1, cust_service_predict$Sunday, 
                                                            ifelse(wday(cust_service_predict$call_date) == 2, cust_service_predict$Monday,
                                                                   ifelse(wday(cust_service_predict$call_date) == 3, cust_service_predict$Tuesday,     
                                                                          ifelse(wday(cust_service_predict$call_date) == 4, cust_service_predict$Wednesday,     
                                                                                 ifelse(wday(cust_service_predict$call_date) == 5, cust_service_predict$Thursday,     
                                                                                        ifelse(wday(cust_service_predict$call_date) == 6, cust_service_predict$Friday,     
                                                                                               cust_service_predict$Saturday))))))
  
  cust_service_predict$lower_bound <- ifelse(wday(cust_service_predict$call_date) == 1, quantile(calls_Sunday$customer_service, perc_lower), 
                                             ifelse(wday(cust_service_predict$call_date) == 2, quantile(calls_Monday$customer_service, perc_lower),
                                                    ifelse(wday(cust_service_predict$call_date) == 3, quantile(calls_Tuesday$customer_service, perc_lower),     
                                                           ifelse(wday(cust_service_predict$call_date) == 4, quantile(calls_Wednesday$customer_service, perc_lower),     
                                                                  ifelse(wday(cust_service_predict$call_date) == 5, quantile(calls_Thursday$customer_service, perc_lower),     
                                                                         ifelse(wday(cust_service_predict$call_date) == 6, quantile(calls_Friday$customer_service, perc_lower),     
                                                                                quantile(calls_Saturday$customer_service, perc_lower)))))))
  
  cust_service_predict$upper_bound <- ifelse(wday(cust_service_predict$call_date) == 1, quantile(calls_Sunday$customer_service, perc_upper),
                                             ifelse(wday(cust_service_predict$call_date) == 2, quantile(calls_Monday$customer_service, perc_upper),
                                                    ifelse(wday(cust_service_predict$call_date) == 3, quantile(calls_Tuesday$customer_service, perc_upper),     
                                                           ifelse(wday(cust_service_predict$call_date) == 4, quantile(calls_Wednesday$customer_service, perc_upper),     
                                                                  ifelse(wday(cust_service_predict$call_date) == 5, quantile(calls_Thursday$customer_service, perc_upper),     
                                                                         ifelse(wday(cust_service_predict$call_date) == 6, quantile(calls_Friday$customer_service, perc_upper),     
                                                                                quantile(calls_Saturday$customer_service, perc_upper)))))))
  
  cust_service_predict$customer_service_prediction <- ifelse(cust_service_predict$customer_service_unbounded < cust_service_predict$lower_bound, cust_service_predict$lower_bound, 
                                                             ifelse(cust_service_predict$customer_service_unbounded > cust_service_predict$upper_bound, cust_service_predict$upper_bound,
                                                                    cust_service_predict$customer_service_unbounded))
  
  #DB_IVR
  
  #Stepwise regression, DB_IVR
  b <- step(lm(DB_IVR ~ mktg_direct + log(mktg_direct) + lag_DB_IVR), subset = day_of_week == 3)
  
  DB_IVR_reg_1 <- lm(b$terms, subset = day_of_week == 1)
  DB_IVR_reg_2 <- lm(b$terms, subset = day_of_week == 2)
  DB_IVR_reg_3 <- lm(b$terms, subset = day_of_week == 3)
  DB_IVR_reg_4 <- lm(b$terms, subset = day_of_week == 4)
  DB_IVR_reg_5 <- lm(b$terms, subset = day_of_week == 5)
  DB_IVR_reg_6 <- lm(b$terms, subset = day_of_week == 6)
  DB_IVR_reg_7 <- lm(b$terms, subset = day_of_week == 7)
  
  DB_IVR_predict_1 <- predict(DB_IVR_reg_1, projections)
  DB_IVR_predict_2 <- predict(DB_IVR_reg_2, projections)
  DB_IVR_predict_3 <- predict(DB_IVR_reg_3, projections)
  DB_IVR_predict_4 <- predict(DB_IVR_reg_4, projections)
  DB_IVR_predict_5 <- predict(DB_IVR_reg_5, projections)
  DB_IVR_predict_6 <- predict(DB_IVR_reg_6, projections)
  DB_IVR_predict_7 <- predict(DB_IVR_reg_7, projections)
  
  DB_IVR_predict <- cbind(DB_IVR_predict_1, DB_IVR_predict_2, DB_IVR_predict_3, DB_IVR_predict_4, 
                          DB_IVR_predict_5, DB_IVR_predict_6, DB_IVR_predict_7)
  
  #Coerce to dataframe and name
  DB_IVR_predict <- as.data.frame(cbind(projections$call_date, DB_IVR_predict))
  names(DB_IVR_predict) <- c("call_date", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  DB_IVR_predict$call_date <- as.Date(DB_IVR_predict$call_date, origin="1970-01-01")
  
  DB_IVR_predict$DB_IVR_unbounded <- ifelse(wday(DB_IVR_predict$call_date) == 1, DB_IVR_predict$Sunday, 
                                            ifelse(wday(DB_IVR_predict$call_date) == 2, DB_IVR_predict$Monday,
                                                   ifelse(wday(DB_IVR_predict$call_date) == 3, DB_IVR_predict$Tuesday,     
                                                          ifelse(wday(DB_IVR_predict$call_date) == 4, DB_IVR_predict$Wednesday,    
                                                                 ifelse(wday(DB_IVR_predict$call_date) == 5, DB_IVR_predict$Thursday,     
                                                                        ifelse(wday(DB_IVR_predict$call_date) == 6, DB_IVR_predict$Friday,     
                                                                               DB_IVR_predict$Saturday))))))
  
  DB_IVR_predict$lower_bound <- ifelse(wday(DB_IVR_predict$call_date) == 1, quantile(calls_Sunday$DB_IVR, perc_lower), 
                                       ifelse(wday(DB_IVR_predict$call_date) == 2, quantile(calls_Monday$DB_IVR, perc_lower),
                                              ifelse(wday(DB_IVR_predict$call_date) == 3, quantile(calls_Tuesday$DB_IVR, perc_lower),     
                                                     ifelse(wday(DB_IVR_predict$call_date) == 4, quantile(calls_Wednesday$DB_IVR, perc_lower),     
                                                            ifelse(wday(DB_IVR_predict$call_date) == 5, quantile(calls_Thursday$DB_IVR, perc_lower),     
                                                                   ifelse(wday(DB_IVR_predict$call_date) == 6, quantile(calls_Friday$DB_IVR, perc_lower),     
                                                                          quantile(calls_Saturday$DB_IVR, perc_lower)))))))
  
  DB_IVR_predict$upper_bound <- ifelse(wday(DB_IVR_predict$call_date) == 1, quantile(calls_Sunday$DB_IVR, perc_upper),
                                       ifelse(wday(DB_IVR_predict$call_date) == 2, quantile(calls_Monday$DB_IVR, perc_upper),
                                              ifelse(wday(DB_IVR_predict$call_date) == 3, quantile(calls_Tuesday$DB_IVR, perc_upper),     
                                                     ifelse(wday(DB_IVR_predict$call_date) == 4, quantile(calls_Wednesday$DB_IVR, perc_upper),     
                                                            ifelse(wday(DB_IVR_predict$call_date) == 5, quantile(calls_Thursday$DB_IVR, perc_upper),     
                                                                   ifelse(wday(DB_IVR_predict$call_date) == 6, quantile(calls_Friday$DB_IVR, perc_upper),     
                                                                          quantile(calls_Saturday$DB_IVR, perc_upper)))))))
  
  DB_IVR_predict$DB_IVR_prediction <- ifelse(DB_IVR_predict$DB_IVR_unbounded < DB_IVR_predict$lower_bound, DB_IVR_predict$lower_bound, 
                                             ifelse(DB_IVR_predict$DB_IVR_unbounded > DB_IVR_predict$upper_bound, DB_IVR_predict$upper_bound,
                                                    DB_IVR_predict$DB_IVR_unbounded))
  
  #DB_com
  
  #Stepwise regression, DB_com
  c <- step(lm(DB_com ~ mktg_direct + log(mktg_direct) + lag_DB_com), subset = day_of_week == 3)
  
  DB_com_reg_1 <- lm(c$terms, subset = day_of_week == 1)
  DB_com_reg_2 <- lm(c$terms, subset = day_of_week == 2)
  DB_com_reg_3 <- lm(c$terms, subset = day_of_week == 3)
  DB_com_reg_4 <- lm(c$terms, subset = day_of_week == 4)
  DB_com_reg_5 <- lm(c$terms, subset = day_of_week == 5)
  DB_com_reg_6 <- lm(c$terms, subset = day_of_week == 6)
  DB_com_reg_7 <- lm(c$terms, subset = day_of_week == 7)
  
  DB_com_predict_1 <- predict(DB_com_reg_1, projections)
  DB_com_predict_2 <- predict(DB_com_reg_2, projections)
  DB_com_predict_3 <- predict(DB_com_reg_3, projections)
  DB_com_predict_4 <- predict(DB_com_reg_4, projections)
  DB_com_predict_5 <- predict(DB_com_reg_5, projections)
  DB_com_predict_6 <- predict(DB_com_reg_6, projections)
  DB_com_predict_7 <- predict(DB_com_reg_7, projections)
  
  DB_com_predict <- cbind(DB_com_predict_1, DB_com_predict_2, DB_com_predict_3, DB_com_predict_4, 
                          DB_com_predict_5, DB_com_predict_6, DB_com_predict_7)
  
  
  
  
  #Coerce to dataframe and name
  DB_com_predict <- as.data.frame(cbind(projections$call_date, DB_com_predict))
  names(DB_com_predict) <- c("call_date", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  DB_com_predict$call_date <- as.Date(DB_com_predict$call_date, origin="1970-01-01")
  
  DB_com_predict$DB_com_unbounded <- ifelse(wday(DB_com_predict$call_date) == 1, DB_com_predict$Sunday, 
                                            ifelse(wday(DB_com_predict$call_date) == 2, DB_com_predict$Monday,
                                                   ifelse(wday(DB_com_predict$call_date) == 3, DB_com_predict$Tuesday,     
                                                          ifelse(wday(DB_com_predict$call_date) == 4, DB_com_predict$Wednesday,     
                                                                 ifelse(wday(DB_com_predict$call_date) == 5, DB_com_predict$Thursday,     
                                                                        ifelse(wday(DB_com_predict$call_date) == 6, DB_com_predict$Friday,     
                                                                               DB_com_predict$Saturday))))))
  
  DB_com_predict$lower_bound <- ifelse(wday(DB_com_predict$call_date) == 1, quantile(calls_Sunday$DB_com, perc_lower), 
                                       ifelse(wday(DB_com_predict$call_date) == 2, quantile(calls_Monday$DB_com, perc_lower),
                                              ifelse(wday(DB_com_predict$call_date) == 3, quantile(calls_Tuesday$DB_com, perc_lower),     
                                                     ifelse(wday(DB_com_predict$call_date) == 4, quantile(calls_Wednesday$DB_com, perc_lower),     
                                                            ifelse(wday(DB_com_predict$call_date) == 5, quantile(calls_Thursday$DB_com, perc_lower),     
                                                                   ifelse(wday(DB_com_predict$call_date) == 6, quantile(calls_Friday$DB_com, perc_lower),     
                                                                          quantile(calls_Saturday$DB_com, perc_lower)))))))
  
  DB_com_predict$upper_bound <- ifelse(wday(DB_com_predict$call_date) == 1, quantile(calls_Sunday$DB_com, perc_upper),
                                       ifelse(wday(DB_com_predict$call_date) == 2, quantile(calls_Monday$DB_com, perc_upper),
                                              ifelse(wday(DB_com_predict$call_date) == 3, quantile(calls_Tuesday$DB_com, perc_upper),     
                                                     ifelse(wday(DB_com_predict$call_date) == 4, quantile(calls_Wednesday$DB_com, perc_upper),     
                                                            ifelse(wday(DB_com_predict$call_date) == 5, quantile(calls_Thursday$DB_com, perc_upper),     
                                                                   ifelse(wday(DB_com_predict$call_date) == 6, quantile(calls_Friday$DB_com, perc_upper),     
                                                                          quantile(calls_Saturday$DB_com, perc_upper)))))))
  
  DB_com_predict$DB_com_prediction <- ifelse(DB_com_predict$DB_com_unbounded < DB_com_predict$lower_bound, DB_com_predict$lower_bound, 
                                             ifelse(DB_com_predict$DB_com_unbounded > DB_com_predict$upper_bound, DB_com_predict$upper_bound,
                                                    DB_com_predict$DB_com_unbounded))
  
  #iUpdate
  
  #Stepwise regression, iUpdate
  d <- step(lm(iUpdate ~ mktg_direct + log(mktg_direct) + lag_iUpdate), subset = day_of_week == 3)
  
  iUpdate_reg_1 <- lm(d$terms, subset = day_of_week == 1)
  iUpdate_reg_2 <- lm(d$terms, subset = day_of_week == 2)
  iUpdate_reg_3 <- lm(d$terms, subset = day_of_week == 3)
  iUpdate_reg_4 <- lm(d$terms, subset = day_of_week == 4)
  iUpdate_reg_5 <- lm(d$terms, subset = day_of_week == 5)
  iUpdate_reg_6 <- lm(d$terms, subset = day_of_week == 6)
  iUpdate_reg_7 <- lm(d$terms, subset = day_of_week == 7)
  
  iUpdate_predict_1 <- predict(iUpdate_reg_1, projections)
  iUpdate_predict_2 <- predict(iUpdate_reg_2, projections)
  iUpdate_predict_3 <- predict(iUpdate_reg_3, projections)
  iUpdate_predict_4 <- predict(iUpdate_reg_4, projections)
  iUpdate_predict_5 <- predict(iUpdate_reg_5, projections)
  iUpdate_predict_6 <- predict(iUpdate_reg_6, projections)
  iUpdate_predict_7 <- predict(iUpdate_reg_7, projections)
  
  iUpdate_predict <- cbind(iUpdate_predict_1, iUpdate_predict_2, iUpdate_predict_3, iUpdate_predict_4, 
                           iUpdate_predict_5, iUpdate_predict_6, iUpdate_predict_7)
  
  #Coerce to dataframe and name
  iUpdate_predict <- as.data.frame(cbind(projections$call_date, iUpdate_predict))
  names(iUpdate_predict) <- c("call_date", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  iUpdate_predict$call_date <- as.Date(iUpdate_predict$call_date, origin="1970-01-01")
  
  iUpdate_predict$iUpdate_unbounded <- ifelse(wday(iUpdate_predict$call_date) == 1, iUpdate_predict$Sunday, 
                                              ifelse(wday(iUpdate_predict$call_date) == 2, iUpdate_predict$Monday,
                                                     ifelse(wday(iUpdate_predict$call_date) == 3, iUpdate_predict$Tuesday,     
                                                            ifelse(wday(iUpdate_predict$call_date) == 4, iUpdate_predict$Wednesday,     
                                                                   ifelse(wday(iUpdate_predict$call_date) == 5, iUpdate_predict$Thursday,     
                                                                          ifelse(wday(iUpdate_predict$call_date) == 6, iUpdate_predict$Friday,     
                                                                                 iUpdate_predict$Saturday))))))
  
  iUpdate_predict$lower_bound <- ifelse(wday(iUpdate_predict$call_date) == 1, quantile(calls_Sunday$iUpdate, perc_lower), 
                                        ifelse(wday(iUpdate_predict$call_date) == 2, quantile(calls_Monday$iUpdate, perc_lower),
                                               ifelse(wday(iUpdate_predict$call_date) == 3, quantile(calls_Tuesday$iUpdate, perc_lower),     
                                                      ifelse(wday(iUpdate_predict$call_date) == 4, quantile(calls_Wednesday$iUpdate, perc_lower),     
                                                             ifelse(wday(iUpdate_predict$call_date) == 5, quantile(calls_Thursday$iUpdate, perc_lower),     
                                                                    ifelse(wday(iUpdate_predict$call_date) == 6, quantile(calls_Friday$iUpdate, perc_lower),     
                                                                           quantile(calls_Saturday$iUpdate, perc_lower)))))))
  
  iUpdate_predict$upper_bound <- ifelse(wday(iUpdate_predict$call_date) == 1, quantile(calls_Sunday$iUpdate, perc_upper),
                                        ifelse(wday(iUpdate_predict$call_date) == 2, quantile(calls_Monday$iUpdate, perc_upper),
                                               ifelse(wday(iUpdate_predict$call_date) == 3, quantile(calls_Tuesday$iUpdate, perc_upper),     
                                                      ifelse(wday(iUpdate_predict$call_date) == 4, quantile(calls_Wednesday$iUpdate, perc_upper),     
                                                             ifelse(wday(iUpdate_predict$call_date) == 5, quantile(calls_Thursday$iUpdate, perc_upper),     
                                                                    ifelse(wday(iUpdate_predict$call_date) == 6, quantile(calls_Friday$iUpdate, perc_upper),     
                                                                           quantile(calls_Saturday$iUpdate, perc_upper)))))))
  
  iUpdate_predict$iUpdate_prediction <- ifelse(iUpdate_predict$iUpdate_unbounded < iUpdate_predict$lower_bound, iUpdate_predict$lower_bound, 
                                               ifelse(iUpdate_predict$iUpdate_unbounded > iUpdate_predict$upper_bound, iUpdate_predict$upper_bound,
                                                      iUpdate_predict$iUpdate_unbounded))
  
  #Stepwise regression, Direct_Extension
  e <- step(lm(Direct_Extension ~ mktg_direct + log(mktg_direct) + lag_Direct_Extension), subset = day_of_week == 3)
  
  Direct_Extension_reg_1 <- lm(e$terms, subset = day_of_week == 1)
  Direct_Extension_reg_2 <- lm(e$terms, subset = day_of_week == 2)
  Direct_Extension_reg_3 <- lm(e$terms, subset = day_of_week == 3)
  Direct_Extension_reg_4 <- lm(e$terms, subset = day_of_week == 4)
  Direct_Extension_reg_5 <- lm(e$terms, subset = day_of_week == 5)
  Direct_Extension_reg_6 <- lm(e$terms, subset = day_of_week == 6)
  Direct_Extension_reg_7 <- lm(e$terms, subset = day_of_week == 7)
  
  Direct_Extension_predict_1 <- predict(Direct_Extension_reg_1, projections)
  Direct_Extension_predict_2 <- predict(Direct_Extension_reg_2, projections)
  Direct_Extension_predict_3 <- predict(Direct_Extension_reg_3, projections)
  Direct_Extension_predict_4 <- predict(Direct_Extension_reg_4, projections)
  Direct_Extension_predict_5 <- predict(Direct_Extension_reg_5, projections)
  Direct_Extension_predict_6 <- predict(Direct_Extension_reg_6, projections)
  Direct_Extension_predict_7 <- predict(Direct_Extension_reg_7, projections)
  
  Direct_Extension_predict <- cbind(Direct_Extension_predict_1, Direct_Extension_predict_2, Direct_Extension_predict_3, Direct_Extension_predict_4, 
                                    Direct_Extension_predict_5, Direct_Extension_predict_6, Direct_Extension_predict_7)
  
  #Coerce to dataframe and name
  Direct_Extension_predict <- as.data.frame(cbind(projections$call_date, Direct_Extension_predict))
  names(Direct_Extension_predict) <- c("call_date", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  Direct_Extension_predict$call_date <- as.Date(Direct_Extension_predict$call_date, origin="1970-01-01")
  
  Direct_Extension_predict$Direct_Extension_unbounded <- ifelse(wday(Direct_Extension_predict$call_date) == 1, Direct_Extension_predict$Sunday, 
                                                                ifelse(wday(Direct_Extension_predict$call_date) == 2, Direct_Extension_predict$Monday,
                                                                       ifelse(wday(Direct_Extension_predict$call_date) == 3, Direct_Extension_predict$Tuesday,     
                                                                              ifelse(wday(Direct_Extension_predict$call_date) == 4, Direct_Extension_predict$Wednesday,     
                                                                                     ifelse(wday(Direct_Extension_predict$call_date) == 5, Direct_Extension_predict$Thursday,     
                                                                                            ifelse(wday(Direct_Extension_predict$call_date) == 6, Direct_Extension_predict$Friday,     
                                                                                                   Direct_Extension_predict$Saturday))))))
  
  Direct_Extension_predict$lower_bound <- ifelse(wday(Direct_Extension_predict$call_date) == 1, quantile(calls_Sunday$Direct_Extension, perc_lower), 
                                                 ifelse(wday(Direct_Extension_predict$call_date) == 2, quantile(calls_Monday$Direct_Extension, perc_lower),
                                                        ifelse(wday(Direct_Extension_predict$call_date) == 3, quantile(calls_Tuesday$Direct_Extension, perc_lower),     
                                                               ifelse(wday(Direct_Extension_predict$call_date) == 4, quantile(calls_Wednesday$Direct_Extension, perc_lower),     
                                                                      ifelse(wday(Direct_Extension_predict$call_date) == 5, quantile(calls_Thursday$Direct_Extension, perc_lower),     
                                                                             ifelse(wday(Direct_Extension_predict$call_date) == 6, quantile(calls_Friday$Direct_Extension, perc_lower),     
                                                                                    quantile(calls_Saturday$Direct_Extension, perc_lower)))))))
  
  Direct_Extension_predict$upper_bound <- ifelse(wday(Direct_Extension_predict$call_date) == 1, quantile(calls_Sunday$Direct_Extension, perc_upper),
                                                 ifelse(wday(Direct_Extension_predict$call_date) == 2, quantile(calls_Monday$Direct_Extension, perc_upper),
                                                        ifelse(wday(Direct_Extension_predict$call_date) == 3, quantile(calls_Tuesday$Direct_Extension, perc_upper),     
                                                               ifelse(wday(Direct_Extension_predict$call_date) == 4, quantile(calls_Wednesday$Direct_Extension, perc_upper),     
                                                                      ifelse(wday(Direct_Extension_predict$call_date) == 5, quantile(calls_Thursday$Direct_Extension, perc_upper),     
                                                                             ifelse(wday(Direct_Extension_predict$call_date) == 6, quantile(calls_Friday$Direct_Extension, perc_upper),     
                                                                                    quantile(calls_Saturday$Direct_Extension, perc_upper)))))))
  
  Direct_Extension_predict$Direct_Extension_prediction <- ifelse(Direct_Extension_predict$Direct_Extension_unbounded < Direct_Extension_predict$lower_bound, Direct_Extension_predict$lower_bound, 
                                                                 ifelse(Direct_Extension_predict$Direct_Extension_unbounded > Direct_Extension_predict$upper_bound, Direct_Extension_predict$upper_bound,
                                                                        Direct_Extension_predict$Direct_Extension_unbounded))
  
  #Stepwise regression, Organic
  f <- step(lm(Organic ~ mktg_direct + log(mktg_direct) + lag_Organic), subset = day_of_week == 3)
  
  Organic_reg_1 <- lm(f$terms, subset = day_of_week == 1)
  Organic_reg_2 <- lm(f$terms, subset = day_of_week == 2)
  Organic_reg_3 <- lm(f$terms, subset = day_of_week == 3)
  Organic_reg_4 <- lm(f$terms, subset = day_of_week == 4)
  Organic_reg_5 <- lm(f$terms, subset = day_of_week == 5)
  Organic_reg_6 <- lm(f$terms, subset = day_of_week == 6)
  Organic_reg_7 <- lm(f$terms, subset = day_of_week == 7)
  
  Organic_predict_1 <- predict(Organic_reg_1, projections)
  Organic_predict_2 <- predict(Organic_reg_2, projections)
  Organic_predict_3 <- predict(Organic_reg_3, projections)
  Organic_predict_4 <- predict(Organic_reg_4, projections)
  Organic_predict_5 <- predict(Organic_reg_5, projections)
  Organic_predict_6 <- predict(Organic_reg_6, projections)
  Organic_predict_7 <- predict(Organic_reg_7, projections)
  
  Organic_predict <- cbind(Organic_predict_1, Organic_predict_2, Organic_predict_3, Organic_predict_4, 
                           Organic_predict_5, Organic_predict_6, Organic_predict_7)
  
  #Coerce to dataframe and name
  Organic_predict <- as.data.frame(cbind(projections$call_date, Organic_predict))
  names(Organic_predict) <- c("call_date", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  Organic_predict$call_date <- as.Date(Organic_predict$call_date, origin="1970-01-01")
  
  Organic_predict$Organic_unbounded <- ifelse(wday(Organic_predict$call_date) == 1, Organic_predict$Sunday, 
                                              ifelse(wday(Organic_predict$call_date) == 2, Organic_predict$Monday,
                                                     ifelse(wday(Organic_predict$call_date) == 3, Organic_predict$Tuesday,     
                                                            ifelse(wday(Organic_predict$call_date) == 4, Organic_predict$Wednesday,     
                                                                   ifelse(wday(Organic_predict$call_date) == 5, Organic_predict$Thursday,     
                                                                          ifelse(wday(Organic_predict$call_date) == 6, Organic_predict$Friday,     
                                                                                 Organic_predict$Saturday))))))
  
  Organic_predict$lower_bound <- ifelse(wday(Organic_predict$call_date) == 1, quantile(calls_Sunday$Organic, perc_lower), 
                                        ifelse(wday(Organic_predict$call_date) == 2, quantile(calls_Monday$Organic, perc_lower),
                                               ifelse(wday(Organic_predict$call_date) == 3, quantile(calls_Tuesday$Organic, perc_lower),     
                                                      ifelse(wday(Organic_predict$call_date) == 4, quantile(calls_Wednesday$Organic, perc_lower),     
                                                             ifelse(wday(Organic_predict$call_date) == 5, quantile(calls_Thursday$Organic, perc_lower),     
                                                                    ifelse(wday(Organic_predict$call_date) == 6, quantile(calls_Friday$Organic, perc_lower),     
                                                                           quantile(calls_Saturday$Organic, perc_lower)))))))
  
  Organic_predict$upper_bound <- ifelse(wday(Organic_predict$call_date) == 1, quantile(calls_Sunday$Organic, perc_upper),
                                        ifelse(wday(Organic_predict$call_date) == 2, quantile(calls_Monday$Organic, perc_upper),
                                               ifelse(wday(Organic_predict$call_date) == 3, quantile(calls_Tuesday$Organic, perc_upper),     
                                                      ifelse(wday(Organic_predict$call_date) == 4, quantile(calls_Wednesday$Organic, perc_upper),     
                                                             ifelse(wday(Organic_predict$call_date) == 5, quantile(calls_Thursday$Organic, perc_upper),     
                                                                    ifelse(wday(Organic_predict$call_date) == 6, quantile(calls_Friday$Organic, perc_upper),     
                                                                           quantile(calls_Saturday$Organic, perc_upper)))))))
  
  Organic_predict$Organic_prediction <- ifelse(Organic_predict$Organic_unbounded < Organic_predict$lower_bound, Organic_predict$lower_bound, 
                                               ifelse(Organic_predict$Organic_unbounded > Organic_predict$upper_bound, Organic_predict$upper_bound,
                                                      Organic_predict$Organic_unbounded))
  
  #Stepwise regression, Paid_web
  g <- step(lm(Paid_web ~ mktg_direct + log(mktg_direct) + lag_Paid_web), subset = day_of_week == 3)
  
  Paid_web_reg_1 <- lm(g$terms, subset = day_of_week == 1)
  Paid_web_reg_2 <- lm(g$terms, subset = day_of_week == 2)
  Paid_web_reg_3 <- lm(g$terms, subset = day_of_week == 3)
  Paid_web_reg_4 <- lm(g$terms, subset = day_of_week == 4)
  Paid_web_reg_5 <- lm(g$terms, subset = day_of_week == 5)
  Paid_web_reg_6 <- lm(g$terms, subset = day_of_week == 6)
  Paid_web_reg_7 <- lm(g$terms, subset = day_of_week == 7)
  
  Paid_web_predict_1 <- predict(Paid_web_reg_1, projections)
  Paid_web_predict_2 <- predict(Paid_web_reg_2, projections)
  Paid_web_predict_3 <- predict(Paid_web_reg_3, projections)
  Paid_web_predict_4 <- predict(Paid_web_reg_4, projections)
  Paid_web_predict_5 <- predict(Paid_web_reg_5, projections)
  Paid_web_predict_6 <- predict(Paid_web_reg_6, projections)
  Paid_web_predict_7 <- predict(Paid_web_reg_7, projections)
  
  Paid_web_predict <- cbind(Paid_web_predict_1, Paid_web_predict_2, Paid_web_predict_3, Paid_web_predict_4, 
                            Paid_web_predict_5, Paid_web_predict_6, Paid_web_predict_7)
  
  #Coerce to dataframe and name
  Paid_web_predict <- as.data.frame(cbind(projections$call_date, Paid_web_predict))
  names(Paid_web_predict) <- c("call_date", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  Paid_web_predict$call_date <- as.Date(Paid_web_predict$call_date, origin="1970-01-01")
  
  Paid_web_predict$Paid_web_unbounded <- ifelse(wday(Paid_web_predict$call_date) == 1, Paid_web_predict$Sunday, 
                                                ifelse(wday(Paid_web_predict$call_date) == 2, Paid_web_predict$Monday,
                                                       ifelse(wday(Paid_web_predict$call_date) == 3, Paid_web_predict$Tuesday,     
                                                              ifelse(wday(Paid_web_predict$call_date) == 4, Paid_web_predict$Wednesday,     
                                                                     ifelse(wday(Paid_web_predict$call_date) == 5, Paid_web_predict$Thursday,     
                                                                            ifelse(wday(Paid_web_predict$call_date) == 6, Paid_web_predict$Friday,     
                                                                                   Paid_web_predict$Saturday))))))
  
  Paid_web_predict$lower_bound <- ifelse(wday(Paid_web_predict$call_date) == 1, quantile(calls_Sunday$Paid_web, perc_lower), 
                                         ifelse(wday(Paid_web_predict$call_date) == 2, quantile(calls_Monday$Paid_web, perc_lower),
                                                ifelse(wday(Paid_web_predict$call_date) == 3, quantile(calls_Tuesday$Paid_web, perc_lower),     
                                                       ifelse(wday(Paid_web_predict$call_date) == 4, quantile(calls_Wednesday$Paid_web, perc_lower),     
                                                              ifelse(wday(Paid_web_predict$call_date) == 5, quantile(calls_Thursday$Paid_web, perc_lower),     
                                                                     ifelse(wday(Paid_web_predict$call_date) == 6, quantile(calls_Friday$Paid_web, perc_lower),     
                                                                            quantile(calls_Saturday$Paid_web, perc_lower)))))))
  
  Paid_web_predict$upper_bound <- ifelse(wday(Paid_web_predict$call_date) == 1, quantile(calls_Sunday$Paid_web, perc_upper),
                                         ifelse(wday(Paid_web_predict$call_date) == 2, quantile(calls_Monday$Paid_web, perc_upper),
                                                ifelse(wday(Paid_web_predict$call_date) == 3, quantile(calls_Tuesday$Paid_web, perc_upper),     
                                                       ifelse(wday(Paid_web_predict$call_date) == 4, quantile(calls_Wednesday$Paid_web, perc_upper),     
                                                              ifelse(wday(Paid_web_predict$call_date) == 5, quantile(calls_Thursday$Paid_web, perc_upper),     
                                                                     ifelse(wday(Paid_web_predict$call_date) == 6, quantile(calls_Friday$Paid_web, perc_upper),     
                                                                            quantile(calls_Saturday$Paid_web, perc_upper)))))))
  
  Paid_web_predict$Paid_web_prediction <- ifelse(Paid_web_predict$Paid_web_unbounded < Paid_web_predict$lower_bound, Paid_web_predict$lower_bound, 
                                                 ifelse(Paid_web_predict$Paid_web_unbounded > Paid_web_predict$upper_bound, Paid_web_predict$upper_bound,
                                                        Paid_web_predict$Paid_web_unbounded))
  
  #Stepwise regression, other
  h <- step(lm(other ~ mktg_direct + log(mktg_direct) + lag_other), subset = day_of_week == 3)
  
  other_reg_1 <- lm(h$terms, subset = day_of_week == 1)
  other_reg_2 <- lm(h$terms, subset = day_of_week == 2)
  other_reg_3 <- lm(h$terms, subset = day_of_week == 3)
  other_reg_4 <- lm(h$terms, subset = day_of_week == 4)
  other_reg_5 <- lm(h$terms, subset = day_of_week == 5)
  other_reg_6 <- lm(h$terms, subset = day_of_week == 6)
  other_reg_7 <- lm(h$terms, subset = day_of_week == 7)
  
  other_predict_1 <- predict(other_reg_1, projections)
  other_predict_2 <- predict(other_reg_2, projections)
  other_predict_3 <- predict(other_reg_3, projections)
  other_predict_4 <- predict(other_reg_4, projections)
  other_predict_5 <- predict(other_reg_5, projections)
  other_predict_6 <- predict(other_reg_6, projections)
  other_predict_7 <- predict(other_reg_7, projections)
  
  other_predict <- cbind(other_predict_1, other_predict_2, other_predict_3, other_predict_4, 
                         other_predict_5, other_predict_6, other_predict_7)
  
  #Coerce to dataframe and name
  other_predict <- as.data.frame(cbind(projections$call_date, other_predict))
  names(other_predict) <- c("call_date", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  other_predict$call_date <- as.Date(other_predict$call_date, origin="1970-01-01")
  
  other_predict$other_unbounded <- ifelse(wday(other_predict$call_date) == 1, other_predict$Sunday, 
                                          ifelse(wday(other_predict$call_date) == 2, other_predict$Monday,
                                                 ifelse(wday(other_predict$call_date) == 3, other_predict$Tuesday,     
                                                        ifelse(wday(other_predict$call_date) == 4, other_predict$Wednesday,     
                                                               ifelse(wday(other_predict$call_date) == 5, other_predict$Thursday,     
                                                                      ifelse(wday(other_predict$call_date) == 6, other_predict$Friday,     
                                                                             other_predict$Saturday))))))
  
  other_predict$lower_bound <- ifelse(wday(other_predict$call_date) == 1, quantile(calls_Sunday$other, perc_lower), 
                                      ifelse(wday(other_predict$call_date) == 2, quantile(calls_Monday$other, perc_lower),
                                             ifelse(wday(other_predict$call_date) == 3, quantile(calls_Tuesday$other, perc_lower),     
                                                    ifelse(wday(other_predict$call_date) == 4, quantile(calls_Wednesday$other, perc_lower),     
                                                           ifelse(wday(other_predict$call_date) == 5, quantile(calls_Thursday$other, perc_lower),     
                                                                  ifelse(wday(other_predict$call_date) == 6, quantile(calls_Friday$other, perc_lower),     
                                                                         quantile(calls_Saturday$other, perc_lower)))))))
  
  other_predict$upper_bound <- ifelse(wday(other_predict$call_date) == 1, quantile(calls_Sunday$other, perc_upper),
                                      ifelse(wday(other_predict$call_date) == 2, quantile(calls_Monday$other, perc_upper),
                                             ifelse(wday(other_predict$call_date) == 3, quantile(calls_Tuesday$other, perc_upper),     
                                                    ifelse(wday(other_predict$call_date) == 4, quantile(calls_Wednesday$other, perc_upper),     
                                                           ifelse(wday(other_predict$call_date) == 5, quantile(calls_Thursday$other, perc_upper),     
                                                                  ifelse(wday(other_predict$call_date) == 6, quantile(calls_Friday$other, perc_upper),     
                                                                         quantile(calls_Saturday$other, perc_upper)))))))
  
  other_predict$other_prediction <- ifelse(other_predict$other_unbounded < other_predict$lower_bound, other_predict$lower_bound, 
                                           ifelse(other_predict$other_unbounded > other_predict$upper_bound, other_predict$upper_bound,
                                                  other_predict$other_unbounded))
  
  #Get mktg_call quantiles
  #Coerce to dataframe and name
  mktg_direct_predict <- as.data.frame(cbind(projections$call_date, projections$mktg_direct))
  names(mktg_direct_predict) <- c("call_date", "mktg_direct")
  mktg_direct_predict$call_date <- as.Date(mktg_direct_predict$call_date, origin="1970-01-01")
  
  mktg_direct_predict$lower_bound <- ifelse(wday(mktg_direct_predict$call_date) == 1, quantile(calls_Sunday$mktg_direct, perc_lower_mktg), 
                                            ifelse(wday(mktg_direct_predict$call_date) == 2, quantile(calls_Monday$mktg_direct, perc_lower_mktg),
                                                   ifelse(wday(mktg_direct_predict$call_date) == 3, quantile(calls_Tuesday$mktg_direct, perc_lower_mktg),     
                                                          ifelse(wday(mktg_direct_predict$call_date) == 4, quantile(calls_Wednesday$mktg_direct, perc_lower_mktg),     
                                                                 ifelse(wday(mktg_direct_predict$call_date) == 5, quantile(calls_Thursday$mktg_direct, perc_lower_mktg),     
                                                                        ifelse(wday(mktg_direct_predict$call_date) == 6, quantile(calls_Friday$mktg_direct, perc_lower_mktg),     
                                                                               quantile(calls_Saturday$mktg_direct, perc_lower_mktg)))))))
  
  mktg_direct_predict$upper_bound <- ifelse(wday(mktg_direct_predict$call_date) == 1, quantile(calls_Sunday$mktg_direct, perc_upper_mktg),
                                            ifelse(wday(mktg_direct_predict$call_date) == 2, quantile(calls_Monday$mktg_direct, perc_upper_mktg),
                                                   ifelse(wday(mktg_direct_predict$call_date) == 3, quantile(calls_Tuesday$mktg_direct, perc_upper_mktg),     
                                                          ifelse(wday(mktg_direct_predict$call_date) == 4, quantile(calls_Wednesday$mktg_direct, perc_upper_mktg),     
                                                                 ifelse(wday(mktg_direct_predict$call_date) == 5, quantile(calls_Thursday$mktg_direct, perc_upper_mktg),     
                                                                        ifelse(wday(mktg_direct_predict$call_date) == 6, quantile(calls_Friday$mktg_direct, perc_upper_mktg),     
                                                                               quantile(calls_Saturday$mktg_direct, perc_upper_mktg)))))))
  
  #NO BOUNDS FOR MKTG DIRECT!!
  mktg_direct_predict$mktg_direct_prediction <- ifelse(mktg_direct_predict$mktg_direct < mktg_direct_predict$lower_bound, mktg_direct_predict$mktg_direct, 
                                                       ifelse(mktg_direct_predict$mktg_direct > mktg_direct_predict$upper_bound, mktg_direct_predict$mktg_direct,
                                                              mktg_direct_predict$mktg_direct))
  
  #Bind all predictions together
  predictions <- as.data.frame(cbind(mktg_direct_predict$mktg_direct_prediction, cust_service_predict$customer_service_prediction, DB_IVR_predict$DB_IVR_prediction, DB_com_predict$DB_com_prediction,
                                     Direct_Extension_predict$Direct_Extension_prediction, iUpdate_predict$iUpdate_prediction, Organic_predict$Organic_prediction, Paid_web_predict$Paid_web_prediction, 
                                     other_predict$other_prediction)) 
  
  #Bind date
  predictions <- cbind(projections$call_date, predictions)
  
  names(predictions) <- c("call_date", "mktg_direct_projection", "customer_service_projection", "DB_IVR_projection", "DB_com_projection", "Direct_Extension_projection", "iUpdate_projection",
                          "Organic_projection", "Paid_web_projection", "other_projection")
  
  detach(calls)
  
  projections_new <- as.data.frame(cbind(as.Date(projections$call_date, origin="1970-01-01"), as.character(projections$Holiday), as.numeric(as.character(projections$holiday_adjustment))))
  names(projections_new) <- c("call_date", "Holiday", "holiday_adjustment")
  
  projections_new <- merge(projections_new, predictions, by="call_date", all.x="TRUE")
  projections_new$call_date <- as.Date(as.numeric(as.character(projections_new$call_date)), origin="1970-01-01")
  
  #Adjust holidays and coerce to numeric
  projections_new$holiday_adjustment <- as.numeric(as.character(projections_new$holiday_adjustment))
  
  #mktg_direct
  projections_new$mktg_direct_projection <- as.numeric(as.character(projections_new$mktg_direct_projection))
  projections_new$mktg_direct_projection <- ifelse(is.na(projections_new$Holiday) == "FALSE", projections_new$mktg_direct_projection * projections_new$holiday_adjustment, projections_new$mktg_direct_projection)
  
  #customer_service
  projections_new$customer_service_projection <- as.numeric(as.character(projections_new$customer_service_projection))
  projections_new$customer_service_projection <- ifelse(is.na(projections_new$Holiday) == "FALSE", projections_new$customer_service_projection * projections_new$holiday_adjustment, projections_new$customer_service_projection)
  
  #DB_IVR
  projections_new$DB_IVR_projection <- as.numeric(as.character(projections_new$DB_IVR_projection))
  projections_new$DB_IVR_projection <- ifelse(is.na(projections_new$Holiday) == "FALSE", projections_new$DB_IVR_projection * projections_new$holiday_adjustment, projections_new$DB_IVR_projection)
  
  #DB_com
  projections_new$DB_com_projection <- as.numeric(as.character(projections_new$DB_com_projection))
  projections_new$DB_com_projection <- ifelse(is.na(projections_new$Holiday) == "FALSE", projections_new$DB_com_projection * projections_new$holiday_adjustment, projections_new$DB_com_projection)
  
  #Direct_Extension
  projections_new$Direct_Extension_projection <- as.numeric(as.character(projections_new$Direct_Extension_projection))
  projections_new$Direct_Extension_projection <- ifelse(is.na(projections_new$Holiday) == "FALSE", projections_new$Direct_Extension_projection * projections_new$holiday_adjustment, projections_new$Direct_Extension_projection)
  
  #iUpdate
  projections_new$iUpdate_projection <- as.numeric(as.character(projections_new$iUpdate_projection))
  projections_new$iUpdate_projection <- ifelse(is.na(projections_new$Holiday) == "FALSE", projections_new$iUpdate_projection * projections_new$holiday_adjustment, projections_new$iUpdate_projection)
  
  #Organic
  projections_new$Organic_projection <- as.numeric(as.character(projections_new$Organic_projection))
  projections_new$Organic_projection <- ifelse(is.na(projections_new$Holiday) == "FALSE", projections_new$Organic_projection * projections_new$holiday_adjustment, projections_new$Organic_projection)
  
  #Paid_web
  projections_new$Paid_web_projection <- as.numeric(as.character(projections_new$Paid_web_projection))
  projections_new$Paid_web_projection <- ifelse(is.na(projections_new$Holiday) == "FALSE", projections_new$Paid_web_projection * projections_new$holiday_adjustment, projections_new$Paid_web_projection)
  
  #other
  projections_new$other_projection <- as.numeric(as.character(projections_new$other_projection))
  projections_new$other_projection <- ifelse(is.na(projections_new$Holiday) == "FALSE", projections_new$other_projection * projections_new$holiday_adjustment, projections_new$other_projection)
  
  #Total projection
  projections_new$total_projection <- projections_new$mktg_direct_projection + projections_new$customer_service_projection + projections_new$DB_IVR_projection +
    projections_new$DB_com_projection + projections_new$Direct_Extension_projection + projections_new$iUpdate_projection + projections_new$Organic_projection + 
    projections_new$Paid_web_projection + projections_new$other_projection
  
  projections_final <- merge(projections_new, total_calls, by="call_date", all.x="TRUE")
  
  #Error
  projections_final$Error <- projections_final$all_calls - projections_final$total_projection
  
  
  output <- list("calls" = calls, "projections" =  projections, "calls_holiday" = calls_holiday,
                 "cust_service_predict" = cust_service_predict, "projections_1" = projections_1, "full_calls" = full_calls,
                 "full_calls_sunday" = full_calls_sunday, "lag_other_sunday" = lag_other_sunday, "full_sunday" = full_sunday, "all_calls" = all_calls,
                 "cust_service_predict" = cust_service_predict, "DB_IVR_predict" = DB_IVR_predict, "DB_com_predict" = DB_com_predict, 
                 "iUpdate_predict" = iUpdate_predict, "Direct_Extension_predict" = Direct_Extension_predict, "Organic_predict" = Organic_predict, 
                 "Paid_web_predict" = Paid_web_predict, "other_predict" = other_predict, "a" = a, "b" = b, "predictions" = predictions, "calls_Sunday" = calls_Sunday,
                 "mktg_direct_predict" = mktg_direct_predict, "projections_new" = projections_new, "projections_final" = projections_final, "holiday_norm" = holiday_norm,
                 "All_holidays" = All_holidays)
  
  return(output)
  
}