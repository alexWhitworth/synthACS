DM_call_estimate <- function(full_data, training_data, OOS_data, min_days, 
                             estimation_limit_called_proj, momentum_limit, past_proj, 
                             holiday_adj, current_month, current_year) { 
  
  #Make class_of_mail a character string of length 3 only
  full_data$class_of_mail <- substr(full_data$class_of_mail, 1, 3)
  training_data$class_of_mail <- substr(training_data$class_of_mail, 1, 3)
  OOS_data$class_of_mail <- substr(OOS_data$class_of_mail, 1, 3)  
  
  #Re-name variables
  training_data$class <- training_data$class_of_mail
  OOS_data$class <- OOS_data$class_of_mail
  training_data$leads <- training_data$unique_leads
  OOS_data$leads <- OOS_data$unique_leads
  
  #Change OOS data #N/A obs in class to "3rd"
  OOS_data$class_of_mail <- ifelse(is.na(OOS_data$class_of_mail), "3rd", OOS_data$class_of_mail)
  
#   for(i in 1:nrow(OOS_data)) {
#     
#     if(is.na(as.character(OOS_data$class_of_mail[i])) == "TRUE") {
#       OOS_data$class_of_mail[i] <- "3rd"
#     }
#     
#   }
  
  #Create latest inq and non-latest inq training data
  training_data_1 <- training_data[training_data$campaign_group != "Latest INQ", ]
  training_data_2 <- training_data[training_data$campaign_group == "Latest INQ", ]
  
  # [AW] For training_data_1 get response rate by (class_of_mail, rounds, days_to_response)
  # remove any RR calculated from < 30 total responders
  #----------------------------------------------------------------------------------------

  #Step 1, get sum of responsders on training_data sample, by days_to_response, rounds, round, class_of_mail. This is agg_stats_1
  agg_stats_1 <- aggregate(as.numeric(training_data_1$responders), 
                           by = list(training_data_1$days_to_response, training_data_1$rounds, training_data_1$class), 
                           FUN=sum, na.rm = "TRUE")
  
  #Step 2, get sum of leads on training_data sample, by days_to_response, rounds, round, class_of_mail. This is agg_stats_1a
  agg_stats_1a <- aggregate(as.numeric(training_data_1$leads), 
                            by = list(training_data_1$days_to_response, training_data_1$rounds, training_data_1$class), 
                            FUN=sum, na.rm = "TRUE")
  
  #Step 3, merge agg_stats_1 and agg_stats_1a in order to estimate response rates for initial (and single) mailings.
  agg_stats_2 <- merge(agg_stats_1, agg_stats_1a, by=c("Group.1", "Group.2", "Group.3"))
  
  #Step 3, calculate response rate from agg_stats_2
  agg_stats_2$response_rate_initial <- agg_stats_2$x.x / agg_stats_2$x.y
  
  #Drop obs with less than min_days responders
  agg_stats_3_new <- agg_stats_2[agg_stats_2$x.x >= min_days, ]
  agg_stats_3 <- agg_stats_3_new 
  
  #Step 5, order by response date.
  agg_stats_3 <- agg_stats_3[order(agg_stats_3$Group.3, agg_stats_3$Group.2, agg_stats_3$Group.1) ,]
  
  # [AW] For training_data_2 get response rate by (class_of_mail, rounds, days_to_response)
  # remove any RR calculated from < 30 total responders
  #----------------------------------------------------------------------------------------

  #Now, repeat for latest INQ campaigns
  agg_stats_1_a <- aggregate(as.numeric(training_data_2$responders), 
                             by = list(training_data_2$days_to_response), FUN=sum, na.rm = "TRUE")
  
  #Step 2, get sum of leads on training_data sample, by days_to_response, rounds, round, class_of_mail. This is agg_stats_1a
  agg_stats_1a_a <- aggregate(as.numeric(training_data_2$leads), 
                              by = list(training_data_2$days_to_response), FUN=sum, na.rm = "TRUE")
  
  #Step 3, merge agg_stats_1 and agg_stats_1a in order to estimate response rates for initial (and single) mailings.
  agg_stats_2_a <- merge(agg_stats_1_a, agg_stats_1a_a, by="Group.1")
  
  #Step 3, calculate response rate from agg_stats_2
  agg_stats_2_a$response_rate_initial <- agg_stats_2_a$x.x / agg_stats_2_a$x.y
  
  #Drop obs with less than min_days responders
  agg_stats_3_new_a <- agg_stats_2_a[agg_stats_2_a$x.x >= min_days, ]
  agg_stats_3_a <- agg_stats_3_new_a

  # [AW] for agg_stats_3 & agg_stats_3_a (above), use linear interpolation to estimate missing data
  # response dates. 
  # [AW] I do this via imputation for more accurate estimation -- see camp_out_calc_adj.R
  #----------------------------------------------------------------------------------------
  
  #Curves to form linear interpolation from
  agg_stats_3_single_1st <- agg_stats_3[(as.character(agg_stats_3$Group.3) == "1st" & as.character(agg_stats_3$Group.2) == "single"), ]
  agg_stats_3_single_3rd <- agg_stats_3[(as.character(agg_stats_3$Group.3) == "3rd" & as.character(agg_stats_3$Group.2) == "single"), ]
  agg_stats_3_multiple_1st <- agg_stats_3[(as.character(agg_stats_3$Group.3) == "1st" & as.character(agg_stats_3$Group.2) == "multiple"), ]
  agg_stats_3_multiple_3rd <- agg_stats_3[(as.character(agg_stats_3$Group.3) == "3rd" & as.character(agg_stats_3$Group.2) == "multiple"), ]
  
  #Latset Inq
  agg_stats_latest_inq <- agg_stats_3_a
  
  #Linear interpolation, days_to_response
  linear_interp_single_3rd <- approx(as.numeric(as.character(agg_stats_3_single_3rd$Group.1)), agg_stats_3_single_3rd$response_rate_initial, xout = seq(0, 120, 1), rule=2)
  linear_interp_single_3rd <- as.data.frame(linear_interp_single_3rd)
  linear_interp_single_3rd$days_to_response <- linear_interp_single_3rd$x
  linear_interp_single_3rd$class_of_mail <- "3rd"
  linear_interp_single_3rd$rounds <- "single"
  linear_interp_single_3rd$linear_interp_single_3rd <- linear_interp_single_3rd$y
  
  linear_interp_multiple_3rd <- approx(as.numeric(as.character(agg_stats_3_multiple_3rd$Group.1)), agg_stats_3_multiple_3rd$response_rate_initial, xout = seq(0, 120, 1), rule=2)
  linear_interp_multiple_3rd <- as.data.frame(linear_interp_multiple_3rd)
  linear_interp_multiple_3rd$days_to_response <- linear_interp_multiple_3rd$x
  linear_interp_multiple_3rd$class_of_mail <- "3rd"
  linear_interp_multiple_3rd$rounds <- "multiple"
  linear_interp_multiple_3rd$linear_interp_multiple_3rd <- linear_interp_multiple_3rd$y
  
  if(nrow(agg_stats_3_multiple_1st) > 0) {
    linear_interp_multiple_1st <- approx(as.numeric(as.character(agg_stats_3_multiple_1st$Group.1)), agg_stats_3_multiple_1st$response_rate_initial, xout = seq(0, 120, 1), rule=2)
    linear_interp_multiple_1st <- as.data.frame(linear_interp_multiple_1st)
    linear_interp_multiple_1st$days_to_response <- linear_interp_multiple_1st$x
    linear_interp_multiple_1st$class_of_mail <- "1st"
    linear_interp_multiple_1st$rounds <- "multiple"
    linear_interp_multiple_1st$linear_interp_multiple_1st <- linear_interp_multiple_1st$y
  }
  
  #Otherwise assume multiple 1st follows the multiple 3rd response curve
  else {
    linear_interp_multiple_1st <- linear_interp_multiple_3rd 
    linear_interp_multiple_1st$class_of_mail <- "1st"
    linear_interp_multiple_1st$linear_interp_multiple_1st <- linear_interp_multiple_1st$linear_interp_multiple_3rd
    linear_interp_multiple_1st$linear_interp_multiple_3rd <- NULL
  }
  
  if(nrow(agg_stats_3_single_1st) > 0) {
    linear_interp_single_1st <- approx(as.numeric(as.character(agg_stats_3_single_1st$Group.1)), agg_stats_3_single_1st$response_rate_initial, xout = seq(0, 120, 1), rule=2)
    linear_interp_single_1st <- as.data.frame(linear_interp_single_1st)
    linear_interp_single_1st$days_to_response <- linear_interp_single_1st$x
    linear_interp_single_1st$class_of_mail <- "1st"
    linear_interp_single_1st$rounds <- "single"
    linear_interp_single_1st$linear_interp_single_1st <- linear_interp_single_1st$y
  }
  
  #Otherwise assume single 1st follows the single 3rd response curve
  else {
    linear_interp_single_1st <- linear_interp_single_3rd
    linear_interp_single_1st$class_of_mail <- "1st"
    linear_interp_single_1st$linear_interp_single_1st <- linear_interp_single_1st$linear_interp_single_3rd
    linear_interp_single_1st$linear_interp_single_3rd <- NULL
  }
  
  #Latest inq
  linear_interp_latest_inq <- approx(as.numeric(as.character(agg_stats_latest_inq$Group.1)), agg_stats_latest_inq$response_rate_initial, xout = seq(0, 120, 1), rule=2)
  linear_interp_latest_inq <- as.data.frame(linear_interp_latest_inq)
  linear_interp_latest_inq$days_to_response <- linear_interp_latest_inq$x
  linear_interp_latest_inq$class_of_mail <- "3rd"
  linear_interp_latest_inq$rounds <- "single"
  linear_interp_latest_inq$linear_interp_latest_inq <- linear_interp_latest_inq$y
  
  # [AW] Recall: full_data, training_data, and OOS_data all come from c2g -- see get_model_data.R
  # Here we normalize the direct marketing response call data from c2g to the total call volume
  # ie- how does a subset of calls compare to the whole set of calls?
  #----------------------------------------------------------------------------------------

  #Normalized training data
  training_data_norm <- subset(training_data, is.na(as.numeric(as.character(training_data$Called))) == "FALSE")
  
  #Perform normalization from sum_responders to called by response_day_of_week
  sum_responders <- aggregate(as.numeric(training_data_norm$responders), 
                              by = list(training_data_norm$response_date), FUN=sum, na.rm = "TRUE")
  sum_responders$sum_responders <- sum_responders$x
  
  training_data_norm_1 <- merge(training_data_norm, sum_responders, by.x = "response_date", by.y = "Group.1", all.x = "TRUE")
  
  #Unique training data by response_date and day of week
  training_data_unique_norm <- training_data_norm_1[!duplicated(training_data_norm_1[,c("response_date")]), ]
  
  norm_responders <- aggregate(as.numeric(as.character(training_data_unique_norm$sum_responders)), 
                               by = list(training_data_unique_norm$response_day_of_week), 
                               FUN=sum, na.rm = "TRUE")
  norm_called <- aggregate(as.numeric(as.character(training_data_unique_norm$Called)), 
                           by = list(training_data_unique_norm$response_day_of_week), FUN=sum, na.rm = "TRUE")
  
  #Normalization by day, Responders to Called
  normalization <- merge(norm_responders, norm_called, by = "Group.1") 
  normalization$norm_day <- 1 /(normalization$x.x / normalization$x.y) # ratio of (calls / day) to (resp / day) by day of week
  
  #Normalization RR estimates by day
  norm_RR_leads <- aggregate(as.numeric(as.character(training_data_unique_norm$leads)), 
                             by=list(training_data_unique_norm$response_day_of_week), FUN=sum, na.rm="TRUE")
  norm_RR_by_day <- merge(norm_responders, norm_RR_leads, by="Group.1")
  # (resp / leads by day of week) / (resp / leads -- all days of week)
  # ie - ratio of RR by day of week vs overall
  norm_RR_by_day$RR_day_adj <- (norm_RR_by_day$x.x / norm_RR_by_day$x.y) / (sum(norm_RR_by_day$x.x) / sum(norm_RR_by_day$x.y))
  
  # [AW] Merge normalizations (RR normalized by day of week) and (calls / day to resp / day)
  # to OOS data
  # Then merge in RR by campaign class data and linear interpolations
  #----------------------------------------------------------------------------------------

  #Merge normalization back to OOS data
  OOS_data_1 <- merge(OOS_data, normalization, by.x = "response_day_of_week", by.y = "Group.1", all.x ="TRUE")
  OOS_data_1 <- merge(OOS_data_1, norm_RR_by_day, by.x="response_day_of_week", by.y = "Group.1", all.x="TRUE")
  
  #Merge inital response rates to OOS data
  OOS_data_2 <- merge(OOS_data_1, agg_stats_3, by.x = c("days_to_response", "rounds", "class_of_mail"), by.y = c("Group.1", "Group.2", "Group.3"), all.x = "TRUE")
  
  #Merge in response curves
  OOS_data_6 <- merge(OOS_data_2, linear_interp_single_1st, by = c("days_to_response", "rounds", "class_of_mail"), all.x = "TRUE")    
  OOS_data_7 <- merge(OOS_data_6, linear_interp_multiple_1st, by = c("days_to_response", "rounds", "class_of_mail"), all.x = "TRUE")   
  OOS_data_8 <- merge(OOS_data_7, linear_interp_single_3rd, by = c("days_to_response", "rounds", "class_of_mail"), all.x = "TRUE")    
  OOS_data_9 <- merge(OOS_data_8, linear_interp_multiple_3rd, by = c("days_to_response", "rounds", "class_of_mail"), all.x = "TRUE")    
  OOS_data_9 <- merge(OOS_data_9, linear_interp_latest_inq, by = "days_to_response", all.x = "TRUE")
  
  # [AW] OOS 9 is the only important one; all priors are copies

  #Add in projected_response_rate column
  
  #NEW-assign correct linear interp to each row
  for(i in 1:nrow(OOS_data_9)) {
    OOS_data_9$projected_response_rate[i] <- 
      sum(OOS_data_9$linear_interp_single_1st[i],  OOS_data_9$linear_interp_single_3rd[i], 
          OOS_data_9$linear_interp_multiple_1st[i],  OOS_data_9$linear_interp_multiple_3rd[i],  na.rm="TRUE")
  }
  
  #Replace latest inq campaings with latest inq estimate
  OOS_data_9$projected_response_rate <- 
    ifelse(as.character(OOS_data_9$campaign_group) == "Latest INQ w/ Phone" | 
             as.character(OOS_data_9$campaign_group) == "Latest INQ w/out Phone",
                 OOS_data_9$linear_interp_latest_inq, OOS_data_9$projected_response_rate)
  
  #Pull unique data from OOS, Called
  OOS_unique <- OOS_data[!duplicated(OOS_data[,c("response_date")]), ]
  OOS_unique <- data.frame(OOS_unique$response_date, OOS_unique$Called)
  
  training_data_Sat <- training_data_unique_norm[as.character(training_data_unique_norm$response_day_of_week) == "Saturday", ]
  training_data_Sun <- training_data_unique_norm[as.character(training_data_unique_norm$response_day_of_week) == "Sunday", ]
  training_data_weekends <- rbind(training_data_Sat, training_data_Sun)
  
  #Get unique training data
  training_data_proj <- subset(training_data, is.na(as.numeric(as.character(training_data$projected))) == "FALSE")
  training_data_unique_proj <- training_data_proj[!duplicated(training_data_proj[,c("response_date")]), ]
  
  # [AW] Project responses; several cases
  #----------------------------------------------------------------------------------------

  #Case 1, past_proj == 'FALSE'
  if(past_proj == "FALSE") {
    # [AW] responses = RR * leads; also calculates RMSE
    
    OOS_data_9$projected_responses <- OOS_data_9$projected_response_rate * as.numeric(as.character(OOS_data_9$leads)) 
    
    #total_projected_responses
    total_projected_responses <- aggregate(OOS_data_9$projected_responses, 
                                           by=list(OOS_data_9$response_date), FUN=sum, na.rm = "TRUE")
    total_projected_responses$total_projected_responses <- total_projected_responses$x
    total_projected_responses$x <- NULL
    
    #Merge in called and total projected responses
    total_projected_responses_1 <- merge(total_projected_responses, OOS_unique, 
                                         by.x="Group.1", by.y="OOS_unique.response_date", all.x="TRUE")
    
    #Rename columns in total_projected_responses_1 to fit to full_data
    total_projected_responses_1$projected <- total_projected_responses_1$total_projected_responses
    total_projected_responses_1$total_projected_responses <- NULL
    
    #RMSE
    total_projected_responses_1$OOS_unique.Called <- as.numeric(as.character(total_projected_responses_1$OOS_unique.Called))
    RMSE_new <- sqrt(mean((total_projected_responses_1$projected- total_projected_responses_1$OOS_unique.Called)^2))
    
  }
  
  #Case 2, past_proj 'TRUE' 
  #No past proj or weeekend_norm
  if(past_proj == "TRUE") {
    # [AW] responses = RR * leads * adjustment factor for day of week (based on invalid regressions)
    
    #Regressions to normalized for day of week and projected to called 
    # [AW] These regression are run on ~ 3-4 DF.... probably not valid -- also diagnostic plots are all wrong
    Tuesday_reg <- lm(training_data_unique_proj$Called ~ 0 + training_data_unique_proj$projected, 
                      weights = training_data_unique_proj$Called, subset = training_data_unique_proj$response_day_of_week == "Tuesday")
    Monday_reg <- lm(training_data_unique_proj$Called ~ 0 + training_data_unique_proj$projected, 
                     weights = training_data_unique_proj$Called, subset = training_data_unique_proj$response_day_of_week == "Monday")
    Wednesday_reg <- lm(training_data_unique_proj$Called ~ 0 + training_data_unique_proj$projected, 
                        weights = training_data_unique_proj$Called, subset = training_data_unique_proj$response_day_of_week == "Wednesday")
    Thursday_reg <- lm(training_data_unique_proj$Called ~ 0 + training_data_unique_proj$projected, 
                       weights = training_data_unique_proj$Called, subset = training_data_unique_proj$response_day_of_week == "Thursday")
    Friday_reg <- lm(training_data_unique_proj$Called ~ 0 + training_data_unique_proj$projected, 
                     weights = training_data_unique_proj$Called, subset = training_data_unique_proj$response_day_of_week == "Friday")
    
    #Load in all calls
    all_call_regs <- all_call_estimates(all_calls)
    
    #Current Date
    date_today <- as.Date(paste(current_year, current_month, 1, sep="-"))
    
    
    #Average of weekends versus other days 
    #weekdays only
    weekdays_only <- training_data_unique_proj[training_data_unique_proj$response_day_of_week != "Saturday" &
                                                 training_data_unique_proj$response_day_of_week != "Sunday", ]
    avg_weekdays_only <- mean(weekdays_only$Called, na.rm = "TRUE")
    
    #saturdays only
    saturdays_only <- training_data_unique_proj[training_data_unique_proj$response_day_of_week == "Saturday", ]
    avg_saturdays_only <- mean(saturdays_only$Called, na.rm = "TRUE")
    
    #sundays only
    sundays_only <- training_data_unique_proj[training_data_unique_proj$response_day_of_week == "Sunday", ]
    avg_sundays_only <- mean(sundays_only$Called, na.rm = "TRUE")
    
    day_reg_adj <- data.frame(V1= c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
                                     "Friday", "Saturday"),
                              day_reg_adj= c(avg_sundays_only / avg_weekdays_only, Monday_reg$coefficients[1],
                                     Tuesday_reg$coefficients[1], Wednesday_reg$coefficients[1],
                                     Thursday_reg$coefficients[1], Friday_reg$coefficients[1],
                                     avg_saturdays_only / avg_weekdays_only)
                              V3= rep(0,7))
    
    day_reg_adj[,2] <- ifelse(day_reg_adj[,2] > estimation_limit_called_proj, 
                              estimation_limit_called_proj, day_reg_adj[,2])
    
    OOS_data_9 <- merge(OOS_data_9, day_reg_adj, by.x="response_day_of_week", by.y="V1", all.x = "TRUE")
    
    #CHANGE FOR PAST PROJECTIONS TO NOT BE BIASED
    OOS_data_9$projected_responses     <- OOS_data_9$projected_response_rate * as.numeric(as.character(OOS_data_9$leads))  
    OOS_data_9$projected_responses_new <- OOS_data_9$projected_response_rate * as.numeric(as.character(OOS_data_9$leads)) * 
      as.numeric(as.character(OOS_data_9$day_reg_adj))
    
        
    #total_projected_responses
    total_projected_responses <- aggregate(OOS_data_9$projected_responses, 
                                           by=list(OOS_data_9$response_date), FUN=sum, na.rm = "TRUE")
    total_projected_responses$total_projected_responses <- total_projected_responses$x
    total_projected_responses$x <- NULL 
    
    total_projected_responses_1 <- merge(total_projected_responses, OOS_unique, 
                                         by.x="Group.1", by.y="OOS_unique.response_date", all.x="TRUE")
    total_projected_responses_1$projected <- total_projected_responses_1$total_projected_responses
    
    total_projected_responses_new <- aggregate(OOS_data_9$projected_responses_new, 
                                               by=list(OOS_data_9$response_date), FUN=sum, na.rm = "TRUE")
    total_projected_responses_new$total_projected_responses_new <- total_projected_responses$x
    #total_projected_responses_new$x <- NULL 
    
    total_projected_responses_1 <- merge(total_projected_responses_1, total_projected_responses_new, 
                                         by="Group.1", all.x="TRUE")
    total_projected_responses_1$proj_new <- total_projected_responses_1$x
    
    total_projected_responses_1 <- merge(total_projected_responses_1, total_projected_responses_new, 
                                         by="Group.1", all.x="TRUE")
    
    #Form unique OOS data
    OOS_data_9_unique <- data.frame(OOS_data_9$response_date, OOS_data_9$year_response_date, 
                                    OOS_data_9$month_response_date, OOS_data_9$day_response_date, 
                                    OOS_data_9$response_day_of_week)
    OOS_data_9_unique <- OOS_data_9_unique[!duplicated(OOS_data_9_unique$OOS_data_9.response_date), ]
    
    total_projected_responses_1 <- merge(total_projected_responses_1, OOS_data_9_unique, 
                                         by.x="Group.1", by.y="OOS_data_9.response_date", all.x="TRUE")
    total_projected_responses_1 <- 
      total_projected_responses_1[order(total_projected_responses_1$OOS_data_9.year_response_date, 
                                        total_projected_responses_1$OOS_data_9.month_response_date,
                                        total_projected_responses_1$OOS_data_9.day_response_date), ]
    
    
    #Eliminate
    total_projected_responses_1$projected_new <- total_projected_responses_1$proj_new
    total_projected_responses_1$perc_error <- (total_projected_responses_1$proj_new - 
                 total_projected_responses_1$OOS_unique.Called) / total_projected_responses_1$OOS_unique.Called
    
    
    total_projected_responses_1$Called_weights <- total_projected_responses_1$OOS_unique.Called
    
    
    for(i in 1:nrow(total_projected_responses_1)) {
      #total_projected_responses_1$perc_error
      if(as.character(total_projected_responses_1$OOS_data_9.response_day_of_week[i]) == "Saturday" | 
           as.character(total_projected_responses_1$OOS_data_9.response_day_of_week[i]) == "Sunday") {
        total_projected_responses_1$perc_error[i] <- 0
        total_projected_responses_1$Called_weights[i] <- 0
      }
      
      #If perc_error is NA then replace with 0
      total_projected_responses_1$perc_error <- 
        ifelse(is.na(total_projected_responses_1$perc_error), 0, total_projected_responses_1$perc_error)
      
      if(total_projected_responses_1$perc_error[i] > momentum_limit) {
        total_projected_responses_1$perc_error[i] <- momentum_limit
      } else if(total_projected_responses_1$perc_error[i] < -momentum_limit) {
        total_projected_responses_1$perc_error[i] <- -momentum_limit
      }
      
      total_projected_responses_1$error_by_called[i] <- 
        total_projected_responses_1$perc_error[i] * total_projected_responses_1$Called_weights[i]
      
      if(i >= 2) {
        total_projected_responses_1$projected_new[i] <- 
          total_projected_responses_1$proj_new[i] / (1 + (sum(total_projected_responses_1$error_by_called[1:(i-1)]) / 
                    sum(total_projected_responses_1$Called_weights[1:(i-1)])))
      }
      
    }
    #End eliminate
    
    #Calculate RMSE
    total_projected_responses_1$OOS_unique.Called <- as.numeric(as.character(total_projected_responses_1$OOS_unique.Called))
    RMSE_new <- sqrt(mean((total_projected_responses_1$proj_new - total_projected_responses_1$OOS_unique.Called)^2))
    
  }
  
  if(past_proj == "FALSE") {
    
    #Add new projections to full data
    full_data <- merge(full_data, total_projected_responses_1, by.x="response_date", by.y="Group.1", all.x="TRUE")
    
    #Initialize projected
    full_data$projected <- NA
    for(i in 1:nrow(full_data)) {
      
      if(is.na(full_data$projected[i]) == "TRUE") {
        full_data$projected[i] <- full_data$projected.y[i]
      }
      
      else {
        full_data$projected[i] <- full_data$projected.x[i]
      }
      
    }
    
    full_data$OOS_unique.Called <- NULL
    full_data$projected.x <- NULL
    full_data$projected.y <- NULL
    
    #Change names total projected responses
    total_projected_responses_1 <- total_projected_responses_1[ , c(1, 2, 3)]
    names(total_projected_responses_1) <- c("response_date", "Called", "projected")
    
  }
  
  if(past_proj == "TRUE") {
    
    #Add new projections to full data
    full_data <- merge(full_data, total_projected_responses_1, by.x="response_date", by.y="Group.1", all.x="TRUE")
    
    
    #Initialize projected
    full_data$projected <- NA
    for(i in 1:nrow(full_data)) {
      
      if(is.na(full_data$projected.x[i]) == "FALSE") {
        full_data$projected[i] <- full_data$projected.x[i]
      }
      
      else {
        full_data$projected[i] <- full_data$projected.y[i]
      }
      
    }
    
    full_data$OOS_unique.Called <- NULL
    full_data$total_projected_responses <- NULL
    full_data$projected.x <- NULL
    full_data$projected.y <- NULL
    full_data$x.x <- NULL
    full_data$proj_new <- NULL
    full_data$x.y <- NULL
    full_data$OOS_data_9.year_response_date <- NULL
    full_data$OOS_data_9.month_response_date <- NULL
    full_data$OOS_data_9.day_response_date <- NULL
    full_data$OOS_data_9.response_day_of_week <- NULL
    full_data$projected_new <- NULL
    full_data$perc_error <- NULL
    full_data$Called_weights <- NULL
    full_data$error_by_called <- NULL
    
    #Only keep useful total_projected_responses columns
    
    total_projected_responses_1 <- total_projected_responses_1[ , c(1, 3, 6, 8, 9, 10, 11)]
    names(total_projected_responses_1) <- c("response_date", "Called", "projected", "year_response_date", "month_response_date", "day_response_date", "response_day_of_week")
    
    #No weekends total projected responses
    total_projected_responses_no_weekends <- total_projected_responses_1[as.character(total_projected_responses_1$response_day_of_week) != "Saturday" &
                                                                           as.character(total_projected_responses_1$response_day_of_week) != "Sunday", ]
    
    total_projected_responses_1 <- total_projected_responses_1[ , c(1, 2, 3, 7)]
    total_projected_responses_no_weekends <- total_projected_responses_no_weekends[ , c(1, 2, 3, 7)]
    RMSE_no_weekends <- sqrt(mean((total_projected_responses_no_weekends$projected - total_projected_responses_no_weekends$Called)^2))
    
    #Now, get rid of weekday column
    total_projected_responses_1 <- total_projected_responses_1[ , c(1, 2, 3)]
    
  }
  
  full_data_new <- merge(full_data, total_projected_responses_1, by="response_date", all.x="TRUE")
  
  #Order full_data
  full_data <- full_data[order(full_data$year_response_date, full_data$month_response_date, full_data$day_response_date), ]
  
  #Merge in monthly_adjustment
  total_projected_responses_1$month <- month(total_projected_responses_1$response_date)
  total_projected_responses_1 <- merge(total_projected_responses_1, monthly_adjust$monthly_adjustment, by="month", all.x="TRUE")
  
  total_projected_responses_1$projected <- total_projected_responses_1$projected * total_projected_responses_1$monthly_adjustment
  total_projected_responses_1$monthly_adjustment <- total_projected_responses_1$month <- NULL
  
  #Full estimates 
  full_estimates <- call_regressions(all_call_regs$calls, total_projected_responses_1, min(total_projected_responses_1$response_date), 12, 0, 1, 0, 1, all_calls)
  
  
  if(past_proj == "TRUE") {
    output <- list("total_projected_responses_1" = total_projected_responses_1, 
                   "total_projected_responses_no_weekends" = total_projected_responses_no_weekends, 
                   "full_data" = full_data, "RMSE_new" = RMSE_new, 
                   "RMSE_no_weekends" = RMSE_no_weekends, "day_reg_adj" = day_reg_adj, 
                   "avg_weekdays_only" = avg_weekdays_only, "avg_saturdays_only" = avg_saturdays_only, 
                   "avg_sundays_only" = avg_sundays_only,
                   "training_data_unique_proj" = training_data_unique_proj, 
                   "weekdays_only" = weekdays_only, "saturdays_only" = saturdays_only, 
                   "OOS_data_9" = OOS_data_9, 
                   "linear_interp_single_1st" = linear_interp_single_1st,
                   "linear_interp_single_3rd" = linear_interp_single_3rd, 
                   "OOS_data_2" = OOS_data_2, "OOS_data_6" = OOS_data_6, "OOS_data_7" = OOS_data_7, 
                   "OOS_data_8" = OOS_data_8, "OOS_data_9_unique" = OOS_data_9_unique, 
                   "full_data_new" = full_data_new, "all_call_regs" = all_call_regs, 
                   "date_today" = date_today, "full_data" = full_data, "full_estimates" = full_estimates)
  }
  
  else {
    output <- list("total_projected_responses_1" = total_projected_responses_1, "full_data" = full_data, "RMSE_new" = RMSE_new)
  }
  
  
}