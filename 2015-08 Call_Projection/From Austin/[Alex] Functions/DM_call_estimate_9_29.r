function(full_data, training_data, OOS_data, min_days, estimation_limit_called_proj, momentum_limit, past_proj) { 
    
    #Re-name variables
    training_data$class <- training_data$class_of_mail
    OOS_data$class <- OOS_data$class_of_mail
    training_data$leads <- training_data$unique_leads
    OOS_data$leads <- OOS_data$unique_leads
    
    #Change OOS data #N/A obs in class to "3rd"
    for(i in 1:nrow(OOS_data)) {
        if(is.na(as.character(OOS_data$class_of_mail[i])) == "TRUE") {
            OOS_data$class_of_mail[i] <- "3rd"
        }
    }
    
    #Create latest inq and non-latest inq training data
    training_data_1 <- training_data[training_data$campaign_group != "Latest INQ w/out Phone" & 
                                       training_data$campaign_group != "Latest INQ w/ Phone", ]
    training_data_2 <- training_data[training_data$campaign_group == "Latest INQ w/out Phone" | 
                                       training_data$campaign_group == "Latest INQ w/ Phone", ]
    
    #------------------------------------------------------
    
    #Step 1, get sum of responsders on training_data sample, by days_to_response, rounds, round, class_of_mail. This is agg_stats_1
    agg_stats_1 <- aggregate(as.numeric(training_data_1$responders), 
                             by = list(training_data_1$days_to_response, training_data_1$rounds, 
                                       training_data_1$class), FUN=sum, na.rm = "TRUE")
    
    #Step 2, get sum of leads on training_data sample, by days_to_response, rounds, round, class_of_mail. This is agg_stats_1a
    agg_stats_1a <- aggregate(as.numeric(training_data_1$leads), 
                              by = list(training_data_1$days_to_response, training_data_1$rounds, 
                                        training_data_1$class), FUN=sum, na.rm = "TRUE")
    
    #Step 3, merge agg_stats_1 and agg_stats_1a in order to estimate response rates for initial (and single) mailings.
    agg_stats_2 <- merge(agg_stats_1, agg_stats_1a, by=c("Group.1", "Group.2", "Group.3"))
    
    #Step 3, calculate response rate from agg_stats_2
    agg_stats_2$response_rate_initial <- agg_stats_2$x.x / agg_stats_2$x.y
    
    #Drop obs with less than min_days responders
    agg_stats_3_new <- agg_stats_2[agg_stats_2$x.x >= min_days, ]
    agg_stats_3 <- agg_stats_3_new 
    
    #Step 5, order by response date.
    agg_stats_3 <- agg_stats_3[order(agg_stats_3$Group.3, agg_stats_3$Group.2, agg_stats_3$Group.1) ,]
    
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
    
    #Curves to form linear interpolation from
    agg_stats_3_single_1st <- agg_stats_3[(as.character(agg_stats_3$Group.3) == "1st" & 
                                             as.character(agg_stats_3$Group.2) == "single"), ]
    agg_stats_3_single_3rd <- agg_stats_3[(as.character(agg_stats_3$Group.3) == "3rd" & 
                                             as.character(agg_stats_3$Group.2) == "single"), ]
    agg_stats_3_multiple_1st <- agg_stats_3[(as.character(agg_stats_3$Group.3) == "1st" & 
                                               as.character(agg_stats_3$Group.2) == "multiple"), ]
    agg_stats_3_multiple_3rd <- agg_stats_3[(as.character(agg_stats_3$Group.3) == "3rd" & 
                                               as.character(agg_stats_3$Group.2) == "multiple"), ]
    
    #------------------------------------------------------
    
    #Latset Inq
    agg_stats_latest_inq <- agg_stats_3_a
    
    #Linear interpolation, days_to_response
    linear_interp_single_3rd <- approx(as.numeric(as.character(agg_stats_3_single_3rd$Group.1)), 
                                       agg_stats_3_single_3rd$response_rate_initial, xout = seq(0, 120, 1), rule=2)
    linear_interp_single_3rd <- as.data.frame(linear_interp_single_3rd)
    linear_interp_single_3rd$days_to_response <- linear_interp_single_3rd$x
    linear_interp_single_3rd$class_of_mail <- "3rd"
    linear_interp_single_3rd$rounds <- "single"
    linear_interp_single_3rd$linear_interp_single_3rd <- linear_interp_single_3rd$y
    
    linear_interp_multiple_3rd <- approx(as.numeric(as.character(agg_stats_3_multiple_3rd$Group.1)), 
                                         agg_stats_3_multiple_3rd$response_rate_initial, xout = seq(0, 120, 1), rule=2)
    linear_interp_multiple_3rd <- as.data.frame(linear_interp_multiple_3rd)
    linear_interp_multiple_3rd$days_to_response <- linear_interp_multiple_3rd$x
    linear_interp_multiple_3rd$class_of_mail <- "3rd"
    linear_interp_multiple_3rd$rounds <- "multiple"
    linear_interp_multiple_3rd$linear_interp_multiple_3rd <- linear_interp_multiple_3rd$y
    
    if(nrow(agg_stats_3_multiple_1st) > 0) {
      linear_interp_multiple_1st <- approx(as.numeric(as.character(agg_stats_3_multiple_1st$Group.1)), 
                                           agg_stats_3_multiple_1st$response_rate_initial, xout = seq(0, 120, 1), rule=2)
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
        linear_interp_single_1st <- approx(as.numeric(as.character(agg_stats_3_single_1st$Group.1)), 
                                           agg_stats_3_single_1st$response_rate_initial, xout = seq(0, 120, 1), rule=2)
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
    linear_interp_latest_inq <- approx(as.numeric(as.character(agg_stats_latest_inq$Group.1)), 
                                       agg_stats_latest_inq$response_rate_initial, xout = seq(0, 120, 1), rule=2)
    linear_interp_latest_inq <- as.data.frame(linear_interp_latest_inq)
    linear_interp_latest_inq$days_to_response <- linear_interp_latest_inq$x
    linear_interp_latest_inq$class_of_mail <- "3rd"
    linear_interp_latest_inq$rounds <- "single"
    linear_interp_latest_inq$linear_interp_latest_inq <- linear_interp_latest_inq$y

    #------------------------------------------------------
    
    #Normalized training data
    training_data_norm <- subset(training_data, is.na(as.numeric(as.character(training_data$Called))) == "FALSE")
    
    #Perform normalization from sum_responders to called by response_day_of_week
    sum_responders <- aggregate(as.numeric(training_data_norm$responders), 
                                by = list(training_data_norm$response_date), FUN=sum, na.rm = "TRUE")
    sum_responders$sum_responders <- sum_responders$x
    
    training_data_norm_1 <- merge(training_data_norm, sum_responders, by.x = "response_date", by.y = "Group.1", all.x = "TRUE")
    
    #Unique training data by response_date
    training_data_unique_norm <- training_data_norm_1[!duplicated(training_data_norm_1[,c("response_date")]), ]
    
    norm_responders <- aggregate(as.numeric(as.character(training_data_unique_norm$sum_responders)), 
                                 by = list(training_data_unique_norm$response_day_of_week), FUN=sum, na.rm = "TRUE")
    norm_called <- aggregate(as.numeric(as.character(training_data_unique_norm$Called)), 
                             by = list(training_data_unique_norm$response_day_of_week), FUN=sum, na.rm = "TRUE")
    
    #Normalization by day, Responders to Called
    normalization <- merge(norm_responders, norm_called, by = "Group.1") 
    normalization$norm_day <- 1 /(normalization$x.x / normalization$x.y)
    
    #Normalization RR estimates by day
    norm_RR_leads <- aggregate(as.numeric(as.character(training_data_unique_norm$leads)), 
                               by=list(training_data_unique_norm$response_day_of_week), FUN=sum, na.rm="TRUE")
    norm_RR_by_day <- merge(norm_responders, norm_RR_leads, by="Group.1")
    norm_RR_by_day$RR_day_adj <- (norm_RR_by_day$x.x / norm_RR_by_day$x.y) / (sum(norm_RR_by_day$x.x) / sum(norm_RR_by_day$x.y))
    
    #Merge normalization back to OOS data
    OOS_data_1 <- merge(OOS_data, normalization, by.x = "response_day_of_week", by.y = "Group.1", all.x ="TRUE")
    OOS_data_1 <- merge(OOS_data_1, norm_RR_by_day, by.x="response_day_of_week", by.y = "Group.1", all.x="TRUE")
    
    #Merge inital response rates to OOS data
    OOS_data_2 <- merge(OOS_data_1, agg_stats_3, 
                        by.x = c("days_to_response", "rounds", "class_of_mail"), 
                        by.y = c("Group.1", "Group.2", "Group.3"), all.x = "TRUE")
    
    #Merge in response curves
    OOS_data_6 <- merge(OOS_data_2, linear_interp_single_1st, by = c("days_to_response", "rounds", "class_of_mail"), all.x = "TRUE")    
    OOS_data_7 <- merge(OOS_data_6, linear_interp_multiple_1st, by = c("days_to_response", "rounds", "class_of_mail"), all.x = "TRUE")   
    OOS_data_8 <- merge(OOS_data_7, linear_interp_single_3rd, by = c("days_to_response", "rounds", "class_of_mail"), all.x = "TRUE")    
    OOS_data_9 <- merge(OOS_data_8, linear_interp_multiple_3rd, by = c("days_to_response", "rounds", "class_of_mail"), all.x = "TRUE")    
    OOS_data_9 <- merge(OOS_data_9, linear_interp_latest_inq, by = "days_to_response", all.x = "TRUE")
    
    #Add in projected_response_rate column
    
    #NEW-assign correct linear interp to each row
    for(i in 1:nrow(OOS_data_9)) {
      OOS_data_9$projected_response_rate[i] <- sum(OOS_data_9$linear_interp_single_1st[i],  
                                                   OOS_data_9$linear_interp_single_3rd[i], 
                                                   OOS_data_9$linear_interp_multiple_1st[i],  
                                                   OOS_data_9$linear_interp_multiple_3rd[i],
      na.rm="TRUE")
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
      
    #------------------------------------------------------
    
    #Case 1, past_proj == 'FALSE'
    if(past_proj == "FALSE") {
        
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
        
        #Calculate projected response
        #OOS_data_9$projected_responses <- OOS_data_9$projected_response_rate * as.numeric(as.character(OOS_data_9$leads)) * OOS_data_9$RR_day_adj
        # * OOS_data_9$norm_day
        
        #past called to responders by day
        #sum_responders <- aggregate(training_data$responders, by=list(training_data$day_of_week), FUN=sum, na.rm = "TRUE")
        #sum_called <- aggregate(training_data_unique_proj$Called, by=list(training_data_unique_proj$day_of_week), FUN=sum, na.rm = "TRUE")
        #responders_to_called <- merge(sum_responders, sum_called, by="Group.1")   
        
        #Regressions to normalized for day of week and projected to called 
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
        Saturday_reg <- lm(training_data_unique_proj$Called ~ 0 + training_data_unique_proj$projected, 
                           weights = training_data_unique_proj$Called, subset = training_data_unique_proj$response_day_of_week == "Saturday")
        Sunday_reg <- lm(training_data_unique_proj$Called ~ 0 + training_data_unique_proj$projected,
                         weights = training_data_unique_proj$Called, subset = training_data_unique_proj$response_day_of_week == "Sunday")
        
        day_reg_adj <- matrix(0, 7, 3)
        day_reg_adj[1, 1] <- "Sunday"
        day_reg_adj[2, 1] <- "Monday"
        day_reg_adj[3, 1] <- "Tuesday"
        day_reg_adj[4, 1] <- "Wednesday"
        day_reg_adj[5, 1] <- "Thursday"
        day_reg_adj[6, 1] <- "Friday"
        day_reg_adj[7, 1] <- "Saturday"
        
        day_reg_adj[1, 2] <- as.numeric(Sunday_reg$coefficients[1])
        day_reg_adj[2, 2] <- as.numeric(Monday_reg$coefficients[1])
        day_reg_adj[3, 2] <- as.numeric(Tuesday_reg$coefficients[1])
        day_reg_adj[4, 2] <- as.numeric(Wednesday_reg$coefficients[1])
        day_reg_adj[5, 2] <- as.numeric(Thursday_reg$coefficients[1])
        day_reg_adj[6, 2] <- as.numeric(Friday_reg$coefficients[1])
        day_reg_adj[7, 2] <- as.numeric(Saturday_reg$coefficients[1])
        
        #Force Sat. and Sunday to no adjustment
        day_reg_adj[1, 2] <- 1
        day_reg_adj[7, 2] <- 1
        
        for(i in 1:nrow(day_reg_adj)) {
            
            if(day_reg_adj[i, 2] > estimation_limit_called_proj) {
                day_reg_adj[i, 2] <- estimation_limit_called_proj
            }
            
        }
        
        day_reg_adj <- as.data.frame(day_reg_adj)
        day_reg_adj$day_reg_adj <- day_reg_adj$V2
        day_reg_adj$V2 <- NULL
        
        OOS_data_9 <- merge(OOS_data_9, day_reg_adj, by.x="response_day_of_week", by.y="V1", all.x = "TRUE")
        
        #CHANGE FOR PAST PROJECTIONS TO NOT BE BIASED
        OOS_data_9$projected_responses <- OOS_data_9$projected_response_rate * as.numeric(as.character(OOS_data_9$leads))  
        OOS_data_9$projected_responses_new <- OOS_data_9$projected_response_rate * 
          as.numeric(as.character(OOS_data_9$leads)) * as.numeric(as.character(OOS_data_9$day_reg_adj))
        
        #total_projected_responses
        total_projected_responses <- aggregate(OOS_data_9$projected_responses, by=list(OOS_data_9$response_date), FUN=sum, na.rm = "TRUE")
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
            }
            
            if(total_projected_responses_1$perc_error[i] < -momentum_limit) {
                total_projected_responses_1$perc_error[i] <- -momentum_limit
            }
            
            total_projected_responses_1$error_by_called[i] <- total_projected_responses_1$perc_error[i] * 
              total_projected_responses_1$Called_weights[i]
            
            if(i >= 2) {
                total_projected_responses_1$projected_new[i] <- total_projected_responses_1$proj_new[i] / 
                  (1 + (sum(total_projected_responses_1$error_by_called[1:(i-1)]) / 
                          sum(total_projected_responses_1$Called_weights[1:(i-1)])) )
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
        names(total_projected_responses_1) <- c("response_date", "Called", "projected", 
              "year_response_date", "month_response_date", "day_response_date", "response_day_of_week")
        
        #No weekends total projected responses
        total_projected_responses_no_weekends <- 
          total_projected_responses_1[as.character(total_projected_responses_1$response_day_of_week) != "Saturday" &
        as.character(total_projected_responses_1$response_day_of_week) != "Sunday", ]
 
        total_projected_responses_1 <- total_projected_responses_1[ , c(1, 2, 3, 7)]
        total_projected_responses_no_weekends <- total_projected_responses_no_weekends[ , c(1, 2, 3, 7)]
        RMSE_no_weekends <- sqrt(mean((total_projected_responses_no_weekends$projected - total_projected_responses_no_weekends$Called)^2))
    }
    
    
    
    #Order full_data
    full_data <- full_data[order(full_data$year_response_date, full_data$month_response_date, full_data$day_response_date), ]
    
    
    if(past_proj == "TRUE") {
        output <- list("total_projected_responses_1" = total_projected_responses_1, 
                       "total_projected_responses_no_weekends" = total_projected_responses_no_weekends, 
                       "full_data" = full_data, "RMSE_new" = RMSE_new, 
                        "RMSE_no_weekends" = RMSE_no_weekends)
    }
    
    else {
        output <- list("total_projected_responses_1" = total_projected_responses_1, 
                       "full_data" = full_data, "RMSE_new" = RMSE_new)
    }
    
        
}
