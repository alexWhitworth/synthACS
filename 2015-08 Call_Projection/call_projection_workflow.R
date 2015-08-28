

# devtools::install_github("dandb/data-analysis/2015-08 Call_Projection/callProjection", 
#                          auth_token= "684f19755192333326787d9f055cfd67244452e8")

setwd("G:/Whitworth_Alex/2015-08 Call_Projection/")

library(callProjection)
library(dplyr)

# current call projection workflow
#---------------------------------------------------------------
# 1. pull in call data
called <- pull_call_data(channel= "c2g", call_date= '4/01/2014') # use defaults

# 2. Aggregate call data, pull / clean campaign tracking and response data
# model_data <- get_model_data(channel= "c2g", historical= FALSE)
model_data <- get_model_data(channel= "c2g", historical= TRUE, hist_yr= 2015, hist_mo= 9)
# 3. impute missing days response for complete campaigns
camp_complete_imp <- impute_zero_resp_all(dat= model_data$camp_complete, days_tracking= 90, comp_camp= TRUE)
# 4. calculate adjustment rate for top performing ongoing campaigns
top_ongoing <- camp_out_calc_adj(model_data$camp_outstanding, channel= "c2g")
# 5. summarize complete campaigns by campaign class and day of response
# use state-space model depending on weekend or not
camp_complete_imp$class_of_mail <- 
  ifelse(!is.na(camp_complete_imp$class_of_mail), camp_complete_imp$class_of_mail,
    ifelse(is.na(camp_complete_imp$class_of_mail) & substr(camp_complete_imp$cell_code, 5,5) == "1", "1st",
      ifelse(is.na(camp_complete_imp$class_of_mail) & substr(camp_complete_imp$cell_code, 5,5) == "3", "3rd", NA)))

camp_comp_stats <- camp_complete_imp %>% group_by(campaign_type, class_of_mail, days_to_response) %>% 
  summarize(n= length(cell_code), 
            mean_daily_resp_rate= mean(responders / unique_leads, na.rm=TRUE),
            mean_pct_resp= mean(pct_of_responders, na.rm=TRUE),
            n_daily_resp= mean(pct_of_responders, na.rm=TRUE) * sum(total_responders, na.rm=TRUE)) %>%
  group_by(campaign_type, class_of_mail) %>% mutate(mean_cum_rr= cumsum(mean_pct_resp))

# 6. baseline forecasts for ongoing campaigns and campaigns which have not begun yet
# and adjust forecasts at campaign level for high performing campaigns
base_forecasts <- create_baseline_forecasts(model_data$camp_proj, camp_comp_stats, top_ongoing)

# 7. calculate / apply adjustments for day of week -- aggregate level
# and calculate / apply adjustments for holidays    -- aggregate level
projections <- adj_base_forecasts(base_forecasts[[2]], called, 
                                  rbind(model_data$camp_complete, model_data$camp_outstanding), 
                                  seasonal_adj_type= "ensemble", call_hist= called,
                                  seasonal_wks = 5, windsor.q = c(.25, .65))


# note 1: no seasonality of RR by date / month observed
# note 2: Weekly calls: sat / sun roughly equivalent; M/T roughly equivalent; W-F roughly equivalent

write.csv(projections[[1]], "./projections_sept-2015.csv", row.names= FALSE)
