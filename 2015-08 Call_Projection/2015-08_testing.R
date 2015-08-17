


setwd("G:/Whitworth_Alex/2015-08 Call_Projection/")
load("./testing_data.Rdata")
source("./testing_functions.R")

library(callProjection)
library(dplyr)

# 1. pull in call data 
#----------------------------------------------------------
# called <- pull_call_data(channel= "c2g", call_date= '4/01/2014')
# calls_by_day_cat <- called[, .(calls= sum(call_count)) ,by= .(category, call_date)]
# calls_by_day_cat$category <- ifelse(calls_by_day_cat$category == "D&B IVR", "db_ivr",
#                              ifelse(calls_by_day_cat$category == "DandB.com", "dandb_com",
#                              ifelse(calls_by_day_cat$category == "Marketing Direct", "mkt_direct",
#                              ifelse(calls_by_day_cat$category == "Organic", "organic",
#                              ifelse(calls_by_day_cat$category == "Paid Media, Web & Affiliate", "paid_etc",
#                              ifelse(calls_by_day_cat$category == "iUpdate", "iupdate", calls_by_day_cat$category))))))
# calls <- merge(called[, .(calls= sum(call_count)) ,by= .(call_date)], 
#                dcast.data.table(calls_by_day_cat, formula= call_date ~ category, value.var= "calls"), 
#                by= "call_date", all= TRUE)
# setnames(calls, old= names(calls), new= c("call_date", "act_calls", "act_dandb", "act_dbivr",
#                                           "act_iupdate", "act_mkt", "act_org", "act_paid"))
# rm(calls_by_day_cat)

# save.image("./testing_data.Rdata")

# 2. initialize needed params and loop
#----------------------------------------------------------
hist_yr <- c(rep(2015,7))
hist_mo <- c(seq(1,7,1))
hist_names <- vector("character", length= length(hist_yr))

projections.ens <- list()
projections.ets <- list()
projections.stl <- list()
projections.wks <- list()
projections.wk_stl <- list()
actual <- list()

for (j in 1:length(hist_yr)) {
  hist_names[j] <- paste("x", hist_yr[j], hist_mo[j], sep= "-")
  
  actual[[j]] <- calls[year(call_date) == hist_yr[j] & month(call_date) == hist_mo[j],]
  
  model_data <- get_model_data(channel= "c2g", historical= TRUE, 
                               hist_yr= hist_yr[j], hist_mo= hist_mo[j])
  
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
  projections.ens[[j]] <- adj_base_forecasts(base_forecasts[[2]], called, 
                                         rbind(model_data$camp_complete, model_data$camp_outstanding), 
                                         seasonal_adj_type= "ensemble", call_hist= called)
  projections.ets[[j]] <- adj_base_forecasts(base_forecasts[[2]], called, 
                                             rbind(model_data$camp_complete, model_data$camp_outstanding), 
                                             seasonal_adj_type= "ets", call_hist= called)
  projections.stl[[j]] <- adj_base_forecasts(base_forecasts[[2]], called, 
                                             rbind(model_data$camp_complete, model_data$camp_outstanding), 
                                             seasonal_adj_type= "stl", call_hist= called)
  projections.wks[[j]] <- adj_base_forecasts(base_forecasts[[2]], called, 
                                             rbind(model_data$camp_complete, model_data$camp_outstanding), 
                                             seasonal_adj_type= "wk_avg", call_hist= called)
  projections.wk_stl[[j]] <- adj_base_forecasts(base_forecasts[[2]], called, 
                                             rbind(model_data$camp_complete, model_data$camp_outstanding), 
                                             seasonal_adj_type= "wk_stl", call_hist= called)
  
  
  
  rm(model_data, camp_complete_imp, top_ongoing, camp_comp_stats, base_forecasts); gc(verbose= FALSE)
}

names(actual) <- names(projections.ets) <- names(projections.ens) <- names(projections.stl) <- 
  names(projections.wks) <- names(projections.wk_stl) <- hist_names

rm(j, hist_yr, hist_mo, hist_names)

# 3. Evaluate
#----------------------------------------------------------
projections.ets[[2]][[1]] <- projections.ets[[2]][[1]][1:28,]
projections.ens[[2]][[1]] <- projections.ens[[2]][[1]][1:28,]
projections.stl[[2]][[1]] <- projections.stl[[2]][[1]][1:28,]
projections.wks[[2]][[1]] <- projections.wks[[2]][[1]][1:28,]
projections.wk_stl[[2]][[1]] <- projections.wk_stl[[2]][[1]][1:28,]

acc_ets <- compute_acc(projections.ets, actual)
acc_stl <- compute_acc(projections.stl, actual)
acc_ens <- compute_acc(projections.ens, actual)
acc_wks <- compute_acc(projections.wks, actual)
acc_wk_stl <- compute_acc(projections.wk_stl, actual)

rm(rmse, mnAD, mxAD, wmnAD, compute_acc)

save.image("./testing_data.Rdata")


# 4. Examine specific month -- April / July
#----------------------------------------------------------
model_data <- get_model_data(channel= "c2g", historical= TRUE, 
                             hist_yr= 2015, hist_mo= 7)

camp_complete_imp <- impute_zero_resp_all(dat= model_data$camp_complete, days_tracking= 90, comp_camp= TRUE)

top_ongoing <- camp_out_calc_adj(model_data$camp_outstanding, channel= "c2g")

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

base_forecasts <- create_baseline_forecasts(model_data$camp_proj, camp_comp_stats, top_ongoing)

projections <- adj_base_forecasts(base_forecasts[[2]], called, 
                                  rbind(model_data$camp_complete, model_data$camp_outstanding), 
                                  seasonal_adj_type= "ensemble", call_hist= called)

rmse(actual[[7]]$act_mkt, projections[[1]]$mkt_direct) / mean(actual[[7]]$act_mkt)
mnAD(actual[[7]]$act_mkt, projections[[1]]$mkt_direct) / mean(actual[[4]]$act_mkt)


# 5. Create some plots
#----------------------------------------------------------
proj <- rbindlist(lapply(projections.ens, function(x) {x[[1]]$wday <- NULL; return(x[[1]])}), fill=TRUE)
act <- rbindlist(actual)

library(ggplot2)

ggplot(proj, aes(x= act$act_mkt, y= proj$mkt_direct)) + geom_point() + geom_smooth() +
  labs(x= "Actual Calls", y= "Projected Calls",
       title= "Marketing Direct")
ggplot(proj, aes(x= act$act_dbivr, y= proj$db_ivr)) + geom_point() + geom_smooth()
ggplot(proj, aes(x= act$act_dandb, y= proj$dandb.com)) + geom_point() + geom_smooth()
ggplot(proj, aes(x= act$act_iupdate, y= proj$iupdate)) + geom_point() + geom_smooth()
ggplot(proj, aes(x= act$act_org, y= proj$organic)) + geom_point() + geom_smooth()
ggplot(proj, aes(x= act$act_paid, y= proj$paid_etc)) + geom_point() + geom_smooth()

knitr::kable(acc_ens[,-3,1])
