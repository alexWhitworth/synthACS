

setwd("G:/Whitworth_Alex/2015-08 Call_Projection/")
load("./testing_base.Rdata")
source("./testing_functions.R")

library(callProjection)
library(dplyr)
library(ggplot2)

#----------------------------------------------------------
# 00. Pre allocate testing space
#----------------------------------------------------------
wk_stl <- wks <- stl <- ets <- ensemble <- new.env()

season_wk_param <- rep(c(2,5,6,7,8), 4)
cap_param       <- rep(c(.5, .55, .6, .65), each= 5)   
if (length(cap_param) != length(season_wk_param)) {
  stop("error")
} else {
  len <- length(cap_param)  
}

# len lists (tuning paramters) of length 7 (monthly projections) for each model 
# len lists of length 1 for accuracy metrics
ensemble$p <- replicate(len, vector(mode= "list", length= 7), simplify= FALSE)
ensemble$a <- vector(mode= "list", length= len)

ets$p <- replicate(len, vector(mode= "list", length= 7), simplify= FALSE)
ets$a <- vector(mode= "list", length= len)

stl$p <- replicate(len, vector(mode= "list", length= 7), simplify= FALSE)
stl$a <- vector(mode= "list", length= len)

wks$p <- replicate(len, vector(mode= "list", length= 7), simplify= FALSE)
wks$a <- vector(mode= "list", length= len)

wk_stl$p <- replicate(len, vector(mode= "list", length= 7), simplify= FALSE)
wk_stl$a <- vector(mode= "list", length= len)

actual <- list()


#----------------------------------------------------------
# 01. Test 1 -- 
#----------------------------------------------------------
hist_yr <- c(rep(2015,7))
hist_mo <- c(seq(1,7,1))
hist_names <- vector("character", length= length(hist_yr))

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
  
  for (i in 1:len) {
    #-----------------------
    # ensemble models
    #-----------------------
    ensemble$p[[i]][[j]] <- adj_base_forecasts(base_forecasts[[2]], called, 
                                               rbind(model_data$camp_complete, model_data$camp_outstanding), 
                                               seasonal_wks= season_wk_param[i], seasonal_adj_type= "ensemble", 
                                               call_hist= called, windsor.q= c(.25, cap_param[i]))  
    #-----------------------
    # ets models
    #-----------------------
    ets$p[[i]][[j]] <- adj_base_forecasts(base_forecasts[[2]], called, 
                                          rbind(model_data$camp_complete, model_data$camp_outstanding), 
                                          seasonal_wks= season_wk_param[i], seasonal_adj_type= "ets", 
                                          call_hist= called, windsor.q= c(.25, cap_param[i]))
    #-----------------------
    # stl models
    #-----------------------
    stl$p[[i]][[j]] <- adj_base_forecasts(base_forecasts[[2]], called, 
                                          rbind(model_data$camp_complete, model_data$camp_outstanding), 
                                          seasonal_wks= season_wk_param[i], seasonal_adj_type= "stl", 
                                          call_hist= called, windsor.q= c(.25, cap_param[i]))
    #-----------------------
    # wks models
    #-----------------------
    wks$p[[i]][[j]] <- adj_base_forecasts(base_forecasts[[2]], called, 
                                          rbind(model_data$camp_complete, model_data$camp_outstanding), 
                                          seasonal_wks= season_wk_param[i], seasonal_adj_type= "wk_avg", 
                                          call_hist= called, windsor.q= c(.25, cap_param[i]))
    #-----------------------
    # wk_stl models
    #-----------------------
    wk_stl$p[[i]][[j]] <- adj_base_forecasts(base_forecasts[[2]], called, 
                                             rbind(model_data$camp_complete, model_data$camp_outstanding), 
                                             seasonal_wks= season_wk_param[i], seasonal_adj_type= "wk_stl", 
                                             call_hist= called, windsor.q= c(.25, cap_param[i]))
  }
  # clean up
  rm(model_data, camp_complete_imp, top_ongoing, camp_comp_stats, base_forecasts); gc(verbose= FALSE)
}

rm(j, hist_yr, hist_mo, hist_names, i, len)

# Evaluate
#----------------------------------------------------------
ensemble$p <- lapply(ensemble$p, function(x) { # needed to clean feb error
  x[[2]][[1]] <- x[[2]][[1]][1:28,]
  return(x)
})

ets$p <- lapply(ets$p, function(x) { # needed to clean feb error
  x[[2]][[1]] <- x[[2]][[1]][1:28,]
  return(x)
})

stl$p <- lapply(stl$p, function(x) { # needed to clean feb error
  x[[2]][[1]] <- x[[2]][[1]][1:28,]
  return(x)
})

wks$p <- lapply(wks$p, function(x) { # needed to clean feb error
  x[[2]][[1]] <- x[[2]][[1]][1:28,]
  return(x)
})

wk_stl$p <- lapply(wk_stl$p, function(x) { # needed to clean feb error
  x[[2]][[1]] <- x[[2]][[1]][1:28,]
  return(x)
})

ensemble$a <- lapply(ensemble$p, function(x) {
  compute_acc(x, actual)
})

ets$a <- lapply(ets$p, function(x) {
  compute_acc(x, actual)
})

stl$a <- lapply(stl$p, function(x) {
  compute_acc(x, actual)
})

wks$a <- lapply(wks$p, function(x) {
  compute_acc(x, actual)
})

wk_stl$a <- lapply(wk_stl$p, function(x) {
  compute_acc(x, actual)
})

rm(rmse, mnAD, mxAD, wmnAD, compute_acc)

#----------------------------------------------------------
# 02. Examine test results
#----------------------------------------------------------
results <- extract_acc_2xwrap(list(ensemble$a, ets$a, stl$a, wks$a, wk_stl$a), 
                           name_vec= dimnames(ets$a[[1]])[[3]])

rm(extract_acc, extract_acc_wrap, extract_acc_2xwrap)
save.image("./testing_data_full3.Rdata")

apply(results[[1]], 2, function(x) {which(x == min(x))}) # any model, 5 wks, 65% 
apply(results[[2]], 2, function(x) {which(x == min(x))}) # any model, 5 wks, 50%
apply(results[[3]], 2, function(x) {which(x == min(x))}) # any model, 2 wks, 60% -- 55% best at 5 weeks
apply(results[[4]], 2, function(x) {which(x == min(x))}) # any model, 6 wks, 70% -- pretty minor differnce at 65%
apply(results[[5]], 2, function(x) {which(x == min(x))}) # any model, 5 wks, 55%
apply(results[[6]], 2, function(x) {which(x == min(x))}) # any model, 6 wks, 60%

mkt_direct <- data.frame(wks= season_wk_param, pct= cap_param, results[[1]])
db_ivr <- data.frame(wks= season_wk_param, pct= cap_param, results[[2]])
dandb_com <- data.frame(wks= season_wk_param, pct= cap_param, results[[3]])
organic <- data.frame(wks= season_wk_param, pct= cap_param, results[[4]])
paid_etc <- data.frame(wks= season_wk_param, pct= cap_param, results[[5]])
iupdate <- data.frame(wks= season_wk_param, pct= cap_param, results[[6]])

#----------------------------------------------------------
# 03a. Plotting -- interactions of tuning parameters
#----------------------------------------------------------
interaction.plot(x.factor= mkt_direct$wks, trace.factor= mkt_direct$pct, response= mkt_direct$avg_rmse)
interaction.plot(x.factor= mkt_direct$wks, trace.factor= mkt_direct$pct, response= mkt_direct$avg_mnAD)

interaction.plot(x.factor= db_ivr$wks, trace.factor= db_ivr$pct, response= db_ivr$avg_rmse)
interaction.plot(x.factor= db_ivr$wks, trace.factor= db_ivr$pct, response= db_ivr$avg_mnAD)

interaction.plot(x.factor= dandb_com$wks, trace.factor= dandb_com$pct, response= dandb_com$avg_rmse)
interaction.plot(x.factor= dandb_com$wks, trace.factor= dandb_com$pct, response= dandb_com$avg_mnAD)

interaction.plot(x.factor= organic$wks, trace.factor= organic$pct, response= organic$avg_rmse)
interaction.plot(x.factor= organic$wks, trace.factor= organic$pct, response= organic$avg_mnAD)

interaction.plot(x.factor= paid_etc$wks, trace.factor= paid_etc$pct, response= paid_etc$avg_rmse)
interaction.plot(x.factor= paid_etc$wks, trace.factor= paid_etc$pct, response= paid_etc$avg_mnAD)

interaction.plot(x.factor= iupdate$wks, trace.factor= iupdate$pct, response= iupdate$avg_rmse)
interaction.plot(x.factor= iupdate$wks, trace.factor= iupdate$pct, response= iupdate$avg_mnAD)

#----------------------------------------------------------
# 03b. Plotting -- actual vs projections
#----------------------------------------------------------

library(ggplot2)
proj <- rbindlist(lapply(ets$p[[17]], function(x) {x[[1]]$wday <- NULL; return(x[[1]])}), fill=TRUE)
act <- rbindlist(actual)
act$call_date <- as.Date(as.POSIXct(act$call_date, "PST")) 
act_proj <- merge(proj, act, by= "call_date")
# deltas
act_proj$mkt_d <- abs(act_proj$act_mkt - act_proj$mkt_direct)
act_proj$dbcom_d <- abs(act_proj$act_dandb - act_proj$dandb.com)
act_proj$org_d <- abs(act_proj$act_org - act_proj$organic)

head(act_proj[, .(call_date, act_mkt, mkt_direct, mkt_d, holiday)][order(-mkt_d)], 20)
head(act_proj[, .(call_date, act_org, organic, org_d, holiday)][order(-org_d)], 20)

# mkt direct
png("mkt_direct.png", height= 500, width= 600, units= "px")
ggplot(proj, aes(x= act$act_mkt, y= proj$mkt_direct)) + geom_point() + geom_smooth() +
  labs(x= "Actual Calls", y= "Projected Calls",
       title= "Marketing Direct") + ylim(c(0, 1500)) + xlim(c(0, 1500)) +
  theme(axis.title= element_text(face= "bold"), plot.title= element_text(face="bold", size= rel(1.5))) 
dev.off()

# dbivr
png("db_ivr.png", height= 500, width= 600, units= "px")
ggplot(proj, aes(x= act$act_dbivr, y= proj$db_ivr)) + geom_point() + geom_smooth() +
  labs(x= "Actual Calls", y= "Projected Calls",
       title= "DB IVR") + ylim(c(0, 150)) + xlim(c(0, 200)) +
  theme(axis.title= element_text(face= "bold"), plot.title= element_text(face="bold", size= rel(1.5))) 
dev.off()

# dandb.com
png("db_com.png", height= 500, width= 600, units= "px")
ggplot(proj, aes(x= act$act_dandb, y= proj$db_ivr)) + geom_point() + geom_smooth() +
  labs(x= "Actual Calls", y= "Projected Calls",
       title= "DandB.com") + ylim(c(0, 150)) + xlim(c(0, 350)) +
  theme(axis.title= element_text(face= "bold"), plot.title= element_text(face="bold", size= rel(1.5))) 
dev.off()

# organic
png("organic.png", height= 500, width= 600, units= "px")
ggplot(proj, aes(x= act$act_org, y= proj$organic)) + geom_point() + geom_smooth() +
  labs(x= "Actual Calls", y= "Projected Calls",
       title= "Organic") + ylim(c(0, 90)) + xlim(c(0, 250)) +
  theme(axis.title= element_text(face= "bold"), plot.title= element_text(face="bold", size= rel(1.5))) +
  geom_text(data= NULL, x= 200, y= 77, label= "July 13, 14, &15 2015", colour= "red") +
  geom_text(data= NULL, x= 150, y= 30, label= "July 11 & 12 2015", colour= "red")
dev.off()


# paid_etc
png("paid_etc.png", height= 500, width= 600, units= "px")
ggplot(proj, aes(x= act$act_paid, y= proj$organic)) + geom_point() + geom_smooth() +
  labs(x= "Actual Calls", y= "Projected Calls",
       title= "Paid Media etc") + ylim(c(0, 100)) + xlim(c(0, 500)) +
  theme(axis.title= element_text(face= "bold"), plot.title= element_text(face="bold", size= rel(1.5)),
        axis.text= element_text(colour= "red"))
dev.off()

# iupdate
png("iupdate.png", height= 500, width= 600, units= "px")
ggplot(proj, aes(x= act$act_iupdate, y= proj$organic)) + geom_point() + geom_smooth() +
  labs(x= "Actual Calls", y= "Projected Calls",
       title= "iUpdate") + ylim(c(0, 100)) + xlim(c(0, 225)) +
  theme(axis.title= element_text(face= "bold"), plot.title= element_text(face="bold", size= rel(1.5)),
        axis.text= element_text(colour= "red"))
dev.off()