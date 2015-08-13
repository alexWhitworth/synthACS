
# initial data pulls
#---------------------------------------------------------------
called <- pull_call_data() # use defaults

ch <- odbcConnect("c2g")
camp_resp <- sqlQuery(ch, "SELECT * FROM [c2g].[dbo].[c2g_campaign_response]
                      where year(date) >= 2014 and days_to_response >= 0 and left(cell_code, 1)='D'", 
                      stringsAsFactors= FALSE) # includes daily response rate
camp_track <- sqlQuery(ch, "SELECT * FROM [c2g].[dbo].[c2g_campaign_tracker]
                       where year(date) >= 2014", stringsAsFactors= FALSE)
close(ch); rm(ch)

model_data <- get_model_data(called)

camp_complete_imp <- impute_zero_resp_all(dat= model_data$camp_complete, days_tracking= 90, comp_camp= TRUE)
top_incomplete <- camp_out_calc_adj(model_data$camp_outstanding, channel= "c2g")

# -----------------------------------------------
setwd("G:/Whitworth_Alex/2015-08 Call_Projection/")
source("./data_pull.R")
source("./get_model_data.R")
source("./new_campaign_proj.R")
source("./impute_miss_rr.R")
source("./holiday_scrape.R")
source("./camp_out_calc_adj.R")
source("./create_baseline_forecasts.R")

library(RODBC)
library(dplyr)
library(data.table)
library(ggplot2)

load("./2015-08 toy_data.Rdata")
# save.image("./2015-08 toy_data.Rdata")
#---------------------------------------------------------------


# campaigns do not have responses over all 90 days of tracking
camp_days <- data.table(model_data$camp_complete)[, .N, by= .(cell_code)]
summary(camp_days)

#---------------------------------------------------------------
# examine response rate (daily and cumulative) before imputing missing response days
sum1 <- model_data$camp_complete %>% group_by(campaign_type, class_of_mail, days_to_response) %>% 
  summarize(n= length(cell_code), resp_rate= mean(pct_of_responders, na.rm=TRUE), 
            daily_resp= mean(pct_of_responders, na.rm=TRUE) * sum(total_responders, na.rm=TRUE)) %>%
  group_by(campaign_type, class_of_mail) %>% mutate(cum_rr= cumsum(resp_rate))


ggplot(sum1, aes(x= days_to_response, y= resp_rate, colour= campaign_type)) + geom_line() + 
  facet_grid(campaign_type~ class_of_mail) + theme(legend.position= "bottom")

ggplot(sum1, aes(x= days_to_response, y= cum_rr, colour= campaign_type)) + geom_line() + 
  facet_grid(campaign_type~ class_of_mail) + theme(legend.position= "bottom")


#---------------------------------------------------------------
# function tests
# test_dat <- model_data$camp_complete[model_data$camp_complete$cell_code == "D-T-3-15-05-003", ]
# test_dat2 <- model_data$camp_complete
# test_dat2 <- impute_zero_resp_all(dat= test_dat2)

camp_complete_imp <- impute_zero_resp_all(dat= model_data$camp_complete, days_tracking= 90, comp_camp= TRUE)

#---------------------------------------------------------------
# examine response rate (daily and cumulative) after imputing missing response days
#---------------------------------------------------------------
camp_comp_stats <- camp_complete_imp %>% group_by(campaign_type, class_of_mail, days_to_response) %>% 
  summarize(n= length(cell_code), 
            mean_daily_resp_rate= mean(responders / unique_leads, na.rm=TRUE),
            mean_pct_resp= mean(pct_of_responders, na.rm=TRUE),
            n_daily_resp= mean(pct_of_responders, na.rm=TRUE) * sum(total_responders, na.rm=TRUE)) %>%
  group_by(campaign_type, class_of_mail) %>% mutate(mean_cum_rr= cumsum(mean_pct_resp))

# check syntax -- seems right but doesn't work
# sum_comp_resp <- data.table(camp_complete_imp)[, .(n= .N, 
#       resp_rate= mean(pct_of_responders, na.rm=TRUE),
#       daily_resp= mean(pct_of_responders, na.rm=TRUE) * sum(total_responders, na.rm=TRUE)),
#      by= .(campaign_type, class_of_mail, days_to_response)][, 
#       cum_rr := cumsum(resp_rate),
#       by= .(campaign_type, class_of_mail, days_to_response)]

# graphically
ggplot(camp_comp_stats, aes(x= days_to_response, y= mean_daily_resp_rate, colour= campaign_type)) + geom_line() + 
  facet_grid(campaign_type~ class_of_mail) + theme(legend.position= "bottom") +
  scale_x_continuous(breaks= seq(0,90,10))

ggplot(camp_comp_stats, aes(x= days_to_response, y= mean_pct_resp, colour= campaign_type)) + geom_line() + 
  facet_grid(campaign_type~ class_of_mail) + theme(legend.position= "bottom") +
  scale_x_continuous(breaks= seq(0,90,10))

ggplot(camp_comp_stats, aes(x= days_to_response, y= mean_cum_rr, colour= campaign_type)) + geom_line() + 
  facet_grid(campaign_type~ class_of_mail) + theme(legend.position= "bottom") +
  scale_x_continuous(breaks= seq(0,90,10))

# export
write.csv(camp_comp_stats, file= "./responses1.csv", row.names= FALSE)

#------------------------------------------------------------
# examine call volume by day of week and year
#---------------------------------------------------------------
calls <- within(data.table(model_data$called_by_day), {
  wday= lubridate::wday(call_date)
})


tapply(calls$Called, list(interaction(year(calls$call_date), month(calls$call_date)), calls$wday), mean)
tapply(calls$Called, list(interaction(year(calls$call_date), month(calls$call_date)), calls$wday), sd)

tapply(calls$Called, list(year(calls$call_date), calls$wday), mean)
tapply(calls$Called, list(year(calls$call_date), calls$wday), sd)

ggplot(calls, aes(x= call_date, y= Called)) + geom_point() + geom_line() +
  theme(legend.position= "bottom", axis.text.x= element_text(angle= 90))

# sat / sun roughly equivalent; M/T roughly equivalent; W-F roughly equivalent
ggplot(calls, aes(x= call_date, y= Called)) + geom_point() + geom_line() +
  facet_wrap(~ wday) + theme(legend.position= "bottom",
                             axis.text.x= element_text(angle= 90))

ggplot(calls[calls$wday %in% 2:6,], aes(x= call_date, y= Called)) + geom_point() + geom_line() +
  facet_wrap(~ wday) + theme(legend.position= "bottom",
                             axis.text.x= element_text(angle= 90))

ggplot(calls[calls$wday %in% c(1,7),], aes(x= call_date, y= Called)) + geom_point() + geom_line() +
  facet_wrap(~ wday) + theme(legend.position= "bottom",
                             axis.text.x= element_text(angle= 90))

calls[wday == 1][Called > 400,] # mothers day & some random Sunday
calls[wday == 7][Called > 400,] # some random saturday

calls[Called > 1500,] # all on M or TU; random days
calls[as.Date(call_date) %in% c(as.Date(calls[Called > 1500, call_date]), 
                                as.Date(as.POSIXct(calls[Called > 1500, call_date])) + 1)]

# generally speaking, a day with very high call volume is followed by a lower volume day

calls[as.Date(call_date) %in% c(as.Date(calls[Called > 1200, call_date]), 
                                as.Date(as.POSIXct(calls[Called > 1200, call_date])) + 1)]

#------------------------------------------------------------
# examine seasonal patterns in response rate
#---------------------------------------------------------------
library(RODBC)
ch <- odbcConnect("c2g")
camp_track <- sqlQuery(ch, "SELECT * FROM [c2g].[dbo].[c2g_campaign_tracker]
                       where year(date) >= 2014", stringsAsFactors= FALSE)
close(ch); rm(ch)

c_track_inc <- camp_track[camp_track$days_of_tracking < 90,]; rm(camp_track)
c_track <- camp_track[camp_track$days_of_tracking == 90,]; rm(camp_track)
c_track$campaign_type <- ifelse(c_track$campaign_type == "AdHoc", "ad hoc", 
                         ifelse(c_track$campaign_type == "BAU", "bau", c_track$campaign_type))
c_track <- data.table(c_track[c_track$campaign_type %in% c("ad hoc", "bau", "latest inquiry"),],
                      key= "cell_code")

# no real seasonality of RR by date
ggplot(c_track[!is.na(c_track$class_of_mail),], 
       aes(x= date, y= response_rate_duns, colour= campaign_type)) + geom_line() + geom_point() +
  facet_grid(campaign_type ~ class_of_mail) + 
  theme(legend.position= "bottom",
        axis.text.x= element_text(angle= 90))


