
#---------------------------------------------------------------
# examine all calls data
#---------------------------------------------------------------
called <- pull_call_data(channel= "c2g", call_date= '1/01/2014') # use defaults

calls_by_day <- called[, .(calls= sum(call_count)) ,by= .(call_date)]
calls_by_day_cat <- called[, .(calls= sum(call_count)) ,by= .(category, call_date)]
calls <- merge(calls_by_day, dcast.data.table(calls_by_day_cat, formula= call_date ~ category, value.var= "calls"), 
               by= "call_date", all= TRUE)
setnames(calls, old= names(calls), new= c("call_date","calls", "dv_ivr", "dandb_com", 
                                          "mkt_direct", "organic", "paid_etc", "iupdate"))

calls$wday <- wday(calls$call_date)
calls_by_day_cat$wday <- wday(calls_by_day_cat$call_date)

camp_tot <- rbind(model_data$camp_complete, model_data$camp_outstanding)
mkt_calls_by_day <- camp_tot[, .(calls= sum(responders)), by= response_date]
mkt_calls_by_day$response_date <- NULL
calls_by_day_cat$call_date <- as.Date(calls_by_day_cat$call_date)

call_data <- merge(calls_by_day_cat[category == "Marketing Direct",], mkt_calls_by_day, by= "call_date", all.x=TRUE)
call_data <- call_data[call_date < as.Date("2015-08-01", format= "%Y-%m-%d"),]
rm(camp_tot, mkt_calls_by_day)
call_data$year <- year(call_data$call_date); call_data$mo <- month(call_data$call_date)
call_data$week <- week(call_data$call_date)
call_ratio <- call_data[, .(ratio= sum(calls.x, na.rm=T) / sum(calls.y, na.rm=T)), by= .(year, week)]
call_ratio$ma3 <- ma(call_ratio$ratio, order= 3, centre= FALSE)
call_ratio$ma5 <- ma(call_ratio$ratio, order= 5, centre= FALSE)
call_ratio$ma10 <- ma(call_ratio$ratio, order= 10, centre= FALSE)

# ratio of calls / camp_resp.responses is highly irregular
# MA-3 has slight irregularities; gets smoother as MA increases
# MA-5 probably preferred
ggplot(data.frame(date= seq(2014+3/52, 2015+ 32/52, 1/52), 
                  ratio= call_ratio$ratio[-c(1,2,85)]), aes(x= date, y= ratio)) + 
  geom_point() + geom_line() + stat_smooth() + scale_x_continuous(breaks= seq(2014+3/52, 2015+ 32/52, 1/52)) +
  theme(axis.text.x= element_text(angle= 90))

ggplot(data.frame(date= seq(2014+3/52, 2015+ 31/52, 1/52), 
                  ratio= call_ratio$ma3[-c(1,2, 84:85)]), aes(x= date, y= ratio)) + 
  geom_point() + geom_line() + stat_smooth() + scale_x_continuous(breaks= seq(2014, 2015+ 32/52, 1/52)) +
  theme(axis.text.x= element_text(angle= 90)) + xlim(c(2014+6/52, 2015+32/52)) + ylim(c(0,3))

ggplot(data.frame(date= seq(2014+3/52, 2015+ 30/52, 1/52), 
                  ratio= call_ratio$ma5[-c(1,2, 83:85)]), aes(x= date, y= ratio)) + 
  geom_point() + geom_line() + stat_smooth() + scale_x_continuous(breaks= seq(2014, 2015+ 32/52, 1/52)) +
  theme(axis.text.x= element_text(angle= 90)) + xlim(c(2014+6/52, 2015+32/52)) + ylim(c(0,3))

ggplot(data.frame(date= seq(2014, 2015+ 26/52, 1/52), 
                  ratio= call_ratio$ma10[-c(80:85)]), aes(x= date, y= ratio)) + 
  geom_point() + geom_line() + stat_smooth() + scale_x_continuous(breaks= seq(2014, 2015+ 26/52, 1/52)) +
  theme(axis.text.x= element_text(angle= 90)) + xlim(c(2014+6/52, 2015+32/52)) + ylim(c(0,3))

### ignore weekends and 2 categories we don't predict
ggplot(calls_by_day_cat[wday %in% c(2:6) & category == "Marketing Direct"],
       aes(x= call_date, y= calls, colour= category)) + 
  geom_point() + geom_line() +
  theme(legend.position= "bottom",axis.text.x= element_text(angle= 90)) +
  stat_smooth(method= "loess")

ggplot(calls_by_day_cat[wday %in% c(2:6) & 
                          !category %in% c("Customer Service", "Direct Extension", "Marketing Direct")],
       aes(x= call_date, y= calls, colour= category)) + 
  geom_point() + geom_line() +
  guides(col= guide_legend(nrow= 2)) + 
  theme(legend.position= "bottom", axis.text.x= element_text(angle= 90)) +
  stat_smooth(method= "loess")

### by day of week
ggplot(calls_by_day_cat[wday == 2 & category != "Customer Service" &
                          category != "Direct Extension",], aes(x= call_date, y= calls, colour= category)) + 
  geom_point() + geom_line() +
  guides(col= guide_legend(nrow= 3)) + 
  theme(legend.position= "bottom", axis.text.x= element_text(angle= 90)) +
  stat_smooth(method= "loess", formula= y ~ x + x^2)

# correlation
cor(as.data.frame(calls)[, -c(1,3,6,11,12)], use= "pairwise.complete.obs")
#                                 calls   D&B IVR DandB.com Marketing Direct   Organic Paid Media, Web & Affiliate   iUpdate
# calls                       1.0000000 0.9676952 0.9383269        0.9104512 0.7568049                   0.9385318 0.9677271
# D&B IVR                     0.9676952 1.0000000 0.9386285        0.8514770 0.7077951                   0.9126484 0.9455434
# DandB.com                   0.9383269 0.9386285 1.0000000        0.8242461 0.7142065                   0.8518264 0.9068049
# Marketing Direct            0.9104512 0.8514770 0.8242461        1.0000000 0.7363629                   0.8371063 0.8615682
# Organic                     0.7568049 0.7077951 0.7142065        0.7363629 1.0000000                   0.7213347 0.7229116
# Paid Media, Web & Affiliate 0.9385318 0.9126484 0.8518264        0.8371063 0.7213347                   1.0000000 0.9158076
# iUpdate                     0.9677271 0.9455434 0.9068049        0.8615682 0.7229116                   0.9158076 1.0000000


monthly_adjust <- monthly_adjustment(called, control_year_min= 2013, 
                                     control_year_max= lubridate::year(Sys.Date()) - 1)


#---------------------------------------------------------------
# library(forecast) - calls data
#---------------------------------------------------------------
library(forecast)
call_data$calls.y <- ifelse(is.na(call_data$calls.y), 0, call_data$calls.y)
t1 <- ets(ts(call_data$calls.y[call_data$call_date >= as.Date("2015-01-01", 
          format= "%Y-%m-%d")], frequency= 7), model= "ZZA")
t2 <- stl(ts(call_data$calls.x, frequency=7), s.window= 7, robust= TRUE)
t3 <- ets(ts(call_data$calls.y / call_data$calls.x, frequency = 7), model= "ZZA")



