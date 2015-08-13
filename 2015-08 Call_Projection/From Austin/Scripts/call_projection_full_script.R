#Final call projection script. Automatically called weekly (Monday morning) by Windows Task Scheduler
#Bring in 'RODBC'
library(RODBC)

#Open channel to server
ch <- odbcConnect("phoenix_new")

#First, call function which pulls all the data and runs the model to produce forecasts
load("/Users/ashelton/Documents/The Call Projection Model/R_workspace_Call_Projection_model_The_NEW_call_projection_model_4_30'")

#Query ALL calls for holiday adjustments
calls_holiday <- call_query_all()

#Define all_calls
all_calls <- calls_holiday$called

#Function to merge all calls to mktg direct only, and regress calls types on mktg direct 
all_call_regs <- all_call_estimates(all_calls)

#Create monthly adjustment
monthly_adjust <- monthly_adjustment(all_calls, 2013, year(Sys.Date()) - 1)

#Full weekly call projection
w <- weekly_call_projection_full()

#Final adjustments off of lagged error terms
x <- adjust_lagged_error(w$y$full_estimates, 4, .05)

y_0 <- write_calls_to_server(x$full_estimates_1)

z_0 <- write_calls_to_server_2(x$full_estimates_1)

###Duplicate adjustment

#Save workspace
##save.image("/Users/ashelton/Documents/The Call Projection Model/R_workspace_Call_Projection_model_The_NEW_call_projection_model_1'")

#Write to csv
write.csv(x$full_estimates_1, paste("/Users/ashelton/Documents/The Call Projection Model/Updated_forecasts_", 
                                    month(Sys.Date()), "_", day(Sys.Date()), "_", year(Sys.Date()), ".csv", sep=""))

#Finally create plots of called to projected for all_calls and mktg_direct
called_projected <- as.data.frame(cbind(z_1$full_call_projections$all_calls, z_1$full_call_projections$total_projection))
names(called_projected) <- c("Called", "projected")

attach(called_projected)
linreg_called_projected <- lm(Called ~ projected)
detach(called_projected)

#Regress calls on Projected
library(ggplot2)
calls_to_projected <- ggplot(data = called_projected, aes(x=projected, y=Called, group = 1)) +
  geom_point() + xlab("projected") + ylab("Called") + 
  ggtitle("Called vs. Projected Historical") + 
  theme(plot.title = element_text(lineheight=1.5, face="bold")) + 
  geom_abline(predict(linreg_called_projected, called_projected))

#Mktg direct only
called_projected_mktg_direct <- as.data.frame(
  cbind(z_1$full_call_projections$mktg_direct, z_1$full_call_projections$mktg_direct_projection))
names(called_projected_mktg_direct) <- c("Called", "projected")

attach(called_projected_mktg_direct)
linreg_called_projected <- lm(Called ~ projected)
detach(called_projected_mktg_direct)

#Regress calls on Projected
calls_to_projected_mktg_direct <-
  ggplot(data = called_projected_mktg_direct, aes(x=projected, y=Called, group = 1)) + 
  geom_point() + xlab("projected") + ylab("Called") + 
  ggtitle("Called vs. Projected Historical") + 
  theme(plot.title = element_text(lineheight=1.5, face="bold")) + geom_abline()

##sd and mean all_calls vs projected
Bias <- mean((called_projected$projected - called_projected$Called), na.rm="TRUE")
sd_Error <- sd((called_projected$projected - called_projected$Called), na.rm="TRUE")

#Fit 
attach(called_projected)
linreg_called_projected <- lm(Called ~ projected)
detach(called_projected)

r_squared <- summary(linreg_called_projected)$r.squared

##sd and mean all_calls vs projected
Bias_mktg <- mean((called_projected_mktg_direct$projected - called_projected_mktg_direct$Called), na.rm="TRUE")
sd_Error_mktg <- sd((called_projected_mktg_direct$projected - called_projected_mktg_direct$Called), na.rm="TRUE")

#Fit 
attach(called_projected_mktg_direct)
linreg_called_projected <- lm(Called ~ projected)
detach(called_projected_mktg_direct)

r_squared_mktg <- summary(linreg_called_projected)$r.squared

save.image("/Users/ashelton/Documents/The Call Projection Model/last_model_run")

#Backup projections
full_data <- z_0$full_call_projections

#Make sure day of week is good
full_data$day_of_week <- weekdays(as.Date(full_data$call_date))

Sundays <- full_data[full_data$day_of_week == "Sunday", ]
Mondays <- full_data[full_data$day_of_week == "Monday", ]
Tuesdays <- full_data[full_data$day_of_week == "Tuesday", ]
Wednesdays <- full_data[full_data$day_of_week == "Wednesday", ]
Thursdays <- full_data[full_data$day_of_week == "Thursday", ]
Fridays <- full_data[full_data$day_of_week == "Friday", ]
Saturdays <- full_data[full_data$day_of_week == "Saturday", ]

Sundays_mktg_direct_projection_backup <- 
  ifelse(is.na(Lag(Sundays$mktg_direct, 1)) == FALSE, Lag(Sundays$mktg_direct, 1), Lag(Sundays$mktg_direct_projection, 1)) +
  ifelse(is.na(Lag(Sundays$mktg_direct, 2)) == FALSE, Lag(Sundays$mktg_direct, 2), Lag(Sundays$mktg_direct_projection, 2)) + 
  ifelse(is.na(Lag(Sundays$mktg_direct, 3)) == FALSE, Lag(Sundays$mktg_direct, 3), Lag(Sundays$mktg_direct_projection, 3)) +
  ifelse(is.na(Lag(Sundays$mktg_direct, 4)) == FALSE, Lag(Sundays$mktg_direct, 4), Lag(Sundays$mktg_direct_projection, 4))
Sundays$mktg_direct_projection <- Sundays_mktg_direct_projection_backup 

Sundays_customer_service_projection_backup <- 
  ifelse(is.na(Lag(Sundays$customer_service, 1)) == FALSE, Lag(Sundays$customer_service, 1), Lag(Sundays$customer_service_projection, 1)) +
  ifelse(is.na(Lag(Sundays$customer_service, 2)) == FALSE, Lag(Sundays$customer_service, 2), Lag(Sundays$customer_service_projection, 2)) + 
  ifelse(is.na(Lag(Sundays$customer_service, 3)) == FALSE, Lag(Sundays$customer_service, 3), Lag(Sundays$customer_service_projection, 3)) +
  ifelse(is.na(Lag(Sundays$customer_service, 4)) == FALSE, Lag(Sundays$customer_service, 4), Lag(Sundays$customer_service_projection, 4))
Sundays$customer_service_projection <- Sundays_customer_service_projection_backup 

Sundays_DB_IVR_projection_backup <- 
  ifelse(is.na(Lag(Sundays$DB_IVR, 1)) == FALSE, Lag(Sundays$DB_IVR, 1), Lag(Sundays$DB_IVR_projection, 1)) +
  ifelse(is.na(Lag(Sundays$DB_IVR, 2)) == FALSE, Lag(Sundays$DB_IVR, 2), Lag(Sundays$DB_IVR_projection, 2)) + 
  ifelse(is.na(Lag(Sundays$DB_IVR, 3)) == FALSE, Lag(Sundays$DB_IVR, 3), Lag(Sundays$DB_IVR_projection, 3)) +
  ifelse(is.na(Lag(Sundays$DB_IVR, 4)) == FALSE, Lag(Sundays$DB_IVR, 4), Lag(Sundays$DB_IVR_projection, 4))
Sundays$DB_IVR_projection <- Sundays_DB_IVR_projection_backup 

Sundays_DB_com_projection_backup <- 
  ifelse(is.na(Lag(Sundays$DB_com, 1)) == FALSE, Lag(Sundays$DB_com, 1), Lag(Sundays$DB_com_projection, 1)) +
  ifelse(is.na(Lag(Sundays$DB_com, 2)) == FALSE, Lag(Sundays$DB_com, 2), Lag(Sundays$DB_com_projection, 2)) + 
  ifelse(is.na(Lag(Sundays$DB_com, 3)) == FALSE, Lag(Sundays$DB_com, 3), Lag(Sundays$DB_com_projection, 3)) +
  ifelse(is.na(Lag(Sundays$DB_com, 4)) == FALSE, Lag(Sundays$DB_com, 4), Lag(Sundays$DB_com_projection, 4))
Sundays$DB_com_projection <- Sundays_DB_com_projection_backup 

Sundays_Direct_Extension_projection_backup <- 
  ifelse(is.na(Lag(Sundays$Direct_Extension, 1)) == FALSE, Lag(Sundays$Direct_Extension, 1), 
         Lag(Sundays$Direct_Extension_projection, 1)) +
  ifelse(is.na(Lag(Sundays$Direct_Extension, 2)) == FALSE, Lag(Sundays$Direct_Extension, 2), 
         Lag(Sundays$Direct_Extension_projection, 2)) + 
  ifelse(is.na(Lag(Sundays$Direct_Extension, 3)) == FALSE, Lag(Sundays$Direct_Extension, 3), 
         Lag(Sundays$Direct_Extension_projection, 3)) +
  ifelse(is.na(Lag(Sundays$Direct_Extension, 4)) == FALSE, Lag(Sundays$Direct_Extension, 4), 
         Lag(Sundays$Direct_Extension_projection, 4))
Sundays$Direct_Extension_projection <- Sundays_Direct_Extension_projection_backup 

Sundays_iUpdate_projection_backup <- 
  ifelse(is.na(Lag(Sundays$iUpdate, 1)) == FALSE, Lag(Sundays$iUpdate, 1), Lag(Sundays$iUpdate_projection, 1)) +
  ifelse(is.na(Lag(Sundays$iUpdate, 2)) == FALSE, Lag(Sundays$iUpdate, 2), Lag(Sundays$iUpdate_projection, 2)) + 
  ifelse(is.na(Lag(Sundays$iUpdate, 3)) == FALSE, Lag(Sundays$iUpdate, 3), Lag(Sundays$iUpdate_projection, 3)) +
  ifelse(is.na(Lag(Sundays$iUpdate, 4)) == FALSE, Lag(Sundays$iUpdate, 4), Lag(Sundays$iUpdate_projection, 4))
Sundays$iUpdate_projection <- Sundays_iUpdate_projection_backup 

Sundays_Organic_projection_backup <- 
  ifelse(is.na(Lag(Sundays$Organic, 1)) == FALSE, Lag(Sundays$Organic, 1), Lag(Sundays$Organic_projection, 1)) +
  ifelse(is.na(Lag(Sundays$Organic, 2)) == FALSE, Lag(Sundays$Organic, 2), Lag(Sundays$Organic_projection, 2)) + 
  ifelse(is.na(Lag(Sundays$Organic, 3)) == FALSE, Lag(Sundays$Organic, 3), Lag(Sundays$Organic_projection, 3)) +
  ifelse(is.na(Lag(Sundays$Organic, 4)) == FALSE, Lag(Sundays$Organic, 4), Lag(Sundays$Organic_projection, 4))
Sundays$Organic_projection <- Sundays_Organic_projection_backup 

Sundays_Paid_web_projection_backup <- 
  ifelse(is.na(Lag(Sundays$Paid_web, 1)) == FALSE, Lag(Sundays$Paid_web, 1), Lag(Sundays$Paid_web_projection, 1)) +
  ifelse(is.na(Lag(Sundays$Paid_web, 2)) == FALSE, Lag(Sundays$Paid_web, 2), Lag(Sundays$Paid_web_projection, 2)) + 
  ifelse(is.na(Lag(Sundays$Paid_web, 3)) == FALSE, Lag(Sundays$Paid_web, 3), Lag(Sundays$Paid_web_projection, 3)) +
  ifelse(is.na(Lag(Sundays$Paid_web, 4)) == FALSE, Lag(Sundays$Paid_web, 4), Lag(Sundays$Paid_web_projection, 4))
Sundays$Paid_web_projection <- Sundays_Paid_web_projection_backup 

Sundays_other_projection_backup <- 
  ifelse(is.na(Lag(Sundays$other, 1)) == FALSE, Lag(Sundays$other, 1), Lag(Sundays$other_projection, 1)) +
  ifelse(is.na(Lag(Sundays$other, 2)) == FALSE, Lag(Sundays$other, 2), Lag(Sundays$other_projection, 2)) + 
  ifelse(is.na(Lag(Sundays$other, 3)) == FALSE, Lag(Sundays$other, 3), Lag(Sundays$other_projection, 3)) +
  ifelse(is.na(Lag(Sundays$other, 4)) == FALSE, Lag(Sundays$other, 4), Lag(Sundays$other_projection, 4))
Sundays$other_projection <- Sundays_other_projection_backup 

#Mondays
Mondays_mktg_direct_projection_backup <- 
  ifelse(is.na(Lag(Mondays$mktg_direct, 1)) == FALSE, Lag(Mondays$mktg_direct, 1), Lag(Mondays$mktg_direct_projection, 1)) +
  ifelse(is.na(Lag(Mondays$mktg_direct, 2)) == FALSE, Lag(Mondays$mktg_direct, 2), Lag(Mondays$mktg_direct_projection, 2)) + 
  ifelse(is.na(Lag(Mondays$mktg_direct, 3)) == FALSE, Lag(Mondays$mktg_direct, 3), Lag(Mondays$mktg_direct_projection, 3)) +
  ifelse(is.na(Lag(Mondays$mktg_direct, 4)) == FALSE, Lag(Mondays$mktg_direct, 4), Lag(Mondays$mktg_direct_projection, 4))
Mondays$mktg_direct_projection <- Mondays_mktg_direct_projection_backup 

Mondays_customer_service_projection_backup <- 
  ifelse(is.na(Lag(Mondays$customer_service, 1)) == FALSE, Lag(Mondays$customer_service, 1), Lag(Mondays$customer_service_projection, 1)) +
  ifelse(is.na(Lag(Mondays$customer_service, 2)) == FALSE, Lag(Mondays$customer_service, 2), Lag(Mondays$customer_service_projection, 2)) + 
  ifelse(is.na(Lag(Mondays$customer_service, 3)) == FALSE, Lag(Mondays$customer_service, 3), Lag(Mondays$customer_service_projection, 3)) +
  ifelse(is.na(Lag(Mondays$customer_service, 4)) == FALSE, Lag(Mondays$customer_service, 4), Lag(Mondays$customer_service_projection, 4))
Mondays$customer_service_projection <- Mondays_customer_service_projection_backup 

Mondays_DB_IVR_projection_backup <- 
  ifelse(is.na(Lag(Mondays$DB_IVR, 1)) == FALSE, Lag(Mondays$DB_IVR, 1), Lag(Mondays$DB_IVR_projection, 1)) +
  ifelse(is.na(Lag(Mondays$DB_IVR, 2)) == FALSE, Lag(Mondays$DB_IVR, 2), Lag(Mondays$DB_IVR_projection, 2)) + 
  ifelse(is.na(Lag(Mondays$DB_IVR, 3)) == FALSE, Lag(Mondays$DB_IVR, 3), Lag(Mondays$DB_IVR_projection, 3)) +
  ifelse(is.na(Lag(Mondays$DB_IVR, 4)) == FALSE, Lag(Mondays$DB_IVR, 4), Lag(Mondays$DB_IVR_projection, 4))
Mondays$DB_IVR_projection <- Mondays_DB_IVR_projection_backup 

Mondays_DB_com_projection_backup <- 
  ifelse(is.na(Lag(Mondays$DB_com, 1)) == FALSE, Lag(Mondays$DB_com, 1), Lag(Mondays$DB_com_projection, 1)) +
  ifelse(is.na(Lag(Mondays$DB_com, 2)) == FALSE, Lag(Mondays$DB_com, 2), Lag(Mondays$DB_com_projection, 2)) + 
  ifelse(is.na(Lag(Mondays$DB_com, 3)) == FALSE, Lag(Mondays$DB_com, 3), Lag(Mondays$DB_com_projection, 3)) +
  ifelse(is.na(Lag(Mondays$DB_com, 4)) == FALSE, Lag(Mondays$DB_com, 4), Lag(Mondays$DB_com_projection, 4))
Mondays$DB_com_projection <- Mondays_DB_com_projection_backup 

Mondays_Direct_Extension_projection_backup <- 
  ifelse(is.na(Lag(Mondays$Direct_Extension, 1)) == FALSE, Lag(Mondays$Direct_Extension, 1), Lag(Mondays$Direct_Extension_projection, 1)) +
  ifelse(is.na(Lag(Mondays$Direct_Extension, 2)) == FALSE, Lag(Mondays$Direct_Extension, 2), Lag(Mondays$Direct_Extension_projection, 2)) + 
  ifelse(is.na(Lag(Mondays$Direct_Extension, 3)) == FALSE, Lag(Mondays$Direct_Extension, 3), Lag(Mondays$Direct_Extension_projection, 3)) +
  ifelse(is.na(Lag(Mondays$Direct_Extension, 4)) == FALSE, Lag(Mondays$Direct_Extension, 4), Lag(Mondays$Direct_Extension_projection, 4))
Mondays$Direct_Extension_projection <- Mondays_Direct_Extension_projection_backup 

Mondays_iUpdate_projection_backup <- 
  ifelse(is.na(Lag(Mondays$iUpdate, 1)) == FALSE, Lag(Mondays$iUpdate, 1), Lag(Mondays$iUpdate_projection, 1)) +
  ifelse(is.na(Lag(Mondays$iUpdate, 2)) == FALSE, Lag(Mondays$iUpdate, 2), Lag(Mondays$iUpdate_projection, 2)) + 
  ifelse(is.na(Lag(Mondays$iUpdate, 3)) == FALSE, Lag(Mondays$iUpdate, 3), Lag(Mondays$iUpdate_projection, 3)) +
  ifelse(is.na(Lag(Mondays$iUpdate, 4)) == FALSE, Lag(Mondays$iUpdate, 4), Lag(Mondays$iUpdate_projection, 4))
Mondays$iUpdate_projection <- Mondays_iUpdate_projection_backup 

Mondays_Organic_projection_backup <- 
  ifelse(is.na(Lag(Mondays$Organic, 1)) == FALSE, Lag(Mondays$Organic, 1), Lag(Mondays$Organic_projection, 1)) +
  ifelse(is.na(Lag(Mondays$Organic, 2)) == FALSE, Lag(Mondays$Organic, 2), Lag(Mondays$Organic_projection, 2)) + 
  ifelse(is.na(Lag(Mondays$Organic, 3)) == FALSE, Lag(Mondays$Organic, 3), Lag(Mondays$Organic_projection, 3)) +
  ifelse(is.na(Lag(Mondays$Organic, 4)) == FALSE, Lag(Mondays$Organic, 4), Lag(Mondays$Organic_projection, 4))
Mondays$Organic_projection <- Mondays_Organic_projection_backup 

Mondays_Paid_web_projection_backup <- 
  ifelse(is.na(Lag(Mondays$Paid_web, 1)) == FALSE, Lag(Mondays$Paid_web, 1), Lag(Mondays$Paid_web_projection, 1)) +
  ifelse(is.na(Lag(Mondays$Paid_web, 2)) == FALSE, Lag(Mondays$Paid_web, 2), Lag(Mondays$Paid_web_projection, 2)) + 
  ifelse(is.na(Lag(Mondays$Paid_web, 3)) == FALSE, Lag(Mondays$Paid_web, 3), Lag(Mondays$Paid_web_projection, 3)) +
  ifelse(is.na(Lag(Mondays$Paid_web, 4)) == FALSE, Lag(Mondays$Paid_web, 4), Lag(Mondays$Paid_web_projection, 4))
Mondays$Paid_web_projection <- Mondays_Paid_web_projection_backup 

Mondays_other_projection_backup <- 
  ifelse(is.na(Lag(Mondays$other, 1)) == FALSE, Lag(Mondays$other, 1), Lag(Mondays$other_projection, 1)) +
  ifelse(is.na(Lag(Mondays$other, 2)) == FALSE, Lag(Mondays$other, 2), Lag(Mondays$other_projection, 2)) + 
  ifelse(is.na(Lag(Mondays$other, 3)) == FALSE, Lag(Mondays$other, 3), Lag(Mondays$other_projection, 3)) +
  ifelse(is.na(Lag(Mondays$other, 4)) == FALSE, Lag(Mondays$other, 4), Lag(Mondays$other_projection, 4))
Mondays$other_projection <- Mondays_other_projection_backup 

#Tuesdays
Tuesdays_mktg_direct_projection_backup <- 
  ifelse(is.na(Lag(Tuesdays$mktg_direct, 1)) == FALSE, Lag(Tuesdays$mktg_direct, 1), Lag(Tuesdays$mktg_direct_projection, 1)) +
  ifelse(is.na(Lag(Tuesdays$mktg_direct, 2)) == FALSE, Lag(Tuesdays$mktg_direct, 2), Lag(Tuesdays$mktg_direct_projection, 2)) + 
  ifelse(is.na(Lag(Tuesdays$mktg_direct, 3)) == FALSE, Lag(Tuesdays$mktg_direct, 3), Lag(Tuesdays$mktg_direct_projection, 3)) +
  ifelse(is.na(Lag(Tuesdays$mktg_direct, 4)) == FALSE, Lag(Tuesdays$mktg_direct, 4), Lag(Tuesdays$mktg_direct_projection, 4))
Tuesdays$mktg_direct_projection <- Tuesdays_mktg_direct_projection_backup 

Tuesdays_customer_service_projection_backup <- 
  ifelse(is.na(Lag(Tuesdays$customer_service, 1)) == FALSE, Lag(Tuesdays$customer_service, 1), Lag(Tuesdays$customer_service_projection, 1)) +
  ifelse(is.na(Lag(Tuesdays$customer_service, 2)) == FALSE, Lag(Tuesdays$customer_service, 2), Lag(Tuesdays$customer_service_projection, 2)) + 
  ifelse(is.na(Lag(Tuesdays$customer_service, 3)) == FALSE, Lag(Tuesdays$customer_service, 3), Lag(Tuesdays$customer_service_projection, 3)) +
  ifelse(is.na(Lag(Tuesdays$customer_service, 4)) == FALSE, Lag(Tuesdays$customer_service, 4), Lag(Tuesdays$customer_service_projection, 4))
Tuesdays$customer_service_projection <- Tuesdays_customer_service_projection_backup 

Tuesdays_DB_IVR_projection_backup <- 
  ifelse(is.na(Lag(Tuesdays$DB_IVR, 1)) == FALSE, Lag(Tuesdays$DB_IVR, 1), Lag(Tuesdays$DB_IVR_projection, 1)) +
  ifelse(is.na(Lag(Tuesdays$DB_IVR, 2)) == FALSE, Lag(Tuesdays$DB_IVR, 2), Lag(Tuesdays$DB_IVR_projection, 2)) + 
  ifelse(is.na(Lag(Tuesdays$DB_IVR, 3)) == FALSE, Lag(Tuesdays$DB_IVR, 3), Lag(Tuesdays$DB_IVR_projection, 3)) +
  ifelse(is.na(Lag(Tuesdays$DB_IVR, 4)) == FALSE, Lag(Tuesdays$DB_IVR, 4), Lag(Tuesdays$DB_IVR_projection, 4))
Tuesdays$DB_IVR_projection <- Tuesdays_DB_IVR_projection_backup 

Tuesdays_DB_com_projection_backup <- 
  ifelse(is.na(Lag(Tuesdays$DB_com, 1)) == FALSE, Lag(Tuesdays$DB_com, 1), Lag(Tuesdays$DB_com_projection, 1)) +
  ifelse(is.na(Lag(Tuesdays$DB_com, 2)) == FALSE, Lag(Tuesdays$DB_com, 2), Lag(Tuesdays$DB_com_projection, 2)) + 
  ifelse(is.na(Lag(Tuesdays$DB_com, 3)) == FALSE, Lag(Tuesdays$DB_com, 3), Lag(Tuesdays$DB_com_projection, 3)) +
  ifelse(is.na(Lag(Tuesdays$DB_com, 4)) == FALSE, Lag(Tuesdays$DB_com, 4), Lag(Tuesdays$DB_com_projection, 4))
Tuesdays$DB_com_projection <- Tuesdays_DB_com_projection_backup 

Tuesdays_Direct_Extension_projection_backup <-
  ifelse(is.na(Lag(Tuesdays$Direct_Extension, 1)) == FALSE, Lag(Tuesdays$Direct_Extension, 1), Lag(Tuesdays$Direct_Extension_projection, 1)) +
  ifelse(is.na(Lag(Tuesdays$Direct_Extension, 2)) == FALSE, Lag(Tuesdays$Direct_Extension, 2), Lag(Tuesdays$Direct_Extension_projection, 2)) + 
  ifelse(is.na(Lag(Tuesdays$Direct_Extension, 3)) == FALSE, Lag(Tuesdays$Direct_Extension, 3), Lag(Tuesdays$Direct_Extension_projection, 3)) +
  ifelse(is.na(Lag(Tuesdays$Direct_Extension, 4)) == FALSE, Lag(Tuesdays$Direct_Extension, 4), Lag(Tuesdays$Direct_Extension_projection, 4))
Tuesdays$Direct_Extension_projection <- Tuesdays_Direct_Extension_projection_backup 

Tuesdays_iUpdate_projection_backup <- 
  ifelse(is.na(Lag(Tuesdays$iUpdate, 1)) == FALSE, Lag(Tuesdays$iUpdate, 1), Lag(Tuesdays$iUpdate_projection, 1)) +
  ifelse(is.na(Lag(Tuesdays$iUpdate, 2)) == FALSE, Lag(Tuesdays$iUpdate, 2), Lag(Tuesdays$iUpdate_projection, 2)) + 
  ifelse(is.na(Lag(Tuesdays$iUpdate, 3)) == FALSE, Lag(Tuesdays$iUpdate, 3), Lag(Tuesdays$iUpdate_projection, 3)) +
  ifelse(is.na(Lag(Tuesdays$iUpdate, 4)) == FALSE, Lag(Tuesdays$iUpdate, 4), Lag(Tuesdays$iUpdate_projection, 4))
Tuesdays$iUpdate_projection <- Tuesdays_iUpdate_projection_backup 

Tuesdays_Organic_projection_backup <- 
  ifelse(is.na(Lag(Tuesdays$Organic, 1)) == FALSE, Lag(Tuesdays$Organic, 1), Lag(Tuesdays$Organic_projection, 1)) +
  ifelse(is.na(Lag(Tuesdays$Organic, 2)) == FALSE, Lag(Tuesdays$Organic, 2), Lag(Tuesdays$Organic_projection, 2)) + 
  ifelse(is.na(Lag(Tuesdays$Organic, 3)) == FALSE, Lag(Tuesdays$Organic, 3), Lag(Tuesdays$Organic_projection, 3)) +
  ifelse(is.na(Lag(Tuesdays$Organic, 4)) == FALSE, Lag(Tuesdays$Organic, 4), Lag(Tuesdays$Organic_projection, 4))
Tuesdays$Organic_projection <- Tuesdays_Organic_projection_backup 

Tuesdays_Paid_web_projection_backup <- 
  ifelse(is.na(Lag(Tuesdays$Paid_web, 1)) == FALSE, Lag(Tuesdays$Paid_web, 1), Lag(Tuesdays$Paid_web_projection, 1)) +
  ifelse(is.na(Lag(Tuesdays$Paid_web, 2)) == FALSE, Lag(Tuesdays$Paid_web, 2), Lag(Tuesdays$Paid_web_projection, 2)) + 
  ifelse(is.na(Lag(Tuesdays$Paid_web, 3)) == FALSE, Lag(Tuesdays$Paid_web, 3), Lag(Tuesdays$Paid_web_projection, 3)) +
  ifelse(is.na(Lag(Tuesdays$Paid_web, 4)) == FALSE, Lag(Tuesdays$Paid_web, 4), Lag(Tuesdays$Paid_web_projection, 4))
Tuesdays$Paid_web_projection <- Tuesdays_Paid_web_projection_backup 

Tuesdays_other_projection_backup <- 
  ifelse(is.na(Lag(Tuesdays$other, 1)) == FALSE, Lag(Tuesdays$other, 1), Lag(Tuesdays$other_projection, 1)) +
  ifelse(is.na(Lag(Tuesdays$other, 2)) == FALSE, Lag(Tuesdays$other, 2), Lag(Tuesdays$other_projection, 2)) + 
  ifelse(is.na(Lag(Tuesdays$other, 3)) == FALSE, Lag(Tuesdays$other, 3), Lag(Tuesdays$other_projection, 3)) +
  ifelse(is.na(Lag(Tuesdays$other, 4)) == FALSE, Lag(Tuesdays$other, 4), Lag(Tuesdays$other_projection, 4))
Tuesdays$other_projection <- Tuesdays_other_projection_backup 

#Wednesdays
Wednesdays_mktg_direct_projection_backup <- 
  ifelse(is.na(Lag(Wednesdays$mktg_direct, 1)) == FALSE, Lag(Wednesdays$mktg_direct, 1), Lag(Wednesdays$mktg_direct_projection, 1)) +
  ifelse(is.na(Lag(Wednesdays$mktg_direct, 2)) == FALSE, Lag(Wednesdays$mktg_direct, 2), Lag(Wednesdays$mktg_direct_projection, 2)) + 
  ifelse(is.na(Lag(Wednesdays$mktg_direct, 3)) == FALSE, Lag(Wednesdays$mktg_direct, 3), Lag(Wednesdays$mktg_direct_projection, 3)) +
  ifelse(is.na(Lag(Wednesdays$mktg_direct, 4)) == FALSE, Lag(Wednesdays$mktg_direct, 4), Lag(Wednesdays$mktg_direct_projection, 4))
Wednesdays$mktg_direct_projection <- Wednesdays_mktg_direct_projection_backup 

Wednesdays_customer_service_projection_backup <- 
  ifelse(is.na(Lag(Wednesdays$customer_service, 1)) == FALSE, Lag(Wednesdays$customer_service, 1), Lag(Wednesdays$customer_service_projection, 1)) +
  ifelse(is.na(Lag(Wednesdays$customer_service, 2)) == FALSE, Lag(Wednesdays$customer_service, 2), Lag(Wednesdays$customer_service_projection, 2)) + 
  ifelse(is.na(Lag(Wednesdays$customer_service, 3)) == FALSE, Lag(Wednesdays$customer_service, 3), Lag(Wednesdays$customer_service_projection, 3)) +
  ifelse(is.na(Lag(Wednesdays$customer_service, 4)) == FALSE, Lag(Wednesdays$customer_service, 4), Lag(Wednesdays$customer_service_projection, 4))
Wednesdays$customer_service_projection <- Wednesdays_customer_service_projection_backup 

Wednesdays_DB_IVR_projection_backup <- 
  ifelse(is.na(Lag(Wednesdays$DB_IVR, 1)) == FALSE, Lag(Wednesdays$DB_IVR, 1), Lag(Wednesdays$DB_IVR_projection, 1)) +
  ifelse(is.na(Lag(Wednesdays$DB_IVR, 2)) == FALSE, Lag(Wednesdays$DB_IVR, 2), Lag(Wednesdays$DB_IVR_projection, 2)) + 
  ifelse(is.na(Lag(Wednesdays$DB_IVR, 3)) == FALSE, Lag(Wednesdays$DB_IVR, 3), Lag(Wednesdays$DB_IVR_projection, 3)) +
  ifelse(is.na(Lag(Wednesdays$DB_IVR, 4)) == FALSE, Lag(Wednesdays$DB_IVR, 4), Lag(Wednesdays$DB_IVR_projection, 4))
Wednesdays$DB_IVR_projection <- Wednesdays_DB_IVR_projection_backup 

Wednesdays_DB_com_projection_backup <- 
  ifelse(is.na(Lag(Wednesdays$DB_com, 1)) == FALSE, Lag(Wednesdays$DB_com, 1), Lag(Wednesdays$DB_com_projection, 1)) +
  ifelse(is.na(Lag(Wednesdays$DB_com, 2)) == FALSE, Lag(Wednesdays$DB_com, 2), Lag(Wednesdays$DB_com_projection, 2)) + 
  ifelse(is.na(Lag(Wednesdays$DB_com, 3)) == FALSE, Lag(Wednesdays$DB_com, 3), Lag(Wednesdays$DB_com_projection, 3)) +
  ifelse(is.na(Lag(Wednesdays$DB_com, 4)) == FALSE, Lag(Wednesdays$DB_com, 4), Lag(Wednesdays$DB_com_projection, 4))
Wednesdays$DB_com_projection <- Wednesdays_DB_com_projection_backup 

Wednesdays_Direct_Extension_projection_backup <- 
  ifelse(is.na(Lag(Wednesdays$Direct_Extension, 1)) == FALSE, Lag(Wednesdays$Direct_Extension, 1), 
         Lag(Wednesdays$Direct_Extension_projection, 1)) +
  ifelse(is.na(Lag(Wednesdays$Direct_Extension, 2)) == FALSE, Lag(Wednesdays$Direct_Extension, 2),
         Lag(Wednesdays$Direct_Extension_projection, 2)) + 
  ifelse(is.na(Lag(Wednesdays$Direct_Extension, 3)) == FALSE, Lag(Wednesdays$Direct_Extension, 3), 
         Lag(Wednesdays$Direct_Extension_projection, 3)) +
  ifelse(is.na(Lag(Wednesdays$Direct_Extension, 4)) == FALSE, Lag(Wednesdays$Direct_Extension, 4), 
         Lag(Wednesdays$Direct_Extension_projection, 4))
Wednesdays$Direct_Extension_projection <- Wednesdays_Direct_Extension_projection_backup 

Wednesdays_iUpdate_projection_backup <- 
  ifelse(is.na(Lag(Wednesdays$iUpdate, 1)) == FALSE, Lag(Wednesdays$iUpdate, 1), Lag(Wednesdays$iUpdate_projection, 1)) +
  ifelse(is.na(Lag(Wednesdays$iUpdate, 2)) == FALSE, Lag(Wednesdays$iUpdate, 2), Lag(Wednesdays$iUpdate_projection, 2)) + 
  ifelse(is.na(Lag(Wednesdays$iUpdate, 3)) == FALSE, Lag(Wednesdays$iUpdate, 3), Lag(Wednesdays$iUpdate_projection, 3)) +
  ifelse(is.na(Lag(Wednesdays$iUpdate, 4)) == FALSE, Lag(Wednesdays$iUpdate, 4), Lag(Wednesdays$iUpdate_projection, 4))
Wednesdays$iUpdate_projection <- Wednesdays_iUpdate_projection_backup 

Wednesdays_Organic_projection_backup <- 
  ifelse(is.na(Lag(Wednesdays$Organic, 1)) == FALSE, Lag(Wednesdays$Organic, 1), Lag(Wednesdays$Organic_projection, 1)) +
  ifelse(is.na(Lag(Wednesdays$Organic, 2)) == FALSE, Lag(Wednesdays$Organic, 2), Lag(Wednesdays$Organic_projection, 2)) + 
  ifelse(is.na(Lag(Wednesdays$Organic, 3)) == FALSE, Lag(Wednesdays$Organic, 3), Lag(Wednesdays$Organic_projection, 3)) +
  ifelse(is.na(Lag(Wednesdays$Organic, 4)) == FALSE, Lag(Wednesdays$Organic, 4), Lag(Wednesdays$Organic_projection, 4))
Wednesdays$Organic_projection <- Wednesdays_Organic_projection_backup 

Wednesdays_Paid_web_projection_backup <- 
  ifelse(is.na(Lag(Wednesdays$Paid_web, 1)) == FALSE, Lag(Wednesdays$Paid_web, 1), Lag(Wednesdays$Paid_web_projection, 1)) +
  ifelse(is.na(Lag(Wednesdays$Paid_web, 2)) == FALSE, Lag(Wednesdays$Paid_web, 2), Lag(Wednesdays$Paid_web_projection, 2)) + 
  ifelse(is.na(Lag(Wednesdays$Paid_web, 3)) == FALSE, Lag(Wednesdays$Paid_web, 3), Lag(Wednesdays$Paid_web_projection, 3)) +
  ifelse(is.na(Lag(Wednesdays$Paid_web, 4)) == FALSE, Lag(Wednesdays$Paid_web, 4), Lag(Wednesdays$Paid_web_projection, 4))
Wednesdays$Paid_web_projection <- Wednesdays_Paid_web_projection_backup 

Wednesdays_other_projection_backup <- 
  ifelse(is.na(Lag(Wednesdays$other, 1)) == FALSE, Lag(Wednesdays$other, 1), Lag(Wednesdays$other_projection, 1)) +
  ifelse(is.na(Lag(Wednesdays$other, 2)) == FALSE, Lag(Wednesdays$other, 2), Lag(Wednesdays$other_projection, 2)) + 
  ifelse(is.na(Lag(Wednesdays$other, 3)) == FALSE, Lag(Wednesdays$other, 3), Lag(Wednesdays$other_projection, 3)) +
  ifelse(is.na(Lag(Wednesdays$other, 4)) == FALSE, Lag(Wednesdays$other, 4), Lag(Wednesdays$other_projection, 4))
Wednesdays$other_projection <- Wednesdays_other_projection_backup 

#Thursdays
Thursdays_mktg_direct_projection_backup <- 
  ifelse(is.na(Lag(Thursdays$mktg_direct, 1)) == FALSE, Lag(Thursdays$mktg_direct, 1), Lag(Thursdays$mktg_direct_projection, 1)) +
  ifelse(is.na(Lag(Thursdays$mktg_direct, 2)) == FALSE, Lag(Thursdays$mktg_direct, 2), Lag(Thursdays$mktg_direct_projection, 2)) + 
  ifelse(is.na(Lag(Thursdays$mktg_direct, 3)) == FALSE, Lag(Thursdays$mktg_direct, 3), Lag(Thursdays$mktg_direct_projection, 3)) +
  ifelse(is.na(Lag(Thursdays$mktg_direct, 4)) == FALSE, Lag(Thursdays$mktg_direct, 4), Lag(Thursdays$mktg_direct_projection, 4))
Thursdays$mktg_direct_projection <- Thursdays_mktg_direct_projection_backup 

Thursdays_customer_service_projection_backup <- 
  ifelse(is.na(Lag(Thursdays$customer_service, 1)) == FALSE, Lag(Thursdays$customer_service, 1), Lag(Thursdays$customer_service_projection, 1)) +
  ifelse(is.na(Lag(Thursdays$customer_service, 2)) == FALSE, Lag(Thursdays$customer_service, 2), Lag(Thursdays$customer_service_projection, 2)) + 
  ifelse(is.na(Lag(Thursdays$customer_service, 3)) == FALSE, Lag(Thursdays$customer_service, 3), Lag(Thursdays$customer_service_projection, 3)) +
  ifelse(is.na(Lag(Thursdays$customer_service, 4)) == FALSE, Lag(Thursdays$customer_service, 4), Lag(Thursdays$customer_service_projection, 4))
Thursdays$customer_service_projection <- Thursdays_customer_service_projection_backup 

Thursdays_DB_IVR_projection_backup <- 
  ifelse(is.na(Lag(Thursdays$DB_IVR, 1)) == FALSE, Lag(Thursdays$DB_IVR, 1), Lag(Thursdays$DB_IVR_projection, 1)) +
  ifelse(is.na(Lag(Thursdays$DB_IVR, 2)) == FALSE, Lag(Thursdays$DB_IVR, 2), Lag(Thursdays$DB_IVR_projection, 2)) + 
  ifelse(is.na(Lag(Thursdays$DB_IVR, 3)) == FALSE, Lag(Thursdays$DB_IVR, 3), Lag(Thursdays$DB_IVR_projection, 3)) +
  ifelse(is.na(Lag(Thursdays$DB_IVR, 4)) == FALSE, Lag(Thursdays$DB_IVR, 4), Lag(Thursdays$DB_IVR_projection, 4))
Thursdays$DB_IVR_projection <- Thursdays_DB_IVR_projection_backup 

Thursdays_DB_com_projection_backup <- 
  ifelse(is.na(Lag(Thursdays$DB_com, 1)) == FALSE, Lag(Thursdays$DB_com, 1), Lag(Thursdays$DB_com_projection, 1)) +
  ifelse(is.na(Lag(Thursdays$DB_com, 2)) == FALSE, Lag(Thursdays$DB_com, 2), Lag(Thursdays$DB_com_projection, 2)) + 
  ifelse(is.na(Lag(Thursdays$DB_com, 3)) == FALSE, Lag(Thursdays$DB_com, 3), Lag(Thursdays$DB_com_projection, 3)) +
  ifelse(is.na(Lag(Thursdays$DB_com, 4)) == FALSE, Lag(Thursdays$DB_com, 4), Lag(Thursdays$DB_com_projection, 4))
Thursdays$DB_com_projection <- Thursdays_DB_com_projection_backup 

Thursdays_Direct_Extension_projection_backup <- 
  ifelse(is.na(Lag(Thursdays$Direct_Extension, 1)) == FALSE, Lag(Thursdays$Direct_Extension, 1), 
         Lag(Thursdays$Direct_Extension_projection, 1)) +
  ifelse(is.na(Lag(Thursdays$Direct_Extension, 2)) == FALSE, Lag(Thursdays$Direct_Extension, 2), 
         Lag(Thursdays$Direct_Extension_projection, 2)) + 
  ifelse(is.na(Lag(Thursdays$Direct_Extension, 3)) == FALSE, Lag(Thursdays$Direct_Extension, 3), 
         Lag(Thursdays$Direct_Extension_projection, 3)) +
  ifelse(is.na(Lag(Thursdays$Direct_Extension, 4)) == FALSE, Lag(Thursdays$Direct_Extension, 4),
         Lag(Thursdays$Direct_Extension_projection, 4))
Thursdays$Direct_Extension_projection <- Thursdays_Direct_Extension_projection_backup 

Thursdays_iUpdate_projection_backup <- 
  ifelse(is.na(Lag(Thursdays$iUpdate, 1)) == FALSE, Lag(Thursdays$iUpdate, 1), Lag(Thursdays$iUpdate_projection, 1)) +
  ifelse(is.na(Lag(Thursdays$iUpdate, 2)) == FALSE, Lag(Thursdays$iUpdate, 2), Lag(Thursdays$iUpdate_projection, 2)) + 
  ifelse(is.na(Lag(Thursdays$iUpdate, 3)) == FALSE, Lag(Thursdays$iUpdate, 3), Lag(Thursdays$iUpdate_projection, 3)) +
  ifelse(is.na(Lag(Thursdays$iUpdate, 4)) == FALSE, Lag(Thursdays$iUpdate, 4), Lag(Thursdays$iUpdate_projection, 4))
Thursdays$iUpdate_projection <- Thursdays_iUpdate_projection_backup 

Thursdays_Organic_projection_backup <- 
  ifelse(is.na(Lag(Thursdays$Organic, 1)) == FALSE, Lag(Thursdays$Organic, 1), Lag(Thursdays$Organic_projection, 1)) +
  ifelse(is.na(Lag(Thursdays$Organic, 2)) == FALSE, Lag(Thursdays$Organic, 2), Lag(Thursdays$Organic_projection, 2)) + 
  ifelse(is.na(Lag(Thursdays$Organic, 3)) == FALSE, Lag(Thursdays$Organic, 3), Lag(Thursdays$Organic_projection, 3)) +
  ifelse(is.na(Lag(Thursdays$Organic, 4)) == FALSE, Lag(Thursdays$Organic, 4), Lag(Thursdays$Organic_projection, 4))
Thursdays$Organic_projection <- Thursdays_Organic_projection_backup 

Thursdays_Paid_web_projection_backup <- 
  ifelse(is.na(Lag(Thursdays$Paid_web, 1)) == FALSE, Lag(Thursdays$Paid_web, 1), Lag(Thursdays$Paid_web_projection, 1)) +
  ifelse(is.na(Lag(Thursdays$Paid_web, 2)) == FALSE, Lag(Thursdays$Paid_web, 2), Lag(Thursdays$Paid_web_projection, 2)) + 
  ifelse(is.na(Lag(Thursdays$Paid_web, 3)) == FALSE, Lag(Thursdays$Paid_web, 3), Lag(Thursdays$Paid_web_projection, 3)) +
  ifelse(is.na(Lag(Thursdays$Paid_web, 4)) == FALSE, Lag(Thursdays$Paid_web, 4), Lag(Thursdays$Paid_web_projection, 4))
Thursdays$Paid_web_projection <- Thursdays_Paid_web_projection_backup 

Thursdays_other_projection_backup <- 
  ifelse(is.na(Lag(Thursdays$other, 1)) == FALSE, Lag(Thursdays$other, 1), Lag(Thursdays$other_projection, 1)) +
  ifelse(is.na(Lag(Thursdays$other, 2)) == FALSE, Lag(Thursdays$other, 2), Lag(Thursdays$other_projection, 2)) + 
  ifelse(is.na(Lag(Thursdays$other, 3)) == FALSE, Lag(Thursdays$other, 3), Lag(Thursdays$other_projection, 3)) +
  ifelse(is.na(Lag(Thursdays$other, 4)) == FALSE, Lag(Thursdays$other, 4), Lag(Thursdays$other_projection, 4))
Thursdays$other_projection <- Thursdays_other_projection_backup 

#Fridays
Fridays_mktg_direct_projection_backup <- 
  ifelse(is.na(Lag(Fridays$mktg_direct, 1)) == FALSE, Lag(Fridays$mktg_direct, 1), Lag(Fridays$mktg_direct_projection, 1)) +
  ifelse(is.na(Lag(Fridays$mktg_direct, 2)) == FALSE, Lag(Fridays$mktg_direct, 2), Lag(Fridays$mktg_direct_projection, 2)) + 
  ifelse(is.na(Lag(Fridays$mktg_direct, 3)) == FALSE, Lag(Fridays$mktg_direct, 3), Lag(Fridays$mktg_direct_projection, 3)) +
  ifelse(is.na(Lag(Fridays$mktg_direct, 4)) == FALSE, Lag(Fridays$mktg_direct, 4), Lag(Fridays$mktg_direct_projection, 4))
Fridays$mktg_direct_projection <- Fridays_mktg_direct_projection_backup 

Fridays_customer_service_projection_backup <- 
  ifelse(is.na(Lag(Fridays$customer_service, 1)) == FALSE, Lag(Fridays$customer_service, 1), Lag(Fridays$customer_service_projection, 1)) +
  ifelse(is.na(Lag(Fridays$customer_service, 2)) == FALSE, Lag(Fridays$customer_service, 2), Lag(Fridays$customer_service_projection, 2)) + 
  ifelse(is.na(Lag(Fridays$customer_service, 3)) == FALSE, Lag(Fridays$customer_service, 3), Lag(Fridays$customer_service_projection, 3)) +
  ifelse(is.na(Lag(Fridays$customer_service, 4)) == FALSE, Lag(Fridays$customer_service, 4), Lag(Fridays$customer_service_projection, 4))
Fridays$customer_service_projection <- Fridays_customer_service_projection_backup 

Fridays_DB_IVR_projection_backup <- 
  ifelse(is.na(Lag(Fridays$DB_IVR, 1)) == FALSE, Lag(Fridays$DB_IVR, 1), Lag(Fridays$DB_IVR_projection, 1)) +
  ifelse(is.na(Lag(Fridays$DB_IVR, 2)) == FALSE, Lag(Fridays$DB_IVR, 2), Lag(Fridays$DB_IVR_projection, 2)) + 
  ifelse(is.na(Lag(Fridays$DB_IVR, 3)) == FALSE, Lag(Fridays$DB_IVR, 3), Lag(Fridays$DB_IVR_projection, 3)) +
  ifelse(is.na(Lag(Fridays$DB_IVR, 4)) == FALSE, Lag(Fridays$DB_IVR, 4), Lag(Fridays$DB_IVR_projection, 4))
Fridays$DB_IVR_projection <- Fridays_DB_IVR_projection_backup 

Fridays_DB_com_projection_backup <- 
  ifelse(is.na(Lag(Fridays$DB_com, 1)) == FALSE, Lag(Fridays$DB_com, 1), Lag(Fridays$DB_com_projection, 1)) +
  ifelse(is.na(Lag(Fridays$DB_com, 2)) == FALSE, Lag(Fridays$DB_com, 2), Lag(Fridays$DB_com_projection, 2)) + 
  ifelse(is.na(Lag(Fridays$DB_com, 3)) == FALSE, Lag(Fridays$DB_com, 3), Lag(Fridays$DB_com_projection, 3)) +
  ifelse(is.na(Lag(Fridays$DB_com, 4)) == FALSE, Lag(Fridays$DB_com, 4), Lag(Fridays$DB_com_projection, 4))
Fridays$DB_com_projection <- Fridays_DB_com_projection_backup 

Fridays_Direct_Extension_projection_backup <- 
  ifelse(is.na(Lag(Fridays$Direct_Extension, 1)) == FALSE, Lag(Fridays$Direct_Extension, 1), Lag(Fridays$Direct_Extension_projection, 1)) +
  ifelse(is.na(Lag(Fridays$Direct_Extension, 2)) == FALSE, Lag(Fridays$Direct_Extension, 2), Lag(Fridays$Direct_Extension_projection, 2)) + 
  ifelse(is.na(Lag(Fridays$Direct_Extension, 3)) == FALSE, Lag(Fridays$Direct_Extension, 3), Lag(Fridays$Direct_Extension_projection, 3)) +
  ifelse(is.na(Lag(Fridays$Direct_Extension, 4)) == FALSE, Lag(Fridays$Direct_Extension, 4), Lag(Fridays$Direct_Extension_projection, 4))
Fridays$Direct_Extension_projection <- Fridays_Direct_Extension_projection_backup 

Fridays_iUpdate_projection_backup <- 
  ifelse(is.na(Lag(Fridays$iUpdate, 1)) == FALSE, Lag(Fridays$iUpdate, 1), Lag(Fridays$iUpdate_projection, 1)) +
  ifelse(is.na(Lag(Fridays$iUpdate, 2)) == FALSE, Lag(Fridays$iUpdate, 2), Lag(Fridays$iUpdate_projection, 2)) + 
  ifelse(is.na(Lag(Fridays$iUpdate, 3)) == FALSE, Lag(Fridays$iUpdate, 3), Lag(Fridays$iUpdate_projection, 3)) +
  ifelse(is.na(Lag(Fridays$iUpdate, 4)) == FALSE, Lag(Fridays$iUpdate, 4), Lag(Fridays$iUpdate_projection, 4))
Fridays$iUpdate_projection <- Fridays_iUpdate_projection_backup 

Fridays_Organic_projection_backup <- 
  ifelse(is.na(Lag(Fridays$Organic, 1)) == FALSE, Lag(Fridays$Organic, 1), Lag(Fridays$Organic_projection, 1)) +
  ifelse(is.na(Lag(Fridays$Organic, 2)) == FALSE, Lag(Fridays$Organic, 2), Lag(Fridays$Organic_projection, 2)) + 
  ifelse(is.na(Lag(Fridays$Organic, 3)) == FALSE, Lag(Fridays$Organic, 3), Lag(Fridays$Organic_projection, 3)) +
  ifelse(is.na(Lag(Fridays$Organic, 4)) == FALSE, Lag(Fridays$Organic, 4), Lag(Fridays$Organic_projection, 4))
Fridays$Organic_projection <- Fridays_Organic_projection_backup 

Fridays_Paid_web_projection_backup <- 
  ifelse(is.na(Lag(Fridays$Paid_web, 1)) == FALSE, Lag(Fridays$Paid_web, 1), Lag(Fridays$Paid_web_projection, 1)) +
  ifelse(is.na(Lag(Fridays$Paid_web, 2)) == FALSE, Lag(Fridays$Paid_web, 2), Lag(Fridays$Paid_web_projection, 2)) + 
  ifelse(is.na(Lag(Fridays$Paid_web, 3)) == FALSE, Lag(Fridays$Paid_web, 3), Lag(Fridays$Paid_web_projection, 3)) +
  ifelse(is.na(Lag(Fridays$Paid_web, 4)) == FALSE, Lag(Fridays$Paid_web, 4), Lag(Fridays$Paid_web_projection, 4))
Fridays$Paid_web_projection <- Fridays_Paid_web_projection_backup 

Fridays_other_projection_backup <- 
  ifelse(is.na(Lag(Fridays$other, 1)) == FALSE, Lag(Fridays$other, 1), Lag(Fridays$other_projection, 1)) +
  ifelse(is.na(Lag(Fridays$other, 2)) == FALSE, Lag(Fridays$other, 2), Lag(Fridays$other_projection, 2)) + 
  ifelse(is.na(Lag(Fridays$other, 3)) == FALSE, Lag(Fridays$other, 3), Lag(Fridays$other_projection, 3)) +
  ifelse(is.na(Lag(Fridays$other, 4)) == FALSE, Lag(Fridays$other, 4), Lag(Fridays$other_projection, 4))
Fridays$other_projection <- Fridays_other_projection_backup 

#Saturdays
Saturdays_mktg_direct_projection_backup <- 
  ifelse(is.na(Lag(Saturdays$mktg_direct, 1)) == FALSE, Lag(Saturdays$mktg_direct, 1), Lag(Saturdays$mktg_direct_projection, 1)) +
  ifelse(is.na(Lag(Saturdays$mktg_direct, 2)) == FALSE, Lag(Saturdays$mktg_direct, 2), Lag(Saturdays$mktg_direct_projection, 2)) + 
  ifelse(is.na(Lag(Saturdays$mktg_direct, 3)) == FALSE, Lag(Saturdays$mktg_direct, 3), Lag(Saturdays$mktg_direct_projection, 3)) +
  ifelse(is.na(Lag(Saturdays$mktg_direct, 4)) == FALSE, Lag(Saturdays$mktg_direct, 4), Lag(Saturdays$mktg_direct_projection, 4))
Saturdays$mktg_direct_projection <- Saturdays_mktg_direct_projection_backup 

Saturdays_customer_service_projection_backup <- 
  ifelse(is.na(Lag(Saturdays$customer_service, 1)) == FALSE, Lag(Saturdays$customer_service, 1), Lag(Saturdays$customer_service_projection, 1)) +
  ifelse(is.na(Lag(Saturdays$customer_service, 2)) == FALSE, Lag(Saturdays$customer_service, 2), Lag(Saturdays$customer_service_projection, 2)) + 
  ifelse(is.na(Lag(Saturdays$customer_service, 3)) == FALSE, Lag(Saturdays$customer_service, 3), Lag(Saturdays$customer_service_projection, 3)) +
  ifelse(is.na(Lag(Saturdays$customer_service, 4)) == FALSE, Lag(Saturdays$customer_service, 4), Lag(Saturdays$customer_service_projection, 4))
Saturdays$customer_service_projection <- Saturdays_customer_service_projection_backup 

Saturdays_DB_IVR_projection_backup <-
  ifelse(is.na(Lag(Saturdays$DB_IVR, 1)) == FALSE, Lag(Saturdays$DB_IVR, 1), Lag(Saturdays$DB_IVR_projection, 1)) +
  ifelse(is.na(Lag(Saturdays$DB_IVR, 2)) == FALSE, Lag(Saturdays$DB_IVR, 2), Lag(Saturdays$DB_IVR_projection, 2)) + 
  ifelse(is.na(Lag(Saturdays$DB_IVR, 3)) == FALSE, Lag(Saturdays$DB_IVR, 3), Lag(Saturdays$DB_IVR_projection, 3)) +
  ifelse(is.na(Lag(Saturdays$DB_IVR, 4)) == FALSE, Lag(Saturdays$DB_IVR, 4), Lag(Saturdays$DB_IVR_projection, 4))
Saturdays$DB_IVR_projection <- Saturdays_DB_IVR_projection_backup 

Saturdays_DB_com_projection_backup <- 
  ifelse(is.na(Lag(Saturdays$DB_com, 1)) == FALSE, Lag(Saturdays$DB_com, 1), Lag(Saturdays$DB_com_projection, 1)) +
  ifelse(is.na(Lag(Saturdays$DB_com, 2)) == FALSE, Lag(Saturdays$DB_com, 2), Lag(Saturdays$DB_com_projection, 2)) + 
  ifelse(is.na(Lag(Saturdays$DB_com, 3)) == FALSE, Lag(Saturdays$DB_com, 3), Lag(Saturdays$DB_com_projection, 3)) +
  ifelse(is.na(Lag(Saturdays$DB_com, 4)) == FALSE, Lag(Saturdays$DB_com, 4), Lag(Saturdays$DB_com_projection, 4))
Saturdays$DB_com_projection <- Saturdays_DB_com_projection_backup 

Saturdays_Direct_Extension_projection_backup <- 
  ifelse(is.na(Lag(Saturdays$Direct_Extension, 1)) == FALSE, Lag(Saturdays$Direct_Extension, 1),
         Lag(Saturdays$Direct_Extension_projection, 1)) +
  ifelse(is.na(Lag(Saturdays$Direct_Extension, 2)) == FALSE, Lag(Saturdays$Direct_Extension, 2),
         Lag(Saturdays$Direct_Extension_projection, 2)) + 
  ifelse(is.na(Lag(Saturdays$Direct_Extension, 3)) == FALSE, Lag(Saturdays$Direct_Extension, 3),
         Lag(Saturdays$Direct_Extension_projection, 3)) +
  ifelse(is.na(Lag(Saturdays$Direct_Extension, 4)) == FALSE, Lag(Saturdays$Direct_Extension, 4),
         Lag(Saturdays$Direct_Extension_projection, 4))
Saturdays$Direct_Extension_projection <- Saturdays_Direct_Extension_projection_backup 

Saturdays_iUpdate_projection_backup <- 
  ifelse(is.na(Lag(Saturdays$iUpdate, 1)) == FALSE, Lag(Saturdays$iUpdate, 1), Lag(Saturdays$iUpdate_projection, 1)) +
  ifelse(is.na(Lag(Saturdays$iUpdate, 2)) == FALSE, Lag(Saturdays$iUpdate, 2), Lag(Saturdays$iUpdate_projection, 2)) + 
  ifelse(is.na(Lag(Saturdays$iUpdate, 3)) == FALSE, Lag(Saturdays$iUpdate, 3), Lag(Saturdays$iUpdate_projection, 3)) +
  ifelse(is.na(Lag(Saturdays$iUpdate, 4)) == FALSE, Lag(Saturdays$iUpdate, 4), Lag(Saturdays$iUpdate_projection, 4))
Saturdays$iUpdate_projection <- Saturdays_iUpdate_projection_backup 

Saturdays_Organic_projection_backup <- 
  ifelse(is.na(Lag(Saturdays$Organic, 1)) == FALSE, Lag(Saturdays$Organic, 1), Lag(Saturdays$Organic_projection, 1)) +
  ifelse(is.na(Lag(Saturdays$Organic, 2)) == FALSE, Lag(Saturdays$Organic, 2), Lag(Saturdays$Organic_projection, 2)) + 
  ifelse(is.na(Lag(Saturdays$Organic, 3)) == FALSE, Lag(Saturdays$Organic, 3), Lag(Saturdays$Organic_projection, 3)) +
  ifelse(is.na(Lag(Saturdays$Organic, 4)) == FALSE, Lag(Saturdays$Organic, 4), Lag(Saturdays$Organic_projection, 4))
Saturdays$Organic_projection <- Saturdays_Organic_projection_backup 

Saturdays_Paid_web_projection_backup <- 
  ifelse(is.na(Lag(Saturdays$Paid_web, 1)) == FALSE, Lag(Saturdays$Paid_web, 1), Lag(Saturdays$Paid_web_projection, 1)) +
  ifelse(is.na(Lag(Saturdays$Paid_web, 2)) == FALSE, Lag(Saturdays$Paid_web, 2), Lag(Saturdays$Paid_web_projection, 2)) + 
  ifelse(is.na(Lag(Saturdays$Paid_web, 3)) == FALSE, Lag(Saturdays$Paid_web, 3), Lag(Saturdays$Paid_web_projection, 3)) +
  ifelse(is.na(Lag(Saturdays$Paid_web, 4)) == FALSE, Lag(Saturdays$Paid_web, 4), Lag(Saturdays$Paid_web_projection, 4))
Saturdays$Paid_web_projection <- Saturdays_Paid_web_projection_backup 

Saturdays_other_projection_backup <- 
  ifelse(is.na(Lag(Saturdays$other, 1)) == FALSE, Lag(Saturdays$other, 1), Lag(Saturdays$other_projection, 1)) +
  ifelse(is.na(Lag(Saturdays$other, 2)) == FALSE, Lag(Saturdays$other, 2), Lag(Saturdays$other_projection, 2)) + 
  ifelse(is.na(Lag(Saturdays$other, 3)) == FALSE, Lag(Saturdays$other, 3), Lag(Saturdays$other_projection, 3)) +
  ifelse(is.na(Lag(Saturdays$other, 4)) == FALSE, Lag(Saturdays$other, 4), Lag(Saturdays$other_projection, 4))
Saturdays$other_projection <- Saturdays_other_projection_backup 

#Now bind all days together
backup_projections <- rbind(Sundays, Mondays, Tuesdays, Wednesdays, Thursdays, Fridays, Saturdays)

#Sort backup projections by date
backup_projections <- backup_projections[order(backup_projections$call_date), ]

#Divide by 4 to make an average
backup_projections$mktg_direct_projection <- backup_projections$mktg_direct_projection / 4
backup_projections$customer_service_projection <- backup_projections$customer_service_projection / 4
backup_projections$DB_IVR_projection <- backup_projections$DB_IVR_projection / 4
backup_projections$DB_com_projection <- backup_projections$DB_com_projection / 4 
backup_projections$Direct_Extension_projection <- backup_projections$Direct_Extension_projection / 4
backup_projections$iUpdate_projection <- backup_projections$iUpdate_projection / 4
backup_projections$Organic_projection <- backup_projections$Organic_projection / 4 
backup_projections$Paid_web_projection <- backup_projections$Paid_web_projection / 4
backup_projections$other_projection <- backup_projections$other_projection / 4

#Make sure Error and total_projection are correct
backup_projections$total_projection <- backup_projections$mktg_direct_projection + backup_projections$customer_service_projection + 
                                       backup_projections$DB_IVR_projection + backup_projections$DB_com_projection + 
                                       backup_projections$Direct_Extension_projection + backup_projections$iUpdate_projection + 
                                       backup_projections$Organic_projection + backup_projections$Paid_web_projection + 
                                       backup_projections$other_projection

#Now, keep backup projections where date is greater than or equal to DM_matrix_validity_date
matrix_validity_date <- as.Date('2015-02-7')
backup_projections_used <- backup_projections[as.Date(backup_projections$call_date) > matrix_validity_date, ]

#full data prior to backup
full_data_primary_model <- full_data[as.Date(full_data$call_date) <= matrix_validity_date, ]

#Adjusted projections
adjusted_projections <- as.data.frame(rbind(full_data_primary_model, backup_projections_used))
adjusted_projections <- adjusted_projections[order(adjusted_projections$call_date), ]

#Adjusted projections error, av. miss
adjusted_projections$Error <- adjusted_projections$total_projection - adjusted_projections$all_calls

sd_Error <- aggregate(adjusted_projections$Error, 
                      by=list(adjusted_projections$Year, adjusted_projections$Month), FUN=sd, na.rm="TRUE")
names(sd_Error) <- c("Year", "Month", "sd_Error")

Average_Miss <- aggregate(adjusted_projections$Error, 
                          by=list(adjusted_projections$Year, adjusted_projections$Month), FUN=mean, na.rm="TRUE")
names(Average_Miss) <- c("Year", "Month", "Average_Miss")

adjusted_projections$sd_Error <- NULL
adjusted_projections$Average_Miss <- NULL

adjusted_projections <- merge(adjusted_projections, sd_Error, by=c("Year", "Month"), all.x=TRUE)
adjusted_projections <- merge(adjusted_projections, Average_Miss, by=c("Year", "Month"), all.x=TRUE)

adjusted_projections <- adjusted_projections[order(adjusted_projections$call_date), ]

write.csv(adjusted_projections, 
          paste("/Users/ashelton/Documents/The Call Projection Model/adjusted_updated_forecasts_", month(Sys.Date()), 
                "_", year(Sys.Date()), ".csv", sep=""))

#Done!


  
