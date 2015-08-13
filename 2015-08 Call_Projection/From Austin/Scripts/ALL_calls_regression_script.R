library(ggplot2)
library(lubridate)

#Put all calls into dataframe
all_calls_df <- as.data.frame(cbind(all_calls, dates, mktg_calls))
names(all_calls_df) <- c("All_calls", "Dates", "mktg_calls_only")
all_calls_df$Dates <- as.Date(all_calls_df$Dates, origin = "1970-01-01")
all_calls_df <- all_calls_df[is.na(all_calls_df$All_calls) == "FALSE", ]

#call_plot <- ggplot(data = all_calls_df, aes(x=Dates, y=All_calls)) + geom_line(size = .5) + geom_point(colour="red", size = 2) +
###ggtitle("All Calls and Mktg Calls only")
  #theme(plot.title = element_text(lineheight=1.5, face="bold"))

#Add weekday
all_calls_df$weekday <- wday(all_calls_df$Dates)

#Linear regression of ALL calls (less mktg calls) on mktg calls
#Must come BEFORE the holiday transformation

attach(all_calls_df)
Sunday_linreg_all_calls <- lm(All_calls ~ mktg_calls_only, subset = all_calls_df$weekday == 1)
Monday_linreg_all_calls <- lm(All_calls ~ mktg_calls_only, subset = all_calls_df$weekday == 2)
Tuesday_linreg_all_calls <- lm(All_calls ~ mktg_calls_only, subset = all_calls_df$weekday == 3)
Wednesday_linreg_all_calls <- lm(All_calls ~ mktg_calls_only, subset = all_calls_df$weekday == 4)
Thursday_linreg_all_calls <- lm(All_calls ~ mktg_calls_only, subset = all_calls_df$weekday == 5)
Friday_linreg_all_calls <- lm(All_calls ~ mktg_calls_only, subset = all_calls_df$weekday == 6)
Saturday_linreg_all_calls <- lm(All_calls ~ mktg_calls_only, subset = all_calls_df$weekday == 7)
detach(all_calls_df)