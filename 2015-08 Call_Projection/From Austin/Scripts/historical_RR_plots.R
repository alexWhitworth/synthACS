#linear interp single 1st
linear_interp_single_1st <- as.data.frame(cbind(x$y$DM_output$linear_interp_single_1st$days_to_response, x$y$DM_output$linear_interp_single_1st$linear_interp_single_1st * 100))
names(linear_interp_single_1st) <- c("days_to_response", "RR")

linear_interp_single_1st_plot <- ggplot(data = linear_interp_single_1st, aes(x=days_to_response, y= RR, group = 1)) + geom_line(size =  1) +
geom_point(colour="red", size = 2) + xlab("days to response") + ylab("Response Rate") + ggtitle("Response Rate Curve, Single 1st") +
theme(plot.title = element_text(lineheight=1.5, face="bold")) + geom_abline(predict(weibull_single_1st))
#linear interp single 3rd
linear_interp_single_3rd <- as.data.frame(cbind(x$y$DM_output$linear_interp_single_3rd$days_to_response, x$y$DM_output$linear_interp_single_3rd$linear_interp_single_3rd))
names(linear_interp_single_3rd) <- c("days_to_response", "RR")

linear_interp_single_3rd_plot <- ggplot(data = linear_interp_single_3rd, aes(x=days_to_response, y= RR, group = 1)) + geom_line(size =  1) +
  geom_point(colour="red", size = 2) + xlab("days to response") + ylab("Response Rate") + ggtitle("Response Rate Curve, Single 3rd") +
  theme(plot.title = element_text(lineheight=1.5, face="bold"))

#library(MASS)
#weibull_single_1st <- fitdistr(x$y$DM_output$linear_interp_single_1st$linear_interp_single_1st * 100, densfun = "weibull")

called_projected <- as.data.frame(cbind(x$full_projections$Called, x$full_projections$projected))
names(called_projected) <- c("Called", "projected")

#Regress calls on Projected
calls_to_projected <- ggplot(data = called_projected, aes(x=projected, y=Called, group = 1)) + geom_point() + xlab("projected") + ylab("Called") + 
ggtitle("Called vs. Projected Historical") + theme(plot.title = element_text(lineheight=1.5, face="bold")) + geom_abline(predict(linreg_called_projected, called_projected))

attach(called_projected)
linreg_called_projected <- lm(Called ~ projected)
detach(called_projected)