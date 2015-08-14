
#' @title Adjust baseline forecasts for weekday seasonality.
#' @description Create exponential smoothing estimates of weekday seasonality
#' via STL. Use seasonality estimates to adjust baseline forecasts.
#' @param baseline A \code{data.frame} of aggregate forecasts. One of the return
#' values from \code{create_baseline_forecasts()}. 
#' @param calls A \code{data.frame} of call data from Phoenix with categorical data. Returned
#' from \code{data_pull()}.
#' @param camp_tot A \code{data.frame} of campaign resposne data from c2g.campaign_response. 
#' Should be combined from the returns of \code{get_model_data()$camp_complete} and 
#' \code{get_model_data()$camp_outstanding}. Used for adjusting calls to c2g tracked reponses.
#' @param seasonal_wks An integer for the number of trailing weeks used to calculate the weekly
#' seasonal pattern in the response trend. Used for model tuning. Defaults to 4 (for now)
#' @param seasonal_adj_type \code{character}. A method for weekday seasonal adjustment.
#' @param call_hist A \code{data.frame} of historical calls data on which to examine holiday patterns.
#' @param beg_year An integer specifying first year of holidays to pull. Defaults to 2014.
#' @param end_year An integer specifying last year of holidays to pull. Defaults to \code{year(Sys.Date())}.
#' @param etsmodel A three-character string identifying method using the framework terminology 
#' of Hyndman et al. (2002) and Hyndman et al. (2008). The first letter denotes the error type ("A", "M" 
#' or "Z"); the second letter denotes the trend type ("N","A","M" or "Z"); and the third letter denotes 
#' the season type ("N","A","M" or "Z"). In all cases, "N"=none, "A"=additive, "M"=multiplicative and 
#' "Z"=automatically selected. 
#' @return A \code{data.frame} of call projections by category for the month.
#' @export
adj_base_forecasts <- function(baseline, calls, camp_tot, seasonal_wks= 4,
                               seasonal_adj_type= c("stl", "ets", "wk_avg", "ensemble", "wk_stl"),
                               call_hist, beg_year= 2014, end_year= year(Sys.Date()),
                               etsmodel= "ZZZ") {
  
  # 00. Initiate
  seasonal_adj_type <- match.arg(seasonal_adj_type, several.ok= FALSE)
  
  # 01. Aggregate call data, do basic munging, and merge datasets
  # The goal here is to create a training dataset for seasonality adjustments
  #--------------------------------------------------------------
  # update category names
  calls_by_day_cat <- calls[, .(calls= sum(call_count)) ,by= .(category, call_date)]
  calls_by_day_cat$category <- ifelse(calls_by_day_cat$category == "D&B IVR", "db_ivr",
                    ifelse(calls_by_day_cat$category == "DandB.com", "dandb_com",
                    ifelse(calls_by_day_cat$category == "Marketing Direct", "mkt_direct",
                    ifelse(calls_by_day_cat$category == "Organic", "organic",
                    ifelse(calls_by_day_cat$category == "Paid Media, Web & Affiliate", "paid_etc",
                    ifelse(calls_by_day_cat$category == "iUpdate", "iupdate", calls_by_day_cat$category))))))
  calls <- merge(calls[, .(calls= sum(call_count)) ,by= .(call_date)], 
                 dcast.data.table(calls_by_day_cat, formula= call_date ~ category, value.var= "calls"), 
                 by= "call_date", all= TRUE)
  
  calls$wday <- wday(calls$call_date)
  calls_by_day_cat$wday <- wday(calls_by_day_cat$call_date) 
  
  # 01a. get at least 6 months training data
  #---------------------------
  if (max(camp_tot$response_date) >=  as.Date("2015-07-01", format= "%Y-%m-%d")) {
    # calls15 == all calls training data
    # resp_by_day == resp / day from camp_resp
    calls15 <- calls[call_date > as.POSIXct("2015-01-01", format= "%Y-%m-%d"),]
    resp_by_day <- camp_tot[, .(responses= sum(responders)), by= response_date][
      response_date > as.Date("2015-01-01", format= "%Y-%m-%d"),][order(response_date)]
  } else {
    # Note: naming convention below doesn't really make sense for older data
    calls15 <- calls[call_date > as.POSIXct(median(camp_tot$response_date)) ,]
    resp_by_day <- camp_tot[, .(responses= sum(responders)), by= response_date][
      response_date > median(camp_tot$response_date),][order(response_date)]
  }
  
  # make sure no missing
  days <- data.frame(response_date= seq(min(resp_by_day$response_date), max(resp_by_day$response_date), 1))
  resp_by_day <- merge(days, resp_by_day, all.x= TRUE); rm(days)
  resp_by_day$responses <- ifelse(is.na(resp_by_day$responses), 0, resp_by_day$responses)
  resp_by_day$wday <- wday(resp_by_day$response_date)
  
  # merge to calls15 data
  setnames(resp_by_day, old= "response_date", new= "call_date")
  calls15$call_date <- as.Date(calls15$call_date)
  calls15 <- merge(calls15, resp_by_day, by= c("call_date", "wday"), all.x=TRUE)[!is.na(responses),]
  
  # build ratios -- only looking at 2015
  # if NA == 0 (one is missing where missing --> 0)
  ratios  <- data.frame(call_date= calls15$call_date, apply(data.frame(
    dbivr_mkt= calls15$db_ivr / calls15$mkt_direct,
    dandb_mkt= calls15$dandb_com / calls15$mkt_direct,
    organic_mkt= calls15$organic / calls15$mkt_direct,
    paidetc_mkt= calls15$paid_etc / calls15$mkt_direct,
    iupdate_mkt= calls15$iupdate / calls15$mkt_direct,
    mkt_to_resp= calls15$mkt_direct / calls15$responses), 2, 
    function(x) {ifelse(is.na(x) | is.infinite(x), 0, x)}))
  
  rm(resp_by_day); gc(verbose= FALSE); # clean up
  
  # 02. Examine, and adjust for, day-of-week seasonality
  #--------------------------------------------------------------
  stl1 <- stl(ts(calls15$responses, start=c(1, calls15$wday[1]), frequency= 7), s.window = 7, robust=T)
  stl_decomp <- data.frame(call_date= calls15$call_date, wday= calls15$wday,
                           stl1$time.series)
  ets1 <- ets(ts(calls15$responses+1, start=c(1, calls15$wday[1]), frequency= 7), model= etsmodel)
  ets_decomp <- data.frame(call_date= calls15$call_date, wday= calls15$wday,
                           fitted= ets1$fitted, residuals=ets1$residuals)
  
  adj_stl <- tapply(stl_decomp$seasonal[(nrow(stl_decomp)- seasonal_wks * 7 + 1):nrow(stl_decomp)], 
                         stl_decomp$wday[(nrow(stl_decomp)- seasonal_wks * 7 + 1):nrow(stl_decomp)], mean)
  adj_ets <- tapply(ets_decomp$fitted[(nrow(ets_decomp)- seasonal_wks * 7 + 1):nrow(ets_decomp)], 
                          ets_decomp$wday[(nrow(ets_decomp)- seasonal_wks * 7 + 1):nrow(ets_decomp)], mean)
  adj_ets <- adj_ets - mean(adj_ets)
  adj_wks <- tapply(calls15$responses[(nrow(calls15)- seasonal_wks * 7 + 1):nrow(calls15)],
                    calls15$wday[(nrow(calls15)- seasonal_wks * 7 + 1):nrow(calls15)], mean)
  adj_wks <- adj_wks - mean(adj_wks)
  
  # adjust
  n <- length(baseline$responders)
  rep <- n %% 7
  wkday1 <- wday(baseline$call_date[1])
  
  if (seasonal_adj_type == "stl") {
    wk1 <- adj_stl[wkday1:length(adj_stl)]; 
    len1 <- length(wk1); wks <- floor((n - len1) / 7); extra <- (n - len1) %% 7
    if (length(wk1) + wks * 7 == n) {
      adj <- c(wk1, rep(adj_stl, wks))
    } else {
      adj <- c(wk1, rep(adj_stl, wks), adj_stl[1:extra])
    }
  } else if (seasonal_adj_type == "ets") {
    wk1 <- adj_ets[wkday1:length(adj_ets)]; 
    len1 <- length(wk1); wks <- floor((n - len1) / 7); extra <- (n - len1) %% 7
    if (length(wk1) + wks * 7 == n) {
      adj <- c(wk1, rep(adj_ets, wks))
    } else {
      adj <- c(wk1, rep(adj_ets, wks), adj_ets[1:extra])
    }
  } else if (seasonal_adj_type == "wk_avg") {
    wk1 <- adj_wks[wkday1:length(adj_wks)]; 
    len1 <- length(wk1); wks <- floor((n - len1) / 7); extra <- (n - len1) %% 7
    if (length(wk1) + wks * 7 == n) {
      adj <- c(wk1, rep(adj_wks, wks))
    } else {
      adj <- c(wk1, rep(adj_wks, wks), adj_wks[1:extra])
    }
  } else if (seasonal_adj_type == "ensemble") {
    adj_ens <- apply(rbind(adj_stl, adj_ets, adj_wks), 2, mean)
    wk1 <- adj_ens[wkday1:length(adj_ens)]; 
    len1 <- length(wk1); wks <- floor((n - len1) / 7); extra <- (n - len1) %% 7
    if (length(wk1) + wks * 7 == n) {
      adj <- c(wk1, rep(adj_ens, wks))
    } else {
      adj <- c(wk1, rep(adj_ens, wks), adj_ens[1:extra])
    }
  } else if (seasonal_adj_type == "wk_stl") {
    adj_ens <- apply(rbind(adj_stl, adj_wks), 2, mean)
    wk1 <- adj_ens[wkday1:length(adj_ens)]; 
    len1 <- length(wk1); wks <- floor((n - len1) / 7); extra <- (n - len1) %% 7
    if (length(wk1) + wks * 7 == n) {
      adj <- c(wk1, rep(adj_ens, wks))
    } else {
      adj <- c(wk1, rep(adj_ens, wks), adj_ens[1:extra])
    }
  }  else {stop("Invalid seasonal_adj_type.")}
  
  baseline$responders <- baseline$responders + adj
  # if any <= 0, use ETS forecasts
  ets_forecasts <- forecast(ets1, h= n)$mean
  if (any(baseline$responders <= 0)) {
    baseline$responders[which(baseline$responders <= 0)] <- (ets_forecasts[which(baseline$responders <= 0)] - 1)
  }
  
  # 03. Adjust for calls / resp ratio
  #--------------------------------------------------------------
  av_ratio <- tapply(ratios$mkt_to_resp[(nrow(ratios)- seasonal_wks * 7 + 1):nrow(ratios)], 
                     wday(ratios$call_date[(nrow(ratios)- seasonal_wks * 7 + 1):nrow(ratios)]), mean)
  
  if (length(wk1) + wks * 7 == n) {
    ratio_adj <- c(av_ratio[wkday1:length(av_ratio)], rep(av_ratio, wks))
  }  else {
  ratio_adj <- c(av_ratio[wkday1:length(av_ratio)], rep(av_ratio, wks), av_ratio[1:extra])
  }
  baseline$responders <- baseline$responders * ratio_adj
  
  # 04. Project non "marketing direct" calls
  #--------------------------------------------------------------
  ets_dbivr   <- ets(ts(ratios$dbivr_mkt, start=c(1, wday(ratios$call_date[1])), frequency= 7))
  ets_dandb   <- ets(ts(ratios$dandb_mkt, start=c(1, wday(ratios$call_date[1])), frequency= 7))
  ets_organic <- ets(ts(ratios$organic_mkt, start=c(1, wday(ratios$call_date[1])), frequency= 7))
  ets_paidetc <- ets(ts(ratios$paidetc_mkt, start=c(1, wday(ratios$call_date[1])), frequency= 7))
  ets_iupdate <- ets(ts(ratios$iupdate_mkt, start=c(1, wday(ratios$call_date[1])), frequency= 7))
  
  
  # 05. combine projections, Examine and adjust for holiday effects
  #--------------------------------------------------------------
  projections <- data.frame(call_date= baseline$call_date, wkday= baseline$day_of_week, 
                            wday= wday(baseline$call_date),
                            mkt_direct= round(baseline$responders, 2),
                            db_ivr= round(baseline$responders * forecast(ets_dbivr, h=n)$mean, 2),
                            dandb.com= round(baseline$responders * forecast(ets_dandb, h=n)$mean, 2),
                            organic= round(baseline$responders * forecast(ets_organic, h=n)$mean, 2),
                            paid_etc= round(baseline$responders * forecast(ets_paidetc, h=n)$mean, 2),
                            iupdate= round(baseline$responders * forecast(ets_iupdate, h=n)$mean, 2))
  
  projections <- holiday_adj(cur_forecasts= projections, beg_year= beg_year, end_year= end_year,
                             call_hist= call_hist)
  
  
  # 06. Finalize and return
  #--------------------------------------------------------------
  
  return(projections)
}
