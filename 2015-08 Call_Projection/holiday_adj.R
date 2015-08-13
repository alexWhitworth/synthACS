
# call_hist <- pull_call_data(channel= "c2g", call_date= '04/01/2014')

#' @paran cur_forecasts A \code{data.frame} of the current, unadjusted (for holidays), forecasts.
#' @param beg_year An \code{integer} specifying the first year in which you would like holidays
#' @param end_year An \code{integer} specifying the last year in which you would like holidays. 
#' Defaults to the current year.
#' @param call_hist A \code{data.frame} of historical calls data on which to examine holiday patterns.
#' @return ??
#' @export
holiday_adj <- function(cur_forecasts, beg_year= 2014, end_year= year(Sys.Date()), call_hist) {
 
  require(data.table)
  
  # 01. Pull in holidays, do basic munging and merging
  #--------------------------------------------------------------
  holidays <- data.table(holiday_scrape(beg_year, end_year))[order(date2)]
  setnames(holidays, old= "date2", new= "call_date")
  
  
  calls_by_day_cat <- call_hist[, .(calls= sum(call_count)) ,by= .(category, call_date)]
  calls_by_day_cat$category <- ifelse(calls_by_day_cat$category == "D&B IVR", "db_ivr",
                               ifelse(calls_by_day_cat$category == "DandB.com", "dandb_com",
                               ifelse(calls_by_day_cat$category == "Marketing Direct", "mkt_direct",
                               ifelse(calls_by_day_cat$category == "Organic", "organic",
                               ifelse(calls_by_day_cat$category == "Paid Media, Web & Affiliate", "paid_etc",
                               ifelse(calls_by_day_cat$category == "iUpdate", "iupdate", calls_by_day_cat$category))))))
  calls <- merge(call_hist[, .(calls= sum(call_count)) ,by= .(call_date)], 
                 dcast.data.table(calls_by_day_cat, formula= call_date ~ category, value.var= "calls"), 
                 by= "call_date", all= TRUE)
  
  calls$wday <- wday(calls$call_date)
  calls$call_date <- as.Date(calls$call_date)
  
  # merge holidays to both calls and forecasts, do some quick munging
  calls <- merge(calls, holidays, by= "call_date", all.x= TRUE)
  proj <- cur_forecasts; setnames(proj, "response_date", "call_date")
  proj <- merge(proj, holidays, by= "call_date", all.x= TRUE)
  calls$holiday <- ifelse(is.na(calls$holiday), "no holiday", calls$holiday)
  calls$holiday <- relevel(as.factor(calls$holiday), ref= "no holiday") 
  proj$holiday <- ifelse(is.na(proj$holiday), "no holiday", proj$holiday)
  proj$holiday <- relevel(as.factor(proj$holiday), ref= "no holiday") 
  
  # 02. Determine holiday effect by holiday
  # Note: only need to look at holidays in a given month for adjustments
  # Note2: currently not looking at past holidays to back-adjust / normalize historical data 
  #--------------------------------------------------------------
  cur_mo <- month(cur_forecasts$response_date[1]); cur_yr <- year(cur_forecasts$response_date[1])
  proj$flag <- ifelse(month(proj$call_date) == cur_mo & year(proj$call_date) == cur_yr &
                         proj$holiday != "no holiday", TRUE, NA)
  
  hols <- which(proj$flag == TRUE)
  if (length(hols) > 0) {
    library(MASS)
    # library(pscl)
    nb.mkt     <- glm.nb(mkt_direct ~ 0 + holiday + as.factor(wday), data= calls)
    nb.ivr     <- glm.nb(db_ivr ~ 0 + holiday + as.factor(wday), data= calls)
    
    # try poisson & NB
    nb.dandb   <- glm.nb(dandb_com ~ 0 + holiday + as.factor(wday), data= calls)
    nb.iupdate <- glm.nb(iupdate ~ 0 + holiday + as.factor(wday), data= calls)
    nb.organic <- glm.nb(organic ~ 0 + holiday + as.factor(wday), data= calls)
    nb.paid    <- glm.nb(paid_etc ~ 0 + holiday + as.factor(wday), data= calls)
    
    # examine results
    newdat <- expand.grid(wday= 1:7, holiday= as.factor(names(table(calls$holiday))))
    pred <- data.frame(newdat, predicted_calls=predict(nb.mkt, newdat, "response"))
    
  }
}


test <- holiday_adj(base_forecasts[[2]], 2012, call_hist= call_hist)