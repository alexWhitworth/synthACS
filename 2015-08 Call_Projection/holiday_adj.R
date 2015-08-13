
#' @paran cur_forecasts A \code{data.frame} of the current, unadjusted (for holidays), forecasts.
#' @param beg_year An \code{integer} specifying the first year in which you would like holidays
#' @param end_year An \code{integer} specifying the last year in which you would like holidays. 
#' Defaults to the current year.
#' @param call_hist A \code{data.frame} of historical calls data on which to examine holiday patterns.
#' @return A \code{data.frame} of forecasts that have been adjusted for holidays.
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
  projections <- cur_forecasts; 
  projections <- merge(projections, holidays, by= "call_date", all.x= TRUE)
  
  calls$holiday <- ifelse(is.na(calls$holiday), "no holiday", calls$holiday)
  calls$holiday <- relevel(as.factor(calls$holiday), ref= "no holiday") 
  projections$holiday <- ifelse(is.na(projections$holiday), "no holiday", projections$holiday)
  projections$holiday <- relevel(as.factor(projections$holiday), ref= "no holiday") 
  
  # 02. Determine holiday effect by holiday
  # Note: only need to look at holidays in a given month for adjustments
  # Note2: currently not looking at past holidays to back-adjust / normalize historical data 
  #--------------------------------------------------------------
  cur_mo <- month(cur_forecasts$call_date[1]); cur_yr <- year(cur_forecasts$call_date[1])
  projections$flag <- ifelse(month(projections$call_date) == cur_mo & year(projections$call_date) == cur_yr &
                               projections$holiday != "no holiday", TRUE, NA)
  
  hols <- which(projections$flag == TRUE)
  if (length(hols) > 0) {
    library(MASS) # negative binomial regression to handle overdispersion.
    nb.mkt     <- glm.nb(mkt_direct ~ 0 + holiday + as.factor(wday), data= calls)
    nb.ivr     <- glm.nb(db_ivr ~ 0 + holiday + as.factor(wday), data= calls)
    nb.dandb   <- glm.nb(dandb_com ~ 0 + holiday + as.factor(wday), data= calls)
    nb.iupdate <- glm.nb(iupdate ~ 0 + holiday + as.factor(wday), data= calls)
    nb.organic <- glm.nb(organic ~ 0 + holiday + as.factor(wday), data= calls)
    nb.paid    <- glm.nb(paid_etc ~ 0 + holiday + as.factor(wday), data= calls)
    
    # convert predicted results to call adjustments
    newdat <- expand.grid(wday= 1:7, holiday= as.factor(names(table(calls$holiday))))
    pred <- data.frame(newdat, 
                       pred_mkt= predict(nb.mkt, newdat, "response"),
                       pred_ivr= predict(nb.ivr, newdat, "response"),
                       pred_dandb= predict(nb.dandb, newdat, "response"),
                       pred_iupdate= predict(nb.iupdate, newdat, "response"),
                       pred_organic= predict(nb.organic, newdat, "response"),
                       pred_paid= predict(nb.paid, newdat, "response"))
   
    pred[,-c(1,2)] <- apply(pred[,-c(1,2)], 2, function(x) {round(x - x[1:7], 2)})
    
    # apply holiday adjustments
    projections <- apply_hol_adj(cur_forecasts= projections, pred_adj= pred) 
  
  } else {
    projections$flag <- NULL
  }
  
  # 03. Output 
  #--------------------------------------------------------------
  return(projections)
}


#test <- holiday_adj(base_forecasts[[2]], 2012, call_hist= call_hist)

#' @description This function should only be called internally.
#' It applies the holiday adjustments to the forecasts
#' @param cur_forecasts A \code{data.frame}. The internal object \code{projections}.
#' @param pred_adj A \code{data.frame}. The internal object \code{pred}.
apply_hol_adj <- function(cur_forecasts, pred_adj) {
  p2 <- merge(cur_forecasts, pred_adj, by= c("wday", "holiday"), all.x= TRUE)
  p2 <- p2[order(p2$call_date),]
  
  # update
  p2$mkt_direct <- p2$mkt_direct - p2$pred_mkt
  p2$db_ivr     <- p2$db_ivr - p2$pred_ivr
  p2$dandb.com  <- p2$dandb.com - p2$pred_dandb
  p2$organic    <- p2$organic - p2$pred_organic
  p2$paid_etc   <- p2$paid_etc - p2$pred_paid
  p2$iupdate    <- p2$iupdate - p2$pred_iupdate
  
  # remove extra columns
  p2$flag <- NULL;  p2$wday <- NULL
  p2$pred_mkt <- NULL
  p2$pred_ivr <- NULL
  p2$pred_dandb <- NULL
  p2$pred_iupdate <- NULL
  p2$pred_organic <- NULL
  p2$pred_paid <- NULL
  
  return(p2[,c("call_date", "wkday", "mkt_direct", "db_ivr", 
                "dandb.com", "organic", "paid_etc", "iupdate", "holiday")])
}
