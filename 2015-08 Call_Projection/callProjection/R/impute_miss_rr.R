
#' @title  Impute Zero Response Rate - missing data
#' @description Imputes missing response rate data for single campaign without 90 days of 
#' reported tracking (ie-- days where 0 individuals responded)
#' @param dat A \code{data.frame} of a completed campaign.
#' @param days_tracking An integer corresponding to the days of tracking across campaigns. 
#' Defaults to 90.
#' @return A \code{data.frame} with imputed zero responses on days without responses.
#' @export
impute_zero_resp <- function(dat, days_tracking= 90) {
  require(data.table)
  
  # 01. Initialize needed values
  #-------------------------------------
  mail_date <- dat$date[1]
  end_date <- mail_date + 90
  
  # 02. ID missing response dates, impute, and recombine / return
  #-------------------------------------
  samp_row <- within(data.table(dat[1, ]), {
    responders= 0
    pct_of_leads= 0
    pct_of_responders= 0
  })
  
  miss <- c(0:90)[-which(0:90 %in% dat$days_to_response)]
  if (length(miss) > 0) {
    miss_dat <- within(rbindlist(replicate(length(miss), samp_row, simplify= FALSE)), {
      days_to_response= miss
    })
   
    dat <- within(rbindlist(list(miss_dat, dat))[order(days_to_response)], {
      response_date= mail_date + days_to_response
      response_day_of_week = wday(response_date)
    })
  }
  return(dat)
}

#' @title  Impute Zero Response Rate - missing data (all campaigns)
#' @description Wrapper for \code{impute_zero_resp}. Imputes missing response rate data 
#' for all campaigns without 90 days of reported tracking (ie-- days where 0 individuals 
#' responded) in a provided dataset.
#' @param dat A \code{data.frame} of all completed campaigns.
#' @param days_tracking An integer corresponding to the days of tracking across campaigns. 
#' Defaults to 90.
#' @param group A character vector of length 1 corresponding to the grouping variable. 
#' Defaults to \code{"cell_code"}.
#' @param comp_camp \code{Logical}. Are the campaigns complete or not? Defaults to TRUE. 
#' @return A \code{data.frame} with imputed zero responses on days without responses.
#' @export
impute_zero_resp_all <- function(dat, days_tracking= 90, group= "cell_code", comp_camp= TRUE) {
  require(data.table)
  dat <- data.table(dat)
  dat$responders <- as.double(dat$responders)
  
  if (comp_camp == TRUE) { # for complete campaigns
    return(dat[, impute_zero_resp(.SD, days_tracking= days_tracking), by= group])
  } else { # for incomplete campaigns
    camp_tracking <- dat[, .(max_tr= max(days_of_tracking, na.rm=TRUE)), by= .(group)][order(group)]
    dat  <- split(dat, dat[[group]]); dat <- dat[order(names(dat))]
    
    for (i in 1:length(dat)) {
      dat[[i]] <- impute_zero_resp(dat[[i]], days_tracking= camp_tracking[[i]])
    }
    return(rbindlist(dat)) 
  }
}