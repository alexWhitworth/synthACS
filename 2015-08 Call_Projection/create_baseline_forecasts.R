
#' @title Create baseline call projection forecasts
#' @description Creates the baseline call projections for the model based on historical 
#' response rates for each campaign class (\code{campaign_type:class_of_mail}).
#' @param camp_proj A \code{data.table}. Should be one of the returned values from 
#' \code{get_model_data()}.
#' @param camp_comp_stats A \code{data.table} of summary statistics of completed 
#' campaigns computed at the \code{group_by(campaign_type, class_of_mail, days_to_response)} 
#' level.
#' @param top_outstanding A \code{list}. Should be the results from \code{camp_out_calc_adj()}.
#' @return ???
#' @export
create_baseline_forecasts <- function(camp_proj, camp_comp_stats, top_outstanding) {
  require(data.table)
  
  camp_proj <- camp_proj[, .(cell_code, campaign_type, class_of_mail, response_date, response_day_of_week, 
                       days_of_tracking, days_to_response, unique_leads, total_responders, responders,
                       pct_of_responders)]
  # fix NA class of mail
  camp_proj$class_of_mail <- ifelse(!is.na(camp_proj$class_of_mail), camp_proj$class_of_mail,
                          ifelse(is.na(camp_proj$class_of_mail) & substr(camp_proj$cell_code, 5,5) == "1", "1st",
                          ifelse(is.na(camp_proj$class_of_mail) & substr(camp_proj$cell_code, 5,5) == "3", "3rd", NA)))
  
  # 01. Forecast forward for each campaign by day and campaign class
  #---------------------------------------------------------------
  projections <- new.env()
  for (c in names(table(camp_comp_stats$campaign_type))) {
    for (m in names(table(camp_comp_stats$class_of_mail))) {
      # stats for campaign class
      comp_resp <- camp_comp_stats[campaign_type == c & class_of_mail == m,]
      # oustanding campaigns for campaign class
      to_proj   <- camp_proj[campaign_type == c & class_of_mail == m,]
      
      codes <- names(table(to_proj$cell_code))
      proj <- list()
      for (i in 1:length(codes)) {
        # campaign to project
        camp_to_proj <- to_proj[cell_code == codes[i], ]
        # RR for specific campaign
        mean_rr <- comp_resp[days_to_response %in% camp_to_proj$days_to_response ,]$mean_daily_resp_rate
        # calc
        camp_to_proj$responders= round(camp_to_proj$unique_leads * mean_rr, 0)
        proj[[i]] <- failwith(NULL, camp_to_proj) # this is problematic if NULL output. need try() or something else
      }
      assign(paste("proj", c, m, sep= "_"), rbindlist(proj), envir= projections)
    }
    
  }
  
  
  
  # xx. Make adjustments for top performing outstanding-campaigns
  #---------------------------------------------------------------
  
  
  
}


camp_p <- model_data$camp_proj
x <- camp_p[, .(days_of_tracking, min= min(response_date), max= max(response_date)),
             by= cell_code]
as.data.frame(x[!duplicated(x),])


x2 <- camp_p[cell_code == "D-T-1-15-06-076",]
