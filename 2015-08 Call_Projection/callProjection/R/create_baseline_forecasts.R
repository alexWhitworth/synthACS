
#' @title Create baseline call projection forecasts
#' @description Creates the baseline call projections for the model based on historical 
#' response rates for each campaign class (\code{campaign_type:class_of_mail}).
#' @param camp_proj A \code{data.table}. Should be one of the returned values from 
#' \code{get_model_data()}.
#' @param camp_comp_stats A \code{data.table} of summary statistics of completed 
#' campaigns computed at the \code{group_by(campaign_type, class_of_mail, days_to_response)} 
#' level.
#' @param top_outstanding A \code{list}. Should be the results from \code{camp_out_calc_adj()}.
#' @return A \code{list} of length two with: (1) Daily forecasts at the campaign level; and (2)
#' aggregated daily forecasts
#' @export
create_baseline_forecasts <- function(camp_proj, camp_comp_stats, top_outstanding) {
  require(data.table)
  require(stringr)
  
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
      
      # forecate at campaign level
      codes <- names(table(to_proj$cell_code))
      to_proj <- split(to_proj, to_proj$cell_code)
      proj <- lapply(to_proj, function(x) {
        # RR for specific campaign
        mean_rr <- comp_resp[days_to_response %in% x$days_to_response ,]$mean_daily_resp_rate
        # calc
        x$responders <- round(x$unique_leads * mean_rr, 3)
        return(x)
      })
      # 02. Make adjustments for top performing outstanding-campaigns
      #---------------------------------------------------------------
      adj_ind <- which(which(str_extract(names(top_outstanding[[1]]), c) == c) == 
              which(str_extract(names(top_outstanding[[1]]), m) == m))
      
      if (length(adj_ind) > 0)  {adj_codes <- codes[codes %in% top_outstanding[[1]][[adj_ind]]]}
      if (exists("adj_codes") && length(adj_codes) > 0) {
        proj <- lapply(proj, function(x, adj_codes) {
          x$responders <- ifelse(x$cell_code[1] %in% adj_codes, 
                            round(x$responders * top_outstanding[[2]][[adj_ind]], 3), x$responders)
          return(x)
        }, adj_codes= adj_codes)
      }
      # assign class projections to list
      if (length(proj) > 0) {
        assign(paste("proj", c, m, sep= "_"), rbindlist(proj), envir= projections)
      }
    }
  }
  # 03. combine and return
  #---------------------------------------------------------------
  if (length(ls(projections)) > 0) {
    projections <- rbindlist(as.list(projections))
    setnames(projections, old= "response_date", new= "call_date")
    projections2 <- projections[, .(day_of_week= response_day_of_week[1], 
                                    responders= sum(responders)) , by= call_date]
    
    return(list(projections, projections2))
  }  else {
    break
  }
}


