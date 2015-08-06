
#' @title Calculate adjustment rate for ongoing campaigns
#' @description Input IB call data, pull campaign tracking data. ...
#' @param camp_out A \code{data.frame} with data on oustanding campaigns. Is an output 
#' from \code{get_model_data()}.
#' @param channel A character string corresponding to the appropriate ODBC connection. Defaults to "c2g"
#' @return ...
camp_out_calc_adj <- function(camp_out, channel= "c2g") {
  library(RODBC)
  library(data.table)
  
  # 01. Pull campaign tracker data, subset complete and incomplete campaigns
  #--------------------------------------------------------
  ch <- odbcConnect("c2g")
  camp_track <- data.table(sqlQuery(ch, "SELECT * FROM [c2g].[dbo].[c2g_campaign_tracker]
                       where year(date) >= 2014", stringsAsFactors= FALSE), key= "cell_code")
  close(ch); rm(ch)
  
  # subset complete campaigns
  camp_comp <- camp_track[tolower(campaign_type) %in% c("ad hoc", "bau", "latest inquiry") & 
                            camp_track$days_of_tracking == 90,][,
                          ':='(campaign_type= tolower(campaign_type))] 
  
  # subset incomplete campaigns
  camp_inc <- camp_track[tolower(campaign_type) %in% c("ad hoc", "bau", "latest inquiry") & 
                           camp_track$days_of_tracking < 90,][,
                         ':='(campaign_type= tolower(campaign_type))] 
  rm(camp_track) # clean up
  
  # recode class of mail == NA
  camp_comp$class_of_mail <- ifelse(!is.na(camp_comp$class_of_mail) | 
                  substr(camp_comp$cell_code, 1,1) == "E", camp_comp$class_of_mail,
           ifelse(substr(camp_comp$cell_code, 5, 5) == "1", "1st",
                  ifelse(substr(camp_comp$cell_code, 5, 5) == "3", "3rd", camp_comp$class_of_mail)))

  camp_inc$class_of_mail <- ifelse(!is.na(camp_inc$class_of_mail) | 
                  substr(camp_inc$cell_code, 1,1) == "E", camp_inc$class_of_mail,
           ifelse(substr(camp_inc$cell_code, 5, 5) == "1", "1st",
                  ifelse(substr(camp_inc$cell_code, 5, 5) == "3", "3rd", camp_inc$class_of_mail)))
  
  
  # 02. summarize response rates of complete campaigns
  # then ID top performing complete campaigns to determine adjustment 
  # Note: based on EDA, I will only adjust for high performing (top 10%) campaigns 
  # where class sample >= 50
  #--------------------------------------------------------
  # complete campaigns -- N, mean, and sd
  comp_rr <- camp_comp[, .(n=.N,mean_rr= mean(response_rate_duns, na.rm=TRUE),
                           sd_rr= sd(response_rate_duns, na.rm=TRUE),
                           p_90= quantile(response_rate_duns, .9, na.rm=TRUE)), 
                     by= .(campaign_type, class_of_mail)][order(campaign_type, class_of_mail)]
  
  # top_campaigns -- cell codes and data from campaign_response
  top_camp <- extract_top(camp_comp, comp_rr)
  
  # 03. summarize top campaigns over time relative to all campaigns
  #--------------------------------------------------------
  
  
  
  
  
  # 04. summarize response rates of complete campaigns
  #--------------------------------------------------------
  # incomplete campaigns with >= 0 days of tracking.
  inc_rr <- camp_inc[days_of_tracking >= 0, .(days_of_tracking, rr= unique_responses_duns / unique_leads), 
                     by= .(cell_code, campaign_type, class_of_mail)]
  inc_sum_stats <- inc_rr[!is.na(class_of_mail)][, .(n= .N, mean_rr= mean(rr)),
                          by= .(campaign_type, class_of_mail)][order(campaign_type, class_of_mail)]
  top_inc_camp <- camp_inc[which()]
  
}


extract_top <- function(campaigns, rr_stats, channel= "c2g") {
  require(RODBC)
  require(data.table)
  
  ch <- odbcConnect(channel)
  camp_resp <- data.table(sqlQuery(ch, "SELECT * FROM [c2g].[dbo].[c2g_campaign_response] 
                                  where year(date) >= 2014", stringsAsFactors= FALSE), 
                          key= c("cell_code", "response_date")); close(ch); rm(ch)
  
  top_camp <- not_top  <- top_camp_cell_codes <- list()
  for (i in 1:nrow(rr_stats)) {
    if (rr_stats$n[i] < 50 | is.na(rr_stats$class_of_mail[i])) {next}
    # extract cell codes for top campaigns
    #-------------------------------------------------------- 
    camp_sub <- campaigns[campaign_type == rr_stats$campaign_type[i] &
                                      class_of_mail == rr_stats$class_of_mail[i]]
    top_camp_cell_codes[[length(top_camp_cell_codes) + 1]] <- 
      camp_sub$cell_code[which(camp_sub$response_rate_duns > rr_stats$p_90[i])]
  
    # extract relevant campaign_response data
    #--------------------------------------------------------
    top_camp[[length(top_camp) + 1]] <- camp_resp[cell_code %in% 
                  top_camp_cell_codes[[length(top_camp_cell_codes)]],]
    
    not_top[[length(not_top) + 1]] <- camp_resp[!(cell_code %in% 
                  top_camp_cell_codes[[length(top_camp_cell_codes)]]) & 
                  campaign_type == rr_stats$campaign_type[i] &
                  class_of_mail == rr_stats$class_of_mail[i],]
    # apply class name
    names(not_top)[length(not_top)] <- names(top_camp)[length(top_camp)] <- 
      paste(rr_stats$campaign_type[i], rr_stats$class_of_mail[i], sep= "_")
  }
  return(list(top=top_camp, not_top=not_top))
}