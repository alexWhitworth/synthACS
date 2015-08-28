
#' @title Calculate adjustment rate (ie lift) for oustanding campaigns 
#' @description Calculates adjustment rate (ie-lift) for ongoing campaigns by class and 
#' IDs top ongoing capaigns where statistically significant. Class is defined as 
#' \code{interaction(campaign_type, class_of_mail)}.
#' @param camp_out A \code{data.frame} with data on oustanding campaigns. Is an output 
#' from \code{get_model_data()}.
#' @param channel A character string corresponding to the appropriate ODBC connection. Defaults to "c2g"
#' @return A \code{list} of length two: (1) \code{list} of top performing campaigns by class, 
#' (2) the adjustment rate (lift) for each class.
#' @export
camp_out_calc_adj <- function(camp_out, channel= "c2g") {
  library(RODBC)
  library(data.table)
  
  # 01. Pull campaign tracker data, subset complete and incomplete campaigns
  #--------------------------------------------------------
  ch <- odbcConnect(channel)
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
  top_camp <- extract_top(camp_comp, comp_rr, channel= channel)
  
  # [AW 8/28] remove campaigns that haven't mailed yet
  # This is needed for use with future projection -- there are campaigns in here that haven't mailed yet
  # EDGE CASE!!
  top_camp$top <- lapply(top_camp$top, function(x) return(x[!is.na(x$days_to_response), ]))
  top_camp$all <- lapply(top_camp$all, function(x) return(x[!is.na(x$days_to_response), ]))
  
  # impute missing resonse dates to 0
  top_camp$top <- lapply(top_camp$top, function(x) {impute_zero_resp_all(dat= x)}) # use other defaults
  top_camp$all <- lapply(top_camp$all, function(x) {impute_zero_resp_all(dat= x)}) # use other defaults
  
  # get cummulative Responses
  top_camp$top <- lapply(top_camp$top, function(x) {require(data.table);
    x[order(cell_code, days_to_response)][, 
      ':='(cum_resp= cumsum(responders)), by= .(cell_code)][,
      ':='(cum_rr= cum_resp / unique_leads), by= .(cell_code)]
  })
  
  top_camp$all <- lapply(top_camp$all, function(x) {require(data.table);
    x[order(cell_code, days_to_response)][, 
      ':='(cum_resp= cumsum(responders)), by= .(cell_code)][,
      ':='(cum_rr= cum_resp / unique_leads), by= .(cell_code)]
  })
  
  # 03. Compare top campaigns over time relative to all campaigns
  #--------------------------------------------------------
  camp_sum_stats <- list()
  camp_sum_stats[[1]] <- lapply(top_camp$top, function(x) {
    x[days_to_response %in% seq(10,90, 10), .SD, by= cell_code][,
          .(n= .N, avg_cum_rr= mean(cum_rr, na.rm=TRUE), sd_cum_rr= sd(cum_rr, na.rm=TRUE)), 
          by= days_to_response]
  }); names(camp_sum_stats[[1]]) <- names(top_camp$top)
  
  camp_sum_stats[[2]] <- lapply(top_camp$all, function(x) {
    x[days_to_response %in% seq(10,90, 10), .SD, by= cell_code][,
          .(n= .N, avg_cum_rr= mean(cum_rr, na.rm=TRUE), sd_cum_rr= sd(cum_rr, na.rm=TRUE)), 
          by= days_to_response]
  }); names(camp_sum_stats[[2]]) <- names(top_camp$all)
  
  # test statistical significance of differences b/w top and all campaigns by class
  # and calculate lift
  sig_diff <- list()
  for (i in 1:length(camp_sum_stats[[1]])) {
    sig_diff[[i]] <- listwise.t.test(camp_sum_stats[[1]][[i]], camp_sum_stats[[2]][[i]])
  }; names(sig_diff) 
  
  lift <- sapply(sig_diff, function(x) {
    mean(as.data.frame(x)$lift)
  }); names(lift) <- names(top_camp$top)
  
  # 04. ID top incomplete campaigns
  #--------------------------------------------------------
  # subset incomplete campaigns with ~ [20,90] days of tracking.
  inc_camp_track <- camp_inc[days_of_tracking >= 0, .(days_of_tracking= round(days_of_tracking, -1), 
                     rr= response_rate_duns), 
                     by= .(cell_code, campaign_type, class_of_mail)][days_of_tracking %in% seq(20,90,10)]
  
  # ID top performing incomplete campaigns by class
  top_inc <- id_top_inc(comp_rr, camp_sum_stats[[1]], inc_camp_track)
  
  # 05. Return
  #--------------------------------------------------------
  return(list(top_ongoing_camp= top_inc, adj_rate= lift))
}

#' @title Extract top / not top campaign data
#' @description Pulls campaign data from [campaign_response] for top campaigns and not-top campaigns
#' by class (\code{campaign_type:class_of_mail})
#' @param campaigns A \code{data.frame} with data on complete campaigns from [campaign_tracker].
#' @param rr_stats A \code{data.frame} with summary statistics on complete campaigns.
#' @param channel A character string corresponding to the appropriate ODBC connection. Defaults to "c2g"
#' @return a \code{list} of two \code{data.table}s: (1) top campaign data; (2) not-top campaign data
#' @export
extract_top <- function(campaigns, rr_stats, channel= "c2g") {
  require(RODBC)
  require(data.table)
  
  ch <- odbcConnect(channel)
  camp_resp <- data.table(sqlQuery(ch, "SELECT * FROM [c2g].[dbo].[c2g_campaign_response] 
                                  where year(date) >= 2014", stringsAsFactors= FALSE), 
                          key= c("cell_code", "response_date")); close(ch); rm(ch)
  
  top_camp <- all  <- top_camp_cell_codes <- list()
  
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
    
    all[[length(all) + 1]] <- camp_resp[campaign_type == rr_stats$campaign_type[i] &
                  class_of_mail == rr_stats$class_of_mail[i],]
    # apply class name
    names(all)[length(all)] <- names(top_camp)[length(top_camp)] <- 
      paste(rr_stats$campaign_type[i], rr_stats$class_of_mail[i], sep= "_")
  }
  return(list(top=top_camp, all=all))
}

#' @title Listwise t-test
#' @description Computes pairs of Weltch t-tests for matching 
#' @param df1 A \code{data.frame} with four columns: (1) a key, (2) sample size, 
#' (3) a vector of means, (4) a vector of SDs
#' @param df2 A \code{data.frame} with four columns: (1) a key, (2) sample size, 
#' (3) a vector of means, (4) a vector of SDs
#' @param alpha The significance level of test. Defaults to 0.05
#' @export
listwise.t.test <- function(df1, df2, alpha= 0.05) {
  if (!all(dim(df1) == dim(df2))) stop("Input equal dimension data frames.")
  if (df1[,1] != df2[,1]) stop("Data frames must have matching keys")
  # build output
  df1 <- as.data.frame(df1); df2 <- as.data.frame(df2)
  n <- nrow(df1)
  results <- matrix(NA, nrow= n, ncol= 5, dimnames= 
                      list(1:n, c("key", "t.stat", "W.S. df", "p.value", "lift")))
  results[,1] <- df1[,1]
  for (i in 1:n) {
    t <- (df1[i,3] - df2[i,3]) / sqrt(df1[i,4]^2 / df1[i,2] + df2[i,4]^2 / df2[i,2])
    df <- (df1[i,4]^2 / df1[i,2] + df2[i,4]^2 / df2[i,2])^2 / (
      (df1[i,4]^2 / df1[i,2])^2 / (df1[i,2] - 1) + (df2[i,4]^2 / df2[i,2])^2 / (df2[i,2] - 1))
    p.val <- round(1 - pt(abs(t), df), 5)
    lift <- ifelse(p.val < alpha, round(df1[i,3] / df2[i,3], 5), NA)
    results[i, 2:5] <- c(t, df, p.val, lift)
  }
  return(results)
}

#' @title Identify top performing ongoing campaigns
#' @description Using historical data on complete campaigns and data on incomplete / 
#' ongoing campaigns, identify ongoing campaigns which are expected to be top performing 
#' campaigns.
#' @param  complete_camp_rr A \code{data.table}. Should be the "comp_rr" internal from
#'  camp_out_calc_adj().
#' @param top_camp_list A \code{list} with summary statistics for top performing complete
#' campaigns. Should be the "camp_sum_stats[[1]]" internal from camp_out_calc_adj().
#' @param inc_campaigns A \code{data.table}. Should be the "inc_camp_track" internal
#' from camp_out_calc_adj().
#' @export
id_top_inc <- function(complete_camp_rr, top_camp_list, inc_campaigns) {
  require(data.table)
  # subset to match extract_top()
  complete_camp_rr <- complete_camp_rr[n >= 50 & !is.na(class_of_mail),][,
     name := paste(campaign_type, class_of_mail, sep= "_")] 
  
  # cycle through classes (campaign_type:class_of_mail) to:
  #   1. subset incomplete campaigns
  #   2. compare response rate to top response rates
  #   3. ID top campaigns and return
  top_subsets <- list()
  for (i in 1:nrow(complete_camp_rr)) {
    inc_subset <- inc_campaigns[campaign_type == complete_camp_rr$campaign_type[i] &
                                class_of_mail == complete_camp_rr$class_of_mail[i],]
    top_camp_stats <- top_camp_list[[which(names(top_camp_list) == complete_camp_rr$name[i])]]
    
    temp <- list()
    for (j in seq(20, 90, 10)) {
      temp[[length(temp) + 1]] <- inc_subset[days_of_tracking == j &
         rr >= top_camp_stats$avg_cum_rr[which(top_camp_stats$days_to_response == j)],]$cell_code
    }
    top_subsets[[length(top_subsets) + 1]] <- do.call("c", temp)
    names(top_subsets)[length(top_subsets)] <- complete_camp_rr$name[i]
  }
  return(top_subsets)
}



