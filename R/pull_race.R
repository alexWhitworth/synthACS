

#' @title Pull ACS race data
#' @description Pull ACS data for a specified geography from base tables
#' B01001B-I and B02001. ' These tables reference population counts by race.
#' @param endyear An integer, indicating the latest year of the data in the survey.
#' @param span An integer in \code{c(1,3,5)} indicating the span of the desired data.
#' @param geography a valid \code{geo.set} object specifying the census geography or 
#' geographies to be fetched.
#' @return A \code{list} containing the endyear, span, a \code{data.frame} of estimates,
#' a \code{data.frame} of standard errors, and a \code{data.frame} of the geography 
#' metadata from \code{\link[acs]{acs.fetch}}.
#' @seealso \code{\link[acs]{acs.fetch}}, \code{\link[acs]{geo.make}}
#' @export
pull_race_data <- function(endyear, span, geography) {
  # 00 -- error checking
  #----------------------------------------------
  if (! span %in% c(1,3,5)) stop("The ACS API only supports data spans of 1, 3, and 5 years.")
  if (endyear %% 1 != 0 | endyear < 2009) stop("endyear must be an integer >= 2009 (when ACS data begins).")
  # other span/endyear issues handled by library(acs)
  if (!is.geo.set(geography)) stop("Supply valid geography -- class 'geo.set'.")
  
  # 01 -- pull data
  #----------------------------------------------
  race_all <- acs.fetch(endyear = endyear, span= span, geography = geography, 
                        table.number = "B02001", col.names = "pretty")
  race_aa <- acs.fetch(endyear = endyear, span= span, geography = geography, 
                       table.number = "B01001B", col.names = "pretty")
  race_nat <- acs.fetch(endyear = endyear, span= span, geography = geography, 
                        table.number = "B01001C", col.names = "pretty")
  race_asian <- acs.fetch(endyear = endyear, span= span, geography = geography, 
                       table.number = "B01001D", col.names = "pretty")
  race_isl <- acs.fetch(endyear = endyear, span= span, geography = geography, 
                       table.number = "B01001E", col.names = "pretty")
  race_oth <- acs.fetch(endyear = endyear, span= span, geography = geography, 
                       table.number = "B01001F", col.names = "pretty")
  race_2p <- acs.fetch(endyear = endyear, span= span, geography = geography, 
                        table.number = "B01001G", col.names = "pretty")
  race_white <- acs.fetch(endyear = endyear, span= span, geography = geography, 
                       table.number = "B01001H", col.names = "pretty")
  race_hisp <- acs.fetch(endyear = endyear, span= span, geography = geography, 
                       table.number = "B01001I", col.names = "pretty")
  
  
  # 02 -- create lists of EST and SE -- as data.frames
  #----------------------------------------------
  est <- list(tot_pop= data.frame(race_all@estimate[, c(1:2)]),
              aa_pop = data.frame(race_aa@estimate[,c(1,2,17)]),
              nat_pop= data.frame(race_nat@estimate[,c(1,2,17)]),
              asn_pop= data.frame(race_asian@estimate[,c(1,2,17)]),
              isl_pop= data.frame(race_isl@estimate[,c(1,2,17)]),
              oth_pop= data.frame(race_oth@estimate[,c(1,2,17)]),
              r2_pop = data.frame(race_2p@estimate[,c(1,2,17)]),
              whi_pop= data.frame(race_white@estimate[,c(1,2,17)]),
              his_pop= data.frame(race_hisp@estimate[,c(1,2,17)]))
  
  se <- list(tot_pop= data.frame(race_all@standard.error[, c(1:2)]),
              aa_pop = data.frame(race_aa@standard.error[,c(1,2,17)]),
              nat_pop= data.frame(race_nat@standard.error[,c(1,2,17)]),
              asn_pop= data.frame(race_asian@standard.error[,c(1,2,17)]),
              isl_pop= data.frame(race_isl@standard.error[,c(1,2,17)]),
              oth_pop= data.frame(race_oth@standard.error[,c(1,2,17)]),
              r2_pop = data.frame(race_2p@standard.error[,c(1,2,17)]),
              whi_pop= data.frame(race_white@standard.error[,c(1,2,17)]),
              his_pop= data.frame(race_hisp@standard.error[,c(1,2,17)]))
  
  geo <- race_all@geography
  
  rm(race_all, race_aa, race_nat, race_asian, race_isl, race_oth, race_2p, race_white, race_hisp)
  
  # 03 -- combine columns
  #----------------------------------------------
  est <- do.call("cbind", est)
  est <- est[, c(1:2, seq(3,26,3), seq(4,26,3), seq(5,26,3))]
  se  <- do.call("cbind", se)
  se  <- se[, c(1:2, seq(3,26,3), seq(4,26,3), seq(5,26,3))]
  
  names(est) <- names(se) <- c(paste("agg", c("count", "white"), sep= "_"), 
    paste(rep(c("total", "m", "f"), each= 8),
      rep(c("black", "nat_amer", "asian", "pac_isl", "other", "2p_races", "white_alone", "hisp_lat"), 3), sep="_"))
  
  
  # 04 -- combine and return
  #----------------------------------------------
  return(list(endyear= endyear, span= span,
              estimates= est,
              standard_error= se,
              geography= geo))
  
}

