
# @description standard checks to make sure calls to library(acs) will work
# @param endyear An integer, indicating the latest year of the data in the survey.
# @param span An integer in \code{c(1,3,5)} indicating the span of the desired data.
# @param geography a valid \code{geo.set} object specifying the census geography or 
# geographies to be fetched.
check_geo_inputs <- function(endyear, span, geography) {
  if (! span %in% c(1,3,5)) stop("The ACS API only supports data spans of 1, 3, and 5 years.")
  if (endyear %% 1 != 0 | endyear < 2009) stop("endyear must be an integer >= 2009 (when ACS data begins).")
  # other span/endyear issues handled by library(acs)
  if (!acs::is.geo.set(geography)) stop("Supply valid geography -- class 'geo.set'.")
  
  # else okay; return
}

# helper function to coerce all non-probability vectors to factors
# and do some other basic scrubbing
factor_return <- function(df, prob_name) {
  fact_ind <- which(names(df) != prob_name)
  df[,fact_ind] <- lapply(df[fact_ind], function(l) {
    if (!is.factor(l)) return(factor(l))
    else return(l)
  })
  rownames(df) <- NULL
  return(df[stats::complete.cases(df) & df[prob_name] > 0,])
}
