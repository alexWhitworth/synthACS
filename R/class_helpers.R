
#' @title Check macroACS class
#' @description Function that checks if the target object is a \code{macroACS} object.
#' @param x any R object.
#' @return Returns \code{TRUE} if its argument has class "macroACS" among its classes and
#' \code{FALSE} otherwise.
#' @export
is.macroACS <- function(x) {
  inherits(x, "macroACS")
}

#' @title Check micro_synthetic class
#' @description Function that checks if the target object is a \code{micro_synthetic} object.
#' @param x any R object.
#' @return Returns \code{TRUE} if its argument has class "micro_synthetic" among its classes and
#' \code{FALSE} otherwise.
#' @export
is.micro_synthetic <- function(x) {
  inherits(x, "micro_synthetic")
}

#' @title Check macro_micro class
#' @description Function that checks if the target object is a \code{macro_micro} object.
#' @param x any R object.
#' @return Returns \code{TRUE} if its argument has class "macro_micro" among its classes and
#' \code{FALSE} otherwise.
#' @export
is.macro_micro <- function(x) {
  inherits(x, "macro_micro")
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



##---------------------------------------------------------
## Generics for class macroACS
# This set of generic fetches "estimate" or "standard error" data for each of the datasets
# contained in the 'macroACS' class. One method is specified for each dataset
##---------------------------------------------------------

####  helper functions ####
# 1. save duplication of typing on rowmatching
get_rowmatch <- function(string, symbol_table) {
  ns <- length(string)
  out <- vector("list", length= ns)
  for (i in 1:ns) {
    out[[i]] <- grep(paste0("*", string[i], " *"), symbol_table)
  }
  return(Reduce("c", out))
}

# 2. save duplication on input validation
validate_get_inputs <- function(acs, geography, dataset= c("estimate", "st.err")) {
  ## check inputs
  dataset <- match.arg(dataset, several.ok= FALSE)
  if (!is.macroACS(acs)) stop("acs must be of class macroACS.")
  if (!is.character(geography)) {
    stop("geography must be specified as a character vector.")
  } else {
    if (any(nchar(geography) < 4)) 
      stop("Please specify at least 4 characters for geography.")
  }
  # if no error, okay
}

# 3. save duplication on getting datasets
fetch_data <- function(acs, dataset= c("estimate", "st.err"), 
                       choice= c("age_by_sex", "pop_by_race", "marital_status", "edu", "nativity", 
                            "by_inc_12mo", "geo_mob_edu", "ind_inc", "emp_status", "pov_status1")) {
  dataset <- match.arg(dataset, several.ok= FALSE)
  choice <- match.arg(choice, several.ok= FALSE)
  
  if (dataset == "estimate") { return(acs$estimates[[choice]]) } 
  else if (dataset == "st.err") { return(acs$standard_error[[choice]]) }
  else { stop("input 'dataset' is not valid.") }
}

#### now the methods ####

#' @title Get Population Data by Age and Sex for Specified Geography
#' @description Gets data, either estimate or standard error, on a specified geography's population 
#' by age and sex.
#' @param acs An object of class \code{"macroACS"}.
#' @param geography A character vector allowing string matching via \code{\link[base]{grep}} to 
#' a set of specified geographies.
#' @param dataset Either \code{"estimate"} or \code{"st.err"}. Do you want data on estimated 
#' population counts or estimated standard errors?
#' @export
get_age_by_sex <- function(acs, geography, dataset= c("estimate", "st.err")) {
  UseMethod("get_age_by_sex", acs)
}

#' @export
get_age_by_sex.macroACS <- function(acs, geography, dataset= c("estimate", "st.err")) {
  ## check inputs
  validate_get_inputs(acs, geography, dataset)
  ## execute return
  rowid <- get_rowmatch(geography, acs$geography$NAME)
  return(fetch_data(acs, dataset, choice= "age_by_sex")[rowid,])
}


#' @title Get Population Data by Race for Specified Geography
#' @description Gets data, either estimate or standard error, on a specified geography's population 
#' by race.
#' @param acs An object of class \code{"macroACS"}.
#' @param geography A character vector allowing string matching via \code{\link[base]{grep}} to 
#' a set of specified geographies.
#' @param dataset Either \code{"estimate"} or \code{"st.err"}. Do you want data on estimated 
#' population counts or estimated standard errors?
#' @export
get_pop_by_race <- function(acs, geography, dataset= c("estimate", "st.err")) {
  UseMethod("get_pop_by_race", acs)
}

#' @export
get_pop_by_race.macroACS <- function(acs, geography, dataset= c("estimate", "st.err")) {
  ## check inputs
  validate_get_inputs(acs, geography, dataset)
  ## execute return
  rowid <- get_rowmatch(geography, acs$geography$NAME)
  return(fetch_data(acs, dataset, choice= "pop_by_race")[rowid,])
}

#' @title Get Marital Status for Specified Geography
#' @description Gets data, either estimate or standard error, on a specified geography's population 
#' marital status by gender and age.
#' @param acs An object of class \code{"macroACS"}.
#' @param geography A character vector allowing string matching via \code{\link[base]{grep}} to 
#' a set of specified geographies.
#' @param dataset Either \code{"estimate"} or \code{"st.err"}. Do you want data on estimated 
#' population counts or estimated standard errors?
#' @export
get_marital_status <- function(acs, geography, dataset= c("estimate", "st.err")) {
  UseMethod("get_marital_status", acs)
}

#' @export
get_marital_status.macroACS <- function(acs, geography, dataset= c("estimate", "st.err")) {
  ## check inputs
  validate_get_inputs(acs, geography, dataset)
  ## execute return
  rowid <- get_rowmatch(geography, acs$geography$NAME)
  return(fetch_data(acs, dataset, choice= "marital_status")[rowid,])
}

#' @title Get Educational Attainment for Specified Geography
#' @description Gets data, either estimate or standard error, on a specified geography's population 
#' educational attainment by gender and age.
#' @param acs An object of class \code{"macroACS"}.
#' @param geography A character vector allowing string matching via \code{\link[base]{grep}} to 
#' a set of specified geographies.
#' @param dataset Either \code{"estimate"} or \code{"st.err"}. Do you want data on estimated 
#' population counts or estimated standard errors?
#' @export
get_education <- function(acs, geography, dataset= c("estimate", "st.err")) {
  UseMethod("get_education", acs)
}

#' @export
get_education.macroACS <- function(acs, geography, dataset= c("estimate", "st.err")) {
  ## check inputs
  validate_get_inputs(acs, geography, dataset)
  ## execute return
  rowid <- get_rowmatch(geography, acs$geography$NAME)
  return(fetch_data(acs, dataset, choice= "edu")[rowid,])
}

#' @title Get Nativity Status for Specified Geography
#' @description Gets data, either estimate or standard error, on a specified geography's population 
#' nativity status by age.
#' @param acs An object of class \code{"macroACS"}.
#' @param geography A character vector allowing string matching via \code{\link[base]{grep}} to 
#' a set of specified geographies.
#' @param dataset Either \code{"estimate"} or \code{"st.err"}. Do you want data on estimated 
#' population counts or estimated standard errors?
#' @export
get_nativity <- function(acs, geography, dataset= c("estimate", "st.err")) {
  UseMethod("get_nativity", acs)
}

#' @export
get_nativity.macroACS <- function(acs, geography, dataset= c("estimate", "st.err")) {
  ## check inputs
  validate_get_inputs(acs, geography, dataset)
  ## execute return
  rowid <- get_rowmatch(geography, acs$geography$NAME)
  return(fetch_data(acs, dataset, choice= "nativity")[rowid,])
}

#' @title Get Nativity Status by Income for Specified Geography
#' @description Gets data, either estimate or standard error, on a specified geography's population 
#' nativity status by age and income in the previous 12 months.
#' @param acs An object of class \code{"macroACS"}.
#' @param geography A character vector allowing string matching via \code{\link[base]{grep}} to 
#' a set of specified geographies.
#' @param dataset Either \code{"estimate"} or \code{"st.err"}. Do you want data on estimated 
#' population counts or estimated standard errors?
#' @export
get_nativity_by_income <- function(acs, geography, dataset= c("estimate", "st.err")) {
  UseMethod("get_nativity_by_income", acs)
}

#' @export
get_nativity_by_income.macroACS <- function(acs, geography, dataset= c("estimate", "st.err")) {
  ## check inputs
  validate_get_inputs(acs, geography, dataset)
  ## execute return
  rowid <- get_rowmatch(geography, acs$geography$NAME)
  return(fetch_data(acs, dataset, choice= "by_inc_12mo")[rowid,])
}

#' @title Get Geographic Mobility by Educational Attainment for Specified Geography
#' @description Gets data, either estimate or standard error, on a specified geography's population 
#' geographic mobility by educational attainment.
#' @param acs An object of class \code{"macroACS"}.
#' @param geography A character vector allowing string matching via \code{\link[base]{grep}} to 
#' a set of specified geographies.
#' @param dataset Either \code{"estimate"} or \code{"st.err"}. Do you want data on estimated 
#' population counts or estimated standard errors?
#' @export
get_geographic_mobility <- function(acs, geography, dataset= c("estimate", "st.err")) {
  UseMethod("get_geographic_mobility", acs)
}

#' @export
get_geographic_mobility.macroACS <- function(acs, geography, dataset= c("estimate", "st.err")) {
  ## check inputs
  validate_get_inputs(acs, geography, dataset)
  ## execute return
  rowid <- get_rowmatch(geography, acs$geography$NAME)
  return(fetch_data(acs, dataset, choice= "geo_mob_edu")[rowid,])
}

#' @title Get Individual Income for Specified Geography
#' @description Gets data, either estimate or standard error, on a specified geography's population 
#' income in the past 12 months.
#' @param acs An object of class \code{"macroACS"}.
#' @param geography A character vector allowing string matching via \code{\link[base]{grep}} to 
#' a set of specified geographies.
#' @param dataset Either \code{"estimate"} or \code{"st.err"}. Do you want data on estimated 
#' population counts or estimated standard errors?
#' @export
get_ind_income <- function(acs, geography, dataset= c("estimate", "st.err")) {
  UseMethod("get_ind_income", acs)
}

#' @export
get_ind_income.macroACS <- function(acs, geography, dataset= c("estimate", "st.err")) {
  ## check inputs
  validate_get_inputs(acs, geography, dataset)
  ## execute return
  rowid <- get_rowmatch(geography, acs$geography$NAME)
  return(fetch_data(acs, dataset, choice= "ind_inc")[rowid,])
}

#' @title Get Employment Status for Specified Geography
#' @description Gets data, either estimate or standard error, on a specified geography's population 
#' employment status in the prior 12 months by gender and age.
#' @param acs An object of class \code{"macroACS"}.
#' @param geography A character vector allowing string matching via \code{\link[base]{grep}} to 
#' a set of specified geographies.
#' @param dataset Either \code{"estimate"} or \code{"st.err"}. Do you want data on estimated 
#' population counts or estimated standard errors?
#' @export
get_employment_status <- function(acs, geography, dataset= c("estimate", "st.err")) {
  UseMethod("get_employment_status", acs)
}

#' @export
get_employment_status.macroACS <- function(acs, geography, dataset= c("estimate", "st.err")) {
  ## check inputs
  validate_get_inputs(acs, geography, dataset)
  ## execute return
  rowid <- get_rowmatch(geography, acs$geography$NAME)
  return(fetch_data(acs, dataset, choice= "emp_status")[rowid,])
}

#' @title Get Poverty Status for Specified Geography
#' @description Gets data, either estimate or standard error, on a specified geography's population 
#' poverty status in the prior 12 months by gender and employment status.
#' @param acs An object of class \code{"macroACS"}.
#' @param geography A character vector allowing string matching via \code{\link[base]{grep}} to 
#' a set of specified geographies.
#' @param dataset Either \code{"estimate"} or \code{"st.err"}. Do you want data on estimated 
#' population counts or estimated standard errors?
#' @export
get_poverty_status <- function(acs, geography, dataset= c("estimate", "st.err")) {
  UseMethod("get_poverty_status", acs)
}

#' @export
get_poverty_status.macroACS <- function(acs, geography, dataset= c("estimate", "st.err")) {
  ## check inputs
  validate_get_inputs(acs, geography, dataset)
  ## execute return
  rowid <- get_rowmatch(geography, acs$geography$NAME)
  return(fetch_data(acs, dataset, choice= "pov_status1")[rowid,])
}

