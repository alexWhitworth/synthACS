
##---------------------------------------------------------
## CHECK IF OBJECT IS MEMBER OF CLASS
##---------------------------------------------------------

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


#' @title Check synthACS class
#' @description Function that checks if the target object is a \code{synthACS} object.
#' @param x any R object.
#' @return Returns \code{TRUE} if its argument has class "synthACS" among its classes and
#' \code{FALSE} otherwise.
#' @export
is.synthACS <- function(x) {
  inherits(x, "synthACS")
}

##---------------------------------------------------------
## Generics for class macroACS
##---------------------------------------------------------
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
    if (any(nchar(geography) < 4) & geography != "*") 
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
#' a set of specified geographies. All values may be specified by \code{"*"}.
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
  if (geography != "*") {
    rowid <- get_rowmatch(geography, acs$geography$NAME)
    return(fetch_data(acs, dataset, choice= "age_by_sex")[rowid,])
  } else {
    return(fetch_data(acs, dataset, choice= "age_by_sex"))
  }
}


#' @title Get Population Data by Race for Specified Geography
#' @description Gets data, either estimate or standard error, on a specified geography's population 
#' by race.
#' @param acs An object of class \code{"macroACS"}.
#' @param geography A character vector allowing string matching via \code{\link[base]{grep}} to 
#' a set of specified geographies. All values may be specified by \code{"*"}.
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
  if (geography != "*") {
    rowid <- get_rowmatch(geography, acs$geography$NAME)
    return(fetch_data(acs, dataset, choice= "pop_by_race")[rowid,])
  } else {
    return(fetch_data(acs, dataset, choice= "pop_by_race"))
  }
}

#' @title Get Marital Status for Specified Geography
#' @description Gets data, either estimate or standard error, on a specified geography's population 
#' marital status by gender and age.
#' @param acs An object of class \code{"macroACS"}.
#' @param geography A character vector allowing string matching via \code{\link[base]{grep}} to 
#' a set of specified geographies. All values may be specified by \code{"*"}.
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
  if (geography != "*") {
    rowid <- get_rowmatch(geography, acs$geography$NAME)
    return(fetch_data(acs, dataset, choice= "marital_status")[rowid,])
  } else {
    return(fetch_data(acs, dataset, choice= "marital_status"))
  }
}

#' @title Get Educational Attainment for Specified Geography
#' @description Gets data, either estimate or standard error, on a specified geography's population 
#' educational attainment by gender and age.
#' @param acs An object of class \code{"macroACS"}.
#' @param geography A character vector allowing string matching via \code{\link[base]{grep}} to 
#' a set of specified geographies. All values may be specified by \code{"*"}.
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
  if (geography != "*") {
    rowid <- get_rowmatch(geography, acs$geography$NAME)
    return(fetch_data(acs, dataset, choice= "edu")[rowid,])
  } else {
    return(fetch_data(acs, dataset, choice= "edu"))
  }
}

#' @title Get Nativity Status for Specified Geography
#' @description Gets data, either estimate or standard error, on a specified geography's population 
#' nativity status by age.
#' @param acs An object of class \code{"macroACS"}.
#' @param geography A character vector allowing string matching via \code{\link[base]{grep}} to 
#' a set of specified geographies. All values may be specified by \code{"*"}.
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
  if (geography != "*") {
    rowid <- get_rowmatch(geography, acs$geography$NAME)
    return(fetch_data(acs, dataset, choice= "nativity")[rowid,])
  } else {
    return(fetch_data(acs, dataset, choice= "nativity"))
  }
}

#' @title Get Nativity Status by Income for Specified Geography
#' @description Gets data, either estimate or standard error, on a specified geography's population 
#' nativity status by age and income in the previous 12 months.
#' @param acs An object of class \code{"macroACS"}.
#' @param geography A character vector allowing string matching via \code{\link[base]{grep}} to 
#' a set of specified geographies. All values may be specified by \code{"*"}.
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
  if (geography != "*") {
    rowid <- get_rowmatch(geography, acs$geography$NAME)
    return(fetch_data(acs, dataset, choice= "by_inc_12mo")[rowid,])
  } else {
    return(fetch_data(acs, dataset, choice= "by_inc_12mo"))
  }
}

#' @title Get Geographic Mobility by Educational Attainment for Specified Geography
#' @description Gets data, either estimate or standard error, on a specified geography's population 
#' geographic mobility by educational attainment.
#' @param acs An object of class \code{"macroACS"}.
#' @param geography A character vector allowing string matching via \code{\link[base]{grep}} to 
#' a set of specified geographies. All values may be specified by \code{"*"}.
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
  if (geography != "*") {
    rowid <- get_rowmatch(geography, acs$geography$NAME)
    return(fetch_data(acs, dataset, choice= "geo_mob_edu")[rowid,])
  } else {
    return(fetch_data(acs, dataset, choice= "geo_mob_edu"))
  }
}

#' @title Get Individual Income for Specified Geography
#' @description Gets data, either estimate or standard error, on a specified geography's population 
#' income in the past 12 months.
#' @param acs An object of class \code{"macroACS"}.
#' @param geography A character vector allowing string matching via \code{\link[base]{grep}} to 
#' a set of specified geographies. All values may be specified by \code{"*"}.
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
  if (geography != "*") {
    rowid <- get_rowmatch(geography, acs$geography$NAME)
    return(fetch_data(acs, dataset, choice= "ind_inc")[rowid,])
  } else {
    return(fetch_data(acs, dataset, choice= "ind_inc"))
  }
}

#' @title Get Employment Status for Specified Geography
#' @description Gets data, either estimate or standard error, on a specified geography's population 
#' employment status in the prior 12 months by gender and age.
#' @param acs An object of class \code{"macroACS"}.
#' @param geography A character vector allowing string matching via \code{\link[base]{grep}} to 
#' a set of specified geographies. All values may be specified by \code{"*"}.
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
#' a set of specified geographies. All values may be specified by \code{"*"}.
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
  if (geography != "*") {
    rowid <- get_rowmatch(geography, acs$geography$NAME)
    return(fetch_data(acs, dataset, choice= "pov_status1")[rowid,])
  } else {
    return(fetch_data(acs, dataset, choice= "pov_status1"))
  }
}

##---------------------------------------------------------
## GENERICS FOR CLASS "macro_micro" -- SPECIFICALLY ADDING CONSTRAINT LISTS
## FOR THE 10 DEFAULT SYNTHETIC ATTRIBUTES / VARIABLES
##---------------------------------------------------------

# helper function to save typing
# unsures that constraint population equals macro-population (for a geography)
# is used in synthACS-methods when method= "synthetic" (see below functions)
equal_constraint_populations <- function(constr_vec, geo_pop) {
  if (sum(constr_vec) == geo_pop) {return(constr_vec)}
  else if (sum(constr_vec) > geo_pop){ # equality from max
    constr_vec[max(constr_vec)] <- constr_vec[max(constr_vec)] + (geo_pop - sum(constr_vec))
    return(constr_vec)
  } else if (sum(constr_vec) < geo_pop) {
    constr_vec[min(constr_vec)] <- constr_vec[min(constr_vec)] + (geo_pop - sum(constr_vec))
    return(constr_vec)
  }
}


#' @title Add gender constraint to a set of geographies
#' @description Add a new gender constraint to the mapping between a a set of macro datasets and a 
#' matching set of micro dataset (supplied as class 'synthACS').
#' @param obj An object of class \code{"synthACS"}.
#' @param method One of \code{c("synthetic", "macro.table")}. Specifying \code{"synthetic"} indicates
#' that constraints are built by marginalizing the synthetic micro datasets. Specifying 
#' \code{"macro.table"} indicates that the constraints are build from the data in the base ACS tables.
#' @export
all_geog_constraint_gender <- function(obj, method= c("synthetic", "macro.table")) {
  UseMethod("all_geog_constraint_gender", obj)
}

#' @export
all_geog_constraint_gender.synthACS <- function(obj, method= c("synthetic", "macro.table")) {
  method= match.arg(method, several.ok= FALSE)
  
  # A - synthetic
  if (method == "synthetic") {
    g_constraint <- lapply(obj, function(l) {
      # marginalize probability vector by attribute, normalize by total_pop
      constr_vec <- round(tapply(l[[2]]$p, l[[2]]$gender, sum) * l[[1]]$age_by_sex[1], 0)
      # check that population matches macro pop and return
      return(equal_constraint_populations(constr_vec, l[[1]]$age_by_sex[1]))
    })
  }
  # B - macro.table
  else if (method == "macro.table") {
    g_constraint <- lapply(obj, function(l) {
      constr_vec <- l[[1]]$age_by_sex[2:3]
      names(constr_vec) <- c("Male", "Female")
      return(constr_vec)
    })
  }
  return(g_constraint)
}

