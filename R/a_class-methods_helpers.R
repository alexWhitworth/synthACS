
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
  if (geography != "*") {
    rowid <- get_rowmatch(geography, acs$geography$NAME)
    return(fetch_data(acs, dataset, choice= "emp_status")[rowid,])
  } else {
    return(fetch_data(acs, dataset, choice= "emp_status"))
  }
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
  if (sum(constr_vec, na.rm=TRUE) == geo_pop) {return(constr_vec)}
  else if (sum(constr_vec, na.rm=TRUE) > geo_pop){ # equality from max
    idx <- which.max(constr_vec)
    constr_vec[idx] <- constr_vec[idx] + (geo_pop - sum(constr_vec, na.rm=TRUE))
    return(constr_vec)
  } else if (sum(constr_vec, na.rm=TRUE) < geo_pop) { # equality from min
    idx <- which.min(constr_vec)
    constr_vec[idx] <- constr_vec[idx] + (geo_pop - sum(constr_vec, na.rm=TRUE))
    return(constr_vec)
  }
}


#' @title Create gender constraint list to a set of geographies
#' @description Create a new gender constraint list to the mapping between a a set of macro datasets and a 
#' matching set of micro dataset (supplied as class 'synthACS').
#' @param obj An object of class \code{"synthACS"}.
#' @param method One of \code{c("synthetic", "macro.table")}. Specifying \code{"synthetic"} indicates
#' that constraints are built by marginalizing the synthetic micro datasets. Specifying 
#' \code{"macro.table"} indicates that the constraints are build from the data in the base ACS tables.
#' @seealso \code{\link{all_geogs_add_constraint}}
#' @export
all_geog_constraint_gender <- function(obj, method= c("synthetic", "macro.table")) {
  UseMethod("all_geog_constraint_gender", obj)
}

#' @export
all_geog_constraint_gender.synthACS <- function(obj, method= c("synthetic", "macro.table")) {
  method= match.arg(method, several.ok= FALSE)
  
  # A - synthetic
  if (method == "synthetic") {
    constraint <- lapply(obj, function(l) {
      # marginalize probability vector by attribute, normalize by total_pop
      constr_vec <- as.numeric(round(tapply(l[[2]]$p, l[[2]]$gender, sum) * l[[1]]$age_by_sex[1], 0))
      constr_vec <- ifelse(is.na(constr_vec), 0, constr_vec)
      names(constr_vec) <- c("Male", "Female")
      # check that population matches macro pop and return
      return(equal_constraint_populations(constr_vec, l[[1]]$age_by_sex[1]))
    })
  }
  # B - macro.table
  else if (method == "macro.table") {
    constraint <- lapply(obj, function(l) {
      constr_vec <- l[[1]]$age_by_sex[2:3]
      names(constr_vec) <- c("Male", "Female")
      return(equal_constraint_populations(constr_vec, l[[1]]$age_by_sex[1]))
    })
  }
  return(constraint)
}


#' @title Create age constraint list to a set of geographies
#' @description Create a new age constraint list to the mapping between a a set of macro datasets and a 
#' matching set of micro dataset (supplied as class 'synthACS').
#' @param obj An object of class \code{"synthACS"}.
#' @param method One of \code{c("synthetic", "macro.table")}. Specifying \code{"synthetic"} indicates
#' that constraints are built by marginalizing the synthetic micro datasets. Specifying 
#' \code{"macro.table"} indicates that the constraints are build from the data in the base ACS tables.
#' @seealso \code{\link{all_geogs_add_constraint}}
#' @export
all_geog_constraint_age <- function(obj, method= c("synthetic", "macro.table")) {
  UseMethod("all_geog_constraint_age", obj)
}

#' @export
all_geog_constraint_age.synthACS <- function(obj, method= c("synthetic", "macro.table")) {
  method= match.arg(method, several.ok= FALSE)
  
  # A - synthetic
  if (method == "synthetic") {
    constraint <- lapply(obj, function(l) {
      # marginalize probability vector by attribute, normalize by total_pop
      constr_vec <- as.numeric(round(tapply(l[[2]]$p, l[[2]]$age, sum) * l[[1]]$age_by_sex[1], 0))
      constr_vec <- ifelse(is.na(constr_vec), 0, constr_vec)
      names(constr_vec) <- levels(l[[2]]$age)
      # check that population matches macro pop and return
      return(equal_constraint_populations(constr_vec, l[[1]]$age_by_sex[1]))
    })
  }
  # B - macro.table
  else if (method == "macro.table") {
    constraint <- lapply(obj, function(l) {
      constr_vec <- l[[1]]$age_by_sex[-c(1:3)]
      constr_vec <- apply(cbind(constr_vec[1:16], constr_vec[17:32]), 1, sum)
      names(constr_vec) <- levels(l[[2]]$age)
      return(equal_constraint_populations(constr_vec, l[[1]]$age_by_sex[1]))
    })
  }
  return(constraint)
}


#' @title Create marital status constraint list to a set of geographies
#' @description Create a new marital status constraint list to the mapping between a a set of macro 
#' datasets and a matching set of micro dataset (supplied as class 'synthACS').
#' @param obj An object of class \code{"synthACS"}.
#' @param method One of \code{c("synthetic", "macro.table")}. Specifying \code{"synthetic"} indicates
#' that constraints are built by marginalizing the synthetic micro datasets. Specifying 
#' \code{"macro.table"} indicates that the constraints are build from the data in the base ACS tables.
#' @seealso \code{\link{all_geogs_add_constraint}}
#' @export
all_geog_constraint_marital_status <- function(obj, method= c("synthetic", "macro.table")) {
  UseMethod("all_geog_constraint_marital_status", obj)
}

#' @export
all_geog_constraint_marital_status.synthACS <- function(obj, method= c("synthetic", "macro.table")) {
  method= match.arg(method, several.ok= FALSE)
  
  # A - synthetic
  if (method == "synthetic") {
    constraint <- lapply(obj, function(l) {
      # marginalize probability vector by attribute, normalize by total_pop
      constr_vec <- as.numeric(round(tapply(l[[2]]$p, l[[2]]$marital_status, sum) * l[[1]]$age_by_sex[1], 0))
      constr_vec <- ifelse(is.na(constr_vec), 0, constr_vec)
      names(constr_vec) <- levels(l[[2]]$marital_status)
      # check that population matches macro pop and return
      return(equal_constraint_populations(constr_vec, l[[1]]$age_by_sex[1]))
    })
  }
  # B - macro.table
  else if (method == "macro.table") {
    constraint <- lapply(obj, function(l) { 
      # done via name pattern matching and summing across age brackets
      v <- l[[1]]$marital_status[-1]
      nv_mar <- sum(v[which(grepl("nvr_married", names(v)))])
      mar <- v[which(grepl("married", names(v)))]
      mar <- sum(mar[which(!grepl("nvr_married", names(mar)))])
      mar_apart <- sum(v[which(grepl("mar_apart", names(v)))])
      div <- sum(v[which(grepl("divor", names(v)))])
      wid <- sum(v[which(grepl("widow", names(v)))])
      # under 15 not accounted for in base table; assume all never married 
      # (matches assumptions from synth_data_mar)
      pop_u15 <- sum(l$macro_constraints$age_by_sex[c(4,20)])
      
      constr_vec <- c("never_mar"= nv_mar + pop_u15, "married"= mar, "mar_apart"=mar_apart, "widowed"= wid, "divorced"=div)
      return(equal_constraint_populations(constr_vec, l[[1]]$age_by_sex[1]))
    })
  }
  return(constraint)
}


#' @title Create educational attainment constraint list to a set of geographies
#' @description Create a new educational attainment constraint list to the mapping between a a set 
#' of macro datasets and a matching set of micro dataset (supplied as class 'synthACS').
#' @param obj An object of class \code{"synthACS"}.
#' @param method One of \code{c("synthetic", "macro.table")}. Specifying \code{"synthetic"} indicates
#' that constraints are built by marginalizing the synthetic micro datasets. Specifying 
#' \code{"macro.table"} indicates that the constraints are build from the data in the base ACS tables.
#' @seealso \code{\link{all_geogs_add_constraint}}
#' @export
all_geog_constraint_edu <- function(obj, method= c("synthetic", "macro.table")) {
  UseMethod("all_geog_constraint_edu", obj)
}

#' @export
all_geog_constraint_edu.synthACS <- function(obj, method= c("synthetic", "macro.table")) {
  method= match.arg(method, several.ok= FALSE)
  
  # A - synthetic
  if (method == "synthetic") {
    constraint <- lapply(obj, function(l) {
      # marginalize probability vector by attribute, normalize by total_pop
      constr_vec <- as.numeric(round(tapply(l[[2]]$p, l[[2]]$edu_attain, sum) * l[[1]]$age_by_sex[1], 0))
      constr_vec <- ifelse(is.na(constr_vec), 0, constr_vec)
      names(constr_vec) <- levels(l[[2]]$edu_attain)
      # check that population matches macro pop and return
      return(equal_constraint_populations(constr_vec, l[[1]]$age_by_sex[1]))
    })
  }
  # B - macro.table
  else if (method == "macro.table") {
    constraint <- lapply(obj, function(l) {
      # done via name pattern matching and summing across age brackets
      v <- l[[1]]$edu[-c(1:3)]
      lt_hs <- sum(v[which(grepl("lt_hs", names(v)))])
      some_hs <- sum(v[which(grepl("some_hs", names(v)))])
      hs_grad <- sum(v[which(grepl("hs_grad", names(v)))])
      some_col <- sum(v[which(grepl("some_col", names(v)))])
      assoc_dec <- sum(v[which(grepl("assoc_dec", names(v)))])
      ba_deg <- sum(v[which(grepl("ba_deg", names(v)))])
      grad_deg <- sum(v[which(grepl("grad_deg", names(v)))])
      # under 18 not accounted for in base table 
      # assumptions from synth_data_edu:
        # under 15 == less than high school education
        # 15-17 == some high school education
      pop_u15 <- sum(l$macro_constraints$age_by_sex[c(4,20)])
      pop_15_17 <- sum(l$macro_constraints$age_by_sex[c(5,21)])
      
      constr_vec <- c("lt_hs"=  lt_hs + pop_u15, "some_hs"= some_hs + pop_15_17,
                      "hs_grad"= hs_grad, "some_col"= some_col, "assoc_dec"= assoc_dec,
                      "ba_deg"= ba_deg, "grad_deg"= grad_deg)
      
      return(equal_constraint_populations(constr_vec, l[[1]]$age_by_sex[1]))
    })
  }
  return(constraint)
}


#' @title Create employment status constraint list to a set of geographies
#' @description Create a new employment status constraint list to the mapping between a a set 
#' of macro datasets and a matching set of micro dataset (supplied as class 'synthACS').
#' @param obj An object of class \code{"synthACS"}.
#' @param method One of \code{c("synthetic", "macro.table")}. Specifying \code{"synthetic"} indicates
#' that constraints are built by marginalizing the synthetic micro datasets. Specifying 
#' \code{"macro.table"} indicates that the constraints are build from the data in the base ACS tables.
#' @seealso \code{\link{all_geogs_add_constraint}}
#' @export
all_geog_constraint_employment <- function(obj, method= c("synthetic", "macro.table")) {
  UseMethod("all_geog_constraint_employment", obj)
}

#' @export
all_geog_constraint_employment.synthACS <- function(obj, method= c("synthetic", "macro.table")) {
  method= match.arg(method, several.ok= FALSE)
  
  # A - synthetic
  if (method == "synthetic") {
    constraint <- lapply(obj, function(l) {
      # marginalize probability vector by attribute, normalize by total_pop
      constr_vec <- as.numeric(round(tapply(l[[2]]$p, l[[2]]$emp_status, sum) * l[[1]]$age_by_sex[1], 0))
      constr_vec <- ifelse(is.na(constr_vec), 0, constr_vec)
      names(constr_vec) <- levels(l[[2]]$emp_status)
      # check that population matches macro pop and return
      return(equal_constraint_populations(constr_vec, l[[1]]$age_by_sex[1]))
    })
  }
  # B - macro.table
  else if (method == "macro.table") {
    constraint <- lapply(obj, function(l) {
      # done via name pattern matching and summing across age brackets
      v <- l[[1]]$emp_status[-1]
      employed <- sum(v[which(grepl("employed", names(v)))])
      unemp <- sum(v[which(grepl("unemp", names(v)))])
      no_labor <- sum(v[which(grepl("no_labor", names(v)))])
      # under 16 not accounted for in base table 
      # assumptions from synth_data_emp:
      # assumptions: under 15 == not in labor force
      # assumptions: 15-17 best represented by 16-19 (will ignore 16 yr olds and let pop-match fix it)
      pop_u15 <- sum(l$macro_constraints$age_by_sex[c(4,20)])
      
      constr_vec <- c("not_in_labor_force"= no_labor + pop_u15, "employed"= employed, "unemployed"= unemp)
      
      return(equal_constraint_populations(constr_vec, l[[1]]$age_by_sex[1]))
    })
  }
  return(constraint)
}


#' @title Create nativity status constraint list to a set of geographies
#' @description Create a new nativity status constraint list to the mapping between a a set 
#' of macro datasets and a matching set of micro dataset (supplied as class 'synthACS').
#' @param obj An object of class \code{"synthACS"}.
#' @param method One of \code{c("synthetic", "macro.table")}. Specifying \code{"synthetic"} indicates
#' that constraints are built by marginalizing the synthetic micro datasets. Specifying 
#' \code{"macro.table"} indicates that the constraints are build from the data in the base ACS tables.
#' @seealso \code{\link{all_geogs_add_constraint}}
#' @export
all_geog_constraint_nativity <- function(obj, method= c("synthetic", "macro.table")) {
  UseMethod("all_geog_constraint_nativity", obj)
}

#' @export
all_geog_constraint_nativity.synthACS <- function(obj, method= c("synthetic", "macro.table")) {
  method= match.arg(method, several.ok= FALSE)
  
  # A - synthetic
  if (method == "synthetic") {
    constraint <- lapply(obj, function(l) {
      # marginalize probability vector by attribute, normalize by total_pop
      constr_vec <- as.numeric(round(tapply(l[[2]]$p, l[[2]]$nativity, sum) * l[[1]]$age_by_sex[1], 0))
      constr_vec <- ifelse(is.na(constr_vec), 0, constr_vec)
      names(constr_vec) <- levels(l[[2]]$nativity)
      # check that population matches macro pop and return
      return(equal_constraint_populations(constr_vec, l[[1]]$age_by_sex[1]))
    })
  }
  # B - macro.table
  else if (method == "macro.table") {
    constraint <- lapply(obj, function(l) {
      # done via name pattern matching and summing across age brackets
      v <- l[[1]]$nativity[-c(1:10)]
      born_other_state <- sum(v[which(grepl("born_out_state", names(v)))])
      born_out_us <- sum(v[which(grepl("born_st_res", names(v)))])
      born_state_residence <- sum(v[which(grepl("no_labor", names(v)))])
      foreigner <- sum(v[which(grepl("foreigner", names(v)))])
      
      constr_vec <- c("born_other_state"= born_other_state, "born_out_us"= born_out_us, 
                      "born_state_residence"= born_state_residence, "foreigner"= foreigner)
      
      return(equal_constraint_populations(constr_vec, l[[1]]$age_by_sex[1]))
    })
  }
  return(constraint)
}

#' @title Create poverty status constraint list to a set of geographies
#' @description Create a new poverty status constraint list to the mapping between a a set 
#' of macro datasets and a matching set of micro dataset (supplied as class 'synthACS').
#' @param obj An object of class \code{"synthACS"}.
#' @param method One of \code{c("synthetic", "macro.table")}. Specifying \code{"synthetic"} indicates
#' that constraints are built by marginalizing the synthetic micro datasets. Specifying 
#' \code{"macro.table"} indicates that the constraints are build from the data in the base ACS tables.
#' @seealso \code{\link{all_geogs_add_constraint}}
#' @export
all_geog_constraint_poverty <- function(obj, method= c("synthetic", "macro.table")) {
  UseMethod("all_geog_constraint_poverty", obj)
}

#' @export
all_geog_constraint_poverty.synthACS <- function(obj, method= c("synthetic", "macro.table")) {
  method= match.arg(method, several.ok= FALSE)
  
  # A - synthetic
  if (method == "synthetic") {
    constraint <- lapply(obj, function(l) {
      # marginalize probability vector by attribute, normalize by total_pop
      constr_vec <- as.numeric(round(tapply(l[[2]]$p, l[[2]]$pov_status, sum) * l[[1]]$age_by_sex[1], 0))
      constr_vec <- ifelse(is.na(constr_vec), 0, constr_vec)
      names(constr_vec) <- levels(l[[2]]$pov_status)
      # check that population matches macro pop and return
      return(equal_constraint_populations(constr_vec, l[[1]]$age_by_sex[1]))
    })
  }
  # B - macro.table
  else if (method == "macro.table") {
    constraint <- lapply(obj, function(l) {
      # done via name pattern matching and summing across age brackets
      v <- l[[1]]$pov_status1[-1]
      at_above <- sum(v[which(grepl("gt_eq_pov", names(v)))])
      below <- sum(v[which(grepl("lt_pov", names(v)))])
      # synth_pov: (individuals 15 years and over for whom poverty status is determined)
      # proportionally allocate u15
      pop_u15 <- sum(l$macro_constraints$age_by_sex[c(4,20)])
      
      constr_vec <- c("at_above_pov_level"= at_above + round(pop_u15 * at_above / (at_above + below) ,0), 
                      "below_pov_level"= below + round(pop_u15 * below / (at_above + below) ,0))
      
      return(equal_constraint_populations(constr_vec, l[[1]]$age_by_sex[1]))
    })
  }
  return(constraint)
}

#' @title Create geographic mobility constraint list to a set of geographies
#' @description Create a new geographic mobility constraint list to the mapping between a a set 
#' of macro datasets and a matching set of micro dataset (supplied as class 'synthACS').
#' @param obj An object of class \code{"synthACS"}.
#' @param method One of \code{c("synthetic", "macro.table")}. Specifying \code{"synthetic"} indicates
#' that constraints are built by marginalizing the synthetic micro datasets. Specifying 
#' \code{"macro.table"} indicates that the constraints are build from the data in the base ACS tables.
#' @seealso \code{\link{all_geogs_add_constraint}}
#' @export
all_geog_constraint_geog_mob <- function(obj, method= c("synthetic", "macro.table")) {
  UseMethod("all_geog_constraint_geog_mob", obj)
}

#' @export
all_geog_constraint_geog_mob.synthACS <- function(obj, method= c("synthetic", "macro.table")) {
  method= match.arg(method, several.ok= FALSE)
  
  # A - synthetic
  if (method == "synthetic") {
    constraint <- lapply(obj, function(l) {
      # marginalize probability vector by attribute, normalize by total_pop
      constr_vec <- as.numeric(round(tapply(l[[2]]$p, l[[2]]$geog_mobility, sum) * l[[1]]$age_by_sex[1], 0))
      constr_vec <- ifelse(is.na(constr_vec), 0, constr_vec)
      names(constr_vec) <- levels(l[[2]]$geog_mobility)
      # check that population matches macro pop and return
      return(equal_constraint_populations(constr_vec, l[[1]]$age_by_sex[1]))
    })
  }
  # B - macro.table
  else if (method == "macro.table") {
    constraint <- lapply(obj, function(l) {
      # done via name pattern matching and summing across age brackets
      v <- l[[1]]$geo_mob_edu
      CNT <- v[1]
      same_house <- sum(v[which(grepl("same_house_all", names(v)))])
      same_cty   <- sum(v[which(grepl("same_cnty_all", names(v)))])
      same_st    <- sum(v[which(grepl("same_st_all", names(v)))])
      diff_st    <- sum(v[which(grepl("diff_st_all", names(v)))])
      abroad     <- sum(v[which(grepl("abroad_all", names(v)))])
      
      # v <- l[[1]]$edu[4:10] + v <- l[[1]]$edu[39:45] #18-24 only
      # lt_hs <- sum(v[which(grepl("lt_hs", names(v)))])
      # some_hs <- sum(v[which(grepl("some_hs", names(v)))])
      # hs_grad <- sum(v[which(grepl("hs_grad", names(v)))])
      # some_col <- sum(v[which(grepl("some_col", names(v)))])
      # assoc_dec <- sum(v[which(grepl("assoc_dec", names(v)))])
      # ba_deg <- sum(v[which(grepl("ba_deg", names(v)))])
      # grad_deg <- sum(v[which(grepl("grad_deg", names(v)))])
      
      # synth_geomob: (Universe:  Population 25 years and over in the United States)
      # ALLOCATE ALL PROPORTIONALLY BASED ON EDU
      # u15 = less than high school 
      # 15_17 = some HS 
      # 18_24 = have data; but just proportionally allocate equally
      pop_u15 <- sum(l$macro_constraints$age_by_sex[c(4,20)])
      pop_15_17 <- sum(l$macro_constraints$age_by_sex[c(5,21)])
      pop_18_24 <- sum(l$macro_constraints$age_by_sex[c(6,22)])
      
      constr_vec <- c("diff state"= diff_st + round(pop_u15 * v[21] / same_house, 0) + 
                        round(pop_15_17 * v[22] / diff_st, 0) + round(pop_18_24 * diff_st / CNT, 0), 
                      "moved from abroad"= abroad + round(pop_u15 * v[27] / same_house, 0) + 
                        round(pop_15_17 * v[28] / abroad, 0) + round(pop_18_24 * abroad / CNT, 0), 
                      "same county"= same_cty + round(pop_u15 * v[9] / same_house, 0) + 
                        round(pop_15_17 * v[10] / same_cty, 0) + round(pop_18_24 * same_cty / CNT, 0),
                      "same house"= same_house + round(pop_u15 * v[3] / same_house, 0) + 
                        round(pop_15_17 * v[4] / same_house, 0) + round(pop_18_24 * same_house / CNT, 0), 
                      "same state"= same_st + round(pop_u15 * v[15] / same_house, 0) + 
                        round(pop_15_17 * v[16] / same_st, 0) + round(pop_18_24 * same_st / CNT, 0)) 
      
      constr_vec <- ifelse(is.na(constr_vec), 0, constr_vec)
      names(constr_vec) <- levels(l[[2]]$geog_mobility)
      return(equal_constraint_populations(constr_vec, l[[1]]$age_by_sex[1]))
    })
  }
  return(constraint)
}

#' @title Create individual income constraint list to a set of geographies
#' @description Create a new individual income constraint list to the mapping between a a set 
#' of macro datasets and a matching set of micro dataset (supplied as class 'synthACS').
#' @param obj An object of class \code{"synthACS"}.
#' @param method One of \code{c("synthetic", "macro.table")}. Specifying \code{"synthetic"} indicates
#' that constraints are built by marginalizing the synthetic micro datasets. Specifying 
#' \code{"macro.table"} indicates that the constraints are build from the data in the base ACS tables.
#' @seealso \code{\link{all_geogs_add_constraint}}
#' @export
all_geog_constraint_income <- function(obj, method= c("synthetic", "macro.table")) {
  UseMethod("all_geog_constraint_income", obj)
}

#' @export
all_geog_constraint_income.synthACS <- function(obj, method= c("synthetic", "macro.table")) {
  method= match.arg(method, several.ok= FALSE)
  
  # A - synthetic
  if (method == "synthetic") {
    constraint <- lapply(obj, function(l) {
      # marginalize probability vector by attribute, normalize by total_pop
      constr_vec <- as.numeric(round(tapply(l[[2]]$p, l[[2]]$ind_income, sum) * l[[1]]$age_by_sex[1], 0))
      constr_vec <- ifelse(is.na(constr_vec), 0, constr_vec)
      names(constr_vec) <- levels(l[[2]]$ind_income)
      # check that population matches macro pop and return
      return(equal_constraint_populations(constr_vec, l[[1]]$age_by_sex[1]))
    })
  }
  # B - macro.table
  else if (method == "macro.table") {
    constraint <- lapply(obj, function(l) {
      # done via name pattern matching and summing across age brackets
      v <- l[[1]]$by_inc_12mo
      # synth_inc: (Universe:  Population 15 years and over in the United States)
      # proportionally allocate u15 to under 35k
      pop_u15 <- sum(l$macro_constraints$age_by_sex[c(4,20)])
      
      constr_vec <- c("1_lt10k"= v[3] + round(pop_u15 * v[3] / sum(v[2:6]), 0),
                      "10k_lt15k"= v[4] + round(pop_u15 * v[4] / sum(v[2:6]), 0),
                      "15k_lt25k"= v[5] + round(pop_u15 * v[5] / sum(v[2:6]), 0),
                      "25k_lt35k"= v[6] + round(pop_u15 * v[6] / sum(v[2:6]), 0),
                      "35k_lt50k"= v[7],
                      "50k_lt65k"= v[8],
                      "65k_lt75k"= v[9],
                      "gt75k"= v[10],
                      "no_income"= v[2] + round(pop_u15 * v[2] / sum(v[2:6]) , 0))
      
      constr_vec <- ifelse(is.na(constr_vec), 0, constr_vec)
      names(constr_vec) <- levels(l[[2]]$ind_income)
      return(equal_constraint_populations(constr_vec, l[[1]]$age_by_sex[1]))
    })
  }
  return(constraint)
}

#' @title Create race constraint list to a set of geographies
#' @description Create a new race constraint list to the mapping between a a set 
#' of macro datasets and a matching set of micro dataset (supplied as class 'synthACS').
#' @param obj An object of class \code{"synthACS"}.
#' @param method One of \code{c("synthetic", "macro.table")}. Specifying \code{"synthetic"} indicates
#' that constraints are built by marginalizing the synthetic micro datasets. Specifying 
#' \code{"macro.table"} indicates that the constraints are build from the data in the base ACS tables.
#' @seealso \code{\link{all_geogs_add_constraint}}
#' @export
all_geog_constraint_race <- function(obj, method= c("synthetic", "macro.table")) {
  UseMethod("all_geog_constraint_race", obj)
}

#' @export
all_geog_constraint_race.synthACS <- function(obj, method= c("synthetic", "macro.table")) {
  method= match.arg(method, several.ok= FALSE)
  
  # A - synthetic
  if (method == "synthetic") {
    constraint <- lapply(obj, function(l) {
      # marginalize probability vector by attribute, normalize by total_pop
      constr_vec <- as.numeric(round(tapply(l[[2]]$p, l[[2]]$race, sum, na.rm=T) * l[[1]]$age_by_sex[1], 0))
      constr_vec <- ifelse(is.na(constr_vec), 0, constr_vec)
      names(constr_vec) <- levels(l[[2]]$race)
      # check that population matches macro pop and return
      return(equal_constraint_populations(constr_vec, l[[1]]$age_by_sex[1]))
    })
  }
  # B - macro.table
  else if (method == "macro.table") {
    constraint <- lapply(obj, function(l) {
      # done via name pattern matching and summing across age brackets
      v <- l[[1]]$pop_by_race
      # race includes breakout by white alone v hispanic white... need to reduce counts to match
      # total (v[1] vs sum(v[3:10]))
      white   <- round(sum(v[which(grepl("total_white_alone", names(v)))]) * v[1] / sum(v[3:10]), 0)
      black   <- round(sum(v[which(grepl("total_black", names(v)))]) * v[1] / sum(v[3:10]), 0)
      hisp    <- round(sum(v[which(grepl("total_hisp_lat", names(v)))]) * v[1] / sum(v[3:10]), 0)
      asian   <- round(sum(v[which(grepl("total_asian", names(v)))]) * v[1] / sum(v[3:10]), 0)
      native  <- round(sum(v[which(grepl("total_nat_amer", names(v)))]) * v[1] / sum(v[3:10]), 0)
      pac     <- round(sum(v[which(grepl("total_pac_isl", names(v)))]) * v[1] / sum(v[3:10]), 0)
      more2   <- round(sum(v[which(grepl("total_2p_races", names(v)))]) * v[1] / sum(v[3:10]), 0)

      constr_vec <- c("asian"= asian,
                      "black, afr amer"= black,
                      "hispanic, latino"= hisp,
                      "native amer"= native,
                      "pacific islander"= pac,
                      "two or more races"= more2,
                      "white alone"=  white)

      constr_vec <- ifelse(is.na(constr_vec), 0, constr_vec)
      names(constr_vec) <- levels(l[[2]]$race)
      return(equal_constraint_populations(constr_vec, l[[1]]$age_by_sex[1]))
    })
  }
  return(constraint)
}
