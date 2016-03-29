

#' @title Add a new attribute to a synthetic_micro dataset
#' @description Add a new attribute to a synthetic_micro dataset using conditional relationships
#' between the new attribute and existing attributes (eg. wage rate conditioned on age and education 
#' level).  
#' @section Details:
#' New synthetic variables are introduced to the existing data via conditional probability. Similar 
#' to \code{\link{derive_synth_datasets}}, the goal with this function is to generate a joint 
#' probability distribution for an attribute vector; and, to create synthetic individuals from 
#' this distribution. Although no limit is placed on the number of variables on which to condition, 
#' in practice, data rarely exists which allows more than two or three conditioning variables. Other 
#' variables are assumed to be independent from the new attribute. (**see note below)
#' 
#' Conditioning is implemented via pattern matching by matching the names of the \code{attr_vector} 
#' to the existing levels of the data. This is facilitated by hash-tables (\code{ht_list}) to ensure
#' accurate pattern matching. In the hash-table's key-value pair, the key is the actual level for the 
#' variable being conditioned upon, while the value is the regex string found in 
#' \code{names(attr_vector)}.
#' 
#' Successive levels of conditioning may be supplied by providing a vector of \code{conditional_vars}
#' paired with a equal length list of hash tables (\code{ht_list}). A recursive approach is 
#' employed to conditionally partition \code{attr_vector}. In this sense, the *order* in which
#' the conditional variables are supplied matters.
#' 
#' ** There are four different types of conditional/marginal probability models which may be considered
#' for a given new attribute:
#'  (1) Independence: it is assumed that each of the variables is independent of the others
#'  (2) Pairwise conditional independence: it is assumed that attributes are related to 
#'  only one other attribute and independent of all others.
#'  (3) Conditional independence: Attributes can be depedent on some subset of other attributes and 
#'  independent of the rest.
#'  (4) In the most general case, all attributes are jointly interrelated.
#' 
#' @param df An R object of class "synthetic_micro". 
#' @param prob_name A string specifying the column name of the \code{df} containing the
#' probabilities for each synthetic observation.
#' @param attr_name A string specifying the desired name of the new attribute to be added to the data.
#' @param attr_vector A named vector specifying the counts or percentages of the new attribute,
#' or variable, to be added. Names must include appropriate naming for expression matching.
#' @param attr_levels A character vector specifying the complete set of levels for the new 
#' attribute.
#' @param conditional_vars An character vector specifying the existing variables, if any, on which 
#' the new attribute (variable) is to be conditioned on. Variables must be specified in order. 
#' Defaults to \code{NULL} ie- an unconditional new attribute.
#' @param ht_list A \code{list} of equal length to \code{conditional_vars}. Each element \code{k} of
#' \code{ht_list} is a \code{data.frame} constructed as a hash-table with one-to-one correspondence  
#' between \code{ht_list[[k]]} and \code{conditional_vars[k]}. Of the key-value pair, the key is
#' the first column and the value is the second column. See details. 
#' @return A new synthetic_micro dataset with class "synthetic_micro".
#' @export
synthetic_new_attribute <- function(df, prob_name= "p",
                                    attr_name= "variable",
                                    attr_vector, attr_levels, 
                                    conditional_vars= NULL,
                                    ht_list= NULL) {
  # 01. Error checking
  #------------------------------------
  if (missing(attr_vector) | missing(attr_levels) | missing(attr_name)) 
    stop("attr_name, attr_vector, and attr_levels must be supplied.")
  if (!is.character(attr_name)) stop("attr_name must be a string.")
  if (!is.numeric(attr_vector) | !is.vector(attr_vector)) stop("attr_vector must be a numeric vector.")
  if (is.null(names(attr_vector))) stop("attr_vector must be a named vector.")
  if (!(is.character(attr_levels) | is.factor(attr_levels))) 
    stop("attr_levels must be a character or factor vector.")
  if (!is.micro_synthetic(df)) stop("Input appropriate df -- use class 'micro_synthetic'.")
  if (!is.character(prob_name)) stop("prob_name must be a string.")
  if (!exists(prob_name, as.environment(df))) stop("prob_name is not in df.")
  if (!is.null(conditional_vars)) {
    # error check conditional_vars
    if (!all(sapply(conditional_vars, is.character)))
      stop("conditional_vars must be specified as strings.")
    if (!all(sapply(conditional_vars, function(l) exists(l, as.environment(df)))))
      stop("at least one conditional_var is not in df.")
    # error check ht_list, must be co-specified.
    if (is.null(ht_list)) stop("ht_list must be specified")
    else {
      if (!is.list(ht_list) | (length(ht_list) != length(conditional_vars)))
        stop("ht_list must be a list of equal length as conditional_vars.")
      if (!all(unlist(lapply(ht_list, function(l) sapply(l, function(i) is.character(i) | is.factor(i))))))
        stop("all elements of ht_list must be strings or factors.")
    }
  }
  
  # 02. Variable conditioning and Apply new synthetic variable
  #------------------------------------
  if (!is.null(conditional_vars)) {
    dat <- cond_var_split(df= df, prob_name= prob_name,
                          attr_name= attr_name, attr_vector= attr_vector,
                          attr_levels= attr_levels,
                          conditional_vars= conditional_vars,
                          ht_list= ht_list)
  } else { # apply new attribute unconditionally
    dat <- replicate(length(attr_levels), df, simplify = FALSE)
    attr_cnts <- attr_vector / sum(attr_vector)
    dat <- do.call("rbind", mapply(mapply_synth, 
                                   dat= dat, attr_pct= attr_cnts, attr_name= attr_name,
                                   level= attr_levels, prob_name= prob_name,
                                   SIMPLIFY = FALSE)) 
  }
  
  # 03. return
  #------------------------------------
  dat <- dat[dat[prob_name] > 0,]
  if (!is.micro_synthetic(dat))   class(dat) <- c(class(dat), "micro_synthetic")
  return(dat)
}

# simple helper function to reduce typing.
split_df <- function(d, var) {
  split(d, get(var, as.environment(d)))
}



# @param df An R object of class "synthetic_micro". 
# @param prob_name A string specifying the column name of the \code{df} containing the
# probabilities for each synthetic observation.
# @param attr_name A string specifying the desired name of the new attribute to be added to the data.
# @param attr_vector A named vector specifying the counts or percentages of the new attribute,
# or variable, to be added. Names must include appropriate naming for expression matching.
# @param attr_levels A character vector specifying the complete set of levels for the new 
# attribute.
# @param conditional_vars An character vector specifying the existing variables, if any, on which 
# the new attribute (variable) is to be conditioned on. Variables must be specified in order. 
# Defaults to \code{NULL} ie- an unconditional new attribute.
# @param ht_list A \code{list} of equal length to \code{conditional_vars}. Each element \code{k} of
# \code{ht_list} is a \code{data.frame} constructed as a hash-table with one-to-one correspondence  
# between \code{ht_list[[k]]} and \code{conditional_vars[k]}. Of the key-value pair, the key is
# the first column and the value is the second column. See details. 
cond_var_split <- function(df, prob_name, 
                           attr_name= "variable", attr_vector, attr_levels, 
                           conditional_vars, ht_list) {
  cv_n <- length(conditional_vars)
  ht_n <- length(ht_list)
  
  if (cv_n == 1 & ht_n == 1) { # if lengths == 1, bottom of tree. Apply and return
    return(lapply_synth(l= df, prob_name= prob_name,
                 ht= ht_list[[1]], cond_var= conditional_vars[1],
                 attr_name= attr_name, attr_v= attr_vector, levels= attr_levels))
  } else { # else more conditioning needed, use recursion.
    # A. split data conditionally and apply
    dat <- split_df(df, conditional_vars[1])
    
    dat <- lapply(dat, function(l, prob_name, attr_name, attr_vector, 
                                attr_levels, conditional_vars, ht_list) {
      # extract first level, then drop level of conditioning
      c_var <- conditional_vars[1]
      ht <- ht_list[[1]]
      conditional_vars2 <- conditional_vars[-1]
      ht_list2 <- ht_list[-1]
      # B. split attr_vector by ht 
      cond_var_comp <- ht[,2][which(l[,c_var][1] == ht[,1])] 
      attr_cnts <- attr_vector[which(grepl(cond_var_comp, names(attr_vector)))]
      
      # C. apply recursion
      l <- cond_var_split(l, prob_name= prob_name,
                                           attr_name= attr_name, attr_vector= attr_cnts, attr_levels= attr_levels,
                                           conditional_vars= conditional_vars2, ht_list= ht_list2)
    }, prob_name= prob_name, attr_name= attr_name, attr_vector= attr_vector, 
       attr_levels= attr_levels, conditional_vars= conditional_vars, ht_list= ht_list) 
    #### WHY DOES THIS RETURN A LIST OF TRANSPOSED MATRICES VS A LIST OF DATA.FRAMES????
    return(do.call("rbind", dat))
  }
}



# @param l A data.frame -- a conditional subset of df 
# @param ht A hash-table (an element of ht_list) mapping conditional_var[k] to 
# the names of attr_vector (attr_v here)
# @param attr_name the name of the new variable 
# @param attr_v A named vector specifying the counts or percentages of the new attribute.
# Here, a subset of the previously specified attr_vector.
# @param levels the levels of the new variable
# @ prob_name A string specifying the column name within \code{l} containing the
# probabilities for each synthetic observation.
lapply_synth <- function(l, prob_name, ht, cond_var, attr_name= "variable", attr_v, levels) {
  # use ht to pull appropriate elements from v
  cond_var_comp <- ht[,2][which(l[,cond_var][1] == ht[,1])]
  attr_cnts <- attr_v[which(grepl(cond_var_comp, names(attr_v)))]
  # unit-norm
  if (sum(attr_cnts) > 0) attr_cnts <- attr_cnts / sum(attr_cnts)
  
  # replicate data and apply new levels/probabilities
  dat <- replicate(length(levels), l, simplify = FALSE)
  dat <- do.call("rbind", mapply(mapply_synth, 
                 dat= dat, attr_pct= attr_cnts, attr_name= attr_name,
                 level= levels, prob_name= prob_name,
                 SIMPLIFY = FALSE))
  return(dat)
}


# helper function for bottom of recursion -- apply new level to 
# smallest subset of data and update probabilities appropriately
mapply_synth <- function(dat, prob_name, attr_pct, attr_name= "variable", level) {
  p <- get(prob_name, as.environment(dat))
  d_temp <- dat[, which(names(dat) != prob_name)]
  d_temp[attr_name] <- level
  d_temp[prob_name] <- p * attr_pct
  return(d_temp)
}
