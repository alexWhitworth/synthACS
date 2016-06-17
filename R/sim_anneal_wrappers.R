
#' @title Add new constraint to a set of geographies
#' @description Add a new constraint to the mapping between a a set of macro datasets and a matching 
#' set of micro dataset (supplied as class 'macro_micro'). May be called repeatedly to create a 
#' set of constraints across the sub-geographies.
#' @param attr_name The name of the attribute, or variable, that you wish to constrain.
#' @param attr_total_list A list of named integer vectors containing counts per level of the new 
#' constraining attribute for each geography.
#' @param macro_micro The geographical dataset of macro and micro data. Should be of class 
#' \code{"macro_micro"}.
#' @param constraint_list_list A \code{list} of lists containing prior constraints on the same dataset 
#' for which you wish to add to. Defaults to \code{NULL} (ie. the default is that this is the first 
#' constraint.)
#' @return A list of constraint lists.
#' @seealso \code{\link{add_constraint}}
#' @export
#' 
#' @examples \dontrun{
#' # assumes that micro_synthetic already exists in your environment
#' 
#' # 1. build constraints for gender and age
#' g <- all_geog_constraint_gender(micro_synthetic, method= "macro.table")
#' 
#' a <- all_geog_constraint_age(micro_synthetic, method= "macro.table")
#' 
#' # 2. bind constraints to geographies and macro-data
#' cll <- all_geogs_add_constraint(attr_name= "age", attr_total_list= a, 
#'           macro_micro= micro_synthetic)
#' cll <- all_geogs_add_constraint(attr_name= "gender", attr_total_list= g, 
#'           macro_micro= micro_synthetic, constraint_list_list= cll)
#' 
#' }
all_geogs_add_constraint <- function(attr_name= "variable", attr_total_list, macro_micro,
                                     constraint_list_list= NULL) {
  # 00. error checking
  #------------------------------------
  if (missing(attr_name)) 
    stop("attr_name must be supplied.")
  if (!is.list(macro_micro) | !is.synthACS(macro_micro))
    stop("macro_micro must be supplied as a class 'synthACS' object.")
  if (!is.character(attr_name)) stop("attr_name must be a string.")
  if (!all(unlist(lapply(macro_micro, function(l, nm) {exists(nm, as.environment(l[[2]]))}, nm= attr_name))))
    stop(paste("variable", attr_name, "is not contained in all elements of macro_micro.", sep= " "))
  if (is.list(attr_total_list) &
      (!all(unlist(lapply(attr_total_list, is.numeric))) | 
      !all(unlist(lapply(attr_total_list, function(l) all(l %% 1 == 0)))) |
      any(unlist(lapply(attr_total_list, function(l) is.null(names(l)))))))
    stop("attr_totals_list must a list of named numeric integer vectors.")
  
  if ( # similar check to \code{\link{add_constraint}} but across lists
    !all(mapply(function(mm, attr_name, attr_totals) {
    all(names(attr_totals) == levels(get(attr_name, as.environment(mm[[2]]))))
  }, mm= macro_micro, attr_name= rep(attr_name, length(attr_total_list)), 
     attr_totals= attr_total_list, SIMPLIFY= TRUE))
     ) stop("names of attr_totals must match levels and order of the associated variable in micro_data.")
  
  # 01. add constraint -- wrapper
  #------------------------------------
  micro_datas <- lapply(macro_micro, "[[", 2)
  
  if (!is.null(constraint_list_list)) {
    constraint_wrap <- mapply(add_constraint, 
                              attr_name= rep(attr_name, length(micro_datas)),
                              attr_totals= attr_total_list,
                              micro_data= micro_datas, 
                              constraint_list= constraint_list_list, SIMPLIFY= FALSE,
                              USE.NAMES= FALSE)  
  } else {
    constraint_wrap <- mapply(add_constraint, 
                              attr_name= rep(attr_name, length(micro_datas)),
                              attr_totals= attr_total_list,
                              micro_data= micro_datas, 
                              SIMPLIFY= FALSE, USE.NAMES= FALSE)
  }
  
  # 02. return
  return(constraint_wrap)
}


#' @title Optimize the selection of a micro data population for a set of geographies.
#' @description Optimize the candidate micro datasets such that the lowest loss against the 
#' macro dataset constraints are obtained. Loss is defined here as total absolute error (TAE)
#' and constraints are defined by the \code{constraint_list_list}. Optimization is done by
#' simulated annealing and geographies are run in parallel.
#' 
#' @param macro_micro The geographical dataset of macro and micro data. Should be of class 
#' \code{"macro_micro"}.
#' @param prob_name It is assumed that observations are weighted and do not have an equal probability
#' of occurance. This string specifies the variable within each dataset that contains the probability
#' of selection.
#' @param constraint_list_list A list of constraint lists. See \code{\link{add_constraint}}, 
#' \code{\link{all_geogs_add_constraint}}
#' @param p_accept The acceptance probability for the Metropolis acceptance criteria.
#' @param max_iter The maximum number of allowable iterations. Defaults to \code{10000L}
#' @param seed A seed for reproducibility. See \code{\link[base]{set.seed}}
#' @param verbose Logical. Do you wish to see verbose output? Defaults to \code{TRUE}
#' @seealso \code{\link{optimize_microdata}}
#' @export
#' 
#' @examples \dontrun{
#'  # assumes that micro_synthetic and cll already exist in your environment
#'  # see: examples for derive_synth_datasets() and all_geogs_add_constraint()
#'  optimized_la <- all_geog_optimize_microdata(micro_synthetic, prob_name= "p", 
#'      constraint_list_list= cll, p_accept= 0.01, max_iter= 1000L)
#' }
all_geog_optimize_microdata <- function(macro_micro, prob_name= "p", constraint_list_list, 
                                        p_accept= 0.05, max_iter= 10000L,
                                        seed= sample(1L:10000L, size=1, replace=FALSE),
                                        verbose= TRUE) {
  
  # 01. error checking
  #------------------------------------
  if (!is.list(macro_micro) | !is.synthACS(macro_micro))
    stop("macro_micro must be supplied as a class 'synthACS' object.")
  if (!is.numeric(p_accept) | p_accept <= 0 | p_accept >= 1) stop("p_accept must be numeric in (0,1).")
  if ((max_iter %% 1 != 0) | max_iter < 1) stop("max_iter must be an integer.")
  
  # 02. wrap optimize micro in parallel
  #------------------------------------
  mc <- match.call()
  micro_datas <- lapply(macro_micro, "[[", 2)
  
  if (verbose) message("Beginning parallel optimization...")
  
  nnodes <- min(parallel::detectCores() - 1, length(micro_datas))
  if (grepl("Windows", utils::sessionInfo()$running)) {cl <- parallel::makeCluster(nnodes, type= "PSOCK")}
  else {cl <- parallel::makeCluster(nnodes, type= "FORK")}
  
  geography_anneal <- parallel::clusterMap(cl, RECYCLE= TRUE, SIMPLIFY= FALSE, .scheduling= "dynamic",
                                 fun= optimize_microdata, 
                                 micro_data= micro_datas, prob_name= prob_name,
                                 constraint_list= constraint_list_list,
                                 p_accept= p_accept, max_iter= max_iter,
                                 seed= seed, verbose= FALSE)
  
  parallel::stopCluster(cl)
  if (verbose) message("... Optimization complete")
  
  # 03. return
  #------------------------------------
  best_fits <- lapply(geography_anneal, function(l) return(l[["best_fit"]]))
  taes <- lapply(geography_anneal, function(l) return(l[["tae"]]))
  iters <- lapply(geography_anneal, function(l) return(l[["iter"]]))
  
  return(list(best_fit= best_fits, tae= taes, call= mc, p_accept= p_accept, 
              iter= iters, max_iter= max_iter, seed= seed))
}







#' @title Add a new attribute to a set (ie list) of synthetic_micro datasets
#' @description Add a new attribute to a set (ie list) of synthetic_micro datasets using conditional 
#' relationships between the new attribute and existing attributes (eg. wage rate conditioned on age 
#' and education level). The same attribute is added to *each* synthetic_micro dataset, where each
#' dataset is supplied a distinct relationship for attribute creation.
#' @param df_list A \code{list} of R objects each of class "synthetic_micro". 
#' @param prob_name A string specifying the column name of each \code{data.frame} in \code{df_list} 
#' containing the probabilities for each synthetic observation.
#' @param attr_name A string specifying the desired name of the new attribute to be added to the data.
#' @param attr_vector_list A \code{list} of named vectors with each specifying the counts or 
#' percentages of the new attribute, or variable, to be added. Names must include appropriate 
#' naming for expression matching.
#' @param attr_levels A character vector specifying the complete set of levels for the new 
#' attribute.
#' @param conditional_vars An character vector specifying the existing variables, if any, on which 
#' the new attribute (variable) is to be conditioned on for each dataset. Variables must be specified 
#' in order. Defaults to \code{NULL} ie- an unconditional new attribute.
#' @param ht_list A \code{list} of equal length to \code{conditional_vars}. Each element \code{k} of
#' \code{ht_list} is a \code{data.frame} constructed as a hash-table with one-to-one correspondence  
#' between \code{ht_list[[k]]} and \code{conditional_vars[k]}. Of the key-value pair, the key is
#' the first column and the value is the second column. See \code{\link{synthetic_new_attribute}}.
#' @return A list of new synthetic_micro datasets each with class "synthetic_micro".
#' @seealso \code{\link{synthetic_new_attribute}}
#' @export
all_geog_synthetic_new_attribute <- function(df_list, prob_name= "p",
                                             attr_name= "variable",
                                             attr_vector_list, attr_levels, 
                                             conditional_vars= NULL,
                                             ht_list= NULL) {
 
  # 01. error checking
  #------------------------------------
  if (!is.list(df_list) | !is.synthACS(df_list))
    stop("df_list must be supplied as a class 'synthACS' object.")
 
  # 02. wrap synthetic_new_attribute in parallel
  #------------------------------------
  len <- length(df_list)
  
  if (!is.null(conditional_vars)) { # need to replicate() these for clusterMap since they are already lists
    conditional_vars <- replicate(len, conditional_vars, simplify= FALSE)
    ht_list <- replicate(len, ht_list, simplify= FALSE)
  }
  
  nnodes <- min(parallel::detectCores() - 1, len)
  if (grepl("Windows", utils::sessionInfo()$running)) {cl <- parallel::makeCluster(nnodes, type= "PSOCK")}
  else {cl <- parallel::makeCluster(nnodes, type= "FORK")}
  
  synthethic_data <- parallel::clusterMap(cl, RECYCLE= TRUE, SIMPLIFY= FALSE, .scheduling= "dynamic",
                                fun= synthetic_new_attribute,
                                df= df_list, prob_name= prob_name, attr_name= attr_name,
                                attr_vector= attr_vector_list, attr_levels= attr_levels, 
                                conditional_vars= conditional_vars,
                                ht_list= ht_list)
  
  parallel::stopCluster(cl)
  # 03. return
  #------------------------------------
  return(synthethic_data) 
}