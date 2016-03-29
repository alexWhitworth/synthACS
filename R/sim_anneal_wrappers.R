
#' @title Add new constraint to a set set of geographies
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
all_geogs_add_constraint <- function(attr_name= "variable", attr_total_list, macro_micro,
                                     constraint_list_list= NULL) {
  # 00. error checking
  #------------------------------------
  if (missing(attr_name)) 
    stop("attr_name must be supplied.")
  if (!is.list(macro_micro) | !all(unlist(lapply(macro_micro, is.macro_micro)))) 
    stop("macro_micro must be supplied as a list of with each element of class 'macro_micro'.")
  if (!is.character(attr_name)) stop("attr_name must be a string.")
  if (!all(unlist(lapply(macro_micro, function(l, nm) {exists(nm, as.environment(l[[2]]))}, nm= attr_name))))
    stop(paste("variable", attr_name, "is not contained in all elements of macro_micro.", sep= " "))
  if (is.list(attr_total_list) &
      (!all(unlist(lapply(attr_total_list, is.numeric))) | 
      !all(unlist(lapply(attr_total_list, function(l) all(l %% 1 == 0)))) |
      all(unlist(lapply(attr_total_list, function(l) is.null(names(l)))))))
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
#' @param upscale_100 Numeric. Controls the increase in \code{resample_size} for early iterations.
#' @param p_accept The acceptance probability for the Metropolis acceptance criteria.
#' @param max_iter The maximum number of allowable iterations. Defaults to \code{10000L}
#' @param seed A seed for reproducibility. See \code{\link[base]{set.seed}}
#' @param verbose Logical. Do you wish to see verbose output? Defaults to \code{TRUE}
#' @seealso \code{\link{optimize_microdata}}
#' @export
all_geog_optimize_microdata <- function(macro_micro, prob_name= "p", constraint_list_list, 
                                        upscale_100= 5L, p_accept= 0.05, max_iter= 10000L,
                                        seed= sample(1L:10000L, size=1, replace=FALSE),
                                        verbose= TRUE) {
  
  # 01. error checking
  #------------------------------------
  if (!is.list(macro_micro) | !all(unlist(lapply(macro_micro, is.macro_micro)))) 
    stop("macro_micro must be supplied as a list of with each element of class 'macro_micro'.")
  if (!is.numeric(upscale_100) | upscale_100 < 1) stop("upscale_100 must be numeric >=1.")
  if (!is.numeric(p_accept) | p_accept <= 0 | p_accept >= 1) stop("p_accept must be numeric in (0,1).")
  if ((max_iter %% 1 != 0) | max_iter < 1) stop("max_iter must be an integer.")
  
  # 02. wrap optimize micro in parallel
  #------------------------------------
  mc <- match.call()
  micro_datas <- lapply(macro_micro, "[[", 2)
  
  if (verbose) message("Beginning parallel optimization...")
  
  nnodes <- min(detectCores() - 1, length(micro_datas))
  if (grepl("Windows", sessionInfo()$running)) {cl <- makeCluster(nnodes, type= "PSOCK")}
  else {cl <- makeCluster(nnodes, type= "FORK")}
  
  geography_anneal <- clusterMap(cl, RECYCLE= TRUE, SIMPLIFY= FALSE, .scheduling= "dynamic",
                                 fun= optimize_microdata, 
                                 micro_data= micro_datas, prob_name= prob_name,
                                 constraint_list= constraint_list_list,
                                 upscale_100= upscale_100, p_accept= p_accept, max_iter= max_iter,
                                 seed= seed, verbose= FALSE)
  
  stopCluster(cl)
  if (verbose) message("... Optimization complete")
  
  # 03. return
  #------------------------------------
  best_fits <- lapply(geography_anneal, function(l) return(l[["best_fit"]]))
  taes <- lapply(geography_anneal, function(l) return(l[["tae"]]))
  iters <- lapply(geography_anneal, function(l) return(l[["iter"]]))
  
  return(list(best_fit= best_fits, tae= taes, call= mc, p_accept= p_accept, 
              upscale_100= upscale_100, iter= iters, max_iter= max_iter, seed= seed))
}