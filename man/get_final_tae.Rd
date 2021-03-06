% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/a_class-methods_helpers.R
\name{get_final_tae}
\alias{get_final_tae}
\title{Extract the final TAE for a specified geogrpahy from an 'smsm_set' object}
\usage{
get_final_tae(obj, geography)
}
\arguments{
\item{obj}{An object of class \code{'smsm_set'}, typically a result of call to 
\code{\link{all_geog_optimize_microdata}}}

\item{geography}{A string allowing string matching via \code{\link[base]{grep}} to 
a specified geography.}
}
\description{
Extract the final TAE (resulting from the simulated annealing 
algorithm) for a given geography.
}
