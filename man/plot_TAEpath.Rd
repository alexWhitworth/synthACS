% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/a_class-methods_helpers.R
\name{plot_TAEpath}
\alias{plot_TAEpath}
\title{Plot simulated annealing path}
\usage{
plot_TAEpath(object, geography, ...)
}
\arguments{
\item{object}{An object of class \code{'smsm_set'}, typically a result of call to 
\code{\link{all_geog_optimize_microdata}}}

\item{geography}{A string allowing string matching via \code{\link[base]{grep}} to 
a specified geography.}

\item{...}{additional arguments passed to other methods}
}
\description{
Plot the path TAE in the simulated annealing algorithm for a given geography
}
