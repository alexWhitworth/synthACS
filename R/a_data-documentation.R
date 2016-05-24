

#' @title Birth Rates by Age and Race of Mother
#' @description A dataset containing birth rate data in the United States by age and race of the 
#' mother. Data for all races is provided for 1970-2014 and for individual races from 1989-2014.
#' @format A \code{data.frame} with 1,750 observations and 4 variables.
#' \describe{
#'  \item{year}{The year for which data was was recorded.}
#'  \item{race}{The racial group of the mothers. One of \code{all} all races; \code{white} 
#'  non-hispanic whites; \code{black_aa} black / African-American; \code{nat_amer} American Indian
#'  or Native Alaskan; \code{asian_isl} Asian or Pacific Islander; \code{hisp_lat} Hispanic or Latin 
#'  American.}
#'  \item{age_group}{The age group of the mother.}
#'  \item{birth_rate}{The birth rate. See Details.}
#' }
#' @section Details:
#' \itemize{
#' \item{The birth rate is defined as births per 1,000 women in the specified group (age and race).} 
#' \item{Populations are based on census counts enumerated as of April 1 of the census year and estimated
#' as of July 1 for non-census years.}
#' \item{Beginning in 1997, birth rates for age group 45up by relating births to all women age 45 or
#' older to this group. Prior to 1997, only births to women age 45-49 were included.} 
#' }
#' @source \url{http://www.cdc.gov/nchs/nvss/births.htm}
#' @references Hamilton, Brady E., et al. "Births: final data for 2014." National Vital Statistics 
#' Reports 64.12 (2015): 1-64.
"BR2014"

#' @title Total Fertility Rate by race of mother
#' @description A dataset containing total fertility rate data by race of the mother. Data for all 
#' races is provided for 1970-2014 and for individual races from 1989-2014.
#' @format A \code{data.frame} with 175 observations and 3 variables.
#' \describe{
#'  \item{year}{The year for which data was was recorded.}
#'  \item{race}{The racial group of the mothers. One of \code{all} all races; \code{white} 
#'  non-hispanic whites; \code{black_aa} black / African-American; \code{nat_amer} American Indian
#'  or Native Alaskan; \code{asian_isl} Asian or Pacific Islander; \code{hisp_lat} Hispanic or Latin 
#'  American.}
#'  \item{tfr}{The Total Fertility Rate. See Details} 
#' }
#' @section Details:
#' The Total Fertility Rate is defined as the sums of the birth rates for the 5-year age groups
#' found in \code{\link{BR2014}} multiplied by 5. 
#' @source \url{http://www.cdc.gov/nchs/nvss/births.htm}
#' @references Hamilton, Brady E., et al. "Births: final data for 2014." National Vital Statistics 
#' Reports 64.12 (2015): 1-64.
"TFR"

#' @title Multiple Birth Rate data by year and race of mother
#' @description A dataset containing multiple birth rate data by race of the mother. Data for all 
#' races is provided for 1980-2014 and for individual races from 1990-2014.
#' @format A \code{data.frame} with 110 observations and 8 variables.
#' \describe{
#'  \item{year}{The year for which data was was recorded.}
#'  \item{race}{The racial group of the mothers. One of \code{all} all races; \code{white} 
#'  non-hispanic whites; \code{black_aa} non Hispanic black / African-American; \code{hisp_lat} 
#'  Hispanic.}
#'  \item{births}{Total births for the year and racial group in the United States.}
#'  \item{twin_births}{Total twin births for the year and racial group in the United States.}
#'  \item{triplet_more_births}{Total triplet or higher order births for the year and racial group in 
#'  the United States.}
#'  \item{MBRate}{The number of live births in all multiple deliveries per 1,000 live births.}
#'  \item{twinBR}{The number of live births in all twin deliveries per 1,000 live births.}
#'  \item{twinBR}{The number of live births in all triplet or higher order deliveries per 100,000 
#'  live births.}
#' }
#' @section Details:
#' \itemize{
#'  \item{Data for race cateogry \code{"all"} includes races other than white and black and origin 
#'  not stated.}
#'  \item{Race and Hispanic origin are reported separately on birth certificates. Persons of Hispanic
#'  origin may be of any race.}
#' }
#' @source \url{http://www.cdc.gov/nchs/nvss/births.htm}
#' @references Hamilton, Brady E., et al. "Births: final data for 2014." National Vital Statistics 
#' Reports 64.12 (2015): 1-64.
"MBR"


#' @title Raw Death Rate by race and gender
#' @description A dataset containing raw death rate data by race and gender of the deceased. Data  
#' is provided for 1980-2013.
#' @format A \code{data.frame} with 612 observations and 4 variables.
#' \describe{
#'  \item{year}{The year for which data was was recorded.}
#'  \item{race}{The racial group of the deceased One of \code{all} all races; \code{white} 
#'  whites; \code{black_aa} black / African-American; \code{nat_amer} American Indian
#'  or Native Alaskan; \code{asian_isl} Asian or Pacific Islander; \code{hisp_lat} Hispanic.}
#'  \item{gender}{The gender of the deceased. One of \code{c(both, male, female)}}
#'  \item{death_rate}{The raw death rate. See details.}
#' }
#' @section Details:
#' \itemize{
#' \item{The death rate is defined as deaths per 100,000 population.} 
#' \item{Populations are based on census counts enumerated as of April 1 of the census year and estimated
#' as of July 1 for non-census years.}
#' }
#' @source \url{http://www.cdc.gov/nchs/nvss/deaths.htm}
#' @references Xu, J. Q., S. L. Murphy, and K. D. Kochanek. "Deaths: final data for 2013." National 
#' Vital Statistics Reports 64.2 (2015).
"rawDR"

#' @title Age-adjusted Death Rate by race and gender
#' @description A dataset containing raw death rate data by race and gender of the deceased. Data  
#' is provided for 1980-2013.
#' @format A \code{data.frame} with 612 observations and 4 variables.
#' \describe{
#'  \item{year}{The year for which data was was recorded.}
#'  \item{race}{The racial group of the deceased One of \code{all} all races; \code{white} 
#'  whites; \code{black_aa} black / African-American; \code{nat_amer} American Indian
#'  or Native Alaskan; \code{asian_isl} Asian or Pacific Islander; \code{hisp_lat} Hispanic.}
#'  \item{gender}{The gender of the deceased. One of \code{c(both, male, female)}}
#'  \item{adj_death_rate}{The age-adjusted death rate. See details.}
#' }
#' @section Details:
#' \itemize{
#' \item{The age-adjusted death rates are used to compare relative mortality risks among groups and 
#' over time. They were computed by the direct method, which is defined 
#' \deqn{R'= \sum_{i} \frac{P_{si}}{P_{s}}R_i} where \eqn{P_{si}} is the standard population for age group i,
#' \eqn{P_s} is the total US standard population and \eqn{R_i} is the raw death rate for age group i.} 
#' \item{Populations are based on census counts enumerated as of April 1 of the census year and estimated
#' as of July 1 for non-census years.}
#' }
#' @source \url{http://www.cdc.gov/nchs/nvss/deaths.htm}
#' @references Xu, J. Q., S. L. Murphy, and K. D. Kochanek. "Deaths: final data for 2013." National 
#' Vital Statistics Reports 64.2 (2015).
"adjDR"