
library(testthat)
library(synthACS)

context("pull_pov_inc")

test_that("errors work", {
  # create test geography
  ca_counties <- geo.make(state= 'CA', county= '*')
  diamonds <- data.frame(
    carat= rexp(100),
    cut= factor(sample(c("A", "B", "C"), size= 100, replace= TRUE)),
    x= runif(100, min= 0, max= 10),
    y= runif(100, min= 0, max= 10),
    x= runif(100, min= 0, max= 10)
  )
  
  # standard errors
  expect_error(pull_pov_inc(endyear= 2016, span=0, ca_counties))
  expect_error(pull_pov_inc(endyear= 2016, span= -1, ca_counties))
  expect_error(pull_pov_inc(endyear= 2016, span= 7, ca_counties))
  expect_error(pull_pov_inc(endyear= 2000, span=5, ca_counties))
  expect_error(pull_pov_inc(endyear= 2010.5, span=5, ca_counties))
  expect_error(pull_pov_inc(endyear= "ABC", span=5, ca_counties))
  
  ca_counties2 <- ca_counties
  class(ca_counties2) <- "ABC"
  expect_error(pull_pov_inc(endyear= 2010, span=5, ca_counties2))
  expect_error(pull_pov_inc(endyear= 2010, span=5, diamonds))
})

test_that("returns results accurately - counties", {
  # create test geography and data
  ca_geo <- geo.make(state= 'CA', county= 'Los Angeles')
  ca_dat <- pull_pov_inc(2014, 5, ca_geo)
  
  # test:
  synthACS:::confirm_macroACS_class(ca_dat)
})

test_that("returns results accurately - state", {
  # create test geography and data
  ca_geo <- geo.make(state= "CA")
  ca_dat <- pull_pov_inc(2016, 5, ca_geo)
  # test:
  synthACS:::confirm_macroACS_class(ca_dat)
  
  # create test geography and data
  ca_geo <- geo.make(state= "CA", county= '*')
  ca_dat <- pull_pov_inc(2016, 5, ca_geo)
  # test:
  synthACS:::confirm_macroACS_class(ca_dat)
})