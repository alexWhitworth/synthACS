
library(testthat)
library(synthACS)

context("pull_bachelors")

test_that("errors work", {
  # create test geography
  ca_counties <- geo.make(state= 'CA', county= '*')
  data(diamonds, package= "ggplot2")
  
  # standard errors
  expect_error(pull_bachelors(endyear= 2016, span=0, ca_counties))
  expect_error(pull_bachelors(endyear= 2016, span= -1, ca_counties))
  expect_error(pull_bachelors(endyear= 2016, span= 7, ca_counties))
  expect_error(pull_bachelors(endyear= 2000, span=5, ca_counties))
  expect_error(pull_bachelors(endyear= 2010.5, span=5, ca_counties))
  expect_error(pull_bachelors(endyear= "ABC", span=5, ca_counties))
  
  ca_counties2 <- ca_counties
  class(ca_counties2) <- "ABC"
  expect_error(pull_bachelors(endyear= 2010, span=5, ca_counties2))
  expect_error(pull_bachelors(endyear= 2010, span=5, diamonds))
})

test_that("returns results accurately - counties", {
  # create test geography and data
  ca_geo <- geo.make(state= 'CA', county= 'Los Angeles')
  acs_fetch1 <- acs.fetch(endyear= 2016, span= 5, geography= ca_geo, 
                          table.number = "B14001", col.names= "pretty")
  ca_dat <- pull_bachelors(2016, 5, ca_geo)
  
  # test:
  confirm_macroACS_class(ca_dat, acs_fetch1)
})

test_that("returns results accurately - state", {
  # create test geography and data
  ca_geo <- geo.make(state= "CA")
  acs_fetch1 <- acs.fetch(endyear= 2016, span= 5, geography= ca_geo, 
                          table.number = "B14001", col.names= "pretty")
  ca_dat <- pull_bachelors(2012, 5, ca_geo)
  
  # test:
  confirm_macroACS_class(ca_dat, acs_fetch1)
})