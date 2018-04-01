
library(testthat)
library(synthACS)

context("pull_edu")

test_that("errors work", {
  # create test geography
  ca_counties <- geo.make(state= 'CA', county= '*')
  data(diamonds, package= "ggplot2")
  
  # standard errors
  expect_error(pull_edu(endyear= 2012, span=0, ca_counties))
  expect_error(pull_edu(endyear= 2012, span= -1, ca_counties))
  expect_error(pull_edu(endyear= 2012, span= 7, ca_counties))
  expect_error(pull_edu(endyear= 2000, span=5, ca_counties))
  expect_error(pull_edu(endyear= 2010.5, span=5, ca_counties))
  expect_error(pull_edu(endyear= "ABC", span=5, ca_counties))
  
  ca_counties2 <- ca_counties
  class(ca_counties2) <- "ABC"
  expect_error(pull_edu(endyear= 2010, span=5, ca_counties2))
  expect_error(pull_edu(endyear= 2010, span=5, diamonds))
  ## library(acs) warnings
  expect_warning(pull_edu(endyear= 2014, span=5, ca_counties))
  expect_warning(pull_edu(endyear= 2012, span=3, ca_counties))
  expect_warning(pull_edu(endyear= 2012, span=1, ca_counties))
  expect_warning(pull_edu(endyear= 2014, span=3, ca_counties))
})

test_that("returns results accurately - tracts", {
  # create test geography and data
  ca_geo <- geo.make(state= 'CA', county= '*', tract= '*')
  acs_fetch1 <- acs.fetch(endyear= 2012, span= 5, geography= ca_geo, 
                          table.number = "B14001", col.names= "pretty")
  ca_dat <- pull_edu(2012, 5, ca_geo)
  
  # test:
  source("c:/Github_projects/ACSpulls/synthACS/tests/testthat/standard_geo_tests.R")
  test_func(ca_dat, acs_fetch1)
})

test_that("returns results accurately - blocks", {
  # create test geography and data
  ca_geo <- geo.make(state= 'CA', county= '*', tract= '*', block.group= '*')
  acs_fetch1 <- acs.fetch(endyear= 2012, span= 5, geography= ca_geo, 
                          table.number = "B14001", col.names= "pretty")
  ca_dat <- pull_edu(2012, 5, ca_geo)
  
  # test:
  source("c:/Github_projects/ACSpulls/synthACS/tests/testthat/standard_geo_tests.R")
  test_func(ca_dat, acs_fetch1)
})

test_that("returns results accurately - counties", {
  # create test geography and data
  ca_geo <- geo.make(state= 'CA', county= '*')
  acs_fetch1 <- acs.fetch(endyear= 2012, span= 5, geography= ca_geo, 
                          table.number = "B14001", col.names= "pretty")
  ca_dat <- pull_edu(2012, 5, ca_geo)
  
  # test:
  source("c:/Github_projects/ACSpulls/synthACS/tests/testthat/standard_geo_tests.R")
  test_func(ca_dat, acs_fetch1)
})

test_that("returns results accurately - zips", {
  # create test geography and data
  geo <- geo.make(zip.code = '*')
  acs_fetch1 <- acs.fetch(endyear= 2012, span= 5, geography= geo, 
                          table.number = "B14001", col.names= "pretty")
  dat <- pull_edu(2012, 5, geo)
  
  # test:
  source("c:/Github_projects/ACSpulls/synthACS/tests/testthat/standard_geo_tests.R")
  test_func(dat, acs_fetch1)
})

test_that("returns results accurately - state", {
  # create test geography and data
  ca_geo <- geo.make(state= "*")
  acs_fetch1 <- acs.fetch(endyear= 2012, span= 5, geography= ca_geo, 
                          table.number = "B14001", col.names= "pretty")
  ca_dat <- pull_edu(2012, 5, ca_geo)
  
  # test:
  source("c:/Github_projects/ACSpulls/synthACS/tests/testthat/standard_geo_tests.R")
  test_func(ca_dat, acs_fetch1)
})