
library(testthat)
library(synthACS)

#----------------------------------------------------------
context("macroACS methods")
#----------------------------------------------------------

test_that("fetch - age_by_sex - estimates work", {
  # load test data:
  load("C:/Github_projects/ACSpulls/synthACS/tests/testthat/acsdat.Rdata")
  rm(list=ls()[-which(ls() == "ca_dat")])
  
  # run function on all types of use cases
  options(warn=-1)
  age_sex_vector_all <- fetch_data(ca_dat, geography= "*", dataset= "estimate",
                                   choice= "age_by_sex")
  age_sex_vector_some <- fetch_data(ca_dat, geography= c("Alameda", "Amador", "Los Angeles"), 
                                   dataset= "estimate", choice= "age_by_sex")
  age_sex_vector_one <- fetch_data(ca_dat, geography= "San Bernardino", dataset= "estimate", 
                                   choice= "age_by_sex")
  options(warn=0)
  
  # test outputs
  expect_true(exists("age_sex_vector_all"))
  expect_true(exists("age_sex_vector_some"))
  expect_true(exists("age_sex_vector_one"))
  expect_true(is.data.frame(age_sex_vector_all))
  expect_true(is.data.frame(age_sex_vector_some))
  expect_true(is.data.frame(age_sex_vector_one))
  
  expect_equal(nrow(age_sex_vector_all), nrow(ca_dat$geography))
  expect_equal(names(age_sex_vector_all), names(ca_dat$estimates$age_by_sex))
  expect_equal(rownames(age_sex_vector_all), ca_dat$geography$NAME)
  expect_equal(age_sex_vector_all, ca_dat$estimates$age_by_sex)
  
  expect_equal(nrow(age_sex_vector_some), 3L)
  expect_equal(names(age_sex_vector_some), names(ca_dat$estimates$age_by_sex))
  expect_equal(rownames(age_sex_vector_some), 
               c("Alameda County, California", "Amador County, California", "Los Angeles County, California"))
  expect_equal(age_sex_vector_some, ca_dat$estimates$age_by_sex[c(1,3,19),])
  
  expect_equal(nrow(age_sex_vector_one), 1L)
  expect_equal(names(age_sex_vector_one), names(ca_dat$estimates$age_by_sex))
  expect_equal(rownames(age_sex_vector_one), "San Bernardino County, California")
  expect_equal(age_sex_vector_one, ca_dat$estimates$age_by_sex[36,])
  
})

test_that("fetch - age_by_sex - SE work", {
  # load test data:
  load("C:/Github_projects/ACSpulls/synthACS/tests/testthat/acsdat.Rdata")
  rm(list=ls()[-which(ls() == "ca_dat")])
  
  # run function on all types of use cases
  options(warn=-1)
  age_sex_vector_all <- fetch_data(ca_dat, geography= "*", dataset= "st.err",
                                   choice= "age_by_sex")
  age_sex_vector_some <- fetch_data(ca_dat, geography= c("Alameda", "Amador", "Los Angeles"), 
                                    dataset= "st.err", choice= "age_by_sex")
  age_sex_vector_one <- fetch_data(ca_dat, geography= "San Bernardino", dataset= "st.err", 
                                   choice= "age_by_sex")
  options(warn=0)
  
  # test outputs
  expect_true(exists("age_sex_vector_all"))
  expect_true(exists("age_sex_vector_some"))
  expect_true(exists("age_sex_vector_one"))
  expect_true(is.data.frame(age_sex_vector_all))
  expect_true(is.data.frame(age_sex_vector_some))
  expect_true(is.data.frame(age_sex_vector_one))
  
  expect_equal(nrow(age_sex_vector_all), nrow(ca_dat$geography))
  expect_equal(names(age_sex_vector_all), names(ca_dat$standard_error$age_by_sex))
  expect_equal(rownames(age_sex_vector_all), ca_dat$geography$NAME)
  expect_equal(age_sex_vector_all, ca_dat$standard_error$age_by_sex)
  
  expect_equal(nrow(age_sex_vector_some), 3L)
  expect_equal(names(age_sex_vector_some), names(ca_dat$standard_error$age_by_sex))
  expect_equal(rownames(age_sex_vector_some), 
               c("Alameda County, California", "Amador County, California", "Los Angeles County, California"))
  expect_equal(age_sex_vector_some, ca_dat$standard_error$age_by_sex[c(1,3,19),])
  
  expect_equal(nrow(age_sex_vector_one), 1L)
  expect_equal(names(age_sex_vector_one), names(ca_dat$standard_error$age_by_sex))
  expect_equal(rownames(age_sex_vector_one), "San Bernardino County, California")
  expect_equal(age_sex_vector_one, ca_dat$standard_error$age_by_sex[36,])
})

#----------------------------
test_that("fetch - pop_by_race - estimates work", {
  # load test data:
  load("C:/Github_projects/ACSpulls/synthACS/tests/testthat/acsdat.Rdata")
  rm(list=ls()[-which(ls() == "ca_dat")])
  
  # run function on all types of use cases
  options(warn=-1)
  vector_all <- fetch_data(ca_dat, geography= "*", dataset= "estimate",
                           choice= "pop_by_race")
  vector_some <- fetch_data(ca_dat, geography= c("Alameda", "Amador", "Los Angeles"), 
                           dataset= "estimate", choice= "pop_by_race")
  vector_one <- fetch_data(ca_dat, geography= "San Bernardino", dataset= "estimate", 
                           choice= "pop_by_race")
  options(warn=0)
  
  # test outputs
  expect_true(exists("vector_all"))
  expect_true(exists("vector_some"))
  expect_true(exists("vector_one"))
  expect_true(is.data.frame(vector_all))
  expect_true(is.data.frame(vector_some))
  expect_true(is.data.frame(vector_one))
  
  expect_equal(nrow(vector_all), nrow(ca_dat$geography))
  expect_equal(names(vector_all), names(ca_dat$estimates$pop_by_race))
  expect_equal(rownames(vector_all), ca_dat$geography$NAME)
  expect_equal(vector_all, ca_dat$estimates$pop_by_race)
  
  expect_equal(nrow(vector_some), 3L)
  expect_equal(names(vector_some), names(ca_dat$estimates$pop_by_race))
  expect_equal(rownames(vector_some), 
               c("Alameda County, California", "Amador County, California", "Los Angeles County, California"))
  expect_equal(vector_some, ca_dat$estimates$pop_by_race[c(1,3,19),])
  
  expect_equal(nrow(vector_one), 1L)
  expect_equal(names(vector_one), names(ca_dat$estimates$pop_by_race))
  expect_equal(rownames(vector_one), "San Bernardino County, California")
  expect_equal(vector_one, ca_dat$estimates$pop_by_race[36,])
  
})

test_that("fetch - pop_by_race - SE work", {
  # load test data:
  load("C:/Github_projects/ACSpulls/synthACS/tests/testthat/acsdat.Rdata")
  rm(list=ls()[-which(ls() == "ca_dat")])
  
  # run function on all types of use cases
  options(warn=-1)
  vector_all <- fetch_data(ca_dat, geography= "*", dataset= "st.err",
                           choice= "pop_by_race")
  vector_some <- fetch_data(ca_dat, geography= c("Alameda", "Amador", "Los Angeles"), 
                            dataset= "st.err", choice= "pop_by_race")
  vector_one <- fetch_data(ca_dat, geography= "San Bernardino", dataset= "st.err", 
                           choice= "pop_by_race")
  options(warn=0)
  
  # test outputs
  expect_true(exists("vector_all"))
  expect_true(exists("vector_some"))
  expect_true(exists("vector_one"))
  expect_true(is.data.frame(vector_all))
  expect_true(is.data.frame(vector_some))
  expect_true(is.data.frame(vector_one))
  
  expect_equal(nrow(vector_all), nrow(ca_dat$geography))
  expect_equal(names(vector_all), names(ca_dat$standard_error$pop_by_race))
  expect_equal(rownames(vector_all), ca_dat$geography$NAME)
  expect_equal(vector_all, ca_dat$standard_error$pop_by_race)
  
  expect_equal(nrow(vector_some), 3L)
  expect_equal(names(vector_some), names(ca_dat$standard_error$pop_by_race))
  expect_equal(rownames(vector_some), 
               c("Alameda County, California", "Amador County, California", "Los Angeles County, California"))
  expect_equal(vector_some, ca_dat$standard_error$pop_by_race[c(1,3,19),])
  
  expect_equal(nrow(vector_one), 1L)
  expect_equal(names(vector_one), names(ca_dat$standard_error$pop_by_race))
  expect_equal(rownames(vector_one), "San Bernardino County, California")
  expect_equal(vector_one, ca_dat$standard_error$pop_by_race[36,])
  
})


#----------------------------
test_that("span, endyear, geography", {
  # load test data:
  load("C:/Github_projects/ACSpulls/synthACS/tests/testthat/acsdat.Rdata")
  rm(list=ls()[-which(ls() == "ca_dat")])
  
  sp <- get_span(ca_dat)
  expect_equal(sp, ca_dat$span)
  
  end <- get_endyear(ca_dat)
  expect_equal(end, ca_dat$endyear)
  
  g <- get_geography(ca_dat)
  expect_equal(g, ca_dat$geo_title)
})
