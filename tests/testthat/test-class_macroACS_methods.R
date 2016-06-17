
library(testthat)
library(synthACS)

#----------------------------------------------------------
context("macroACS methods")
#----------------------------------------------------------

test_that("get_age_by_sex - estimates work", {
  # load test data:
  load("C:/Github_projects/ACSpulls/synthACS/tests/testthat/acsdat.Rdata")
  rm(list=ls()[-which(ls() == "ca_dat")])
  
  # run function on all types of use cases
  options(warn=-1)
  age_sex_vector_all <- get_age_by_sex(ca_dat, geography= "*", dataset= "estimate")
  age_sex_vector_some <- get_age_by_sex(ca_dat, geography= c("Alameda", "Amador", "Los Angeles"), 
                                        dataset= "estimate")
  age_sex_vector_one <- get_age_by_sex(ca_dat, geography= "San Bernardino", dataset= "estimate")
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

test_that("get_age_by_sex - SE work", {
  # load test data:
  load("C:/Github_projects/ACSpulls/synthACS/tests/testthat/acsdat.Rdata")
  rm(list=ls()[-which(ls() == "ca_dat")])
  
  # run function on all types of use cases
  options(warn=-1)
  age_sex_vector_all <- get_age_by_sex(ca_dat, geography= "*", dataset= "st.err")
  age_sex_vector_some <- get_age_by_sex(ca_dat, geography= c("Alameda", "Amador", "Los Angeles"), 
                                        dataset= "st.err")
  age_sex_vector_one <- get_age_by_sex(ca_dat, geography= "San Bernardino", dataset= "st.err")
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
test_that("get_pop_by_race - estimates work", {
  # load test data:
  load("C:/Github_projects/ACSpulls/synthACS/tests/testthat/acsdat.Rdata")
  rm(list=ls()[-which(ls() == "ca_dat")])
  
  # run function on all types of use cases
  options(warn=-1)
  vector_all <- get_pop_by_race(ca_dat, geography= "*", dataset= "estimate")
  vector_some <- get_pop_by_race(ca_dat, geography= c("Alameda", "Amador", "Los Angeles"), 
                                        dataset= "estimate")
  vector_one <- get_pop_by_race(ca_dat, geography= "San Bernardino", dataset= "estimate")
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

test_that("get_pop_by_race - SE work", {
  # load test data:
  load("C:/Github_projects/ACSpulls/synthACS/tests/testthat/acsdat.Rdata")
  rm(list=ls()[-which(ls() == "ca_dat")])
  
  # run function on all types of use cases
  options(warn=-1)
  vector_all <- get_pop_by_race(ca_dat, geography= "*", dataset= "st.err")
  vector_some <- get_pop_by_race(ca_dat, geography= c("Alameda", "Amador", "Los Angeles"), 
                                 dataset= "st.err")
  vector_one <- get_pop_by_race(ca_dat, geography= "San Bernardino", dataset= "st.err")
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
test_that("get_marital_status - estimates work", {
  # load test data:
  load("C:/Github_projects/ACSpulls/synthACS/tests/testthat/acsdat.Rdata")
  rm(list=ls()[-which(ls() == "ca_dat")])
  
  # run function on all types of use cases
  options(warn=-1)
  vector_all <- get_marital_status(ca_dat, geography= "*", dataset= "estimate")
  vector_some <- get_marital_status(ca_dat, geography= c("Alameda", "Amador", "Los Angeles"), 
                                 dataset= "estimate")
  vector_one <- get_marital_status(ca_dat, geography= "San Bernardino", dataset= "estimate")
  options(warn=0)
  
  # test outputs
  expect_true(exists("vector_all"))
  expect_true(exists("vector_some"))
  expect_true(exists("vector_one"))
  expect_true(is.data.frame(vector_all))
  expect_true(is.data.frame(vector_some))
  expect_true(is.data.frame(vector_one))
  
  expect_equal(nrow(vector_all), nrow(ca_dat$geography))
  expect_equal(names(vector_all), names(ca_dat$estimates$marital_status))
  expect_equal(rownames(vector_all), ca_dat$geography$NAME)
  expect_equal(vector_all, ca_dat$estimates$marital_status)
  
  expect_equal(nrow(vector_some), 3L)
  expect_equal(names(vector_some), names(ca_dat$estimates$marital_status))
  expect_equal(rownames(vector_some), 
               c("Alameda County, California", "Amador County, California", "Los Angeles County, California"))
  expect_equal(vector_some, ca_dat$estimates$marital_status[c(1,3,19),])
  
  expect_equal(nrow(vector_one), 1L)
  expect_equal(names(vector_one), names(ca_dat$estimates$marital_status))
  expect_equal(rownames(vector_one), "San Bernardino County, California")
  expect_equal(vector_one, ca_dat$estimates$marital_status[36,])
  
})

test_that("get_marital_status - SE work", {
  # load test data:
  load("C:/Github_projects/ACSpulls/synthACS/tests/testthat/acsdat.Rdata")
  rm(list=ls()[-which(ls() == "ca_dat")])
  
  # run function on all types of use cases
  options(warn=-1)
  vector_all <- get_marital_status(ca_dat, geography= "*", dataset= "st.err")
  vector_some <- get_marital_status(ca_dat, geography= c("Alameda", "Amador", "Los Angeles"), 
                                 dataset= "st.err")
  vector_one <- get_marital_status(ca_dat, geography= "San Bernardino", dataset= "st.err")
  options(warn=0)
  
  # test outputs
  expect_true(exists("vector_all"))
  expect_true(exists("vector_some"))
  expect_true(exists("vector_one"))
  expect_true(is.data.frame(vector_all))
  expect_true(is.data.frame(vector_some))
  expect_true(is.data.frame(vector_one))
  
  expect_equal(nrow(vector_all), nrow(ca_dat$geography))
  expect_equal(names(vector_all), names(ca_dat$standard_error$marital_status))
  expect_equal(rownames(vector_all), ca_dat$geography$NAME)
  expect_equal(vector_all, ca_dat$standard_error$marital_status)
  
  expect_equal(nrow(vector_some), 3L)
  expect_equal(names(vector_some), names(ca_dat$standard_error$marital_status))
  expect_equal(rownames(vector_some), 
               c("Alameda County, California", "Amador County, California", "Los Angeles County, California"))
  expect_equal(vector_some, ca_dat$standard_error$marital_status[c(1,3,19),])
  
  expect_equal(nrow(vector_one), 1L)
  expect_equal(names(vector_one), names(ca_dat$standard_error$marital_status))
  expect_equal(rownames(vector_one), "San Bernardino County, California")
  expect_equal(vector_one, ca_dat$standard_error$marital_status[36,])
  
})

#----------------------------
test_that("get_education - estimates work", {
  # load test data:
  load("C:/Github_projects/ACSpulls/synthACS/tests/testthat/acsdat.Rdata")
  rm(list=ls()[-which(ls() == "ca_dat")])
  
  # run function on all types of use cases
  options(warn=-1)
  vector_all <- get_education(ca_dat, geography= "*", dataset= "estimate")
  vector_some <- get_education(ca_dat, geography= c("Alameda", "Amador", "Los Angeles"), 
                                    dataset= "estimate")
  vector_one <- get_education(ca_dat, geography= "San Bernardino", dataset= "estimate")
  options(warn=0)
  
  # test outputs
  expect_true(exists("vector_all"))
  expect_true(exists("vector_some"))
  expect_true(exists("vector_one"))
  expect_true(is.data.frame(vector_all))
  expect_true(is.data.frame(vector_some))
  expect_true(is.data.frame(vector_one))
  
  expect_equal(nrow(vector_all), nrow(ca_dat$geography))
  expect_equal(names(vector_all), names(ca_dat$estimates$edu))
  expect_equal(rownames(vector_all), ca_dat$geography$NAME)
  expect_equal(vector_all, ca_dat$estimates$edu)
  
  expect_equal(nrow(vector_some), 3L)
  expect_equal(names(vector_some), names(ca_dat$estimates$edu))
  expect_equal(rownames(vector_some), 
               c("Alameda County, California", "Amador County, California", "Los Angeles County, California"))
  expect_equal(vector_some, ca_dat$estimates$edu[c(1,3,19),])
  
  expect_equal(nrow(vector_one), 1L)
  expect_equal(names(vector_one), names(ca_dat$estimates$edu))
  expect_equal(rownames(vector_one), "San Bernardino County, California")
  expect_equal(vector_one, ca_dat$estimates$edu[36,])
  
})

test_that("get_education - SE work", {
  # load test data:
  load("C:/Github_projects/ACSpulls/synthACS/tests/testthat/acsdat.Rdata")
  rm(list=ls()[-which(ls() == "ca_dat")])
  
  # run function on all types of use cases
  options(warn=-1)
  vector_all <- get_education(ca_dat, geography= "*", dataset= "st.err")
  vector_some <- get_education(ca_dat, geography= c("Alameda", "Amador", "Los Angeles"), 
                                    dataset= "st.err")
  vector_one <- get_education(ca_dat, geography= "San Bernardino", dataset= "st.err")
  options(warn=0)
  
  # test outputs
  expect_true(exists("vector_all"))
  expect_true(exists("vector_some"))
  expect_true(exists("vector_one"))
  expect_true(is.data.frame(vector_all))
  expect_true(is.data.frame(vector_some))
  expect_true(is.data.frame(vector_one))
  
  expect_equal(nrow(vector_all), nrow(ca_dat$geography))
  expect_equal(names(vector_all), names(ca_dat$standard_error$edu))
  expect_equal(rownames(vector_all), ca_dat$geography$NAME)
  expect_equal(vector_all, ca_dat$standard_error$edu)
  
  expect_equal(nrow(vector_some), 3L)
  expect_equal(names(vector_some), names(ca_dat$standard_error$edu))
  expect_equal(rownames(vector_some), 
               c("Alameda County, California", "Amador County, California", "Los Angeles County, California"))
  expect_equal(vector_some, ca_dat$standard_error$edu[c(1,3,19),])
  
  expect_equal(nrow(vector_one), 1L)
  expect_equal(names(vector_one), names(ca_dat$standard_error$edu))
  expect_equal(rownames(vector_one), "San Bernardino County, California")
  expect_equal(vector_one, ca_dat$standard_error$edu[36,])
  
})

#----------------------------
test_that("get_nativity - estimates work", {
  # load test data:
  load("C:/Github_projects/ACSpulls/synthACS/tests/testthat/acsdat.Rdata")
  rm(list=ls()[-which(ls() == "ca_dat")])
  
  # run function on all types of use cases
  options(warn=-1)
  vector_all <- get_nativity(ca_dat, geography= "*", dataset= "estimate")
  vector_some <- get_nativity(ca_dat, geography= c("Alameda", "Amador", "Los Angeles"), 
                               dataset= "estimate")
  vector_one <- get_nativity(ca_dat, geography= "San Bernardino", dataset= "estimate")
  options(warn=0)
  
  
  # test outputs
  expect_true(exists("vector_all"))
  expect_true(exists("vector_some"))
  expect_true(exists("vector_one"))
  expect_true(is.data.frame(vector_all))
  expect_true(is.data.frame(vector_some))
  expect_true(is.data.frame(vector_one))
  
  expect_equal(nrow(vector_all), nrow(ca_dat$geography))
  expect_equal(names(vector_all), names(ca_dat$estimates$nativity))
  expect_equal(rownames(vector_all), ca_dat$geography$NAME)
  expect_equal(vector_all, ca_dat$estimates$nativity)
  
  expect_equal(nrow(vector_some), 3L)
  expect_equal(names(vector_some), names(ca_dat$estimates$nativity))
  expect_equal(rownames(vector_some), 
               c("Alameda County, California", "Amador County, California", "Los Angeles County, California"))
  expect_equal(vector_some, ca_dat$estimates$nativity[c(1,3,19),])
  
  expect_equal(nrow(vector_one), 1L)
  expect_equal(names(vector_one), names(ca_dat$estimates$nativity))
  expect_equal(rownames(vector_one), "San Bernardino County, California")
  expect_equal(vector_one, ca_dat$estimates$nativity[36,])
  
})

test_that("get_nativity - SE work", {
  # load test data:
  load("C:/Github_projects/ACSpulls/synthACS/tests/testthat/acsdat.Rdata")
  rm(list=ls()[-which(ls() == "ca_dat")])
  
  # run function on all types of use cases
  options(warn=-1)
  vector_all <- get_nativity(ca_dat, geography= "*", dataset= "st.err")
  vector_some <- get_nativity(ca_dat, geography= c("Alameda", "Amador", "Los Angeles"), 
                               dataset= "st.err")
  vector_one <- get_nativity(ca_dat, geography= "San Bernardino", dataset= "st.err")
  options(warn=0)
  
  # test outputs
  expect_true(exists("vector_all"))
  expect_true(exists("vector_some"))
  expect_true(exists("vector_one"))
  expect_true(is.data.frame(vector_all))
  expect_true(is.data.frame(vector_some))
  expect_true(is.data.frame(vector_one))
  
  expect_equal(nrow(vector_all), nrow(ca_dat$geography))
  expect_equal(names(vector_all), names(ca_dat$standard_error$nativity))
  expect_equal(rownames(vector_all), ca_dat$geography$NAME)
  expect_equal(vector_all, ca_dat$standard_error$nativity)
  
  expect_equal(nrow(vector_some), 3L)
  expect_equal(names(vector_some), names(ca_dat$standard_error$nativity))
  expect_equal(rownames(vector_some), 
               c("Alameda County, California", "Amador County, California", "Los Angeles County, California"))
  expect_equal(vector_some, ca_dat$standard_error$nativity[c(1,3,19),])
  
  expect_equal(nrow(vector_one), 1L)
  expect_equal(names(vector_one), names(ca_dat$standard_error$nativity))
  expect_equal(rownames(vector_one), "San Bernardino County, California")
  expect_equal(vector_one, ca_dat$standard_error$nativity[36,])
  
})

#----------------------------
test_that("get_nativity_by_income - estimates work", {
  # load test data:
  load("C:/Github_projects/ACSpulls/synthACS/tests/testthat/acsdat.Rdata")
  rm(list=ls()[-which(ls() == "ca_dat")])
  
  # run function on all types of use cases
  options(warn=-1)
  vector_all <- get_nativity_by_income(ca_dat, geography= "*", dataset= "estimate")
  vector_some <- get_nativity_by_income(ca_dat, geography= c("Alameda", "Amador", "Los Angeles"), 
                              dataset= "estimate")
  vector_one <- get_nativity_by_income(ca_dat, geography= "San Bernardino", dataset= "estimate")
  options(warn=0)
  
  # test outputs
  expect_true(exists("vector_all"))
  expect_true(exists("vector_some"))
  expect_true(exists("vector_one"))
  expect_true(is.data.frame(vector_all))
  expect_true(is.data.frame(vector_some))
  expect_true(is.data.frame(vector_one))
  
  expect_equal(nrow(vector_all), nrow(ca_dat$geography))
  expect_equal(names(vector_all), names(ca_dat$estimates$by_inc_12mo))
  expect_equal(rownames(vector_all), ca_dat$geography$NAME)
  expect_equal(vector_all, ca_dat$estimates$by_inc_12mo)
  
  expect_equal(nrow(vector_some), 3L)
  expect_equal(names(vector_some), names(ca_dat$estimates$by_inc_12mo))
  expect_equal(rownames(vector_some), 
               c("Alameda County, California", "Amador County, California", "Los Angeles County, California"))
  expect_equal(vector_some, ca_dat$estimates$by_inc_12mo[c(1,3,19),])
  
  expect_equal(nrow(vector_one), 1L)
  expect_equal(names(vector_one), names(ca_dat$estimates$by_inc_12mo))
  expect_equal(rownames(vector_one), "San Bernardino County, California")
  expect_equal(vector_one, ca_dat$estimates$by_inc_12mo[36,])
  
})

test_that("get_nativity_by_income - SE work", {
  # load test data:
  load("C:/Github_projects/ACSpulls/synthACS/tests/testthat/acsdat.Rdata")
  rm(list=ls()[-which(ls() == "ca_dat")])
  
  # run function on all types of use cases
  options(warn=-1)
  vector_all <- get_nativity_by_income(ca_dat, geography= "*", dataset= "st.err")
  vector_some <- get_nativity_by_income(ca_dat, geography= c("Alameda", "Amador", "Los Angeles"), 
                              dataset= "st.err")
  vector_one <- get_nativity_by_income(ca_dat, geography= "San Bernardino", dataset= "st.err")
  options(warn=0)
  
  # test outputs
  expect_true(exists("vector_all"))
  expect_true(exists("vector_some"))
  expect_true(exists("vector_one"))
  expect_true(is.data.frame(vector_all))
  expect_true(is.data.frame(vector_some))
  expect_true(is.data.frame(vector_one))
  
  expect_equal(nrow(vector_all), nrow(ca_dat$geography))
  expect_equal(names(vector_all), names(ca_dat$standard_error$by_inc_12mo))
  expect_equal(rownames(vector_all), ca_dat$geography$NAME)
  expect_equal(vector_all, ca_dat$standard_error$by_inc_12mo)
  
  expect_equal(nrow(vector_some), 3L)
  expect_equal(names(vector_some), names(ca_dat$standard_error$by_inc_12mo))
  expect_equal(rownames(vector_some), 
               c("Alameda County, California", "Amador County, California", "Los Angeles County, California"))
  expect_equal(vector_some, ca_dat$standard_error$by_inc_12mo[c(1,3,19),])
  
  expect_equal(nrow(vector_one), 1L)
  expect_equal(names(vector_one), names(ca_dat$standard_error$by_inc_12mo))
  expect_equal(rownames(vector_one), "San Bernardino County, California")
  expect_equal(vector_one, ca_dat$standard_error$by_inc_12mo[36,])
  
})

#----------------------------
test_that("get_geographic_mobility - estimates work", {
  # load test data:
  load("C:/Github_projects/ACSpulls/synthACS/tests/testthat/acsdat.Rdata")
  rm(list=ls()[-which(ls() == "ca_dat")])
  
  # run function on all types of use cases
  options(warn=-1)
  vector_all <- get_geographic_mobility(ca_dat, geography= "*", dataset= "estimate")
  vector_some <- get_geographic_mobility(ca_dat, geography= c("Alameda", "Amador", "Los Angeles"), 
                                        dataset= "estimate")
  vector_one <- get_geographic_mobility(ca_dat, geography= "San Bernardino", dataset= "estimate")
  options(warn=0)
  
  # test outputs
  expect_true(exists("vector_all"))
  expect_true(exists("vector_some"))
  expect_true(exists("vector_one"))
  expect_true(is.data.frame(vector_all))
  expect_true(is.data.frame(vector_some))
  expect_true(is.data.frame(vector_one))
  
  expect_equal(nrow(vector_all), nrow(ca_dat$geography))
  expect_equal(names(vector_all), names(ca_dat$estimates$geo_mob_edu))
  expect_equal(rownames(vector_all), ca_dat$geography$NAME)
  expect_equal(vector_all, ca_dat$estimates$geo_mob_edu)
  
  expect_equal(nrow(vector_some), 3L)
  expect_equal(names(vector_some), names(ca_dat$estimates$geo_mob_edu))
  expect_equal(rownames(vector_some), 
               c("Alameda County, California", "Amador County, California", "Los Angeles County, California"))
  expect_equal(vector_some, ca_dat$estimates$geo_mob_edu[c(1,3,19),])
  
  expect_equal(nrow(vector_one), 1L)
  expect_equal(names(vector_one), names(ca_dat$estimates$geo_mob_edu))
  expect_equal(rownames(vector_one), "San Bernardino County, California")
  expect_equal(vector_one, ca_dat$estimates$geo_mob_edu[36,])
  
})

test_that("get_geographic_mobility - SE work", {
  # load test data:
  load("C:/Github_projects/ACSpulls/synthACS/tests/testthat/acsdat.Rdata")
  rm(list=ls()[-which(ls() == "ca_dat")])
  
  # run function on all types of use cases
  options(warn=-1)
  vector_all <- get_geographic_mobility(ca_dat, geography= "*", dataset= "st.err")
  vector_some <- get_geographic_mobility(ca_dat, geography= c("Alameda", "Amador", "Los Angeles"), 
                                        dataset= "st.err")
  vector_one <- get_geographic_mobility(ca_dat, geography= "San Bernardino", dataset= "st.err")
  options(warn=0)
  
  # test outputs
  expect_true(exists("vector_all"))
  expect_true(exists("vector_some"))
  expect_true(exists("vector_one"))
  expect_true(is.data.frame(vector_all))
  expect_true(is.data.frame(vector_some))
  expect_true(is.data.frame(vector_one))
  
  expect_equal(nrow(vector_all), nrow(ca_dat$geography))
  expect_equal(names(vector_all), names(ca_dat$standard_error$geo_mob_edu))
  expect_equal(rownames(vector_all), ca_dat$geography$NAME)
  expect_equal(vector_all, ca_dat$standard_error$geo_mob_edu)
  
  expect_equal(nrow(vector_some), 3L)
  expect_equal(names(vector_some), names(ca_dat$standard_error$geo_mob_edu))
  expect_equal(rownames(vector_some), 
               c("Alameda County, California", "Amador County, California", "Los Angeles County, California"))
  expect_equal(vector_some, ca_dat$standard_error$geo_mob_edu[c(1,3,19),])
  
  expect_equal(nrow(vector_one), 1L)
  expect_equal(names(vector_one), names(ca_dat$standard_error$geo_mob_edu))
  expect_equal(rownames(vector_one), "San Bernardino County, California")
  expect_equal(vector_one, ca_dat$standard_error$geo_mob_edu[36,])
  
})


#----------------------------
test_that("get_ind_income - estimates work", {
  # load test data:
  load("C:/Github_projects/ACSpulls/synthACS/tests/testthat/acsdat.Rdata")
  rm(list=ls()[-which(ls() == "ca_dat")])
  
  # run function on all types of use cases
  options(warn=-1)
  vector_all <- get_ind_income(ca_dat, geography= "*", dataset= "estimate")
  vector_some <- get_ind_income(ca_dat, geography= c("Alameda", "Amador", "Los Angeles"), 
                                         dataset= "estimate")
  vector_one <- get_ind_income(ca_dat, geography= "San Bernardino", dataset= "estimate")
  options(warn=0)
  
  # test outputs
  expect_true(exists("vector_all"))
  expect_true(exists("vector_some"))
  expect_true(exists("vector_one"))
  expect_true(is.data.frame(vector_all))
  expect_true(is.data.frame(vector_some))
  expect_true(is.data.frame(vector_one))
  
  expect_equal(nrow(vector_all), nrow(ca_dat$geography))
  expect_equal(names(vector_all), names(ca_dat$estimates$ind_inc))
  expect_equal(rownames(vector_all), ca_dat$geography$NAME)
  expect_equal(vector_all, ca_dat$estimates$ind_inc)
  
  expect_equal(nrow(vector_some), 3L)
  expect_equal(names(vector_some), names(ca_dat$estimates$ind_inc))
  expect_equal(rownames(vector_some), 
               c("Alameda County, California", "Amador County, California", "Los Angeles County, California"))
  expect_equal(vector_some, ca_dat$estimates$ind_inc[c(1,3,19),])
  
  expect_equal(nrow(vector_one), 1L)
  expect_equal(names(vector_one), names(ca_dat$estimates$ind_inc))
  expect_equal(rownames(vector_one), "San Bernardino County, California")
  expect_equal(vector_one, ca_dat$estimates$ind_inc[36,])
  
})

test_that("get_ind_income - SE work", {
  # load test data:
  load("C:/Github_projects/ACSpulls/synthACS/tests/testthat/acsdat.Rdata")
  rm(list=ls()[-which(ls() == "ca_dat")])
  
  # run function on all types of use cases
  options(warn=-1)
  vector_all <- get_ind_income(ca_dat, geography= "*", dataset= "st.err")
  vector_some <- get_ind_income(ca_dat, geography= c("Alameda", "Amador", "Los Angeles"), 
                                         dataset= "st.err")
  vector_one <- get_ind_income(ca_dat, geography= "San Bernardino", dataset= "st.err")
  options(warn=0)
  
  # test outputs
  expect_true(exists("vector_all"))
  expect_true(exists("vector_some"))
  expect_true(exists("vector_one"))
  expect_true(is.data.frame(vector_all))
  expect_true(is.data.frame(vector_some))
  expect_true(is.data.frame(vector_one))
  
  expect_equal(nrow(vector_all), nrow(ca_dat$geography))
  expect_equal(names(vector_all), names(ca_dat$standard_error$ind_inc))
  expect_equal(rownames(vector_all), ca_dat$geography$NAME)
  expect_equal(vector_all, ca_dat$standard_error$ind_inc)
  
  expect_equal(nrow(vector_some), 3L)
  expect_equal(names(vector_some), names(ca_dat$standard_error$ind_inc))
  expect_equal(rownames(vector_some), 
               c("Alameda County, California", "Amador County, California", "Los Angeles County, California"))
  expect_equal(vector_some, ca_dat$standard_error$ind_inc[c(1,3,19),])
  
  expect_equal(nrow(vector_one), 1L)
  expect_equal(names(vector_one), names(ca_dat$standard_error$ind_inc))
  expect_equal(rownames(vector_one), "San Bernardino County, California")
  expect_equal(vector_one, ca_dat$standard_error$ind_inc[36,])
  
})

#----------------------------
test_that("get_employment_status - estimates work", {
  # load test data:
  load("C:/Github_projects/ACSpulls/synthACS/tests/testthat/acsdat.Rdata")
  rm(list=ls()[-which(ls() == "ca_dat")])
  
  # run function on all types of use cases
  options(warn=-1)
  vector_all <- get_employment_status(ca_dat, geography= "*", dataset= "estimate")
  vector_some <- get_employment_status(ca_dat, geography= c("Alameda", "Amador", "Los Angeles"), 
                                dataset= "estimate")
  vector_one <- get_employment_status(ca_dat, geography= "San Bernardino", dataset= "estimate")
  options(warn=0)
  
  # test outputs
  expect_true(exists("vector_all"))
  expect_true(exists("vector_some"))
  expect_true(exists("vector_one"))
  expect_true(is.data.frame(vector_all))
  expect_true(is.data.frame(vector_some))
  expect_true(is.data.frame(vector_one))
  
  expect_equal(nrow(vector_all), nrow(ca_dat$geography))
  expect_equal(names(vector_all), names(ca_dat$estimates$emp_status))
  expect_equal(rownames(vector_all), ca_dat$geography$NAME)
  expect_equal(vector_all, ca_dat$estimates$emp_status)
  
  expect_equal(nrow(vector_some), 3L)
  expect_equal(names(vector_some), names(ca_dat$estimates$emp_status))
  expect_equal(rownames(vector_some), 
               c("Alameda County, California", "Amador County, California", "Los Angeles County, California"))
  expect_equal(vector_some, ca_dat$estimates$emp_status[c(1,3,19),])
  
  expect_equal(nrow(vector_one), 1L)
  expect_equal(names(vector_one), names(ca_dat$estimates$emp_status))
  expect_equal(rownames(vector_one), "San Bernardino County, California")
  expect_equal(vector_one, ca_dat$estimates$emp_status[36,])
  
})

test_that("get_employment_status - SE work", {
  # load test data:
  load("C:/Github_projects/ACSpulls/synthACS/tests/testthat/acsdat.Rdata")
  rm(list=ls()[-which(ls() == "ca_dat")])
  
  # run function on all types of use cases
  options(warn=-1)
  vector_all <- get_employment_status(ca_dat, geography= "*", dataset= "st.err")
  vector_some <- get_employment_status(ca_dat, geography= c("Alameda", "Amador", "Los Angeles"), 
                                dataset= "st.err")
  vector_one <- get_employment_status(ca_dat, geography= "San Bernardino", dataset= "st.err")
  options(warn=0)
  
  # test outputs
  expect_true(exists("vector_all"))
  expect_true(exists("vector_some"))
  expect_true(exists("vector_one"))
  expect_true(is.data.frame(vector_all))
  expect_true(is.data.frame(vector_some))
  expect_true(is.data.frame(vector_one))
  
  expect_equal(nrow(vector_all), nrow(ca_dat$geography))
  expect_equal(names(vector_all), names(ca_dat$standard_error$emp_status))
  expect_equal(rownames(vector_all), ca_dat$geography$NAME)
  expect_equal(vector_all, ca_dat$standard_error$emp_status)
  
  expect_equal(nrow(vector_some), 3L)
  expect_equal(names(vector_some), names(ca_dat$standard_error$emp_status))
  expect_equal(rownames(vector_some), 
               c("Alameda County, California", "Amador County, California", "Los Angeles County, California"))
  expect_equal(vector_some, ca_dat$standard_error$emp_status[c(1,3,19),])
  
  expect_equal(nrow(vector_one), 1L)
  expect_equal(names(vector_one), names(ca_dat$standard_error$emp_status))
  expect_equal(rownames(vector_one), "San Bernardino County, California")
  expect_equal(vector_one, ca_dat$standard_error$emp_status[36,])
  
})

#----------------------------
test_that("get_poverty_status - estimates work", {
  # load test data:
  load("C:/Github_projects/ACSpulls/synthACS/tests/testthat/acsdat.Rdata")
  rm(list=ls()[-which(ls() == "ca_dat")])
  
  # run function on all types of use cases
  options(warn=-1)
  vector_all <- get_poverty_status(ca_dat, geography= "*", dataset= "estimate")
  vector_some <- get_poverty_status(ca_dat, geography= c("Alameda", "Amador", "Los Angeles"), 
                                       dataset= "estimate")
  vector_one <- get_poverty_status(ca_dat, geography= "San Bernardino", dataset= "estimate")
  options(warn=0)
  
  # test outputs
  expect_true(exists("vector_all"))
  expect_true(exists("vector_some"))
  expect_true(exists("vector_one"))
  expect_true(is.data.frame(vector_all))
  expect_true(is.data.frame(vector_some))
  expect_true(is.data.frame(vector_one))
  
  expect_equal(nrow(vector_all), nrow(ca_dat$geography))
  expect_equal(names(vector_all), names(ca_dat$estimates$pov_status1))
  expect_equal(rownames(vector_all), ca_dat$geography$NAME)
  expect_equal(vector_all, ca_dat$estimates$pov_status1)
  
  expect_equal(nrow(vector_some), 3L)
  expect_equal(names(vector_some), names(ca_dat$estimates$pov_status1))
  expect_equal(rownames(vector_some), 
               c("Alameda County, California", "Amador County, California", "Los Angeles County, California"))
  expect_equal(vector_some, ca_dat$estimates$pov_status1[c(1,3,19),])
  
  expect_equal(nrow(vector_one), 1L)
  expect_equal(names(vector_one), names(ca_dat$estimates$pov_status1))
  expect_equal(rownames(vector_one), "San Bernardino County, California")
  expect_equal(vector_one, ca_dat$estimates$pov_status1[36,])
  
})

test_that("get_poverty_status - SE work", {
  # load test data:
  load("C:/Github_projects/ACSpulls/synthACS/tests/testthat/acsdat.Rdata")
  rm(list=ls()[-which(ls() == "ca_dat")])
  
  # run function on all types of use cases
  options(warn=-1)
  vector_all <- get_poverty_status(ca_dat, geography= "*", dataset= "st.err")
  vector_some <- get_poverty_status(ca_dat, geography= c("Alameda", "Amador", "Los Angeles"), 
                                       dataset= "st.err")
  vector_one <- get_poverty_status(ca_dat, geography= "San Bernardino", dataset= "st.err")
  options(warn=0)
  
  # test outputs
  expect_true(exists("vector_all"))
  expect_true(exists("vector_some"))
  expect_true(exists("vector_one"))
  expect_true(is.data.frame(vector_all))
  expect_true(is.data.frame(vector_some))
  expect_true(is.data.frame(vector_one))
  
  expect_equal(nrow(vector_all), nrow(ca_dat$geography))
  expect_equal(names(vector_all), names(ca_dat$standard_error$pov_status1))
  expect_equal(rownames(vector_all), ca_dat$geography$NAME)
  expect_equal(vector_all, ca_dat$standard_error$pov_status1)
  
  expect_equal(nrow(vector_some), 3L)
  expect_equal(names(vector_some), names(ca_dat$standard_error$pov_status1))
  expect_equal(rownames(vector_some), 
               c("Alameda County, California", "Amador County, California", "Los Angeles County, California"))
  expect_equal(vector_some, ca_dat$standard_error$pov_status1[c(1,3,19),])
  
  expect_equal(nrow(vector_one), 1L)
  expect_equal(names(vector_one), names(ca_dat$standard_error$pov_status1))
  expect_equal(rownames(vector_one), "San Bernardino County, California")
  expect_equal(vector_one, ca_dat$standard_error$pov_status1[36,])
  
})


