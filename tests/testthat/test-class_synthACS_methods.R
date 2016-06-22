
library(testthat)
library(synthACS)

#----------------------------------------------------------
context("synthACS methods")
#----------------------------------------------------------

test_that("gender constraint creates successfully", {
  # load test data:
  load("C:/Github_projects/ACSpulls/synthACS/tests/testthat/par_sim_anneal.Rdata")
  rm(list=ls()[-which(ls() == "syn")])
  
  # run function
  g <- all_geog_constraint_gender(syn, method= "macro.table")
  g2 <- all_geog_constraint_gender(syn, method= "synthetic")
  
  # test outputs
  expect_equal(class(g), "list")
  expect_equal(length(g), length(syn))
  expect_equal(names(g), names(syn))
  expect_true(all(unlist(lapply(g, names)) == rep(c("Male", "Female"), 4)))
  expect_equal(g[[1]], syn[[1]]$macro_constraints$age_by_sex[2:3], check.attributes= FALSE)
  expect_equal(g[[2]], syn[[2]]$macro_constraints$age_by_sex[2:3], check.attributes= FALSE)
  expect_equal(g[[3]], syn[[3]]$macro_constraints$age_by_sex[2:3], check.attributes= FALSE)
  expect_equal(g[[4]], syn[[4]]$macro_constraints$age_by_sex[2:3], check.attributes= FALSE)
  
  expect_equal(class(g2), "list")
  expect_equal(length(g2), length(syn))
  expect_equal(names(g2), names(syn))
  expect_true(all(unlist(lapply(g2, names)) == rep(c("Male", "Female"), 4)))
  expect_equal(g2[[1]], syn[[1]]$macro_constraints$age_by_sex[2:3], check.attributes= FALSE)
  expect_equal(g2[[2]], syn[[2]]$macro_constraints$age_by_sex[2:3], check.attributes= FALSE)
  expect_equal(g2[[3]], syn[[3]]$macro_constraints$age_by_sex[2:3], check.attributes= FALSE)
  expect_equal(g2[[4]], syn[[4]]$macro_constraints$age_by_sex[2:3], check.attributes= FALSE)
  
})


test_that("age constraint creates successfully", {
  # load test data:
  load("C:/Github_projects/ACSpulls/synthACS/tests/testthat/par_sim_anneal.Rdata")
  rm(list=ls()[-which(ls() == "syn")])
  
  # run function
  g <- all_geog_constraint_age(syn, method= "macro.table")
  g2 <- all_geog_constraint_age(syn, method= "synthetic")
  
  # test outputs
  expect_equal(class(g), "list")
  expect_equal(length(g), length(syn))
  expect_equal(names(g), names(syn))
  expect_true(all(unlist(lapply(g, names)) == rep(levels(syn[[1]][[2]]$age), 4)))
  expect_equal(g[[1]], syn[[1]]$macro_constraints$age_by_sex[4:19] + syn[[1]]$macro_constraints$age_by_sex[20:35], check.attributes= FALSE)
  expect_equal(g[[2]], syn[[2]]$macro_constraints$age_by_sex[4:19] + syn[[2]]$macro_constraints$age_by_sex[20:35], check.attributes= FALSE)
  expect_equal(g[[3]], syn[[3]]$macro_constraints$age_by_sex[4:19] + syn[[3]]$macro_constraints$age_by_sex[20:35], check.attributes= FALSE)
  expect_equal(g[[4]], syn[[4]]$macro_constraints$age_by_sex[4:19] + syn[[4]]$macro_constraints$age_by_sex[20:35], check.attributes= FALSE)
  
  expect_equal(class(g2), "list")
  expect_equal(length(g2), length(syn))
  expect_equal(names(g2), names(syn))
  expect_true(all(unlist(lapply(g2, names)) == rep(levels(syn[[1]][[2]]$age), 4)))
  expect_equal(g2[[1]], syn[[1]]$macro_constraints$age_by_sex[4:19] + syn[[1]]$macro_constraints$age_by_sex[20:35], check.attributes= FALSE)
  expect_equal(g2[[2]], syn[[2]]$macro_constraints$age_by_sex[4:19] + syn[[2]]$macro_constraints$age_by_sex[20:35], check.attributes= FALSE)
  expect_equal(g2[[3]], syn[[3]]$macro_constraints$age_by_sex[4:19] + syn[[3]]$macro_constraints$age_by_sex[20:35], check.attributes= FALSE)
  expect_equal(g2[[4]], syn[[4]]$macro_constraints$age_by_sex[4:19] + syn[[4]]$macro_constraints$age_by_sex[20:35], check.attributes= FALSE)
  
})

test_that("marital status constraint creates successfully", {
  # load test data:
  load("C:/Github_projects/ACSpulls/synthACS/tests/testthat/par_sim_anneal.Rdata")
  rm(list=ls()[-which(ls() == "syn")])
  
  # run function
  g <- all_geog_constraint_marital_status(syn, method= "macro.table")
  g2 <- all_geog_constraint_marital_status(syn, method= "synthetic")
  
  # test outputs
  expect_equal(class(g), "list")
  expect_equal(length(g), length(syn))
  expect_equal(names(g), names(syn))
  expect_true(all(unlist(lapply(g, names)) == rep(levels(syn[[1]][[2]]$marital_status), 4)))
  expect_equal(sum(g[[1]]), syn[[1]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g[[2]]), syn[[2]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g[[3]]), syn[[3]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g[[4]]), syn[[4]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  
  expect_equal(class(g2), "list")
  expect_equal(length(g2), length(syn))
  expect_equal(names(g2), names(syn))
  expect_true(all(unlist(lapply(g2, names)) == rep(levels(syn[[1]][[2]]$marital_status), 4)))
  expect_equal(sum(g2[[1]]), syn[[1]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g2[[2]]), syn[[2]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g2[[3]]), syn[[3]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g2[[4]]), syn[[4]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  
})


test_that("employment status constraint creates successfully", {
  # load test data:
  load("C:/Github_projects/ACSpulls/synthACS/tests/testthat/par_sim_anneal.Rdata")
  rm(list=ls()[-which(ls() == "syn")])
  
  # run function
  g <- all_geog_constraint_employment(syn, method= "macro.table")
  g2 <- all_geog_constraint_employment(syn, method= "synthetic")
  
  # test outputs
  expect_equal(class(g), "list")
  expect_equal(length(g), length(syn))
  expect_equal(names(g), names(syn))
  expect_true(all(unlist(lapply(g, names)) == rep(levels(syn[[1]][[2]]$emp_status), 4)))
  expect_equal(sum(g[[1]]), syn[[1]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g[[2]]), syn[[2]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g[[3]]), syn[[3]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g[[4]]), syn[[4]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  
  expect_equal(class(g2), "list")
  expect_equal(length(g2), length(syn))
  expect_equal(names(g2), names(syn))
  expect_true(all(unlist(lapply(g2, names)) == rep(levels(syn[[1]][[2]]$emp_status), 4)))
  expect_equal(sum(g2[[1]]), syn[[1]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g2[[2]]), syn[[2]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g2[[3]]), syn[[3]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g2[[4]]), syn[[4]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  
})

test_that("nativity status constraint creates successfully", {
  # load test data:
  load("C:/Github_projects/ACSpulls/synthACS/tests/testthat/par_sim_anneal.Rdata")
  rm(list=ls()[-which(ls() == "syn")])
  
  # run function
  g <- all_geog_constraint_nativity(syn, method= "macro.table")
  g2 <- all_geog_constraint_nativity(syn, method= "synthetic")
  
  # test outputs
  expect_equal(class(g), "list")
  expect_equal(length(g), length(syn))
  expect_equal(names(g), names(syn))
  expect_true(all(unlist(lapply(g, names)) == rep(levels(syn[[1]][[2]]$nativity), 4)))
  expect_equal(sum(g[[1]]), syn[[1]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g[[2]]), syn[[2]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g[[3]]), syn[[3]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g[[4]]), syn[[4]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  
  expect_equal(class(g2), "list")
  expect_equal(length(g2), length(syn))
  expect_equal(names(g2), names(syn))
  expect_true(all(unlist(lapply(g2, names)) == rep(levels(syn[[1]][[2]]$nativity), 4)))
  expect_equal(sum(g2[[1]]), syn[[1]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g2[[2]]), syn[[2]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g2[[3]]), syn[[3]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g2[[4]]), syn[[4]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  
})

test_that("poverty status constraint creates successfully", {
  # load test data:
  load("C:/Github_projects/ACSpulls/synthACS/tests/testthat/par_sim_anneal.Rdata")
  rm(list=ls()[-which(ls() == "syn")])
  
  # run function
  g <- all_geog_constraint_poverty(syn, method= "macro.table")
  g2 <- all_geog_constraint_poverty(syn, method= "synthetic")
  
  # test outputs
  expect_equal(class(g), "list")
  expect_equal(length(g), length(syn))
  expect_equal(names(g), names(syn))
  expect_true(all(unlist(lapply(g, names)) == rep(levels(syn[[1]][[2]]$poverty), 4)))
  expect_equal(sum(g[[1]]), syn[[1]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g[[2]]), syn[[2]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g[[3]]), syn[[3]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g[[4]]), syn[[4]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  
  expect_equal(class(g2), "list")
  expect_equal(length(g2), length(syn))
  expect_equal(names(g2), names(syn))
  expect_true(all(unlist(lapply(g2, names)) == rep(levels(syn[[1]][[2]]$poverty), 4)))
  expect_equal(sum(g2[[1]]), syn[[1]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g2[[2]]), syn[[2]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g2[[3]]), syn[[3]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g2[[4]]), syn[[4]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  
})

test_that("geographic mobility constraint creates successfully", {
  # load test data:
  load("C:/Github_projects/ACSpulls/synthACS/tests/testthat/par_sim_anneal.Rdata")
  rm(list=ls()[-which(ls() == "syn")])
  
  # run function
  g <- all_geog_constraint_geog_mob(syn, method= "macro.table")
  g2 <- all_geog_constraint_geog_mob(syn, method= "synthetic")
  
  # test outputs
  expect_equal(class(g), "list")
  expect_equal(length(g), length(syn))
  expect_equal(names(g), names(syn))
  expect_true(all(unlist(lapply(g, names)) == rep(levels(syn[[1]][[2]]$geog_mobility), 4)))
  expect_equal(sum(g[[1]]), syn[[1]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g[[2]]), syn[[2]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g[[3]]), syn[[3]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g[[4]]), syn[[4]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  
  expect_equal(class(g2), "list")
  expect_equal(length(g2), length(syn))
  expect_equal(names(g2), names(syn))
  expect_true(all(unlist(lapply(g2, names)) == rep(levels(syn[[1]][[2]]$geog_mobility), 4)))
  expect_equal(sum(g2[[1]]), syn[[1]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g2[[2]]), syn[[2]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g2[[3]]), syn[[3]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g2[[4]]), syn[[4]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  
})

test_that("individual income constraint creates successfully", {
  # load test data:
  load("C:/Github_projects/ACSpulls/synthACS/tests/testthat/par_sim_anneal.Rdata")
  rm(list=ls()[-which(ls() == "syn")])
  
  # run function
  g <- all_geog_constraint_income(syn, method= "macro.table")
  g2 <- all_geog_constraint_income(syn, method= "synthetic")
  
  # test outputs
  expect_equal(class(g), "list")
  expect_equal(length(g), length(syn))
  expect_equal(names(g), names(syn))
  expect_true(all(unlist(lapply(g, names)) == rep(levels(syn[[1]][[2]]$ind_income), 4)))
  expect_equal(sum(g[[1]]), syn[[1]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g[[2]]), syn[[2]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g[[3]]), syn[[3]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g[[4]]), syn[[4]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  
  expect_equal(class(g2), "list")
  expect_equal(length(g2), length(syn))
  expect_equal(names(g2), names(syn))
  expect_true(all(unlist(lapply(g2, names)) == rep(levels(syn[[1]][[2]]$ind_income), 4)))
  expect_equal(sum(g2[[1]]), syn[[1]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g2[[2]]), syn[[2]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g2[[3]]), syn[[3]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g2[[4]]), syn[[4]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  
})

test_that("race constraint creates successfully", {
  # load test data:
  load("C:/Github_projects/ACSpulls/synthACS/tests/testthat/par_sim_anneal.Rdata")
  rm(list=ls()[-which(ls() == "syn")])
  
  # run function
  #g <- all_geog_constraint_race(syn, method= "macro.table")
  g2 <- all_geog_constraint_race(syn, method= "synthetic")
  
  # test outputs
  # expect_equal(class(g), "list")
  # expect_equal(length(g), length(syn))
  # expect_equal(names(g), names(syn))
  # expect_true(all(unlist(lapply(g, names)) %in% levels(syn[[2]][[2]]$race)))
  # expect_equal(sum(g[[1]]), syn[[1]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  # expect_equal(sum(g[[2]]), syn[[2]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  # expect_equal(sum(g[[3]]), syn[[3]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  # expect_equal(sum(g[[4]]), syn[[4]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  
  expect_equal(class(g2), "list")
  expect_equal(length(g2), length(syn))
  expect_equal(names(g2), names(syn))
  expect_true(all(unlist(lapply(g2, names)) %in% levels(syn[[2]][[2]]$race)))
  expect_equal(sum(g2[[1]]), syn[[1]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g2[[2]]), syn[[2]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g2[[3]]), syn[[3]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  expect_equal(sum(g2[[4]]), syn[[4]]$macro_constraints$age_by_sex[1], check.attributes = FALSE)
  
})