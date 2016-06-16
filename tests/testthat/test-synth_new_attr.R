
library(testthat)
library(synthACS)

#----------------------------------------------------------
context("bottom -- mapply_synth")
#----------------------------------------------------------

test_that("mapply - everything works", {
  # create example data
  set.seed(567L)
  df <- data.frame(gender= factor(sample(c("m", "f"), size= 100, replace=T)),
                   age= factor(sample(1:5, size= 100, replace=T)),
                   edu= factor(sample(c("hs", "col", "grad"), size= 100, replace=T)),
                   p= runif(100))
  df$p <- df$p / sum(df$p)
  
  lev <- "new"
  a_p <- 0.5
  a_name <- "var_new"
  
  # test
  dat2 <- synthACS:::mapply_synth(df, "p", attr_pct= a_p, attr_name= a_name, level= lev)
  
  expect_true(all(names(df) %in% names(dat2)))
  expect_true(all(names(dat2) %in% c(a_name, names(df))))
  expect_equal(sum(df$p), sum(dat2$p) / a_p)
  expect_equal(df$p, dat2$p / a_p)
  expect_equal(nrow(df), nrow(dat2))
  expect_equal(ncol(df), ncol(dat2) - 1)
})

#----------------------------------------------------------
context("one level up -- lapply_synth")
#----------------------------------------------------------

test_that("lapply - everything works", {
  # create example data
  set.seed(567L)
  df <- data.frame(gender= factor(sample(c("m", "f"), size= 100, replace=T)),
                   age= factor(sample(1:5, size= 100, replace=T)),
                   edu= factor(sample(c("hs", "col", "grad"), size= 100, replace=T)),
                   p= runif(100))
  df$p <- df$p / sum(df$p)
  
  # and example test elements
  cond_v <- "gender"
  ht <- data.frame(old= levels(df$gender), regex= c("f_", "m_"))
  levels <- c("employed", "unemp", "not_in_labor_force")
  at_v <- structure(c(52, 8, 268, 72, 12, 228, 1338, 93, 297, 921, 
                      105, 554), 
            .Names = c("m_lt_pov_employed", "m_lt_pov_unemp", "m_lt_pov_not_in_labor_force", "f_lt_pov_employed", 
                       "f_lt_pov_unemp", "f_lt_pov_not_in_labor_force", "m_gt_eq_pov_employed", 
                       "m_gt_eq_pov_unemp", "m_gt_eq_pov_not_in_labor_force", "f_gt_eq_pov_employed", 
                       "f_gt_eq_pov_unemp", "f_gt_eq_pov_not_in_labor_force"))
  df2 <- synthACS:::split_df(df, cond_v)
  # run
  dat <- do.call("rbind", lapply(df2, synthACS:::lapply_synth,
                 prob_name= "p", ht= ht, cond_var=cond_v, attr_name= "variable", 
                 attr_v= at_v, levels= levels))
  
  # test output
  expect_equal(nrow(df) * length(ht[,2,drop=T]) * length(levels),
               nrow(dat))
  expect_equal(ncol(df), ncol(dat) - 1)
  expect_true(all(names(df) %in% names(dat)))
  expect_true(all(names(dat) %in% c("variable", names(df))))
  expect_equal(sum(dat$p), 1)
  expect_equal(sum(dat$p), sum(df$p))
  expect_true(all(levels %in% levels(factor(dat$variable))))
  expect_true(all(levels(factor(dat$variable)) %in% levels))
  expect_equal(tapply(dat$p, dat$gender, sum),
               tapply(df$p, df$gender, sum))
})

#----------------------------------------------------------
context("conditional splitting & recursion")
#----------------------------------------------------------

test_that("conditional split -- everything works", {
  # create test data / elements
  # create example data
  set.seed(567L)
  df <- data.frame(gender= factor(sample(c("m", "f"), size= 100, replace=T)),
                   age= factor(sample(1:5, size= 100, replace=T)),
                   pov= factor(sample(c("below poverty", "at above poverty"), 
                                      size= 100, replace=T, prob= c(.15,.85))),
                   p= runif(100))
  df$p <- df$p / sum(df$p)
  
  # and example test elements
  cond_v <- c("gender", "pov")
  ht_list <- list(data.frame(old= levels(df$gender), regex= c("f_", "m_")),
                  data.frame(old= levels(df$pov), regex= c("gt_eq_pov", "lt_pov")))
  
  levels <- c("employed", "unemp", "not_in_labor_force")
  at_v <- structure(c(52, 8, 268, 72, 12, 228, 1338, 93, 297, 921, 
                      105, 554), 
                    .Names = c("m_lt_pov_employed", "m_lt_pov_unemp", "m_lt_pov_not_in_labor_force", "f_lt_pov_employed", 
                               "f_lt_pov_unemp", "f_lt_pov_not_in_labor_force", "m_gt_eq_pov_employed", 
                               "m_gt_eq_pov_unemp", "m_gt_eq_pov_not_in_labor_force", "f_gt_eq_pov_employed", 
                               "f_gt_eq_pov_unemp", "f_gt_eq_pov_not_in_labor_force"))
  # run code
  dat <- synthACS:::cond_var_split(df, "p", attr_name= "variable", 
                        attr_vector= at_v, attr_levels= levels, 
                        conditional_vars= cond_v, ht_list= ht_list)
  # test output
  expect_equal(nrow(df) * length(levels), nrow(dat))
  expect_equal(ncol(df), ncol(dat) - 1)
  expect_true(all(names(df) %in% names(dat)))
  expect_true(all(names(dat) %in% c("variable", names(df))))
  expect_equal(sum(dat$p), 1)
  expect_equal(sum(dat$p), sum(df$p))
  expect_true(all(levels %in% levels(factor(dat$variable))))
  expect_true(all(levels(factor(dat$variable)) %in% levels))
  expect_equal(tapply(dat$p, dat$gender, sum),
               tapply(df$p, df$gender, sum))
  expect_equal(tapply(dat$p, dat$pov, sum),
               tapply(df$p, df$pov, sum))
  
})

#----------------------------------------------------------
context("Synthetic new attribute (top level)")
#----------------------------------------------------------

test_that("error checking", {
  # create test data / elements
  # create example data
  set.seed(567L)
  df <- data.frame(gender= factor(sample(c("m", "f"), size= 100, replace=T)),
                   age= factor(sample(1:5, size= 100, replace=T)),
                   pov= factor(sample(c("below poverty", "at above poverty"), 
                                      size= 100, replace=T, prob= c(.15,.85))),
                   p= runif(100))
  df$p <- df$p / sum(df$p)
  
  at_v <- floor(rnorm(20, 50))
  # create some faulty inputs
  expect_error(synthetic_new_attribute(df= df, prob_name= "p", attr_name= "variable",
                                       attr_vector, attr_levels, 
                                       conditional_vars= NULL, ht_list= NULL))
  expect_error(synthetic_new_attribute(df= df, prob_name= "p", attr_name= "variable",
                                       attr_vector=at_v, attr_levels, 
                                       conditional_vars= NULL, ht_list= NULL))
  expect_error(synthetic_new_attribute(df= df, prob_name= "p", attr_name= "variable",
                                       attr_vector=letters[1:10], attr_levels, 
                                       conditional_vars= NULL, ht_list= NULL))
  names(at_v) <- paste0("x", 1:length(at_v))
  expect_error(synthetic_new_attribute(df= df, prob_name= "p", attr_name= "variable",
                                       attr_vector=at_v, attr_levels, 
                                       conditional_vars= NULL, ht_list= NULL))
  expect_error(synthetic_new_attribute(df= df, prob_name= 57L, attr_name= "variable",
                                       attr_vector=at_v, attr_levels= c("level1", "level2", "level3"), 
                                       conditional_vars= NULL, ht_list= NULL))
  expect_error(synthetic_new_attribute(df= df, prob_name= "abc", attr_name= "variable",
                                       attr_vector=at_v, attr_levels= c("level1", "level2", "level3"), 
                                       conditional_vars= NULL, ht_list= NULL))
  expect_error(synthetic_new_attribute(df= df, prob_name= "p", attr_name,
                                       attr_vector=at_v, attr_levels= c("level1", "level2", "level3"), 
                                       conditional_vars= NULL, ht_list= NULL))
  expect_error(synthetic_new_attribute(df= df, prob_name= "p", attr_name= "variable",
                                       attr_vector=at_v, attr_levels= c("level1", "level2", "level3"), 
                                       conditional_vars= NULL, ht_list= NULL))
  
  class(df) <- "micro_synthetic"
  data(diamonds, package="ggplot2")
  expect_error(synthetic_new_attribute(df= df, prob_name= "p", attr_name= "variable",
                                       attr_vector=at_v, attr_levels= c("level1", "level2", "level3"), 
                                       conditional_vars= c(5,1), ht_list= NULL))
  expect_error(synthetic_new_attribute(df= df, prob_name= "p", attr_name= "variable",
                                       attr_vector=at_v, attr_levels= c("level1", "level2", "level3"), 
                                       conditional_vars= diamonds, ht_list= NULL))
  expect_error(synthetic_new_attribute(df= df, prob_name= "p", attr_name= "variable",
                                       attr_vector=at_v, attr_levels= c("level1", "level2", "level3"), 
                                       conditional_vars= c("gender", "abc"), ht_list= NULL))
  expect_error(synthetic_new_attribute(df= df, prob_name= "p", attr_name= "variable",
                                       attr_vector=at_v, attr_levels= c("level1", "level2", "level3"), 
                                       conditional_vars= c("gender", "pov"), ht_list= NULL))
  expect_error(synthetic_new_attribute(df= df, prob_name= "p", attr_name= "variable",
                                       attr_vector=at_v, attr_levels= c("level1", "level2", "level3"), 
                                       conditional_vars= c("gender", "pov"),
                                       ht_list= list(diamonds, diamonds)))
  
})

test_that("function works appropriately -- conditionally", {
  # create test data / elements
  # create example data
  set.seed(567L)
  df <- data.frame(gender= factor(sample(c("m", "f"), size= 100, replace=T)),
                   age= factor(sample(1:5, size= 100, replace=T)),
                   pov= factor(sample(c("below poverty", "at above poverty"), 
                                      size= 100, replace=T, prob= c(.15,.85))),
                   p= runif(100))
  df$p <- df$p / sum(df$p)
  class(df) <- c("data.frame", "micro_synthetic")
  
  # and example test elements
  cond_v <- c("gender", "pov")
  ht_list <- list(data.frame(old= levels(df$gender), regex= c("f_", "m_")),
                  data.frame(old= levels(df$pov), regex= c("gt_eq_pov", "lt_pov")))
  
  levels <- c("employed", "unemp", "not_in_labor_force")
  at_v <- structure(c(52, 8, 268, 72, 12, 228, 1338, 93, 297, 921, 
                      105, 554), 
                    .Names = c("m_lt_pov_employed", "m_lt_pov_unemp", "m_lt_pov_not_in_labor_force", "f_lt_pov_employed", 
                               "f_lt_pov_unemp", "f_lt_pov_not_in_labor_force", "m_gt_eq_pov_employed", 
                               "m_gt_eq_pov_unemp", "m_gt_eq_pov_not_in_labor_force", "f_gt_eq_pov_employed", 
                               "f_gt_eq_pov_unemp", "f_gt_eq_pov_not_in_labor_force"))
  
  # run
  syn <- synthetic_new_attribute(df= df, prob_name= "p", attr_name= "variable",
                                       attr_vector=at_v, attr_levels= levels, 
                                       conditional_vars= cond_v,
                                       ht_list= ht_list)
  
  # test output
  expect_true(is.micro_synthetic(syn))
  expect_true(is.data.frame(syn))
  expect_equal(sum(syn$p), 1)
  expect_equal(sum(syn$p), sum(df$p))
  expect_equal(tapply(syn$p, syn$gender, sum),
               tapply(df$p, df$gender, sum))
  expect_equal(tapply(syn$p, syn$pov, sum),
               tapply(df$p, df$pov, sum))
  expect_equal(nrow(df) * length(levels), nrow(syn))
  expect_equal(ncol(df), ncol(syn) - 1)
  expect_true(all(names(df) %in% names(syn)))
  expect_true(all(names(syn) %in% c("variable", names(df))))
  expect_true(all(levels %in% levels(factor(syn$variable))))
  expect_true(all(levels(factor(syn$variable)) %in% levels))
  
})

test_that("function works appropriately -- unconditionally", {
  set.seed(567L)
  df <- data.frame(gender= factor(sample(c("m", "f"), size= 100, replace=T)),
                   age= factor(sample(1:5, size= 100, replace=T)),
                   pov= factor(sample(c("below poverty", "at above poverty"), 
                                      size= 100, replace=T, prob= c(.15,.85))),
                   p= runif(100))
  df$p <- df$p / sum(df$p)
  class(df) <- c("data.frame", "micro_synthetic")
  
  # and example test elements
  levels <- c("employed", "unemp", "not_in_labor_force")
  at_v <- c(500, 100, 300)
  names(at_v) <- levels
  
  # run
  syn <- synthetic_new_attribute(df= df, prob_name= "p", attr_name= "variable",
                                 attr_vector=at_v, attr_levels= levels, 
                                 conditional_vars= NULL, ht_list= NULL)
  
  # test output
  expect_true(is.micro_synthetic(syn))
  expect_true(is.data.frame(syn))
  expect_equal(sum(syn$p), 1)
  expect_equal(sum(syn$p), sum(df$p))
  expect_equal(tapply(syn$p, syn$gender, sum),
               tapply(df$p, df$gender, sum))
  expect_equal(tapply(syn$p, syn$pov, sum),
               tapply(df$p, df$pov, sum))
  expect_equal(nrow(df) * length(levels), nrow(syn))
  expect_equal(ncol(df), ncol(syn) - 1)
  expect_true(all(names(df) %in% names(syn)))
  expect_true(all(names(syn) %in% c("variable", names(df))))
  expect_true(all(levels %in% levels(factor(syn$variable))))
  expect_true(all(levels(factor(syn$variable)) %in% levels))
})


#----------------------------------------------------------
context("Synthetic new attribute -- in parallel")
#----------------------------------------------------------


test_that("can add extra attributes in parallel", {
  # create test data / elements
  # create example data
  set.seed(567L)
  df <- data.frame(gender= factor(sample(c("m", "f"), size= 100, replace=T)),
                   age= factor(sample(1:5, size= 100, replace=T)),
                   pov= factor(sample(c("below poverty", "at above poverty"), 
                                      size= 100, replace=T, prob= c(.15,.85))),
                   p= runif(100))
  df$p <- df$p / sum(df$p)
  class(df) <- c("data.frame", "micro_synthetic")
  
  # and example test elements
  cond_v <- c("gender", "pov")
  ht_list <- list(data.frame(old= levels(df$gender), regex= c("f_", "m_")),
                  data.frame(old= levels(df$pov), regex= c("gt_eq_pov", "lt_pov")))
  
  levels <- c("employed", "unemp", "not_in_labor_force")
  at_v <- structure(c(52, 8, 268, 72, 12, 228, 1338, 93, 297, 921, 
                      105, 554), 
                    .Names = c("m_lt_pov_employed", "m_lt_pov_unemp", "m_lt_pov_not_in_labor_force", "f_lt_pov_employed", 
                               "f_lt_pov_unemp", "f_lt_pov_not_in_labor_force", "m_gt_eq_pov_employed", 
                               "m_gt_eq_pov_unemp", "m_gt_eq_pov_not_in_labor_force", "f_gt_eq_pov_employed", 
                               "f_gt_eq_pov_unemp", "f_gt_eq_pov_not_in_labor_force"))
  
  df_list <- replicate(10, df, simplify= FALSE)
  at_v_list <- replicate(10, at_v, simplify= FALSE)
  class(df_list) <- c("list", "synthACS")
  
  # run
  syn <- all_geog_synthetic_new_attribute(df_list, prob_name= "p", attr_name= "variable",
                                          attr_vector_list= at_v_list, attr_levels= levels, 
                                          conditional_vars= cond_v,ht_list= ht_list)
  
  # test output structure
  expect_true(all(unlist(lapply(syn, is.micro_synthetic))))
  expect_true(all(unlist(lapply(syn, is.data.frame))))
  
  # test ouput probabilities
  expect_true(all(unlist(lapply(syn, function(l) sum(l$p) == 1))))
  expect_equal(lapply(syn, function(l) {tapply(l$p, l$gender, sum)}),
               lapply(df_list, function(l) {tapply(l$p, l$gender, sum)}))
  expect_equal(lapply(syn, function(l) {tapply(l$p, l$pov, sum)}),
             lapply(df_list, function(l) {tapply(l$p, l$pov, sum)}))
  
  
  
})