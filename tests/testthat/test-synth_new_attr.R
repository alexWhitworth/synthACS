
library(testthat)
library(synthACS)

#----------------------------------------------------------
context("new attr - bottom of recursion")
#----------------------------------------------------------

test_that("mapply - works as designed (df)", {
  # create example data
  set.seed(567L)
  df <- data.frame(gender= factor(sample(c("m", "f"), size= 100, replace=T)),
                   age= factor(sample(1:5, size= 100, replace=T)),
                   edu= factor(sample(c("hs", "col", "grad"), size= 100, replace=T)),
                   p= runif(100))
  df$p <- df$p / sum(df$p)
  df2  <- data.table::setDT(df)
  
  attr <- data.frame(pct= 0.5, level= "new")
  a_name <- "var_new"
  
  # test
  dat2 <- synthACS:::add_synth_attr_level(df, "p", attr= attr, attr_name= a_name)
  dat3 <- synthACS:::add_synth_attr_level(df2, "p", attr= attr, attr_name= a_name)
  
  # data.frame
  expect_true(all(names(df) %in% names(dat2)))
  expect_true(all(names(dat2) %in% c(a_name, names(df))))
  expect_equal(sum(df$p), sum(dat2$p) / attr[[1]])
  expect_equal(df$p, dat2$p / attr[[1]])
  expect_equal(nrow(df), nrow(dat2))
  expect_equal(ncol(df), ncol(dat2) - 1)
  
  # data.table
  expect_true(all(names(df2) %in% names(dat3)))
  expect_true(all(names(dat3) %in% c(a_name, names(df2))))
  expect_equal(sum(df2$p), sum(dat3$p) / attr[[1]])
  expect_equal(df2$p, dat2$p / attr[[1]])
  expect_equal(nrow(df2), nrow(dat3))
  expect_equal(ncol(df2), ncol(dat3) - 1)
  
  expect_equal(dat2, dat3)
})

# test_that("mapply -- bug catches", {})

#----------------------------------------------------------
context("one level up -- add_synth_attr")
#----------------------------------------------------------
  
test_that("lapply - bug catches", {
  set.seed(567L)
  df <- data.frame(gender= factor(sample(c("m", "f"), size= 100, replace=T)),
                   age= factor(sample(1:5, size= 100, replace=T)),
                   edu= factor(sample(c("hs", "col", "grad"), size= 100, replace=T)),
                   p= runif(100))
  df$p <- df$p / sum(df$p)
  
  # and example test elements
  ST <- data.frame(attr_cnts= as.character(c(0.1,0.1,0.1)),
                   lev= c("employed", "unemp", "not_in_labor_force"))
  ST2 <- data.frame(abc= letters[1:3],
                    attr_cnts= c(60, 10, 30),
                    lev= c("employed", "unemp", "not_in_labor_force"))
  ST3 <- data.frame(lev= c("employed", "unemp", "not_in_labor_force"),
                    attr_cnts= c(60, 10, 30))
  ST4 <- data.frame(lvl= c("employed", "unemp", "not_in_labor_force"),
                    attr_cnts= c(60, 10, 30))
  
  expect_error(synthACS:::add_synth_attr(l= df, prob_name= "p", sym_tbl= ST, attr_name= "variable"))
  expect_error(synthACS:::add_synth_attr(l= df, prob_name= "p", sym_tbl= ST2, attr_name= "variable"))
  expect_error(synthACS:::add_synth_attr(l= df, prob_name= "p", sym_tbl= ST3, attr_name= "variable"))
  expect_error(synthACS:::add_synth_attr(l= df, prob_name= "p", sym_tbl= ST4, attr_name= "variable"))
})

test_that("lapply - valid output with percentages (df)", {
  # create example data
  set.seed(567L)
  df <- data.frame(gender= factor(sample(c("m", "f"), size= 100, replace=T)),
                   age= factor(sample(1:5, size= 100, replace=T)),
                   edu= factor(sample(c("hs", "col", "grad"), size= 100, replace=T)),
                   p= runif(100))
  df$p <- df$p / sum(df$p)
  
  # and example test elements
  sym_tbl <- data.frame(attr_cnts= c(0.6, 0.1, 0.3),
                        lev= c("employed", "unemp", "not_in_labor_force"))
  
  dat <- synthACS:::add_synth_attr(l= df, prob_name= "p", sym_tbl= sym_tbl, attr_name= "variable")
  
  # test output
  expect_equal(nrow(df) * nrow(sym_tbl), nrow(dat))
  expect_equal(ncol(df), ncol(dat) - 1)
  expect_true(all(names(df) %in% names(dat)))
  expect_true(all(names(dat) %in% c("variable", names(df))))
  expect_equal(sum(dat$p), 1)
  expect_equal(sum(dat$p), sum(df$p))
  expect_true(all(sym_tbl$lev %in% levels(factor(dat$variable))))
  expect_true(all(levels(factor(dat$variable)) %in% sym_tbl$lev))
  expect_equal(tapply(dat$p, dat$gender, sum),
               tapply(df$p, df$gender, sum))
  expect_equal(sort(as.vector(tapply(dat$p, dat$variable, sum))), sort(sym_tbl[,1]))
  expect_equal(as.vector(tapply(dat$p, dat$variable, sum)), c(0.6, 0.3, 0.1))
})

test_that("lapply - valid output with percentages (dt)", {
  # create example data
  set.seed(567L)
  df <- data.frame(gender= factor(sample(c("m", "f"), size= 100, replace=T)),
                   age= factor(sample(1:5, size= 100, replace=T)),
                   edu= factor(sample(c("hs", "col", "grad"), size= 100, replace=T)),
                   p= runif(100))
  df$p <- df$p / sum(df$p)
  data.table::setDT(df)
  
  # and example test elements
  sym_tbl <- data.frame(attr_cnts= c(0.6, 0.1, 0.3),
                        lev= c("employed", "unemp", "not_in_labor_force"))
  data.table::setDT(sym_tbl)
  
  dat <- synthACS:::add_synth_attr(l= df, prob_name= "p", sym_tbl= sym_tbl, attr_name= "variable")
  
  # test output
  expect_equal(nrow(df) * nrow(sym_tbl), nrow(dat))
  expect_equal(ncol(df), ncol(dat) - 1)
  expect_true(all(names(df) %in% names(dat)))
  expect_true(all(names(dat) %in% c("variable", names(df))))
  expect_equal(sum(dat$p), 1)
  expect_equal(sum(dat$p), sum(df$p))
  expect_true(all(sym_tbl$lev %in% levels(factor(dat$variable))))
  expect_true(all(levels(factor(dat$variable)) %in% sym_tbl$lev))
  expect_equal(tapply(dat$p, dat$gender, sum),
               tapply(df$p, df$gender, sum))
  expect_equal(sort(as.vector(tapply(dat$p, dat$variable, sum))), c(0.1, 0.3, 0.6))
  expect_equal(as.vector(tapply(dat$p, dat$variable, sum)), c(0.6, 0.3, 0.1))
})


test_that("lapply - valid output with counts  -- DF", {
  # create example data
  set.seed(567L)
  df <- data.frame(gender= factor(sample(c("m", "f"), size= 100, replace=T)),
                   age= factor(sample(1:5, size= 100, replace=T)),
                   edu= factor(sample(c("hs", "col", "grad"), size= 100, replace=T)),
                   p= runif(100))
  df$p <- df$p / sum(df$p)
  
  # and example test elements
  sym_tbl <- data.frame(attr_cnts= c(60, 10, 30),
                        lev= c("employed", "unemp", "not_in_labor_force"))
  
  dat <- synthACS:::add_synth_attr(l= df, prob_name= "p", sym_tbl= sym_tbl, attr_name= "variable")
  
  # test output
  expect_equal(nrow(df) * nrow(sym_tbl), nrow(dat))
  expect_equal(ncol(df), ncol(dat) - 1)
  expect_true(all(names(df) %in% names(dat)))
  expect_true(all(names(dat) %in% c("variable", names(df))))
  expect_equal(sum(dat$p), 1)
  expect_equal(sum(dat$p), sum(df$p))
  expect_true(all(sym_tbl$lev %in% levels(factor(dat$variable))))
  expect_true(all(levels(factor(dat$variable)) %in% sym_tbl$lev))
  expect_equal(tapply(dat$p, dat$gender, sum),
               tapply(df$p, df$gender, sum))
  expect_equal(sort(as.vector(tapply(dat$p, dat$variable, sum))), sort(sym_tbl[,1]) / 100)
  expect_equal(as.vector(tapply(dat$p, dat$variable, sum)), c(0.6, 0.3, 0.1))
})

test_that("lapply - valid output with counts -- DT", {
  # create example data
  set.seed(567L)
  df <- data.frame(gender= factor(sample(c("m", "f"), size= 100, replace=T)),
                   age= factor(sample(1:5, size= 100, replace=T)),
                   edu= factor(sample(c("hs", "col", "grad"), size= 100, replace=T)),
                   p= runif(100))
  df$p <- df$p / sum(df$p)
  data.table::setDT(df)
  
  # and example test elements
  sym_tbl <- data.frame(attr_cnts= c(60, 10, 30),
                        lev= c("employed", "unemp", "not_in_labor_force"))
  data.table::setDT(sym_tbl)
  
  dat <- synthACS:::add_synth_attr(l= df, prob_name= "p", sym_tbl= sym_tbl, attr_name= "variable")
  
  # test output
  expect_equal(nrow(df) * nrow(sym_tbl), nrow(dat))
  expect_equal(ncol(df), ncol(dat) - 1)
  expect_true(all(names(df) %in% names(dat)))
  expect_true(all(names(dat) %in% c("variable", names(df))))
  expect_equal(sum(dat$p), 1)
  expect_equal(sum(dat$p), sum(df$p))
  expect_true(all(sym_tbl$lev %in% levels(factor(dat$variable))))
  expect_true(all(levels(factor(dat$variable)) %in% sym_tbl$lev))
  expect_equal(tapply(dat$p, dat$gender, sum),
               tapply(df$p, df$gender, sum))
  expect_equal(sort(as.vector(tapply(dat$p, dat$variable, sum))), c(0.1, 0.3, 0.6))
  expect_equal(as.vector(tapply(dat$p, dat$variable, sum)), c(0.6, 0.3, 0.1))
})


#----------------------------------------------------------
context("conditional splitting & recursion")
#----------------------------------------------------------

test_that("conditional split -- fully specified conditioning (DF)", {
  # create test data / elements
  # create example data
  set.seed(567L)
  df <- data.frame(gender= factor(sample(c("male", "female"), size= 100, replace=T)),
                   age= factor(sample(1:5, size= 100, replace=T)),
                   pov= factor(sample(c("lt_pov", "gt_eq_pov"), 
                                      size= 100, replace=T, prob= c(.15,.85))),
                   p= runif(100))
  df$p <- df$p / sum(df$p)
  
  # and example test elements
  cond_v <- c("gender", "pov")
  levels <- c("employed", "unemp", "not_in_LF")
  sym_tbl <- data.frame(gender= rep(rep(c("male", "female"), each= 3), 2),
                        pov= rep(c("lt_pov", "gt_eq_pov"), each= 6),
                        cnts= c(52, 8, 268, 72, 12, 228, 1338, 93, 297, 921, 105, 554),
                        lvls= rep(levels, 4))

  # run code
  dat <- synthACS:::cond_var_split(df, "p", attr_name= "variable", 
                        conditional_vars= cond_v, sym_tbl= sym_tbl)
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
  expect_equal(tapply(dat$p, list(dat$pov, dat$gender), sum),
               tapply(df$p, list(df$pov, df$gender), sum))
})

test_that("conditional split -- fully specified conditioning (DT)", {
  # create test data / elements
  # create example data
  set.seed(567L)
  df <- data.frame(gender= factor(sample(c("male", "female"), size= 100, replace=T)),
                   age= factor(sample(1:5, size= 100, replace=T)),
                   pov= factor(sample(c("lt_pov", "gt_eq_pov"), 
                                      size= 100, replace=T, prob= c(.15,.85))),
                   p= runif(100))
  df$p <- df$p / sum(df$p)
  
  # and example test elements
  cond_v <- c("gender", "pov")
  levels <- c("employed", "unemp", "not_in_LF")
  sym_tbl <- data.frame(gender= rep(rep(c("male", "female"), each= 3), 2),
                        pov= rep(c("lt_pov", "gt_eq_pov"), each= 6),
                        cnts= c(52, 8, 268, 72, 12, 228, 1338, 93, 297, 921, 105, 554),
                        lvls= rep(levels, 4))
  data.table::setDT(df); data.table::setDT(sym_tbl)
  
  # run code
  dat <- synthACS:::cond_var_split(df, "p", attr_name= "variable", 
                                   conditional_vars= cond_v, sym_tbl= sym_tbl)
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
  expect_equal(tapply(dat$p, list(dat$pov, dat$gender), sum),
               tapply(df$p, list(df$pov, df$gender), sum))
})

test_that("conditional splitting -- differential conditioning", {
  # create example data
   set.seed(567L)
   df <- data.frame(gender= factor(sample(c("male", "female"), size= 100, replace=T)),
                   edu= factor(sample(c("LT_college", "BA_degree"), size= 100, replace=T)),
                   p= runif(100))
   df$p <- df$p / sum(df$p)

   levels <- c("low", "middle", "high")
   ST2 <- data.frame(gender= c(rep("male", 3), rep("female", 6)),
                     edu= c(rep(NA, 3), rep(c("LT_college", "BA_degree"), each= 3)),
                     attr_pct= c(0.1, 0.8, 0.1, 10, 80, 10, 5, 70, 25),
                     levels= rep(levels, 3))
   dat <- synthACS:::cond_var_split(df, prob_name= "p", attr_name= "SES",
            conditional_vars= c("gender", "edu"),
            sym_tbl= ST2)
   
   # test output
   expect_equal(nrow(df) * 3, nrow(dat))
   expect_equal(ncol(df), ncol(dat) - 1)
   expect_true(all(names(df) %in% names(dat)))
   expect_true(all(names(dat) %in% c("SES", names(df))))
   expect_equal(sum(dat$p), 1)
   expect_equal(sum(dat$p), sum(df$p))
   expect_true(all(levels %in% levels(factor(dat$SES))))
   expect_true(all(levels(factor(dat$variable)) %in% levels))
   expect_equal(tapply(dat$p, dat$gender, sum),
                tapply(df$p, df$gender, sum))
   expect_equal(tapply(dat$p, dat$edu, sum),
                tapply(df$p, df$edu, sum))
   expect_equal(tapply(dat$p, list(dat$edu, dat$gender), sum),
                tapply(df$p, list(df$edu, df$gender), sum))
  
})


#----------------------------------------------------------
context("Synthetic new attribute (top level)")
#----------------------------------------------------------

test_that("error checking", {
  # create test data / elements
  # create example data
  set.seed(567L)
  df <- data.frame(gender= factor(sample(c("male", "female"), size= 100, replace=T)),
                   edu= factor(sample(c("LT_college", "BA_degree"), size= 100, replace=T)),
                   p= runif(100))
  df$p <- df$p / sum(df$p)
  
  levels <- c("low", "middle", "high")
  ST  <- data.frame(gender= c(rep("male", 3), rep("female", 6)),
                    attr_pct= c(0.1, 0.8, 0.1, 10, 80, 10, 5, 70, 25),
                    levels= rep(levels, 3))
  ST2 <- data.frame(gender= c(rep("male", 3), rep("female", 6)),
                    edu= c(rep(NA, 3), rep(c("LT_college", "BA_degree"), each= 3)),
                    attr_pct= c(0.1, 0.8, 0.1, 10, 80, 10, 5, 70, 25),
                    levels= rep(levels, 3))
  ST3 <- ST2; names(ST3) <- c("abc", "def", "attr_pct", "levels")
  
  
  # create some faulty inputs
  expect_error(synthetic_new_attribute(df= df, prob_name= "p", attr_name= "variable",
                                       conditional_vars= NULL, sym_tbl = NULL))
  
  class(df) <- c(class(df), "micro_synthetic")
  expect_error(synthetic_new_attribute(df= df, prob_name= "pp", attr_name= "variable",
                                       conditional_vars= NULL, sym_tbl = NULL))
  expect_error(synthetic_new_attribute(df= df, prob_name= 123L, attr_name= "variable",
                                       conditional_vars= NULL, sym_tbl = NULL))
  expect_error(synthetic_new_attribute(df= df, prob_name= "p", attr_name= 123L,
                                       conditional_vars= NULL, sym_tbl = NULL))
  expect_error(synthetic_new_attribute(df= df, prob_name= c("p", "p2"), attr_name= "variable",
                                       conditional_vars= NULL, sym_tbl = NULL))
  expect_error(synthetic_new_attribute(df= df, prob_name= "p", attr_name= c("v1", "v2"),
                                       conditional_vars= NULL, sym_tbl = NULL))
  
  expect_error(synthetic_new_attribute(df= df, prob_name= "p", attr_name= "variable",
                                       conditional_vars= "abc", sym_tbl = NULL))
  expect_error(synthetic_new_attribute(df= df, prob_name= "p", attr_name= "variable",
                                       conditional_vars= c("gender","abc"), sym_tbl = NULL))
  expect_error(synthetic_new_attribute(df= df, prob_name= "p", attr_name= "variable",
                                       conditional_vars= "gender", sym_tbl = NULL))
  expect_error(synthetic_new_attribute(df= df, prob_name= "p", attr_name= "variable",
                                       conditional_vars= c("gender", "edu"), sym_tbl = ST))
  expect_error(synthetic_new_attribute(df= df, prob_name= "p", attr_name= "variable",
                                       conditional_vars= "gender", sym_tbl = ST2))
  expect_error(synthetic_new_attribute(df= df, prob_name= "p", attr_name= "variable",
                                       conditional_vars= c("gender", "edu"), sym_tbl = ST3))
})

test_that("new attr (top level) - standard conditioning (DF)", {
  set.seed(567L)
  df <- data.frame(gender= factor(sample(c("male", "female"), size= 100, replace=T)),
                   age= factor(sample(1:5, size= 100, replace=T)),
                   pov= factor(sample(c("lt_pov", "gt_eq_pov"), 
                                      size= 100, replace=T, prob= c(.15,.85))),
                   p= runif(100))
  df$p <- df$p / sum(df$p)
  class(df) <- c("data.frame", "micro_synthetic")
  
  # and example test elements
  cond_v <- c("gender", "pov")
  levels <- c("employed", "unemp", "not_in_LF")
  sym_tbl <- data.frame(gender= rep(rep(c("male", "female"), each= 3), 2),
                        pov= rep(c("lt_pov", "gt_eq_pov"), each= 6),
                        cnts= c(52, 8, 268, 72, 12, 228, 1338, 93, 297, 921, 105, 554),
                        lvls= rep(levels, 4))
  
  # run
  syn <- synthetic_new_attribute(df= df, prob_name= "p", attr_name= "variable",
                                 conditional_vars= c("gender", "pov"), sym_tbl= sym_tbl)
  
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

test_that("new attr (top level) - standard conditioning (DT)", {
  set.seed(567L)
  df <- data.frame(gender= factor(sample(c("male", "female"), size= 100, replace=T)),
                   age= factor(sample(1:5, size= 100, replace=T)),
                   pov= factor(sample(c("lt_pov", "gt_eq_pov"), 
                                      size= 100, replace=T, prob= c(.15,.85))),
                   p= runif(100))
  df$p <- df$p / sum(df$p)
  class(df) <- c("data.frame", "micro_synthetic")
  
  # and example test elements
  cond_v <- c("gender", "pov")
  levels <- c("employed", "unemp", "not_in_LF")
  sym_tbl <- data.frame(gender= rep(rep(c("male", "female"), each= 3), 2),
                        pov= rep(c("lt_pov", "gt_eq_pov"), each= 6),
                        cnts= c(52, 8, 268, 72, 12, 228, 1338, 93, 297, 921, 105, 554),
                        lvls= rep(levels, 4))
  data.table::setDT(df); data.table::setDT(sym_tbl)
  
  # run
  syn <- synthetic_new_attribute(df= df, prob_name= "p", attr_name= "variable",
                                 conditional_vars= c("gender", "pov"), sym_tbl= sym_tbl)
  
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

test_that("new attr (top level) - differential conditioning", {
  # create example data
  set.seed(567L)
  df <- data.frame(gender= factor(sample(c("male", "female"), size= 100, replace=T)),
                   edu= factor(sample(c("LT_college", "BA_degree"), size= 100, replace=T)),
                   p= runif(100))
  df$p <- df$p / sum(df$p)
  class(df) <- c("data.frame", "micro_synthetic")
  
  levels <- c("low", "middle", "high")
  ST2 <- data.frame(gender= c(rep("male", 3), rep("female", 6)),
                    edu= c(rep(NA, 3), rep(c("LT_college", "BA_degree"), each= 3)),
                    attr_pct= c(0.1, 0.8, 0.1, 10, 80, 10, 5, 70, 25),
                    levels= rep(levels, 3))
  
  
  # run
  syn <- synthetic_new_attribute(df= df, prob_name= "p", attr_name= "variable",
                                 conditional_vars= c("gender", "edu"), sym_tbl= ST2)
  
  # test output
  expect_true(is.micro_synthetic(syn))
  expect_true(is.data.frame(syn))
  expect_equal(sum(syn$p), 1)
  expect_equal(sum(syn$p), sum(df$p))
  expect_equal(tapply(syn$p, syn$gender, sum),
               tapply(df$p, df$gender, sum))
  expect_equal(tapply(syn$p, syn$edu, sum),
               tapply(df$p, df$edu, sum))
  expect_equal(nrow(df) * length(levels), nrow(syn))
  expect_equal(ncol(df), ncol(syn) - 1)
  expect_true(all(names(df) %in% names(syn)))
  expect_true(all(names(syn) %in% c("variable", names(df))))
  expect_true(all(levels %in% levels(factor(syn$variable))))
  expect_true(all(levels(factor(syn$variable)) %in% levels))
  
})

test_that("new attr (top level) -- unconditionally", {
  set.seed(567L)
  df <- data.frame(gender= factor(sample(c("m", "f"), size= 100, replace=T)),
                   age= factor(sample(1:5, size= 100, replace=T)),
                   pov= factor(sample(c("below poverty", "at above poverty"), 
                                      size= 100, replace=T, prob= c(.15,.85))),
                   p= runif(100))
  df$p <- df$p / sum(df$p)
  class(df) <- c("data.frame", "micro_synthetic")
  
  levels <- c("low", "middle", "high")
  ST2 <- data.frame(attr_pct= c(0.1, 0.8, 0.1),
                    levels= levels)
  
  
  # run
  syn <- synthetic_new_attribute(df= df, prob_name= "p", attr_name= "variable",
                                 conditional_vars= NULL, sym_tbl= ST2)
  
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

test_that("can add extra attributes in parallel (DF)", {
  # create test data / elements
  # create example data
  set.seed(567L)
  df <- data.frame(gender= factor(sample(c("male", "female"), size= 100, replace=T)),
                   age= factor(sample(1:5, size= 100, replace=T)),
                   pov= factor(sample(c("lt_pov", "gt_eq_pov"), 
                                      size= 100, replace=T, prob= c(.15,.85))),
                   p= runif(100))
  df$p <- df$p / sum(df$p)
  class(df) <- c("data.frame", "micro_synthetic")
  
  # and example test elements
  cond_v <- c("gender", "pov")
  levels <- c("employed", "unemp", "not_in_LF")
  sym_tbl <- data.frame(gender= rep(rep(c("male", "female"), each= 3), 2),
                        pov= rep(c("lt_pov", "gt_eq_pov"), each= 6),
                        cnts= c(52, 8, 268, 72, 12, 228, 1338, 93, 297, 921, 105, 554),
                        lvls= rep(levels, 4))
  
  df_list <- replicate(10, df, simplify= FALSE)
  st_list <- replicate(10, sym_tbl, simplify= FALSE)
  
  # run
  syn <- all_geog_synthetic_new_attribute(df_list, prob_name= "p", attr_name= "variable",
                                          conditional_vars= cond_v,st_list= st_list)
  
  # test output structure
  expect_true( all(unlist(lapply(syn, function(l) is.micro_synthetic(l[[2]])))) )
  expect_true( all(unlist(lapply(syn, function(l) is.data.frame(l[[2]])))) )
  
  # test ouput probabilities
  pr <- unlist(lapply(syn, function(l) sum(l[[2]]$p)))
  expect_true(all.equal(pr, rep(1, length(pr)), tolerance = 1e-08))
  expect_equal(lapply(syn, function(l) {tapply(l[[2]]$p, l[[2]]$gender, sum)}),
               lapply(df_list, function(l) {tapply(l$p, l$gender, sum)}))
  expect_equal(lapply(syn, function(l) {tapply(l[[2]]$p, l[[2]]$pov, sum)}),
             lapply(df_list, function(l) {tapply(l$p, l$pov, sum)}))
})

test_that("can add extra attributes in parallel (DT)", {
  # create test data / elements
  # create example data
  set.seed(567L)
  df <- data.frame(gender= factor(sample(c("male", "female"), size= 100, replace=T)),
                   age= factor(sample(1:5, size= 100, replace=T)),
                   pov= factor(sample(c("lt_pov", "gt_eq_pov"), 
                                      size= 100, replace=T, prob= c(.15,.85))),
                   p= runif(100))
  df$p <- df$p / sum(df$p)
  class(df) <- c("data.frame", "micro_synthetic")
  
  # and example test elements
  cond_v <- c("gender", "pov")
  levels <- c("employed", "unemp", "not_in_LF")
  sym_tbl <- data.frame(gender= rep(rep(c("male", "female"), each= 3), 2),
                        pov= rep(c("lt_pov", "gt_eq_pov"), each= 6),
                        cnts= c(52, 8, 268, 72, 12, 228, 1338, 93, 297, 921, 105, 554),
                        lvls= rep(levels, 4))
  data.table::setDT(df); data.table::setDT(sym_tbl)
  
  
  df_list <- replicate(10, df, simplify= FALSE)
  st_list <- replicate(10, sym_tbl, simplify= FALSE)
  
  # run
  syn <- all_geog_synthetic_new_attribute(df_list, prob_name= "p", attr_name= "variable",
                                          conditional_vars= cond_v,st_list= st_list)
  
  # test output structure
  expect_true( all(unlist(lapply(syn, function(l) is.micro_synthetic(l[[2]])))) )
  expect_true( all(unlist(lapply(syn, function(l) is.data.frame(l[[2]])))) )
  
  # test ouput probabilities
  pr <- unlist(lapply(syn, function(l) sum(l[[2]]$p)))
  expect_true(all.equal(pr, rep(1, length(pr)), tolerance = 1e-08))
  expect_equal(lapply(syn, function(l) {tapply(l[[2]]$p, l[[2]]$gender, sum)}),
               lapply(df_list, function(l) {tapply(l$p, l$gender, sum)}))
  expect_equal(lapply(syn, function(l) {tapply(l[[2]]$p, l[[2]]$pov, sum)}),
               lapply(df_list, function(l) {tapply(l$p, l$pov, sum)}))
})

