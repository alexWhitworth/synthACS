
test_func <- function(d, comp) {
  # test correct str()
  expect_true(is.list(d))
  expect_true(length(d), 6)
  expect_equal(names(d), c("endyear", "span", "estimates", "standard_error", "acs_colnames",
                                "geography"))
  expect_true(is.numeric(d$endyear))
  expect_true(is.numeric(d$span))
  expect_true(is.data.frame(d$estimates))
  expect_true(is.data.frame(d$standard_error))
  expect_true(is.list(d$acs_colnames))
  expect_true(is.data.frame(d$geography))
  expect_equal(comp@geography, d$geography)
  
  # data elements
  expect_equal(dim(d$estimates), dim(d$standard_error))
  expect_equal(names(d$estimates), names(d$standard_error))
  expect_true(all(unlist(lapply(d$estimates, is.numeric))))
  expect_true(all(unlist(lapply(d$standard_error, is.numeric))))
}