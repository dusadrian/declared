
x <- declared(
  c(1:5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_values = -1
)


hx <- haven::labelled_spss(
  c(1:5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_values = -1
)

test_that("w_median() works", {
  expect_equal(w_median(x), 3)
  
  expect_equal(w_median(hx), 3)
  
  expect_equal(w_median(x, na.rm = FALSE), 3)
  
  expect_equal(w_median(c(x, NA), na.rm = FALSE), NA_real_)
  
  expect_error(w_median(x, wt = "Hello"))
  
  expect_equal(w_median(x, wt = rep(1/6, 6)), 5)
})
