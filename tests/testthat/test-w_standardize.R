
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

test_that("w_standardize() works", {
  expect_equal(w_standardize(x), w_standardize(x, wt = rep(1, 6)))

  expect_equal(w_standardize(x), w_standardize(hx))

  expect_warning(w_standardize(list(A = 1)), "should be a numerical")

  expect_error(w_standardize(x, wt = "weight"))
})
