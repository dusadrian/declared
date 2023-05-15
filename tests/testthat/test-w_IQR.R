
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

test_that("w_IQR() works", {
  expect_equal(w_IQR(x), w_IQR(x, wt = rep(1, 6)))

  expect_equal(w_IQR(x), w_IQR(hx))
})


test_that("tests have the same output", {
  expect_snapshot(x)
  expect_snapshot(w_IQR(x))
})
