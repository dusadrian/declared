
x <- declared(
  c(1:5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_values = -1
)




test_that("w_mean() works", {
  expect_equal(w_mean(x), 3)
  expect_error(w_mean(x, wt = "Hello"))
})


test_that("w_median() works", {
  expect_equal(w_median(x), 3)
  expect_error(w_median(x, wt = "Hello"))
})

y <- declared(
  c(1, 1:5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_values = -1
)

test_that("w_mode() works", {
  expect_equal(w_mode(y), 1)
  expect_equal(w_sd(y), sd(c(1, 1:5, NA), na.rm=TRUE))
  expect_error(w_mode(x, wt = "Mode"))
})
