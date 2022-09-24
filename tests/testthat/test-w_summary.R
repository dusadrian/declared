
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

test_that("w_summary() works", {
  expect_equal(w_summary(x), w_summary(x, wt = rep(1, 6)))

  expect_equal(w_summary(x), w_summary(hx))

  expect_length(w_summary(c(x, NA)), 7)
})


test_that("tests have the same output", {
  expect_snapshot(x)
  expect_snapshot(w_summary(x))
})
