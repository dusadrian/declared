local_edition(3)
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

test_that("wIQR() works", {
  expect_equal(wIQR(x), wIQR(x, wt = rep(1, 6)))

  expect_equal(wIQR(x), wIQR(hx))
})


test_that("tests have the same output", {
  expect_snapshot(x)
  expect_snapshot(wIQR(x))
})
