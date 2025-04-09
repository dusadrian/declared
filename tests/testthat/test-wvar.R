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

test_that("wvar() works", {
  expect_equal(wvar(x), var(c(1:5, NA), na.rm = TRUE))

  expect_equal(wvar(x), wvar(hx))

  expect_equal(wvar(c(x[-1], NA), wt = rep(1, 6), na.rm = TRUE), 1 + 2/3)

  expect_equal(1 * wvar(c(x[-1], NA), wt = rep(1, 6), na.rm = FALSE), NA_real_)

  expect_equal(wvar(x, wt = rep(1, 6), method = "ML"), 2)

  expect_equal(wvar(x, wt = rep(1, 6), method = "unbiased"), 2.5)

  expect_error(wvar("A"))

  expect_error(wvar(x, wt = "A"))

  expect_error(wvar(x, wt = rep(1, 7)))

  expect_error(wvar(x, wt = c(-1, rep(1, 5))))

  expect_error(wvar(x, wt = rep(1, 6), method = "method"))
})


test_that("tests have the same output", {
  expect_snapshot(x)
  expect_snapshot(wvar(x))
})
