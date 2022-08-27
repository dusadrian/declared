
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


test_that("w_mean() works", {
  expect_equal(w_mean(x), 3)
  
  expect_equal(w_mean(hx), 3)
  
  expect_error(w_mean(x, wt = "A"))
  
  expect_warning(
    w_mean("A"),
    "should be a numerical / logical"
  )

  expect_error(w_mean(x, wt = rep(1/7, 7)))

  expect_equal(w_mean(x, wt = rep(1/6, 6)), 3)

  expect_equal(w_mean(c(1:5, NA), wt = rep(1/6, 6), na.rm = FALSE), NA_real_)

  expect_error(w_mean(x, wt = c(-0.2, rep(0.2, 5))))

  expect_equal(w_mean(x, wt = rep(1/6, 6), trim = 0.6), 5)

  expect_equal(w_mean(x, wt = rep(1/6, 6), trim = 0.1), 3)

  expect_error(w_mean(as.complex(1:6), wt = rep(1/6, 6), trim = 0.1))
})
