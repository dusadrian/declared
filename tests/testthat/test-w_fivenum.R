
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

test_that("w_fivenum() works", {
  expect_equal(w_fivenum(x), w_fivenum(x, wt = rep(1, 6)))

  expect_equal(w_fivenum(x), w_fivenum(hx))

  expect_length(w_fivenum(c(x, NA), wt = rep(1, 7), na.rm = TRUE), 5)

  expect_error(w_fivenum(c(x, NA), wt = rep(1, 7)))

  expect_error(w_fivenum(x, wt = rep(0, 6)))

  expect_error(w_fivenum(list(1:3)))

  expect_error(w_fivenum(x, wt = "A"))

  expect_error(w_fivenum(x, wt = rep(1, length(x) + 1)))
  
  expect_error(
    w_fivenum("A"),
    "should be an atomic numerical vector"
  )
})


test_that("tests have the same output", {
  expect_snapshot(x)
  expect_snapshot(w_fivenum(x))
})
