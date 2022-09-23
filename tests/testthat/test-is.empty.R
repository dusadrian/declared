x <- declared(
    c(1:2, -91),
    labels = c(Good = 1, Bad = 2, Missing = -91),
    na_values = -91
)

test_that("is.empty() works", {
  expect_error(is.empty(list(A = 1)), "should be an atomic vector")
  
  expect_error(is.empty(data.frame(x)))

  expect_equal(is.empty(x), rep(FALSE, 3))

  expect_equal(
    is.empty(c(x, NA)),
    c(FALSE, FALSE, FALSE, TRUE)
  )
})

test_that("tests have the same output", {
  expect_snapshot(x)
  expect_snapshot(is.empty(x))
  expect_snapshot(is.empty(c(x, NA)))
})
