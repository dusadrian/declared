x <- declared(
    c(1:2, -91),
    labels = c(Good = 1, Bad = 2, Missing = -91),
    na_values = -91
)

test_that("is.empty() works", {
  expect_equal(is.empty(x), rep(FALSE, 3))
  expect_equal(is.empty(c(x, NA)), c(rep(FALSE, 3), TRUE))
})
