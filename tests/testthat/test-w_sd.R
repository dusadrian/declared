
x <- declared(
  c(1:5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_values = -1
)

test_that("w_sd() works", {
  expect_equal(w_sd(x), sd(c(1:5, NA), na.rm = TRUE))
})
