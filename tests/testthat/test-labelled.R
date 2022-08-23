x <- declared(
  c(1:5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_values = -1
)



test_that("to_factor.declared() works", {
  expect_true(inherits(as.factor(x), "factor"))
})
