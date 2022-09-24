x <- declared(
  c(1:5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_values = -1
)

test_that("vroom method works", {
  expect_true(is.character(vroom::output_column(x)))
})

test_that("tests have the same output", {
  expect_snapshot(x)
  expect_snapshot(vroom::output_column(x))
})
