
test_that("validate_declared() works", {
  expect_error(validate_declared(as.complex(1:6)))

  expect_error(validate_declared(1:6, labels = LETTERS[1:6]))

  expect_error(validate_declared(1:6, labels = c(A = 1, B = 1, C = 3)))

  expect_error(validate_declared(1:6, label = LETTERS[1:2]))

  expect_error(validate_declared(1:6, na_values = c(1, 2, 3, NA, 5, 6)))

  expect_error(validate_declared(1:6, na_range = 1:3))

  expect_error(validate_declared(1:6, na_range = c(1, NA)))
})
