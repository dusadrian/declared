x <- declared(
  c(1:5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_values = -1
)


test_that("as.factor.declared() works", {
  expect_true(inherits(as.factor(x), "factor"))
  expect_identical(as.factor(x), as.factor(drop_na(x)))
  expect_identical(
    levels(as.factor(x4, drop_na = FALSE)),
    names(labels(x4))
  )
  expect_setequal(as.factor(undeclare(x4)), names(labels(x4)))
  expect_setequal(
    levels(as.factor(undeclare(x4))),
    levels(as.factor(x4, drop_na = FALSE))
  )
})


test_that("as_factor.declared() works", {
  expect_identical(
    haven::as_factor(x4),
    as.factor(x4, drop_na = FALSE)
  )
})
