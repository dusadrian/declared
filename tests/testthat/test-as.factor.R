x <- declared(
  c(1:5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_values = -1
)


test_that("as.factor.declared() works", {
  expect_true(inherits(as.factor(x), "factor"))

  expect_identical(as.factor(x), as.factor(drop_na(x)))

  expect_true(
    all(is.element(
      names(labels(x)),
      levels(as.factor(x, drop_na = FALSE))
    ))
  )

  expect_true(
    all(is.element(
      names(labels(x)),
      levels(as.factor(undeclare(x)))
    ))
  )
  
  expect_setequal(
    levels(as.factor(undeclare(x))),
    levels(as.factor(x, drop_na = FALSE))
  )
})

test_that("tests have the same output", {
  expect_snapshot(x)
  expect_snapshot(as.factor(x))
  expect_snapshot(as.factor(drop_na(x)))
  expect_snapshot(names(labels(x)))
  expect_snapshot(levels(as.factor(undeclare(x))))
  expect_snapshot(levels(as.factor(x, drop_na = FALSE)))
})
