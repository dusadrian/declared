x <- declared(
    c(-2, 1:5, -1),
    labels = c(Good = 1, Bad = 5, DK = -1),
    na_values = c(-1, -2),
    label = "Test variable"
)

x2 <- x
measurement(x) <- "ordinal"

numx <- declared(
    sample(c(18:90, -91), 20, replace = TRUE),
    labels = c("No answer" = -91),
    na_values = -91,
    label = "Respondent's age"
)

numx2 <- numx
measurement(numx2) <- "discrete"

test_that("measurement works", {
  expect_true(grepl("categorical", measurement(x)))

  expect_true(grepl("categorical", measurement(x2)))

  expect_true(grepl("quantitative", measurement(numx)))

  expect_true(grepl("quantitative", measurement(numx2)))
})

y <- 1:5
measurement(y) <- "ordinal"
test_that("measurement has a default method", {
  expect_null(measurement(1:5))
})

test_that("tests have the same output", {
  expect_snapshot(x)
  expect_snapshot(measurement(x))
  expect_snapshot(x2)
  expect_snapshot(measurement(x2))
  expect_snapshot(numx)
  expect_snapshot(measurement(numx))
  expect_snapshot(numx2)
  expect_snapshot(measurement(numx2))
})
