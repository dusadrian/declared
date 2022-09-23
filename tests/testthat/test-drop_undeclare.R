x <- declared(
  label = "Variable label",
  c(1:5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_values = -1
)

hx <- haven::labelled_spss(
  label = "Variable label",
  c(1:5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_values = -1
)

dfd <- data.frame(x, hx)

test_that("undeclare() dropping attributes works", {
  expect_equal(undeclare(x, drop = TRUE), as.integer(c(1:5, -1)))
})


ux <- x
attr(ux, "na_index") <- NULL
attr(ux, "na_values") <- NULL
attr(ux, "na_range") <- NULL
na_index <- attr(x, "na_index")
ux[na_index] <- names(na_index) # automatically coerced to numeric

test_that("undeclare() works", {
  expect_equal(undeclare(x), ux)

  expect_equal(undeclare(x, drop = TRUE), as.integer(c(1:5, -1)))
})


test_that("undeclare() works on data.frames", {
  expect_true(inherits(undeclare(dfd), "data.frame"))
})


test_that("drop() works", {
  expect_equal(drop(x), as.integer(c(1:5, NA)))
  
  expect_equal(undeclare(x, drop = TRUE), drop(undeclare(x)))
})


test_that("drop_na() works", {
  expect_null(attr(drop_na(x), "na_index"))
  
  expect_false(identical(
    attr(drop_na(x), "labels"),
    attr(x, "labels")
  ))

  expect_equal(drop_na(1:5), 1:5)

  expect_equal(drop_na(x), as.declared(drop_na(hx)))
})


test_that("drop_na() works on data.frames", {
  expect_true(inherits(drop_na(dfd), "data.frame"))

  expect_true(inherits(drop_na(dfd, drop_labels = TRUE), "data.frame"))
})


test_that("tests have the same output", {
  expect_snapshot(drop(x))
  expect_snapshot(undeclare(x))
  expect_snapshot(undeclare(x, drop = TRUE))
  expect_snapshot(drop(undeclare(x)))
  expect_snapshot(drop_na(x))
  expect_snapshot(drop_na(hx))
  expect_snapshot(drop_na(dfd))
  expect_snapshot(drop_na(dfd, drop_labels = TRUE))
})
