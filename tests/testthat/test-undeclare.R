x <- declared(
  c(1:5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_values = -1
)


test_that("undeclare() dropping attributes works", {
  expect_equal(undeclare(x, drop = TRUE), as.integer(c(1:5, -1)))
})


ux <- x
attr(ux, "na_index") <- NULL
attr(ux, "na_values") <- NULL
attr(ux, "na_range") <- NULL
na_index <- attr(x, "na_index")
ux[na_index] <- names(na_index)

test_that("undeclare() works", {
  expect_equal(undeclare(x), ux)
})


test_that("drop() works", {
  expect_equal(drop(x), as.integer(c(1:5, NA)))
  expect_equal(undeclare(x, drop = TRUE), drop(undeclare(x)))
})


test_that("drop_na() works", {
  expect_equal(attr(drop_na(x), "na_index"), NULL)
  expect_false(identical(
    attr(drop_na(x), "labels"),
    attr(x, "labels")
  ))
})
