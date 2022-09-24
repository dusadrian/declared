
x <- declared(
  c(1:5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_values = -1
)

hx <- haven::labelled_spss(
  c(1:5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_values = -1
)

test_that("missing_range() works", {
  expect_equal(missing_values(x), -1)

  expect_equal(missing_range(x), missing_range(hx))
})


missing_values(x) <- NULL
missing_range(x) <- c(-5, -1)
test_that("missing_range<-() works", {
  expect_length(missing_range(x), 2)

  expect_error(missing_range(x) <- 1:3)
})


y <- 1:5
missing_range(y) <- -1
test_that("missing_range<-() has default methods", {
  expect_null(missing_range(1:5))

  expect_equal(y, 1:5)
})


dfd <- data.frame(x, hx)
test_that("missing_range() works on data.frames", {
  expect_type(missing_range(dfd), "list")
})


test_that("tests have the same output", {
  expect_snapshot(x)
  expect_snapshot(missing_range(x))
  expect_snapshot(dfd)
  expect_snapshot(missing_range(dfd))
})
