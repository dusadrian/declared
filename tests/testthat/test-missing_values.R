
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

test_that("missing_values() works", {
  expect_equal(missing_values(x), -1)

  expect_equal(missing_values(x), missing_values(hx))
})


missing_values(x) <- c(-2, -1)
test_that("missing_values<-() works", {
  expect_length(missing_values(x), 2)
})


y <- 1:5
missing_values(y) <- -1
test_that("missing_values<-() has default methods", {
  expect_null(missing_values(1:5))

  expect_equal(y, 1:5)
})


dfd <- data.frame(x, hx)
test_that("missing_values() works on data.frames", {
  expect_type(missing_values(dfd), "list")
})


test_that("tests have the same output", {
  expect_snapshot(x)
  expect_snapshot(missing_values(x))
  expect_snapshot(dfd)
  expect_snapshot(missing_values(dfd))
})
