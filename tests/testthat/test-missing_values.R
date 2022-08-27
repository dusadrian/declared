
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
  expect_equal(missing_values(1:5), NULL)

  expect_equal(y, 1:5)
})


dfd <- data.frame(x, hx)
test_that("missing_values() works on data.frames", {
  expect_true(inherits(missing_values(dfd), "list"))
})
