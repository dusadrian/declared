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

fx <- factor(
  c(1:5, -1),
  levels = c(1:5, -1),
  labels = c("Good", 2:4, "Bad", "DK")
)


test_that("as.declared() works", {
  expect_true(inherits(as.declared(fx), "declared"))

  expect_equal(
    names(labels(as.declared(fx, llevels = TRUE))),
    names(labels(x))
  )

  expect_equal(as.declared(hx), x)
  
  expect_true(inherits(as.declared(data.frame ( x = 1:2, y = c("a", "b"))), "data.frame"))
})
