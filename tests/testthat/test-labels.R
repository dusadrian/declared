
x <- declared(
  label = "Variable label",
  c(1:5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_values = -1
)


test_that("labels works in declared()", {
  expect_identical(attr(x, "label"), "Variable label")
  
  expect_identical(label(x), attr(x, "label"))
  
  expect_identical(labels(x), c(Good = 1, Bad = 5, DK = -1))
  
  expect_false(identical(labels(x), labels(x, prefixed = TRUE)))
})


x2 <- x
label(x2) <- "Another variable label"
labels(x2) <- c(Good = 1, Neutral = 3, Bad = 5, DK = -1)
test_that("labels<-.declared work", {
  expect_named(labels(x2))
  
  expect_false(identical(labels(x), labels(x2)))
  
  expect_false(identical(label(x), label(x2)))
  
  expect_error(label(x) <- c("A", "B"))
})


hx <- haven::labelled_spss(
  label = "Variable label",
  c(1:5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_values = -1
)
test_that("labels work for both declared and haven classes", {
  expect_identical(label(x), label(hx))
  
  expect_identical(labels(x), labels(hx))
  
  expect_false(identical(labels(hx), labels(hx, prefixed = TRUE)))
})


hx2 <- hx
label(hx2) <- "Another variable label"
labels(hx2) <- c(Good = 1, Neutral = 3, Bad = 5, DK = -1)
test_that("labels<- work for both declared and haven classes", {
  expect_false(identical(label(hx), label(hx2)))
  
  expect_false(identical(labels(hx), labels(hx2)))
})


dfd <- data.frame(x, hx)

test_that("labels work data frames", {
  expect_type(label(dfd), "list")
})


label(dfd) <- "Label for the entire data frame"
test_that("label() works for entire data frame objects", {
  expect_true(is.character(attr(dfd, "label", exact = TRUE)))
})


label(dfd) <- list(x = "Label for x", hx = "Label for hx")
test_that("label<-() works for all variables in a data frame", {
  expect_false(identical(label(x), label(dfd$x)))
})

test_that("labels() works for all variables in a data frame", {
  expect_false(identical(labels(dfd), labels(dfd, prefixed = TRUE)))
})


dx <- 1:5
test_that("label() has a default", {
  expect_null(label(dx))
})


label(dx) <- "dx"
test_that("label<-() has a default", {
  expect_true(inherits(dx, "labelled"))
})


labels(dx) <- c(Good = 1, Bad = 5, DK = -1)
test_that("labels<-() has a default", {
  expect_identical(labels(dx), as.character(1:5))
})


label(x2, irrelevant = TRUE) <- "Variable label"
label(dfd, irrelevant = TRUE) <- list(x = "Label for x", hx = "Label for hx")
test_that("argument ... has no effect on label<-()", {
  expect_identical(label(x), label(x2))
  
  expect_false(identical(label(x), label(dfd$x)))
})


test_that("errors work for label(s)(<-)", {
  expect_error(label(x) <- c("A", "B"))
  
  expect_error(label(hx) <- c("A", "B"))
  
  expect_error(label(dfd) <- c("A", "B"))
  
  expect_error(label(dfd) <- list(x = "Label for x"))
  
  expect_error(label(dx) <- list(A = "A"))
  
  expect_error(label(dx) <- c("A", "B"))
})


test_that("tests have the same output", {
  expect_snapshot(x)
  expect_snapshot(label(x))
  expect_snapshot(attr(x, "label"))
  expect_snapshot(labels(x))
  expect_snapshot(labels(x, prefixed = TRUE))
  expect_snapshot(hx)
  expect_snapshot(labels(hx))
  expect_snapshot(dfd)
  expect_snapshot(labels(dfd))
  expect_snapshot(labels(dfd, prefixed = TRUE))
})


label(x) <- NULL
label(hx) <- NULL
label(dfd) <- NULL
