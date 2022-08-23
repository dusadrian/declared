x <- declared(
  c(1:5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_values = -1
)

test_that("declared() works", {
  expect_true(inherits(x, "declared"))
  expect_equal(labels(x), c("Good" = 1, "Bad" = 5, "DK" = -1))
  expect_equal(is.na(x), c(rep(FALSE, 5), TRUE))
})

test_that("declared() coercion methods works", {
  expect_equal(as.numeric(x), c(1:5, NA_real_))
  expect_equal(as.character(x), c("Good", "2", "3", "4", "Bad", NA_character_))
  expect_true(is.numeric(undeclare(x)))
})

test_that("declared() other methods works", {
  expect_equal(length(c(x, x)), 12)
  expect_true(all(summary(x)[1:3] == c("Min." =1, "1st Qu." = 2,  "Median" = 3)))
})


