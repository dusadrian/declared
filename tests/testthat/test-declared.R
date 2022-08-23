x <- declared(
  c(1:5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_values = -1
)

test_that("declared() works", {
  expect_true(inherits(x, "declared"))
  expect_equal(labels(x), c("Good" = 1, "Bad" = 5, "DK" = -1))
  expect_equal(attr(x, "na_index"), c("-1" = 6))
  expect_equal(is.na(x), c(rep(FALSE, 5), TRUE))
  expect_equal(x > 0, c(rep(TRUE, 5), FALSE))
  expect_equal(x == -1, c(rep(FALSE, 5), TRUE))
  expect_equal(x == "DK", c(rep(FALSE, 5), TRUE))
  expect_equal(is.na(c(x, 2, -1)), c(rep(FALSE, 5), TRUE, FALSE, TRUE))
})

test_that("declared() coercion methods work", {
  expect_equal(as.numeric(x), c(1:5, NA_real_))
  expect_equal(as.character(x), c("Good", "2", "3", "4", "Bad", NA_character_))
  expect_equal(as.character(x, values = TRUE), c(1:5, NA_character_))
})

summx <- summary(x)

test_that("declared() other methods work", {
  expect_equal(length(c(x, x)), 12)
  expect_true(all(summx[1:3] == c("Min." = 1, "1st Qu." = 2,  "Median" = 3)))
  expect_true(summx["NA's"] == 1)
})
