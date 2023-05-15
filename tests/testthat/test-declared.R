values <- c(1:5, -1)
value_labels <- c(Good = 1, Bad = 5, DK = -1)

x <- declared(
  values,
  labels = value_labels,
  na_values = -1
)

fx <- factor(
  values,
  levels = values,
  labels = c("Good", 2:4, "Bad", "DK")
)

attr(value_labels, "print_as_df") <- TRUE
class(value_labels) <- c("labels_df", class(value_labels))

test_that("declared() works", {
  expect_true(inherits(x, "declared"))
  
  expect_equal(
    labels(x),
    value_labels
  )
  
  expect_equal(
    attr(x, "na_index"),
    c("-1" = 6)
  )
  
  expect_equal(
    is.na(x),
    c(rep(FALSE, 5), TRUE)
  )
  
  expect_equal(
    x > 0,
    c(rep(TRUE, 5), FALSE)
  )
  
  expect_equal(
    x == -1,
    c(rep(FALSE, 5), TRUE)
  )
  
  expect_equal(
    x == "DK",
    c(rep(FALSE, 5), TRUE)
  )
  
  expect_equal(
    is.na(c(x, 2, -1)),
    c(rep(FALSE, 5), TRUE, FALSE, TRUE)
  )

  expect_length(labels(declared(fx, na_values = 6)), 6)

  expect_length(labels(declared(fx, na_values = "DK")), 6)

  expect_length(labels(declared(fx, na_values = 6, llevels = TRUE)), 3)

  expect_error(missing_range(x) <- 1:3)
})



xrec <- admisc::recode(
  values,
  cut = c(0, 3),
  labels = c("DK", "Good", "Bad"),
  values = c(-1, 1, 2)
)

lxrec <- labels(declared(xrec))
class(lxrec) <- setdiff(class(lxrec), "labels_df")
attr(lxrec, "print_as_df") <- NULL

test_that(
  "if the values already have a labels attribute, declared() recognizes it", {
  expect_equal(
    lxrec,
    attr(xrec, "labels", exact = TRUE)
  )
})


test_that("declared() coercion methods work", {
  expect_equal(
    as.numeric(x),
    c(1:5, NA_real_)
  )
  
  expect_equal(
    as.character(x),
    c("Good", "2", "3", "4", "Bad", NA_character_)
  )
  
  expect_equal(
    as.character(x, values = TRUE),
    c(1:5, NA_character_)
  )
})


summx <- summary(x)
test_that("declared() other methods work", {
  expect_equal(length(c(x, x)), 12)
  
  expect_true(all(summx[1:3] == c("Min." = 1, "1st Qu." = 2,  "Median" = 3)))
  
  expect_true(summx["NA's"] == 1)
})


xs <- declared(
  x = sample(
    c("Left", "Middle", "Right", "Apolitic"),
    20,
    replace = TRUE
  ),
  na_values = "Apolitic"
)
test_that("missing values can be character", {
  expect_true(is.character(missing_values(xs)))
})


xc <- declared(
  c(1:5, -1),
  labels = c(Good = 1, Bad = 5, DK = "a"),
  na_values = -1
)

test_that("character values in labels coerce x to character", {
  expect_true(is.character(xc))
})

test_that("tests have the same output", {
  expect_snapshot(x)
  expect_snapshot(labels(x))
  expect_snapshot(attr(x, "na_index"))
  expect_snapshot(fx)
  expect_snapshot(labels(declared(fx, na_values = 6)))
  expect_snapshot(labels(declared(fx, na_values = "DK")))
  expect_snapshot(labels(declared(fx, na_values = 6, llevels = TRUE)))
  expect_snapshot(xc)
  expect_snapshot(is.character(xc))
})
