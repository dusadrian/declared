local_edition(3)
x <- declared(
  label = "Variable label",
  c(1:5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_values = -1
)


xc <- declared(
  c(1:5, "a"),
  labels = c(Good = 1, Bad = 5, DK = "a"),
  na_values = "a"
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
ux[na_index] <- as.integer(names(na_index))

test_that("undeclare() works", {
  expect_equal(undeclare(x), ux)

  expect_equal(undeclare(x, drop = TRUE), as.integer(c(1:5, -1)))
})


test_that ("undeclare() discards inconsistent declared missing metadata", {
  middle_na <- declared (
    c (1, -1, 2, 3),
    labels = c (DK = -1),
    na_values = -1
  )

  model_values <- model.frame (
    value ~ 1,
    data = data.frame (value = middle_na),
    na.action = na.omit
  )$value

  expect_equal (
    undeclare (model_values, drop = TRUE),
    c (1, 2, 3)
  )

  cleaned <- undeclare (model_values)
  expect_null (attr (cleaned, "na_index"))
  expect_null (attr (cleaned, "na_values"))
  expect_null (attr (cleaned, "na_range"))

  refreshed <- model_values[seq_along (model_values)]
  expect_false (anyNA (refreshed))
  expect_null (attr (refreshed, "na_index"))
})


test_that ("an inconsistent index is discarded as a whole", {
  tampered <- declared (
    c (-1, 1, -2, 2),
    na_values = c (-1, -2)
  )

  attributes_tampered <- attributes (tampered)
  attributes (tampered) <- NULL
  tampered[1] <- 99
  attributes (tampered) <- attributes_tampered

  expect_equal (
    undeclare (tampered, drop = TRUE),
    c (99, 1, NA, 2)
  )

  attr (tampered, "na_index") <- c ("-1" = 20)

  expect_equal (
    undeclare (tampered, drop = TRUE),
    c (99, 1, NA, 2)
  )
})


test_that ("valid_na_index() supports declared storage types", {
  integer_values <- declared (c (1L, -1L, 2L), na_values = -1L)
  numeric_values <- declared (c (1, -1, 2), na_values = -1)
  character_values <- declared (c ("A", "DK", "B"), na_values = "DK")
  attr (numeric_values, "na_index") <- as.numeric (
    attr (numeric_values, "na_index")
  )

  expect_true (valid_na_index (integer_values))
  expect_true (valid_na_index (numeric_values))
  expect_true (valid_na_index (character_values))

  attr (integer_values, "na_index") <- 20L
  attr (numeric_values, "na_index") <- 1.5
  attr (character_values, "na_index") <- 1L

  expect_false (valid_na_index (integer_values))
  expect_false (valid_na_index (numeric_values))
  expect_false (valid_na_index (character_values))
})


test_that ("valid_na_index() works with ordinary vectors", {
  values <- c (1, NA, 3)

  expect_true (valid_na_index (values))

  attr (values, "na_index") <- integer (0)
  expect_true (valid_na_index (values))

  attr (values, "na_index") <- 2
  expect_true (valid_na_index (values))

  attr (values, "na_index") <- c (2, 3)
  expect_false (valid_na_index (values))
  expect_false (valid_na_index (list (NA)))
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
  expect_snapshot(undeclare(xc))
})
