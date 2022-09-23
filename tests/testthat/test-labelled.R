set.seed(12345)

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


labelled::na_values(x) <- c(-1, -2)
labelled::na_range(x) <- c(-5, -1)
labelled::val_labels(x) <- c(Goodish = 1, Bad = 5, DK = -1)
labelled::var_label(x) <- "Label for variable x"


test_that("labelled functions work for declared objects", {
  expect_equal(labelled::na_values(x), c(-1, -2))

  expect_equal(labelled::na_range(x), c(-5, -1))

  expect_length(labelled::val_labels(x), 3)

  expect_length(labelled::val_labels(x, prefixed = TRUE), 3)

  expect_true(is.character(labelled::var_label(x)))

  expect_error(labelled::var_label(x) <- c("A", "B"), "should be a single character string")

  expect_length(labelled::drop_unused_value_labels(x), 6)

  expect_equal(labelled::val_label(x, 1), "Goodish")

  expect_equal(labelled::val_label(x, 1, prefixed = TRUE), "[1] Goodish")
  
  expect_error(labelled::val_label(x, 1:2), "should be a single value")
  
  expect_error(labelled::val_label(x, 1) <- c("A", "B"), "should be a single character string")

  expect_null(labelled::val_label(x, 2))
  
  expect_error(labelled::val_label(x, 1:2) <- "A", "should be a single value")
})


labelled::val_label(x, 1) <- "Good"

test_that("labels get overwritten in declared objects", {
  expect_equal(labelled::val_label(x, 1), "Good")
})



labelled::var_label(x) <- NULL
labelled::val_label(x, 1) <- NULL

test_that("labels get deleted in declared objects", {
  expect_null(labelled::var_label(x))
  expect_null(labelled::val_label(x, 1))
})

labelled::val_label(x, 1) <- "Good"

labels(x) <- c(Bad = 5)
labelled::val_label(x, 5) <- NULL

test_that("deleting a single label erases the whole labels attribute", {
  expect_null(labels(x))
})



x <- declared(
  label = "Label for variable x",
  c(1:5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_values = -1
)

test_that("sort_value_labels() works", {
  expect_length(labelled::sort_val_labels(x), 6)

  expect_length(labelled::sort_val_labels(x, according_to = "labels"), 6)
})


test_that("coercion to empty NA works", {
  expect_equal(sum(is.empty(labelled::nolabel_to_na(x))), 3)

  expect_equal(sum(is.empty(labelled::val_labels_to_na(x))), 2)
})


test_that("removing labels works", {
  expect_null(labels(labelled::remove_labels(x)))

  expect_equal(sum(is.empty(labelled::remove_user_na(x))), 1)
})


test_that("labelled type coercion works", {
  expect_true(inherits(labelled::to_factor(x), "factor"))

  expect_true(is.character(labelled::to_character(x)))
})


y <- declared(
  c(1:5, -1),
  na_values = -1
)

cx <- declared(
  x = sample(
    c("a", "b", "c", "z"),
    20,
    replace = TRUE
  ),
  na_values = "z"
)

y <- labelled::copy_labels(x, y)

test_that("labels get copied", {
  expect_equal(labels(x), labels(y))

  expect_error(labelled::copy_labels(x, cx), "should be of same type")
})


test_that("tests have the same output", {
  expect_snapshot(x)
  expect_snapshot(labelled::na_values(x))
  expect_snapshot(labelled::na_range(x))
  expect_snapshot(labelled::val_labels(x))
  expect_snapshot(labelled::val_labels(x, prefixed = TRUE))
  expect_snapshot(labelled::var_label(x))
  expect_snapshot(labelled::drop_unused_value_labels(x))
  expect_snapshot(labelled::val_label(x, 1))
  expect_snapshot(labelled::val_label(x, 1, prefixed = TRUE))
  expect_snapshot(labelled::sort_val_labels(x))
  expect_snapshot(labelled::sort_val_labels(x, according_to = "labels"))
  expect_snapshot(is.empty(labelled::nolabel_to_na(x)))
  expect_snapshot(is.empty(labelled::val_labels_to_na(x)))
  expect_snapshot(labelled::remove_labels(x))
  expect_snapshot(labelled::remove_user_na(x))
  expect_snapshot(labelled::to_factor(x))
  expect_snapshot(labelled::to_character(x))
})
