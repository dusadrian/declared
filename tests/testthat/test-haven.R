set.seed(12345)

x <- declared(
  c(1:5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_values = -1
)


hs <- haven::labelled(
  c(1:5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1)
)


hx <- haven::labelled_spss(
  c(1:5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_values = -1
)

cx <- declared(
  x = sample(
    c("a", "b", "c", "z"),
    20,
    replace = TRUE
  ),
  labels = c("Left" = "a", "Middle" = "b", "Right" = "c", "Apolitic" = "z"),
  na_values = "z"
)

test_that("as.haven() works", {
  expect_equal(as.haven(x), hx)

  expect_equal(as.haven(1:5, interactive = FALSE), as.integer(1:5))

  expect_true(inherits(as.haven(cx), "haven_labelled"))

  expect_true(inherits(as.haven(hs), "haven_labelled"))

  expect_message(as.haven(1:5), "no automatic class method conversion")

  expect_message(
    as.haven(1:5, interactive = TRUE, vname_ = "A"),
    "no automatic class method conversion"
  )
})



dfd <- data.frame(x, hx)

test_that("as.haven() works for data.frames", {
  expect_true(inherits(as.haven(dfd), "data.frame"))

  expect_true(inherits(as.haven(dfd, interactive = TRUE), "data.frame"))
  
  expect_true(
    inherits(
      suppressMessages(as.haven(dfd, only_declared = FALSE)),
      "data.frame"
    )
  )

  expect_true(
    inherits(
      suppressMessages(
        as.haven(dfd, only_declared = FALSE, interactive = TRUE)
      ),
      "data.frame"
    )
  )
})


test_that("as_factor.declared() works", {
  expect_identical(
    haven::as_factor(x),
    factor(
      c("Good", 2:4, "Bad", "DK"),
      levels = c("DK", "Good", 2:4, "Bad")
    )
  )
})


test_that("zap_labels.declared() and zap_missing.declared() work", {
  expect_identical(haven::zap_labels(x), c(1:5, NA))
  
  expect_null(
    attr(
      haven::zap_missing(x),
      "na_index"
    )
  )
})

R_variant <- paste0("R", getRversion()[, 1:2], R.version$platform)

test_that("tests have the same output", {
  expect_snapshot(x)
  expect_snapshot(haven::as_factor(x))
  expect_snapshot(haven::zap_labels(x))
  expect_snapshot(haven::zap_missing(x))
  expect_snapshot(as.haven(x))
  expect_snapshot(as.haven(1:5, interactive = FALSE))
  expect_snapshot(cx)
  expect_snapshot(as.haven(cx))
  expect_snapshot(dfd, variant = R_variant)
  expect_snapshot(as.haven(dfd), variant = R_variant)
  expect_snapshot(as.haven(dfd, interactive = TRUE), variant = R_variant)
  expect_snapshot(as.haven(dfd, only_declared = FALSE), variant = R_variant)
  expect_snapshot(
    as.haven(dfd, only_declared = FALSE, interactive = TRUE),
    variant = R_variant
  )
})
