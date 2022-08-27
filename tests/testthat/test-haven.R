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
      suppressMessages(as.haven(dfd, only_declared = FALSE, interactive = TRUE)),
      "data.frame"
    )
  )
})


test_that("as_factor.declared() works", {
  expect_length(haven::as_factor(x), 6)
})


test_that("zap_labels.declared() and zap_missing.declared() work", {
  expect_length(haven::zap_labels(x), 6)
  
  expect_length(haven::zap_missing(x), 6)
})
