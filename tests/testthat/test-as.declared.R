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

hs <- haven::labelled(
  c(1:5, haven::tagged_na("a")),
  labels = c(Good = 1, Bad = 5, DK = haven::tagged_na("a"))
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
  
  expect_true(
    inherits(
      as.declared(data.frame(x = 1:2, y = c("a", "b"))),
      "data.frame"
    )
  )

  expect_length(as.declared(hs), 6)
})

dfh <- data.frame(
  A = haven::labelled_spss(
    c(1:5, -1),
    labels = c(Good = 1, Bad = 5, DK = -1),
    na_values = -1
  ),
  B = fx
)

dfd <- data.frame(
  A = declared(
    c(1:5, -1),
    labels = c(Good = 1, Bad = 5, DK = -1),
    na_values = -1
  ),
  B = as.declared(fx)
)

test_that("as.declared.data.frame() works", {
  expect_equal(as.declared(dfh), dfd)
})


test_that("as.declared.default() works", {
  expect_identical(as.declared(1:5), 1:5)
})


dfd$C <- 1:6
test_that("as.declared works interactively", {
  expect_true(
    inherits(
      as.declared(dfd, interactive = TRUE),
      "data.frame"
    )
  )

  expect_message(
    as.declared(1:5, interactive = TRUE),
    "no automatic class method conversion"
  )
  
  expect_message(
    as.declared(1:5, interactive = TRUE, vname_ = "C"),
    "no automatic class method conversion"
  )
})

test_that("tests have the same output", {
  expect_snapshot(x)
  expect_snapshot(fx)
  expect_snapshot(hx)
  expect_snapshot(hs)
  expect_snapshot(as.declared(fx))
  expect_snapshot(as.declared(hx))
  expect_snapshot(as.declared(hs))
  expect_snapshot(dfh)
  expect_snapshot(as.declared(dfh))
  expect_snapshot(dfd)
  expect_snapshot(as.declared(dfd, interactive = TRUE))
})
