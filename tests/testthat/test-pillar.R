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
  labels = c("Left" = "a", "Middle" = "b", "Right" = "c", "Apolitic" = "z"),
  na_values = "z"
)

test_that("pillar method works", {
  expect_true(inherits(pillar::pillar_shaft(x), "pillar_shaft"))

  expect_true(inherits(pillar::pillar_shaft(x, use_haven = FALSE), "pillar_shaft"))

  expect_true(inherits(pillar::pillar_shaft(y, use_haven = FALSE), "pillar_shaft"))

  expect_true(
    inherits(
      pillar::pillar_shaft(x, use_haven = FALSE, show_labels = FALSE),
      "pillar_shaft"
    )
  )

  expect_true(
    inherits(
      pillar::pillar_shaft(cx, use_haven = FALSE),
      "pillar_shaft"
    )
  )
})

px <- pillar::pillar_shaft(undeclare(x), use_haven = FALSE)


test_that("format works on pillar for declared objects", {
  expect_equal(
    nchar(
      capture.output(format(px, 10))[2]
    ),
    10
  )
  
  expect_equal(
    nchar(
      capture.output(format(px, 100))[2]
    ),
    100
  )
})


pcx <- pillar::pillar_shaft(undeclare(cx), use_haven = FALSE)
test_that("format works on pillar for declared character objects", {
  expect_equal(
    nchar(
      capture.output(format(pcx, 10))[2]
    ),
    10
  )
  
  expect_equal(
    nchar(
      capture.output(format(pcx, 100))[2]
    ),
    100
  )
})


test_that("tests have the same output", {
  expect_snapshot_output(format(px, 10))
  expect_snapshot_output(format(px, 100))
  expect_snapshot_output(format(pcx, 10))
  expect_snapshot_output(format(pcx, 100))
})
