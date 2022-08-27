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

aa <- pillar::pillar_shaft(undeclare(x), use_haven = FALSE)
bb <- format(aa, 10)
cc <- format(aa, 100)

aa <- pillar::pillar_shaft(undeclare(cx), use_haven = FALSE)
bb <- format(aa, 10)
cc <- format(aa, 100)
