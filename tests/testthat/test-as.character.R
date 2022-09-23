x <- declared(
  c(1:5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_values = -1
)


test_that("as.character.declared() works", {
  expect_identical(
    as.character(x),
    as.character(drop_na(x))
  )

  expect_length(as.character(x), 6)
  
  expect_true(sum(is.na(as.character(x))) == 1)
  
  expect_true(sum(is.na(as.character(x, drop_na = FALSE))) == 0)
  
  expect_identical(
    as.character(x, drop_na = FALSE),
    as.character(undeclare(x))
  )
})

test_that("tests have the same output", {
  expect_snapshot(x)
  expect_snapshot(as.character(x))
  expect_snapshot(as.character(x, values = TRUE))
  expect_snapshot(as.character(x, drop_na = FALSE))
  expect_snapshot(as.character(x, drop_na = FALSE, nolabels = TRUE))
  expect_snapshot(as.character(x, values = TRUE, drop_na = FALSE))
  expect_snapshot(as.character(drop_na(x)))
  expect_snapshot(as.character(undeclare(x)))
  expect_snapshot(as.character(undeclare(x), values = TRUE))
})
