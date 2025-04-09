local_edition(3)
x <- declared(
  c(2, 1:5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_values = -1
)

hx <- haven::labelled_spss(
  c(2, 1:5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_values = -1
)

test_that("wmode() works", {
  expect_warning(
    wmode("A"),
    "should be a numerical / logical"
  )

  expect_message(wmode(x[-1]), "Multiple modes detected")

  expect_equal(
    suppressMessages(wmode(x)),
    suppressMessages(wmode(hx))
  )

  expect_equal(suppressMessages(wmode(x)), 2)

  expect_error(wmode(x, wt = "Mode"))
})

test_that("tests have the same output", {
  expect_snapshot(x)
  expect_snapshot(wmode(x))
  expect_snapshot(hx)
  expect_snapshot(wmode(hx))
})
