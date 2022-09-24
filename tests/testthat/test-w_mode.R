
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

test_that("w_mode() works", {
  expect_warning(
    w_mode("A"),
    "should be a numerical / logical"
  )

  expect_message(w_mode(x[-1]), "Multiple modes detected")

  expect_equal(
    suppressMessages(w_mode(x)),
    suppressMessages(w_mode(hx))
  )
  
  expect_equal(suppressMessages(w_mode(x)), 2)
  
  expect_error(w_mode(x, wt = "Mode"))
})

test_that("tests have the same output", {
  expect_snapshot(x)
  expect_snapshot(w_mode(x))
  expect_snapshot(hx)
  expect_snapshot(w_mode(hx))
})
