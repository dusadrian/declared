local_edition(3)
x <- declared(
  c(1:5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_values = -1
)

test_that("wmeasures() works with vectors", {
  y <- declared (
    c(1, 1, 2, 3, -1),
    labels = c(Good = 1, Bad = 3, DK = -1),
    na_values = -1
  )

  expect_equal(
    unclass (wmeasures(y)),
    c(
      n = 4,
      mode = wmode(y),
      mean = wmean(y),
      median = wmedian(y),
      min = 1,
      max = 3,
      var = wvar(y),
      sd = wsd(y)
    )
  )

  expect_equal(
    unclass (wmeasures(x, what = c("n", "median"))),
    c(n = 5, median = 3)
  )

  expect_equal(
    unclass (wmeasures(c (x, NA), what = c ("n", "NA"))),
    c (n = 5, "NA" = 2)
  )

  expect_equal(
    unclass (wmeasures(y, what = c("var", "mode"))),
    c(var = wvar(y), mode = wmode(y))
  )

  expect_equal(
    unclass (wmeasures(y, what = "variance")),
    c(var = wvar(y))
  )

  expect_equal(
    unclass (wmeasures(y, what = "iqr")),
    c(iqr = wIQR(y))
  )

  expect_equal(
    unclass (wmeasures(x, what = "range")),
    c(min = 1, max = 5)
  )
})


test_that("wmeasures() works with data frames", {
  DF <- data.frame(
    A = x,
    B = c(2, 4, 6, 8, 10, NA)
  )

  expect_equal(
    unclass (wmeasures(DF, what = c("mean", "median")))[, "mean"],
    c(A = wmean(x), B = wmean(DF$B))
  )

  expect_equal(
    unclass (wmeasures(DF, what = "n"))[, "n"],
    c(A = 5, B = 5)
  )

  expect_equal(
    unclass (wmeasures(DF, what = "NA"))[, "NA"],
    c (A = 1, B = 1)
  )

  expect_equal(
    unclass (wmeasures(DF, what = "range"))[, c("min", "max")],
    rbind (
      A = c(min = 1, max = 5),
      B = c(min = 2, max = 10)
    )
  )

  expect_equal(
    unclass (wmeasures(DF, what = "iqr"))[, "iqr"],
    c(A = wIQR(x, na.rm = TRUE), B = wIQR(DF$B, na.rm = TRUE))
  )

  skip_if_not_installed("admisc")

  expect_equal(
    unclass (admisc::using (DF, wmeasures (., what = "n")))[, "n"],
    c(A = 5, B = 5)
  )
})


test_that("wmeasures() works with custom functions", {
  expect_equal(
    wmeasures(
      x,
      what = list(
        Valid = function (x) sum (!is.empty (x)),
        Range = function (x, na.rm = TRUE) diff (range (x, na.rm = na.rm))
      )
    ),
    c(Valid = 6, Range = 4)
  )
})


test_that("wmeasures() uses consistent decimals within each column", {
  y <- declared (
    c(1, 1, 2, 3, -1),
    labels = c(Good = 1, Bad = 3, DK = -1),
    na_values = -1
  )

  output <- capture.output (print (wmeasures (y), startend = FALSE))
  expect_true (grepl ("\\bn\\b", output[1]))
  expect_true (grepl ("\\b5\\b", output[2]))
  expect_false (grepl ("5\\.000", output[2]))

  DF <- data.frame (
    A = y,
    B = c(2, 4, 6, 8, 10)
  )

  output <- capture.output (
    print (wmeasures (DF, what = c ("n", "mean", "sd")), startend = FALSE)
  )

  expect_true (any (grepl ("A", output)))
  expect_false (any (grepl ("5\\.000", output)))
  expect_true (any (grepl ("6\\.00", output)))

  summaries <- matrix (
    c(
      235.14, 136.259, 97.604, 72.271,
      206.971, 108.771, 78.695, 112.077,
      50, 147, 250, 284,
      0, 0, 4, 5
    ),
    nrow = 4,
    dimnames = list (
      c(
        "Very interested",
        "Quite interested",
        "Hardly interested",
        "Not at all interested"
      ),
      c("mean", "sd", "n", "NA")
    )
  )
  class (summaries) <- c("wmeasures", class (summaries))
  output <- capture.output (print (summaries, startend = FALSE))

  expect_true (any (grepl ("235\\.140", output)))
  expect_false (any (grepl ("50\\.000", output)))
})


test_that("wmeasures() validates what", {
  expect_error(wmeasures(x, what = "Hello"), "Unknown measure name")

  expect_error(
    wmeasures(x, what = list(mean = "Hello")),
    "character vector"
  )
})
