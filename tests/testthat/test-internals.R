library(declared)
set.seed(12345)

x <- declared(
  c(1:5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_values = -1
)

incx <- declared(
  c(1, 2, 4, 5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_values = -1
)

tx <- c(1:5, makeTag_("a"))


test_that("anyTagged_() works", {
  expect_false(anyTagged_(x))
  expect_true(anyTagged_(tx))
  expect_true(anyTagged_(data.frame(tx)))
})


test_that("all_missing_values() works", {
  expect_equal(all_missing_values(c(1:5, -91), na_values = -91), -91)
  expect_equal(
    all_missing_values(c(1:5, -91), na_range = c(-91, -99)),
    all_missing_values(c(1:5, -91), na_range = c(-99, -91))
  )
  expect_error(all_missing_values(c(1:5, -91), na_range = c(-91, -91)))
})


test_that("format_declared() works", {
  expect_length(format_declared(x), 6)

  expect_error(format_declared(list(A = 1)))
})


test_that("order_declared() works", {
  expect_length(order_declared(x), 5)

  expect_setequal(
    order_declared(c(x, NA), na.last = FALSE, empty.last = TRUE),
    order_declared(c(x, NA), na.last = TRUE, empty.last = FALSE)
  )

  expect_setequal(
    order_declared(x, na.last = TRUE),
    order_declared(x, na.last = FALSE)
  )

  expect_error(order_declared(list(A = 1)))

  expect_error(order_declared(x, empty.last = "A"))
})


test_that("value_labels() is deprecated", {
  expect_warning(
    value_labels(x),
    "deprecated, use labels"
  )
})


test_that("variable_label() is deprecated", {
  expect_warning(
    variable_label(x),
    "deprecated, use label"
  )
})


test_that("likely_type() works", {
  expect_true(grepl("numeric", likely_type(c(1:5, 2.1))))

  expect_true(grepl("integer", likely_type(1:5)))

  expect_true(grepl("character", likely_type("a")))

  expect_null(likely_type(as.complex(1)))
})


test_that("check_measurement() works", {
  expect_null(check_measurement(NULL))

  expect_true(grepl("categorical", check_measurement("nominal")))

  expect_true(grepl("categorical", check_measurement("ordinal")))

  expect_true(grepl("categorical", check_measurement("qualitative")))

  expect_true(grepl("quantitative", check_measurement("interval")))

  expect_true(grepl("quantitative", check_measurement("ratio")))

  expect_true(grepl("quantitative", check_measurement("discrete")))

  expect_true(grepl("quantitative", check_measurement("continuous")))

  expect_true(grepl("quantitative", check_measurement("metric")))

  expect_true(grepl("quantitative", check_measurement("numeric")))

  expect_true(grepl("quantitative", check_measurement("interval, discrete")))

  expect_error(check_measurement("ordinal, ratio"))

  expect_error(check_measurement("discreet"))

  expect_error(check_measurement("nominal, ordinal"))

  expect_error(check_measurement("interval, ratio"))

  expect_error(check_measurement("discrete, continuous"))
})


y <- declared(
  c(1:5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_values = c(-1, 1, 5)
)

z <- declared(
    c(1:5, "a"),
    labels = c(Good = 1, Bad = 5, DK = -1),
    na_values = c(-1, 1, 5)
)

z2 <- declared(
    c(1:5, -1),
    labels = c(Good = 1, Bad = 5, DK = -1),
    na_range = c(-5, -1)
)


cx1 <- declared(
  x = sample(
    c("Left", "Middle", "Right", "Apolitic"),
    20,
    replace = TRUE
  ),
  na_values = "Apolitic"
)

cx2 <- declared(
  x = sample(
    c("a", "b", "c", "z"),
    20,
    replace = TRUE
  ),
  labels = c("Left" = "a", "Middle" = "b", "Right" = "c", "Apolitic" = "z"),
  na_values = "z"
)


test_that("likely_measurement() works", {
  expect_equal(likely_measurement(x), "categorical")

  expect_equal(likely_measurement(y), "quantitative")

  expect_equal(likely_measurement(z), "")

  expect_equal(likely_measurement(z2), "categorical")

  expect_equal(likely_measurement(cx1), "")

  expect_equal(likely_measurement(cx2), "categorical")

  expect_equal(likely_measurement(1:5), "quantitative")
})


xr <- declared(
  c(1:5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_range = c(-5, -1)
)



test_that("names_values() works", {
  expect_length(names_values(x), 6)

  expect_length(names_values(x, observed = FALSE), 6)

  expect_length(names_values(xr), 6)

  expect_length(names_values(x, drop_na = TRUE), 5)

  expect_error(names_values("A"))
})




atag <- makeTag_("a")
onetag <- makeTag_(1)
minustag <- makeTag_(-1)
bigtag <- makeTag_(99)
bigminustag <- makeTag_(-99)



test_that("tests have the same output", {
  expect_snapshot(x)
  expect_snapshot(format_declared(x))
  expect_snapshot(order_declared(x))
  expect_snapshot(order_declared(x, na.last = TRUE))
  expect_snapshot(order_declared(x, na.last = FALSE))
  expect_snapshot(order_declared(c(x, NA), na.last = FALSE, empty.last = TRUE))
  expect_snapshot(order_declared(c(x, NA), na.last = TRUE, empty.last = FALSE))
  expect_snapshot(likely_type(1:5))
  expect_snapshot(likely_type(c(1:5, 2.1)))
  expect_snapshot(likely_type("a"))
  expect_snapshot(likely_type(as.complex(1)))
  expect_snapshot(check_measurement("nominal"))
  expect_snapshot(check_measurement("ordinal"))
  expect_snapshot(check_measurement("qualitative"))
  expect_snapshot(check_measurement("interval"))
  expect_snapshot(check_measurement("ratio"))
  expect_snapshot(check_measurement("discrete"))
  expect_snapshot(check_measurement("continuous"))
  expect_snapshot(check_measurement("metric"))
  expect_snapshot(check_measurement("numeric"))
  expect_snapshot(check_measurement("interval, discrete"))
  expect_snapshot(likely_measurement(x))
  expect_snapshot(y)
  expect_snapshot(likely_measurement(y))
  expect_snapshot(z)
  expect_snapshot(likely_measurement(z))
  expect_snapshot(cx1)
  expect_snapshot(likely_measurement(cx1))
  expect_snapshot(cx2)
  expect_snapshot(likely_measurement(cx2))
  expect_snapshot(names_values(x))
  expect_snapshot(names_values(x, drop_na = TRUE))
  expect_snapshot(xr)
  expect_snapshot(names_values(xr))
  expect_snapshot(names_values(incx))
  expect_snapshot(names_values(incx, observed = FALSE))
  expect_snapshot(declared(1:5, labels = c(A = 1)))
  expect_snapshot(
    declared(1:10, labels = c(A = 1), na_values = 9, na_range = c(8, 10))
  )
  expect_snapshot(declared(1:5, labels = c(A = 1), na_range = c(8, 10)))
  expect_snapshot(possibleNumeric_(rep(NA, 5)))
  expect_snapshot(possibleNumeric_(rep(TRUE, 5)))
  expect_snapshot(possibleNumeric_(xr))
  expect_snapshot(possibleNumeric_(1:5, each = TRUE))
  expect_snapshot(wholeNumeric_(xr))
  expect_snapshot(wholeNumeric_(1:5, each = TRUE))
  expect_snapshot(tryCatchWEM_(order_declared(list(A = 1))))
  expect_snapshot(tryCatchWEM_(value_labels(x), capture = TRUE))
  expect_snapshot(
    tryCatchWEM_(as.declared(1:5, interactive = TRUE), capture = TRUE)
  )
  expect_snapshot(padLeft_("foo", 5))
  expect_snapshot(padRight_("foo", 5))
  expect_snapshot(padBoth_("foo", 5))
  expect_snapshot(hasTag_(bigtag, ""))
  expect_snapshot(hasTag_(c(atag, NA), "a"))
  expect_snapshot(getTag_(onetag))
  decx <- c(12, 12.3, 12.34)
  expect_snapshot(decx)
  expect_snapshot(numdec_(decx))
  expect_snapshot(numdec_(decx, each = TRUE))
  decx2 <- c("-.1", " 2.75 ", "12", "B", NA, "2e-05", "100.00")
  expect_snapshot(decx2)
  expect_snapshot(numdec_(decx2))
  expect_snapshot(numdec_(decx2, each = TRUE))
  expect_snapshot(trimstr_(" foo ", what = " "))
  expect_snapshot(trimstr_(" foo ", what = "+"))
  expect_snapshot(hasTag_(c(minustag, NA, atag)))
  expect_snapshot(hasTag_(c(bigminustag, atag)))
})


test_that("internal non exported functions work", {
  expect_error(coerceMode_(list(A = 1)))

  expect_false(possibleNumeric_(rep(NA, 5)))

  expect_equal(
    possibleNumeric_(rep(NA, 5), each = TRUE), as.logical(rep(NA, 5))
  )

  expect_false(possibleNumeric_(rep(TRUE, 5)))

  expect_equal(possibleNumeric_(rep(TRUE, 5), each = TRUE), rep(FALSE, 5))

  expect_true(possibleNumeric_(xr))

  labels(xr) <- NULL

  expect_true(possibleNumeric_(xr))

  expect_true(all(possibleNumeric_(1:5, each = TRUE)))

  expect_true(possibleNumeric_(factor(1:5)))

  mbchar <- rawToChar(as.raw(c(227, 129, 130)))

  expect_equal(
    possibleNumeric_(c(1, "a", mbchar), each = TRUE), c(TRUE, FALSE, FALSE)
  )

  expect_equal(asNumeric_(factor(1:2)), 1:2)

  expect_equal(asNumeric_(factor(1:2), levels = FALSE), 1:2)

  expect_true(wholeNumeric_(xr))

  expect_false(wholeNumeric_(rep(NA, 2)))

  expect_false(wholeNumeric_(rep(TRUE, 2)))

  expect_equal(wholeNumeric_(rep(NA, 2), each = TRUE), as.logical(rep(NA, 2)))

  expect_equal(wholeNumeric_(rep(TRUE, 2), each = TRUE), c(FALSE, FALSE))

  expect_equal(wholeNumeric_(1:2, each = TRUE), c(TRUE, TRUE))

  dfm <- data.frame(A = factor(letters[1:2]), B = 3:4, C = 5:6)

  aa <- tryCatchWEM_(order_declared(list(A = 1)))

  aa <- tryCatchWEM_(value_labels(x), capture = TRUE)

  aa <- tryCatchWEM_(as.declared(1:5, interactive = TRUE), capture = TRUE)

  text <- padLeft_("foo", 5)

  text <- padRight_("foo", 5)

  text <- padBoth_("foo", 5)

  expect_true(hasTag_(atag))

  expect_false(hasTag_(NA_integer_))

  expect_false(hasTag_(atag, tag = 1))

  expect_error(hasTag_(atag, tag = list(A = 1)))

  expect_equal(getTag_("a"), as.logical(NA))

  expect_equal(getTag_(atag), "a")

  expect_equal(hasTag_(c(atag, NA), "a"), c(TRUE, FALSE))

  expect_equal(getTag_(onetag), 1)

  expect_false(hasTag_(bigtag, ""))

  expect_true(unlockEnvironment_(asNamespace("stats")))

  x <- c(12, 12.3, 12.34)

  expect_equal(numdec_(x), 2)

  expect_equal(numdec_(x, each = TRUE), c(0, 1, 2))

  x <- c("-.1", " 2.75 ", "12", "B", NA)

  expect_equal(numdec_(x), 2)

  expect_error(numdec_("a"))

  expect_equal(numdec_(x, each = TRUE), c(1, 2, 0, NA, NA))

  expect_equal(numdec_(c(234.1, 3.7500, 1.345, 3e-17)), 3)

  text <- trimstr_("foo", what = "+")

  # expect_equal(hasTag_(c(minustag, NA, atag), -1), c(TRUE, FALSE, FALSE))

  # expect_equal(hasTag_(c(bigminustag, atag), -99), c(TRUE, FALSE))

  # expect_equal(getTag_(c(atag, NA, minustag)), c("a", NA, "-1"))

  # expect_true(hasTag_(makeTag_("-a"), "-a"))

  # expect_true(hasTag_(makeTag_("-ab"), "-ab"))
})


foo <- function(x, y, object = FALSE) {
  funargs <- lapply(
    lapply(
      match.call(), deparse)[-1],
      function(x) gsub("'|\"|[[:space:]]", "", x
    )
  )

  return(c(
    getName_(funargs$x, object = object),
    getName_(funargs$y, object = object)
  ))
}

dfx <- data.frame(x = x, incx = x)


test_that("getName_() works", {
  expect_equal(foo(x, incx), c("x", "incx"))
  expect_equal(foo(dfx$x, dfx$incx), c("x", "incx"))
  expect_equal(foo(dfx[["x"]], dfx[["incx"]]), c("x", "incx"))
  expect_equal(foo(dfx[, c("x", "incx")]), c("x", "incx"))
  expect_equal(foo(dfx[, c(1, 2)]), c("x", "incx"))
  expect_equal(foo(dfx$x, object = TRUE), "dfx")
  expect_equal(foo(dfx[, c(1, 2)], object = TRUE), "dfx")
  expect_equal(foo("dfx"), "dfx")
})
