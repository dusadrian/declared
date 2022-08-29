x <- declared(
  c(1:5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_values = -1
)


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
  expect_true(grepl("numeric", likely_type(c(1:5, 2.0))))
  
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

  expect_length(names_values(xr), 6)

  expect_length(names_values(x, drop_na = TRUE), 5)

  expect_error(names_values("A"))
})

test <- declared(1:5, labels = c(A = 1))
test <- declared(1:10, labels = c(A = 1), na_values = 9, na_range = c(8, 10))
test <- declared(1:5, labels = c(A = 1), na_range = c(8, 10))

test_that("internal non exported functions work", {
  expect_error(coerceMode_(list(A = 1)))

  expect_false(possibleNumeric_(rep(NA, 5)))
  
  expect_equal(possibleNumeric_(rep(NA, 5), each = TRUE), as.logical(rep(NA, 5)))
  
  expect_false(possibleNumeric_(rep(TRUE, 5)))
  
  expect_equal(possibleNumeric_(rep(TRUE, 5), each = TRUE), rep(FALSE, 5))

  expect_true(possibleNumeric_(xr))

  labels(xr) <- NULL

  expect_true(possibleNumeric_(xr))

  expect_true(all(possibleNumeric_(1:5, each = TRUE)))

  expect_true(possibleNumeric_(factor(1:5)))

  mbchar <- rawToChar(as.raw(c(227, 129, 130)))

  expect_equal(possibleNumeric_(c(1, "a", mbchar), each = TRUE), c(TRUE, FALSE, FALSE))

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

  atag <- makeTag_("a")

  expect_true(hasTag_(atag))

  expect_false(hasTag_(NA_integer_))

  expect_false(hasTag_(atag, tag = 1))

  expect_error(hasTag_(atag, tag = list(A = 1)))

  expect_equal(getTag_("a"), as.logical(NA))

  expect_equal(getTag_(atag), "a")

  expect_equal(hasTag_(c(atag, NA), "a"), c(TRUE, FALSE))

  onetag <- makeTag_(1)

  expect_equal(getTag_(onetag), 1)

  minustag <- makeTag_(-1)

  bigtag <- makeTag_(99)

  bigminustag <- makeTag_(-99)

  hastag <- hasTag_(c(minustag, NA, atag))

  hasminusone <- hasTag_(c(minustag, NA, atag), -1)

  hasbigminus <- hasTag_(c(bigminustag, atag), -99)

  seetags <- getTag_(c(atag, NA, minustag))

  seetag <- hasTag_(bigtag, "")

  test <- hasTag_(makeTag_("-a"), "-a")

  test <- hasTag_(makeTag_("-ab"), "-ab")

  expect_true(unlockEnvironment_(asNamespace("stats")))

  x <- c(12, 12.3, 12.34)

  expect_equal(numdec_(x), 15)

  expect_equal(numdec_(x, each = TRUE), c(0, 15, 2))

  x <- c("-.1", " 2.75 ", "12", "B", NA)

  expect_equal(numdec_(x), 2)

  expect_error(numdec_("a"))

  expect_equal(numdec_(x, each = TRUE), c(1, 2, 0, NA, NA))

  text <- trimstr_("foo", what = "+")
})


# test_that("tagged values work", {
#     expect_true(hasTag_(makeTag_("-a"), "-a"))
#     expect_true(hasTag_(makeTag_("-ab"), "-ab"))
# })
