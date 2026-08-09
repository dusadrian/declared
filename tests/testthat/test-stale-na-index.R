local_edition (3)


`stale_declared_` <- function () {
  original <- declared (
    c (1, -99, 2, -98, 4),
    labels = c (One = 1, DK = -99, Refused = -98),
    na_values = -99,
    na_range = c (-98, -97)
  )

  model.frame (
    value ~ 1,
    data = data.frame (value = original),
    na.action = na.omit
  )$value
}


`stored_values_` <- function (x) {
  attributes (x) <- NULL
  x
}


test_that ("stale declared missing metadata is discarded as one unit", {
  x <- stale_declared_ ()
  labels_x <- labels (x)

  expect_equal (stored_values_ (x), c (1, 2, 4))
  expect_false (valid_na_index (x))

  x <- declared:::sanitize_na_index_ (x)

  expect_true (valid_na_index (x))
  expect_equal (stored_values_ (x), c (1, 2, 4))
  expect_equal (labels (x), labels_x)
  expect_null (attr (x, "na_index"))
  expect_null (attr (x, "na_values"))
  expect_null (attr (x, "na_range"))
})


test_that ("metadata access and replacement do not revive stale metadata", {
  x <- stale_declared_ ()

  expect_null (missing_values (x))
  expect_null (missing_range (x))
  expect_null (declared:::all_missing_values (x))
  expect_null (attr (names_values (x), "missing"))

  missing_values (x) <- -77
  expect_equal (missing_values (x), -77)
  expect_null (missing_range (x))
  expect_equal (undeclare (x, drop = TRUE), c (1, 2, 4))

  x <- stale_declared_ ()
  missing_range (x) <- c (-80, -70)
  expect_null (missing_values (x))
  expect_equal (missing_range (x), c (-80, -70))
  expect_equal (undeclare (x, drop = TRUE), c (1, 2, 4))

  source <- stale_declared_ ()
  copied <- declared:::copy_labels.declared (source, declared (c (1, 2, 4)))
  expect_equal (labels (copied), labels (source))
  expect_null (missing_values (copied))
  expect_null (missing_range (copied))
})


test_that ("printing and formatting never display false declared missings", {
  x <- stale_declared_ ()

  expect_equal (as.character (x, values = TRUE), c ("1", "2", "4"))
  expect_equal (format_declared (x), c ("1", "2", "4"))

  printed <- capture.output (print (x))
  expect_false (any (grepl ("NA\\(-99\\)|NA\\(-98\\)", printed)))
  expect_false (any (grepl ("Missing values|Missing range", printed)))

  shaft <- pillar::pillar_shaft (x, use_haven = FALSE)
  pillar_output <- capture.output (format (shaft, 40))
  expect_false (any (grepl ("\\(NA\\)", pillar_output)))
})


test_that ("base summaries use every stored value when the index is stale", {
  x <- stale_declared_ ()
  plain <- c (1, 2, 4)
  wt <- c (1, 2, 3)

  expect_equal (mean (x), mean (plain))
  expect_equal (median (x), median (plain))
  expect_equal (weighted.mean (x, wt), weighted.mean (plain, wt))
  expect_equal (summary (x), summary (plain))
  expect_equal (sum (x), sum (plain))
  expect_equal (min (x), min (plain))
  expect_equal (max (x), max (plain))
  expect_equal (sd (x), sd (plain))
  expect_equal (var (x), var (plain))
  expect_equal (fivenum (x), fivenum (plain))
  expect_true (isTRUE (all.equal (x, declared (plain))))

  factor_x <- as.factor (x, levels = "values")
  expect_false (anyNA (factor_x))
  expect_equal (as.character (factor_x), as.character (plain))
})


test_that ("weighted summaries use every stored value when the index is stale", {
  x <- stale_declared_ ()
  plain <- c (1, 2, 4)
  wt <- c (1, 2, 3)

  expect_equal (wmean (x, wt), wmean (plain, wt))
  expect_equal (wmedian (x, wt), wmedian (plain, wt))
  expect_equal (wquantile (x, wt), wquantile (plain, wt))
  expect_equal (wvar (x, wt), wvar (plain, wt))
  expect_equal (wstandardize (x, wt), wstandardize (plain, wt))
  expect_equal (wfivenum (x, wt), wfivenum (plain, wt))
  expect_equal (wmode (x, wt), wmode (plain, wt))
  expect_equal (
    unclass (wmeasures (x, what = c ("n", "mean", "median", "range"))),
    unclass (wmeasures (plain, what = c ("n", "mean", "median", "range")))
  )
  expect_equal (as.vector (wtable (x)), as.vector (wtable (plain)))
})


test_that ("ordering and cumulative operations ignore a stale index", {
  x <- stale_declared_ ()
  plain <- c (1, 2, 4)

  expect_false (anyNAdeclared (x))
  expect_false (any (is.empty (x)))
  expect_equal (order_declared (x), order (plain))
  expect_equal (xtfrm (x), xtfrm (plain))
  expect_equal (declared:::asNumeric_ (x), plain)

  expect_equal (undeclare (cumsum (x), drop = TRUE), cumsum (plain))
  expect_equal (undeclare (cumprod (x), drop = TRUE), cumprod (plain))
  expect_equal (undeclare (cummax (x), drop = TRUE), cummax (plain))
  expect_equal (undeclare (cummin (x), drop = TRUE), cummin (plain))
})


test_that ("subsetting and combining do not rematerialize false missings", {
  x <- stale_declared_ ()

  subset_x <- x[seq_along (x)]
  combined <- c (x, declared (c (5, 6)))
  repeated <- rep (x, 2)
  dropped <- drop_na (x)

  expect_equal (undeclare (subset_x, drop = TRUE), c (1, 2, 4))
  expect_equal (undeclare (combined, drop = TRUE), c (1, 2, 4, 5, 6))
  expect_equal (undeclare (repeated, drop = TRUE), rep (c (1, 2, 4), 2))
  expect_equal (undeclare (dropped, drop = TRUE), c (1, 2, 4))
  expect_null (missing_values (subset_x))
  expect_null (missing_values (combined))
  expect_null (missing_values (repeated))
  expect_null (missing_values (dropped))

  expect_equal (undeclare (na.omit (x), drop = TRUE), c (1, 2, 4))
  expect_equal (undeclare (na.exclude (x), drop = TRUE), c (1, 2, 4))
  expect_no_error (na.fail (x))
})


test_that ("date conversion does not carry stale missing metadata", {
  original <- declared (
    c (as.Date ("2024-01-01"), -99, as.Date ("2024-01-03")),
    labels = c (DK = -99),
    na_values = -99
  )

  stale <- model.frame (
    value ~ 1,
    data = data.frame (value = original),
    na.action = na.omit
  )$value

  converted <- as.Date (stale)

  expect_equal (format_declared (converted), c ("2024-01-01", "2024-01-03"))
  expect_null (attr (converted, "na_index"))
  expect_null (missing_values (converted))
  expect_null (missing_range (converted))
})


test_that ("conversions do not carry stale missing metadata", {
  x <- stale_declared_ ()

  converted <- as.declared (x)
  haven_x <- as.haven (x)
  restored <- vctrs::vec_restore (vctrs::vec_proxy (x), to = x)

  expect_equal (undeclare (converted, drop = TRUE), c (1, 2, 4))
  expect_equal (unclass (haven::zap_labels (haven_x)), c (1, 2, 4))
  expect_equal (undeclare (restored, drop = TRUE), c (1, 2, 4))

  for (object in list (converted, haven_x, restored)) {
    expect_null (attr (object, "na_values"))
    expect_null (attr (object, "na_range"))
  }
})


test_that ("data-frame binding sanitizes every declared column", {
  x <- stale_declared_ ()

  result <- rbind (
    data.frame (value = x),
    data.frame (value = declared (c (5, 6)))
  )

  expect_equal (undeclare (result$value, drop = TRUE), c (1, 2, 4, 5, 6))
  expect_null (attr (result$value, "na_index"))
  expect_null (missing_values (result$value))
  expect_null (missing_range (result$value))
})


test_that ("valid declared missing metadata keeps its existing semantics", {
  x <- declared (
    c (1, -99, 2),
    labels = c (One = 1, DK = -99),
    na_values = -99
  )

  expect_true (valid_na_index (x))
  expect_equal (mean (x), 1.5)
  expect_equal (undeclare (x, drop = TRUE), c (1, -99, 2))
  expect_equal (missing_values (x), -99)
})
