x <- declared(
  c(1:5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_values = -1
)

test_that("as.character.declared works", {
  expect_equal(as.character(x)[6], NA_character_)

  expect_equal(as.character(x, values = TRUE)[6], NA_character_)
  
  expect_equal(as.character(x, drop_na = FALSE)[6], "DK")
  
  expect_equal(as.character(x, drop_na = FALSE, nolabels = TRUE)[6], "DK")

  expect_equal(as.character(x, values = TRUE, drop_na = FALSE)[6], "-1")
})



x2 <- declared(
  c(1:5, -1),
  labels = c(Goodish = 1, Bad = 5, DK = -1),
  na_values = -1
)

x3 <- declared(
  c(1:5, -91),
  labels = c(Good = 1, Bad = 5, DK = -91),
  na_range = c(-91, -93)
)

x4 <- declared(
  c(1:5, -94),
  labels = c(Good = 1, Bad = 5, Other = -94),
  na_range = c(-92, -94)
)

x5 <- declared(
  c(1:5, -99),
  labels = c(Good = 1, Bad = 5, Inapplicable = -99),
  na_range = c(-97, -99)
)

test_that("c.declared works", {
  expect_true(inherits(c(x, 1), "declared"))

  expect_length(c(x, x), 12)

  expect_error(c(x, x2), "Labels must be unique")
  
  expect_length(c(x3, x4), 12)

  expect_length(c(x, x3), 12)

  expect_length(c(x, x3, x4), 18)

  expect_length(c(x3, x, x4), 18)

  expect_error(c(x3, x5), "Incompatible NA ranges")
})



test_that("[.declared works", {
  expect_true(inherits(x[3], "declared"))
})



x6 <- x
x6[2] <- 1
test_that("[.declared<- works", {
  expect_true(inherits(x6, "declared"))
})


names(x6) <- letters[seq(length(x))]
test_that("names.declared<- works", {
  expect_named(x6)
})


x7 <- declared(
  x = c(-92, 1:5, -91, NA, -91),
  na_value = c(-92, -91)
)

test_that("sort.declared works", {
  expect_length(sort(x7), 5)

  expect_false(
    any(is.na(sort(x7)))
  )
  
  expect_false(
    is.na(sort(x7, na.last = TRUE)[1])
  )

  expect_true(
    is.na(sort(x7, na.last = FALSE)[1])
  )
})



x8 <- declared(
  x = c(-92, 1:5, NA, -91),
  na_value = c(-92, -91)
)

test_that("duplicated.declared works", {
  expect_false(any(duplicated(x8)))

  expect_equal(sum(is.na(x8)), 3)

  expect_length(unique(x8), length(x8))

  expect_false(any(duplicated(x8[is.na(x8)])))
})


test_that("head.declared and tail.declared work", {
  expect_length(head(x, n = -2), 4)
  
  expect_length(head(x, n = -7), 0)

  expect_true(identical(head(x7), head(x8)))

  expect_false(identical(tail(x7), tail(x8)))

  expect_equal(tail(x), tail(x, n = -1))
})

x9 <- x
names(x9) <- letters[seq(length(x))]
test_that("na.omit.declared works", {
  expect_length(na.omit(x), 5)

  expect_named(na.omit(x9))
})


test_that("na.fail.declared works", {
  expect_error(na.fail(x), "missing values in object")

  expect_length(na.fail(undeclare(x)), 6)
})


test_that("na.exclude.declared works", {
  expect_length(na.exclude(x), 5)

  expect_length(na.exclude(x9), 5)
})


test_that("summaries for declared work", {
  expect_equal(mean(x), 3)

  expect_equal(median(x), 3)

  expect_length(summary(x), 7)
})


x10 <- declared(
  c(1:5, -91),
  labels = c(Good = 1, Bad = 5, DK = -91),
  na_values = -91
)

x11 <- declared(
  c(2, 2:5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_values = -1
)

test_that("all.equal.declared works", {
  expect_true(all.equal(x, c(1:5, -1)))

  expect_true(all.equal(x, x))

  expect_true(grepl("relative difference", all.equal(x, x10)))

  expect_true(grepl("relative difference", all.equal(x, x11)))
})

test_that("mathematical operations work for declared objects", {
  expect_equal(abs(x), as.integer(c(1:5, NA)))

  expect_equal(sign(x), c(rep(1, 5), NA))

  expect_length(sqrt(x), 6)

  expect_equal(floor(x), c(1:5, NA))

  expect_equal(ceiling(x), c(1:5, NA))

  expect_equal(trunc(x), c(1:5, NA))

  expect_equal(round(x), c(1:5, NA))

  expect_equal(signif(x), c(1:5, NA))

  expect_length(exp(x), 6)

  expect_length(log(x), 6)

  expect_length(expm1(x), 6)

  expect_length(log1p(x), 6)

  expect_length(cos(x), 6)

  expect_length(sin(x), 6)

  expect_length(tan(x), 6)

  expect_length(cospi(x), 6)

  expect_length(sinpi(x), 6)

  expect_length(tanpi(x), 6)

  expect_true(acos(x[1]) == lgamma(x[1]))

  expect_true(asin(x[1]) != atan(x[1]))

  expect_length(gamma(x), 6)

  expect_true(digamma(x[1]) != trigamma(x[1]))

  expect_length(cumsum(x), 6)

  expect_length(cumprod(x), 6)

  expect_length(cummax(x), 6)

  expect_length(cummin(x), 6)

  expect_length(x + x, 6)

  expect_length(x - x, 6)

  expect_length(x * x, 6)

  expect_length(x / x, 6)

  expect_length(x ^ x, 6)

  expect_length(x %% x, 6)

  expect_length(x %/% x, 6)

  expect_length(x & x, 6)

  expect_length(x | x, 6)

  expect_length(!x, 6)

  expect_true(all(x == x))

  expect_true(any(x == "Good"))

  expect_true(!all(x != x))

  expect_true(any(x != "Good"))

  expect_true(sum(x < x) == 0)

  expect_true(all(x <= x))

  expect_true(all(x >= x))

  expect_true(sum(x > x) == 0)

  expect_length(Arg(x), 6)

  expect_length(Conj(x), 6)

  expect_length(Im(x), 6)

  expect_length(Mod(x), 6)

  expect_length(Re(x), 6)
})


test_that("tests have the same output", {
  expect_snapshot(x)
  expect_snapshot(head(x))
  expect_snapshot(tail(x))
  expect_snapshot(is.na(x))
  expect_snapshot(na.omit(x))
  expect_snapshot(na.fail(undeclare(x)))
  expect_snapshot(na.exclude(x))
  expect_snapshot(c(x, 2, -1))
  expect_snapshot(x7)
  expect_snapshot(sort(x7))
  expect_snapshot(sort(x7, na.last = TRUE))
  expect_snapshot(sort(x7, na.last = FALSE))
  expect_snapshot(x8)
  expect_snapshot(duplicated(x8))
  expect_snapshot(is.na(x8))
  expect_snapshot(unique(x8))
  expect_snapshot(abs(x))
  expect_snapshot(sign(x))
  expect_snapshot(sqrt(x))
  expect_snapshot(floor(x))
  expect_snapshot(ceiling(x))
  expect_snapshot(trunc(x))
  expect_snapshot(round(x))
  expect_snapshot(signif(x))
  expect_snapshot(exp(x))
  expect_snapshot(log(x))
  expect_snapshot(expm1(x))
  expect_snapshot(log1p(x))
  expect_snapshot(cos(x))
  expect_snapshot(sin(x))
  expect_snapshot(tan(x))
  expect_snapshot(cospi(x))
  expect_snapshot(sinpi(x))
  expect_snapshot(tanpi(x))
  expect_snapshot(acos(x[1]))
  expect_snapshot(asin(x[1]))
  expect_snapshot(atan(x[1]))
  expect_snapshot(gamma(x))
  expect_snapshot(lgamma(x[1]))
  expect_snapshot(digamma(x[1]))
  expect_snapshot(trigamma(x[1]))
  expect_snapshot(cumsum(x))
  expect_snapshot(cumprod(x))
  expect_snapshot(cummax(x))
  expect_snapshot(cummin(x))
  expect_snapshot(x - x)
  expect_snapshot(x * x)
  expect_snapshot(x / x)
  expect_snapshot(x ^ x)
  expect_snapshot(x %% x)
  expect_snapshot(x %/% x)
  expect_snapshot(x & x)
  expect_snapshot(x | x)
  expect_snapshot(!x)
  expect_snapshot(x > 0)
  expect_snapshot(x != -1)
  expect_snapshot(x == "DK")
  expect_snapshot(Arg(x))
  expect_snapshot(Conj(x))
  expect_snapshot(Im(x))
  expect_snapshot(Mod(x))
  expect_snapshot(Re(x))
})
