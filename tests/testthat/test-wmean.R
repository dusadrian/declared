local_edition(3)
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


test_that("wmean() works", {
  expect_equal(wmean(x), 3)

  expect_equal(wmean(hx), 3)

  expect_error(wmean(x, wt = "A"))

  expect_warning(
    wmean("A"),
    "should be a numerical / logical"
  )

  expect_error(wmean(x, wt = rep(1/7, 7)))

  expect_equal(wmean(x, wt = rep(1/6, 6)), 3)

  expect_equal(wmean(c(1:5, NA), wt = rep(1/6, 6), na.rm = FALSE), NA_real_)

  expect_error(wmean(x, wt = c(-0.2, rep(0.2, 5))))

  expect_equal(wmean(x, wt = rep(1/6, 6), trim = 0.6), 5)

  expect_equal(wmean(x, wt = rep(1/6, 6), trim = 0.1), 3)

  expect_error(wmean(as.complex(1:6), wt = rep(1/6, 6), trim = 0.1))
})

test_that("wmean() trims observations without weights", {
  y <- c(100, 3, 7, 1, 9, 2, 5, 8, 4, 6)

  expect_equal(wmean(y, trim = 0.1), mean(y, trim = 0.1))
  expect_equal(wmean(y, trim = 0.1), 5.5)

  expect_equal(wmean(x, trim = 0.2), mean(1:5, trim = 0.2))
})

test_that("wmean() trims by the values of x, not by the weighted values", {
  y <- c(10, 20, 30, 40, 50)

  # wt * y would rank 50 below 40, keeping it instead of 40
  expect_equal(wmean(y, wt = c(1, 1, 1, 1, 0.7), trim = 0.2), 30)

  y <- c(100, 3, 7, 1, 9, 2, 5, 8, 4, 6)
  expect_equal(wmean(y, wt = rep(2, 10), trim = 0.1), mean(y, trim = 0.1))
})

test_that("trimmed wmean() shifts with the location of x", {
  y <- c(10, 20, 30, 40, 50)
  wt <- c(1, 1, 1, 1, 0.7)
  m <- wmean(y, wt = wt, trim = 0.2)

  expect_equal(wmean(y + 100, wt = wt, trim = 0.2), m + 100)
  expect_equal(wmean(y - 100, wt = wt, trim = 0.2), m - 100)
})

set.seed(215)

fx <- factor(sample(letters[1:5], 215, replace = TRUE))

xc <- sample(0:4, 215, replace = TRUE)

values <- sample(c(1:7, -91), 215, replace = TRUE)
x2 <- declared(values, labels = c("Good" = 1, "Bad" = 7))

values <- sample(c(1:7, -91, NA), 215, replace = TRUE)
x4 <- declared(
    values,
    labels = c("Good" = 1, "Bad" = 7, "Don't know" = -91),
    na_values = -91
)

DF <- data.frame(
  Area = declared(
    sample(1:2, 215, replace = TRUE, prob = c(0.45, 0.55)),
    labels = c(Rural = 1, Urban = 2)
  ),
  Gender = declared(
    sample(1:2, 215, replace = TRUE, prob = c(0.55, 0.45)),
    labels = c(Males = 1, Females = 2)
  ),
  Age = sample(18:90, 215, replace = TRUE),
  Children = sample(0:5, 215, replace = TRUE)
)

# Weighting: observed proportions
op <- proportions(with(DF, table(Gender, Area)))
# Theoretical proportions: 53% Rural, and 50% Females
tp <- rep(c(0.53, 0.47), each = 2) * rep(c(0.498, 0.502), 2) / op
# The frequency weights
DF$fweight <- tp[match(10 * DF$Area + DF$Gender, c(11, 12, 21, 22))]


library(admisc)
test_that("tests have the same output", {
  expect_snapshot(x)
  expect_snapshot(wmean(x))
  expect_snapshot(xc)
  expect_snapshot(wmean(xc))
  expect_snapshot(hx)
  expect_snapshot(wmean(hx))
  expect_snapshot(using(DF, wmean(Age)))
  expect_snapshot(using(DF, wmean(Age, wt = fweight)))
  expect_snapshot(using(DF, wmean(Age), split.by = Gender))
  expect_snapshot(using(DF, wmean(Age, wt = fweight), split.by = Gender))
  expect_snapshot(using(DF, wmean(Age), split.by = Gender & Children))
  expect_snapshot(
    using(DF, wmean(Age, wt = fweight), split.by = Gender & Children)
  )
})
