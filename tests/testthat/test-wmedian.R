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

test_that("wmedian() works", {
  expect_equal(wmedian(x), 3)

  expect_equal(wmedian(hx), 3)

  expect_equal(wmedian(x, na.rm = FALSE), 3)

  expect_equal(wmedian(c(x, NA), na.rm = FALSE), NA_real_)

  expect_error(wmedian(x, wt = "Hello"))

  expect_equal(wmedian(x, wt = rep(1/6, 6)), 5)
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
  expect_snapshot(wmedian(x))
  expect_snapshot(xc)
  expect_snapshot(wmedian(xc))
  expect_snapshot(hx)
  expect_snapshot(wmedian(hx))
  expect_snapshot(using(DF, wmedian(Age)))
  expect_snapshot(using(DF, wmedian(Age, wt = fweight)))
  expect_snapshot(using(DF, wmedian(Age), split.by = Gender))
  expect_snapshot(using(DF, wmedian(Age, wt = fweight), split.by = Gender))
  expect_snapshot(using(DF, wmedian(Age), split.by = Gender & Children))
  expect_snapshot(
    using(DF, wmedian(Age, wt = fweight), split.by = Gender & Children)
  )
})
