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


test_that("wquantile() works", {
  expect_equal(
    unclass(wquantile(x)),
    quantile(drop(x), na.rm = TRUE)
  )

  expect_equal(wquantile(x), wquantile(hx))

  expect_error(wquantile("A"))

  expect_error(wquantile(x, wt = "weight"))

  expect_error(wquantile(x, wt = rep(1, 7)))

  expect_error(wquantile(x, wt = c(NA, rep(1, 5)), na.rm = FALSE))

  expect_error(
    wquantile(
      x,
      wt = rep(1/6, 6),
      probs = c(seq(0, 0.75, 0.25), 1.1)
    )
  )

  expect_length(
    wquantile(
      x,
      wt = rep(1/6, 6),
      probs = c(NA, seq(0, 0.75, 0.25))
    ),
    4
  )

  expect_length(wquantile(x, wt = rep(1, 6)), 5)
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
  expect_snapshot(wquantile(x))
  expect_snapshot(hx)
  expect_snapshot(wquantile(hx))
  expect_snapshot(using(DF, wquantile(Age)))
  expect_snapshot(using(DF, wquantile(Age, wt = fweight)))
  expect_snapshot(using(DF, wquantile(Age), split.by = Gender))
  expect_snapshot(using(DF, wquantile(Age, wt = fweight), split.by = Gender))
})
