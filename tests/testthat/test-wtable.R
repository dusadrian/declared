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
        label = "Respodent's area",
        labels = c(Rural = 1, Urban = 2)
    ),
    Gender = declared(
        sample(1:2, 215, replace = TRUE, prob = c(0.55, 0.45)),
        label = "Respodent's gender",
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



test_that("wtable() works", {
  expect_true(inherits(wtable(x), "wtable"))

  expect_true(inherits(wtable(c(x, NA)), "wtable"))

  expect_true(inherits(wtable(hx), "wtable"))

  expect_true(inherits(wtable(fx), "wtable"))

  expect_true(inherits(wtable(xc), "wtable"))

  expect_true(inherits(wtable(x, values = FALSE), "wtable"))

  expect_true(inherits(wtable(1:5), "wtable"))

  expect_true(
    inherits(
      with(DF, wtable(Gender, wt = fweight)),
      "wtable"
    )
  )

  expect_true(
    inherits(
      with(DF, wtable(Gender, Area, wt = fweight)),
      "wtable"
    )
  )

  expect_true(
    inherits(
      with(DF, wtable(Gender, Area, wt = fweight, margin = 1)),
      "wtable"
    )
  )

  expect_true(
    inherits(
      with(DF, wtable(c(NA, Gender[-1]), Area)),
      "wtable"
    )
  )

  expect_true(
    inherits(
      with(DF, wtable(Gender, sample(1:2, 215, replace = TRUE))),
      "wtable"
    )
  )

  expect_error(wtable(x, wt = Inf))

  expect_error(wtable(x, wt = 1))

  expect_error(with(DF, wtable(Gender, Area[-1], wt = fweight)))

  expect_error(with(DF, wtable(Gender, Area, margin = 3)))

  expect_error(with(DF, wtable(Gender, list(A = 1))))

  expect_error(wtable(list(A = 1)))

  expect_error(with(DF, wtable(Gender, list(A = 1), wt = fweight)))
})

library(admisc)
test_that("tests have the same output", {
  expect_snapshot(x)
  expect_snapshot(wtable(x))
  expect_snapshot(wtable(x, values = FALSE))
  expect_snapshot(fx)
  expect_snapshot(wtable(fx))
  expect_snapshot(x2)
  expect_snapshot(wtable(x2))
  expect_snapshot(x4)
  expect_snapshot(wtable(x4))
  expect_snapshot(using(DF, wtable(Gender)))
  expect_snapshot(using(DF, wtable(Gender, wt = fweight)))
  expect_snapshot(using(DF, wtable(Gender), split.by = Area))
  expect_snapshot(using(DF, wtable(Gender, wt = fweight), split.by = Area))
  expect_snapshot(using(DF, wtable(Gender, Area)))
  expect_snapshot(using(DF, wtable(Gender, Area, wt = fweight)))
  expect_snapshot(using(DF, wtable(Gender, vlabel = TRUE)))
  expect_snapshot(using(DF, wtable(Gender, Area, vlabel = TRUE)))
})

tst <- declared(
  c(rep(NA, 5)),
  labels = c(A = 1, Refusal = -91),
  na_values = -91
)

test_that("wtable works with a variable having all values NA", {
  expect_snapshot(wtable(tst))
})
