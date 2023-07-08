x <- declared(
  c(1:5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_values = -1
)

test_that("print.declared method works", {
  test <- capture.output(print(x))

  label(x) <- "Variable label"
  test <- capture.output(print(x))

  missing_range(x) <- c(-5, -1)
  test <- capture.output(print(x))

  labels(x) <- NULL
  test <- capture.output(print(x))

  expect_true(is.character(test))
})



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



xl <- declared(
  sample(c(1:4, 500, -1), 215, replace = TRUE),
  labels = c(Good = 1, Bad = 500, DK = -1),
  na_values = -1
)


test_that("print.w_table method works", {
  test <- capture.output(print(w_table(xl)))
  test <- capture.output(print(w_table(c(xl, NA))))
  
  test <- capture.output(print(w_table(xl, values = TRUE)))
  test <- capture.output(print(proportions(w_table(xl))))

  wdf <- with(DF, w_table(Gender, Area, values = TRUE))
  test <- capture.output(print(wdf))
  test <- capture.output(print(proportions(wdf)))

  test <- capture.output(print(proportions(w_table(xl, DF$Area))))
  test <- capture.output(print(w_table(xl, DF$Area, values = TRUE)))

  test <- capture.output(print(with(DF, w_table(Gender, Area))))
  test <- capture.output(print(with(DF, w_table(Gender, values = TRUE))))
  test <- capture.output(
    print(with(DF, w_table(Gender, values = TRUE)), show_values = FALSE)
  )

  test <- capture.output(print(w_table(c(1:5, NA))))
  test <- capture.output(print(with(DF, w_table(Gender, Area, vlabel = TRUE))))
  test <- capture.output(print(with(DF, w_table(Gender, vlabel = TRUE))))
  test <- capture.output(print(labels(xl, print_as_df = FALSE)))

  expect_error(
    capture.output(print(w_table(1:101))),
    "looks like a lot of categories"
  )
})


test_that("print.fobject method works", {
  test <- capture.output(print(w_summary(xl)))
  expect_true(is.character(test))
})


test_that("tests have the same output", {
  expect_snapshot(print(w_table(xl)))
  expect_snapshot(print(w_table(c(xl, NA))))
  expect_snapshot(print(w_table(xl, values = TRUE)))
  expect_snapshot(print(proportions(w_table(xl))))
  wdf <- with(DF, w_table(Gender, Area, values = TRUE))
  expect_snapshot(print(wdf))
  expect_snapshot(print(proportions(wdf)))
  expect_snapshot(print(proportions(w_table(xl, DF$Area))))
  expect_snapshot(print(w_table(xl, DF$Area, values = TRUE)))
  expect_snapshot(print(with(DF, w_table(Gender, Area))))
  expect_snapshot(print(with(DF, w_table(Gender, Area, vlabel = TRUE))))
  expect_snapshot(print(with(DF, w_table(Gender, values = TRUE))))
  expect_snapshot(
    print(with(DF, w_table(Gender, values = TRUE)), show_values = FALSE)
  )
  expect_snapshot(print(w_table(c(1:5, NA))))
  expect_snapshot(cat("w_table(1:101) # for the output below:"))
  expect_snapshot_error(print(w_table(1:101)))
  expect_snapshot(print(w_summary(xl)))
  expect_snapshot(print(labels(xl)))
  expect_snapshot(print(labels(xl, print_as_df = FALSE)))
})

lbls <- 1:10
names(lbls) <- paste("A", lbls, sep = "")

xlarge <- declared(
  sample(c(1:10), 25, replace = TRUE),
  labels = lbls
)

test_that("very many labels are truncated at print", {
  expect_snapshot(print(xlarge))
})
