declared:::.onLoad()
# context("onLoad")

x <- declared(
  c(1:5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_values = -1,
  na_range = c(-5, -1)
)

hx <- haven::labelled_spss(
  c(1:5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_values = -1
)

fx <- factor(
  c(1:5, NA),
  levels = c(1:5),
  labels = c("Good", 2:4, "Bad")
)

fo <- factor(
  c(1:5, -1),
  levels = c(1:5, -1),
  labels = c("Good", 2:4, "Bad", "DK"),
  ordered = TRUE
)

x2 <- declared(
  c(-1, 1:5, -2),
  labels = c(Good = 1, Bad = 5, DK = -1, Other = -2),
  na_range = c(-5, -1)
)

dfd <- data.frame(x, hx, fx, fo)

test_that("onLoad functions work", {
  test <- capture.output(print(dfd[-seq(1, 6), ]))
  test <- capture.output(print(dfd[, -seq(1, 4)]))
  test <- capture.output(print(dfd))
  test <- capture.output(print(dfd, max = 5))
  test <- capture.output(print(dfd, row.names = FALSE))
  expect_error(print(dfd, max = Inf))
  
  test <- capture.output(format(dfd[, -seq(1, 4)]))
  test <- rbind(dfd, dfd)
  expect_error(
    rbind(dfd, dfd[, -4]),
    "numbers of columns of arguments do not match"
  )

  dfd2 <- dfd
  dfd2$fx <- as.character(dfd2$fx)
  test <- rbind(dfd, dfd2)
  test <- rbind(as.list(dfd), as.list(dfd))

  test <- rbind(dfd, dfd[-seq(1, nrow(dfd)), , drop = FALSE])
  test <- rbind(
    dfd[-seq(1, nrow(dfd)), , drop = FALSE],
    dfd[-seq(1, nrow(dfd)), , drop = FALSE]
  )
  test <- rbind(dfd1 = dfd, dfd2 = dfd)
  ldfd <- as.list(dfd)
  test <- rbind(dfd, ldfd)
  test <- rbind(dfd, as.list(dfd[1, , drop = FALSE]))
  
  ldfd$x <- ldfd$x[-1]
  expect_error(rbind(dfd, ldfd), "variables should have the same length")
  test <- suppressWarnings(rbind(dfd, rep(1, 4)))
  expect_warning(rbind(dfd, list(1, 3, 1, "Bad")), "invalid factor level")
  
  dfd2 <- dfd
  dfd2$x <- as.matrix(dfd2$x)
  test <- rbind(dfd2, dfd)
  test <- rbind(as.matrix(dfd), as.matrix(dfd))

  test <- rbind(dfd, dfd, factor.exclude = FALSE)
  test <- rbind(dfd[, 1:2], as.matrix(dfd[, 1:2]))
  test <- rbind(dfd, dfd, make.row.names = FALSE)
  test <- rbind(dfd, dfd[, 4:1])
  dfd2 <- dfd
  names(dfd2)[4] <- "fo2"
  expect_error(rbind(dfd, dfd2), "names do not match previous names")

  expect_length(as.factor(fx), 6)
  expect_length(as.factor(1:6), 6)
  expect_length(as.factor(c(a = 1, b = 2, c = 2, d = 4, e = 5, f = 6)), 6)
  expect_length(as.factor(letters[1:6]), 6)
  expect_length(as.factor(x), 6)
  expect_length(as.factor(x, levels = "values"), 6)
  expect_length(as.factor(x, levels = "both"), 6)
  expect_length(as.factor(x, drop_na = FALSE), 6)
  expect_length(as.factor(x, drop_na = FALSE, nolabels = TRUE), 6)

  expect_equal(drop(x), as.integer(c(1:5, NA)))
  expect_length(drop(matrix(1:3)), 3)

  expect_length(sd(x), 1)
  expect_length(var(x), 1)
  expect_length(var(as.data.frame(x)), 1)
  expect_error(var(x, use = "else"), "invalid 'use' argument")
  expect_length(fivenum(x), 5)
  expect_length(fivenum(drop_na(x), na.rm = TRUE), 5)

  test <- order(x)

  # examples from base ?order
  ii <- order(a <- c(1,1,3:1,1:4,3), b <- c(9,9:1), c <- c(2,1:9))
  test <- rbind(a, b, c)[,ii]
  test <- rbind(a, b, c)[, order(a, -b, c)]
  cb <- as.character(b)
  test <- rbind(a, b, c)[, order(a, -xtfrm(cb), c)]
  test <- rbind(a, b, c)[, order(
    a, cb, c, decreasing = c(FALSE, TRUE, FALSE), method="radix"
  )]
  dd <- transform(data.frame(a, b, c), c = factor(c, labels = LETTERS[9:1]))
  test <- dd[ order(a, -b, c), ]
  test <- dd[ do.call(order, dd), ]
  set.seed(1)  # reproducible example:
  d4 <- data.frame(x = round(   rnorm(100)), y = round(10*runif(100)),
                  z = round( 8*rnorm(100)), u = round(50*runif(100)))
  d4s <- d4[ do.call(order, d4), ]
  i <- which(diff(d4s[, 3]) == 0)
  #   in 2 places, needed 3 cols to break ties:
  test <- d4s[ rbind(i, i+1), ]
  a <- c(5:1, 6:8, 12:9)
  b <- (a - 5)^2
  o <- order(a)
  test <- rbind(a[o], b[o])

  # examples from base ?rbind
  dd <- 10
  test <- rbind(1:4, c = 2, "a++" = 10, dd, deparse.level = 0)
  test <- rbind(1:4, c = 2, "a++" = 10, dd, deparse.level = 1)
  test <- rbind(1:4, c = 2, "a++" = 10, dd, deparse.level = 2)

  b0 <- gl(3,4, labels=letters[1:3])
  bf <- setNames(b0, paste0("o", seq_along(b0)))
  df  <- data.frame(a = 1, B = b0, f = gl(4,3))
  df. <- data.frame(a = 1, B = bf, f = gl(4,3))
  new <- data.frame(a = 8, B ="B", f = "1")
  df1  <- rbind(df , new)
  df.1 <- rbind(df., new)
  expect_identical(df1, rbind(df,  new, make.row.names = FALSE))
  expect_identical(df1, rbind(df.,  new, make.row.names = FALSE))

  expect_error(register_S3_method("haven", "as_factor", "declared", fun = 1))

  dd <- data.frame(a = factor(1:2), b = 3:4, c = 5:6)
  test <- rbind(dd, list(1, 1, 1))

  expect_equal(sum(is.element(x, labels(x))), 3L)
  expect_equal(sum(x %in% labels(x)), 3L)
  expect_equal(is.element(x, labels(x)), x %in% labels(x))
  expect_equal(match(x, labels(x)), c(1L, NA, NA, NA, 2L, 3L))
  expect_equal(sum(is.na(match(x, x2))), 0L)
  expect_equal(sum(is.na(match(x2, x))), 1L)
})

# to test if S3 methods are (re)registered once a package is (re)loaded
unloadNamespace("labelled")
library(labelled)


test_that("tests have the same output", {
  expect_snapshot(x)
  expect_snapshot(fx)
  expect_snapshot(as.factor(fx))
  expect_snapshot(as.factor(c(a = 1, b = 2, c = 2, d = 4, e = 5, f = 6)))
  expect_snapshot(as.factor(letters[1:6]))
  expect_snapshot(as.factor(x))
  expect_snapshot(as.factor(x, drop_na = FALSE))
  expect_snapshot(as.factor(x, drop_na = FALSE, nolabels = TRUE))
  expect_snapshot(sd(x))
  expect_snapshot(var(x))
  expect_snapshot(var(as.data.frame(x), na.rm = TRUE))
  expect_snapshot(fivenum(x))
  expect_snapshot(fivenum(drop_na(x), na.rm = TRUE))
  expect_snapshot(order(x))
  expect_snapshot(as.factor(x, levels = "values"))
  expect_snapshot(as.factor(x, levels = "both"))
  expect_snapshot(is.element(x, labels(x)))
  expect_snapshot(x %in% labels(x))
  expect_snapshot(match(x, labels(x)))
  expect_snapshot(x2)
  expect_snapshot(match(x, x2))
  expect_snapshot(match(x2, x))
})
