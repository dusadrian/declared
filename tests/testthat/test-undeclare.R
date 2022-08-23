x <- declared(
    c(1:5, -1),
    labels = c(Good = 1, Bad = 5, DK = -1),
    na_values = -1
)

na_index <- attr(x, "na_index")
attrx <- attributes(x)
ux <- x
attributes(ux) <- NULL
ux[na_index] <- names(na_index)
ux <- as.integer(ux)



test_that("undeclare() dropping attributes works", {
  expect_true(identical(undeclare(x, drop = TRUE), ux))
})


attrx$na_index <- NULL
attrx$na_values <- NULL
attrx$na_range <- NULL
attributes(ux) <- attrx

test_that("undeclare() works", {
  expect_true(identical(undeclare(x), ux))
})
