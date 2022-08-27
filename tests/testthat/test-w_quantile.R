
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


test_that("w_quantile() works", {
  expect_equal(
    unclass(w_quantile(x)),
    quantile(drop(x), na.rm = TRUE)
  )
  
  expect_equal(w_quantile(x), w_quantile(hx))

  expect_error(w_quantile("A"))
  
  expect_error(w_quantile(x, wt = "weight"))
  
  expect_error(w_quantile(x, wt = rep(1, 7)))

  expect_error(w_quantile(x, wt = c(NA, rep(1, 5)), na.rm = FALSE))
  
  expect_error(
    w_quantile(
      x,
      wt = rep(1/6, 6),
      probs = c(seq(0, 0.75, 0.25), 1.1)
    )
  )

  expect_length(
    w_quantile(
      x,
      wt = rep(1/6, 6),
      probs = c(NA, seq(0, 0.75, 0.25))
    ),
    4
  )

  expect_length(w_quantile(x, wt = rep(1, 6)), 5)
})
