local_edition (3)

x <- declared (
  c (1:5, -1),
  labels = c (Good = 1, Bad = 5, DK = -1),
  na_values = -1
)

df <- data.frame (x = x, y = 10:15)

test_that ("format.declared S3 method works", {
  formatted_df <- format (df)
  expect_equal (as.character (formatted_df$x[6]), "NA(-1)")
})

test_that ("Summary.declared S3 method works for sum, min, max, range", {
  expect_equal (sum (x), 15)
  expect_equal (sum (x, na.rm = TRUE), 15)

  expect_equal (min (x), 1)
  expect_equal (max (x), 5)
  expect_equal (range (x), c (1, 5))

  expect_true (any (x == 3))
  expect_false (all (x == 3))
})
