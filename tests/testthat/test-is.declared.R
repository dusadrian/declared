x <- declared(
    c(1:2, -91),
    labels = c(Good = 1, Bad = 2, Missing = -91),
    na_values = -91
)

test_that("is.declared() works", {
  expect_true(is.declared(x))
  expect_true(anyNAdeclared(x))
  expect_false(anyNAdeclared(1:5))
  expect_false(anyNAempty(x))
})
