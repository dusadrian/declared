x <- declared(
  c(1:5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_values = -1
)

test_that("vctrs method works", {
  expect_equal(vctrs::vec_ptype_abbr(x), "int+lbl")
  
  expect_equal(vctrs::vec_ptype_full(x), "declared<integer>")

  # expect_length(vec_ptype2(x, x), 0)
})

test_that("tests have the same output", {
  expect_snapshot(x)
  expect_snapshot(vctrs::vec_ptype_abbr(x))
  expect_snapshot(vctrs::vec_ptype_full(x))
})
