
myfactor <- factor(
  c(1:5, -1),
  levels = c(Good = 1, Bad = 5, DK = -1)
)

test_that("as.declared() works", {
  expect_true(inherits(as.declared(myfactor), "declared"))
  expect_true(inherits(as.declared(data.frame ( x = 1:2, y = c("a", "b"))), "data.frame"))
})

