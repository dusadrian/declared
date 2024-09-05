local_edition(3)
library(declared)
library(vctrs)
x <- declared(
  c(1:5, -1),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_values = -1
)

xi <- declared(
  as.integer(c(1:5, -1)),
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_values = -1
)

xc <- declared(
  c(1, 2, "a", "b", "a", 1, 2, "b", "b"),
  na_values=c("a", "b")
)

xdate <- declared(
  c(as.Date("2023-12-06"), -1),
  labels = c(DK = -1),
  na_values = -1
)

xr <- declared(
  1:5,
  labels = c(Good = 1, Bad = 5, DK = -1),
  na_range = c(-9, -1)
)

df <- tibble::tibble(xc = xc, y = 1:9)


test_that("vctrs method works", {
  expect_equal(vctrs::vec_ptype_abbr(x), "dbl+lbl")

  expect_equal(vctrs::vec_ptype_full(x), "declared<double>")

  expect_equal(vctrs::vec_ptype_abbr(xi), "int+lbl")

  expect_equal(vctrs::vec_ptype_full(xi), "declared<integer>")

  expect_equal(
    vctrs::vec_restore(vctrs::vec_proxy(xc), to = xc),
    xc
  )

  expect_equal(
    vctrs::vec_restore(vctrs::vec_proxy(xdate), to = xdate),
    xdate
  )

  expect_equal(
    vctrs::vec_restore(vctrs::vec_proxy(xi), to = xi),
    xi
  )

  expect_equal(
    vctrs::vec_restore(vctrs::vec_proxy(xr), to = xr),
    xr
  )

  # expect_length(vec_ptype2(x, x), 0)
})

test_that("tests have the same output", {
  expect_snapshot(x)
  expect_snapshot(vctrs::vec_ptype_abbr(x))
  expect_snapshot(vctrs::vec_ptype_full(x))
  expect_snapshot(xi)
  expect_snapshot(vctrs::vec_ptype_abbr(xi))
  expect_snapshot(vctrs::vec_ptype_full(xi))
  expect_snapshot(xc)
  expect_snapshot(vctrs::vec_restore(vctrs::vec_proxy(xc), to = xc))
  expect_snapshot(df)
  expect_snapshot(df |> dplyr::count(xc))
  # expect_snapshot(
  #   df |>
  #   dplyr::group_by(xc) |>
  #   dplyr::summarize(n = n())
  # )
  # expect_snapshot(
  #   df |>
  #   dplyr::group_by(xc) |>
  #   dplyr::summarise(y_mean = mean(y), n = n()) |>
  #   dplyr::filter(is.na(xc))
  # )
  # expect_snapshot(
  #   df |>
  #   dplyr::group_by(xc) |>
  #   dplyr::mutate(y_mean = mean(y), n = n())
  # )
})
