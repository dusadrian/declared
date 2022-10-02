
`missingValues` <- function (x) {
  # do nothing
  NULL
}


`missingValues<-` <- function (x, value) {

  xchar <- attr (x, "xchar")
  attr (x, "xchar") <- NULL

  class (x) <- setdiff (class (x), "declared")
  other_classes <- setdiff (
    class (x),
    c (
      "integer", "double", "character", "numeric", "complex",
      "haven_labelled", "haven_labelled_spss", "vctrs_vctr"
    )
  )

  notna <- !is.na (value)
  x[notna] <- NA

  if (!all (is.na (x)) & !xchar) {
    x <- coerceMode_ (x)
  }

  if (any (notna)) {
    na_index <- which (notna)
    names (na_index) <- value[notna]
    attr (x, "na_index") <- na_index
  }

  structure (x, class = c ("declared", other_classes, class (x)))
}
