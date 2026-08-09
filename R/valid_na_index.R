#' Validate the declared missing values index
#'
#' Check whether every position stored in the `na_index` attribute identifies a
#' genuine missing value in the vector. This can detect when external code has
#' changed a vector without refreshing its declared missing values metadata.
#'
#' The check examines only the positions in `na_index` and stops at the first
#' invalid position. A missing or empty `na_index` attribute is valid.
#'
#' @param x An atomic vector, possibly containing an `na_index` attribute.
#' @return A logical value. `TRUE` when every indexed value is a genuine `NA`;
#'   otherwise `FALSE`.
#' @examples
#' x <- declared(c(1, -1, 3), na_values = -1)
#'
#' valid_na_index(x)
#'
#' attr(x, "na_index") <- 1
#' valid_na_index(x)
#' @export
`valid_na_index` <- function (x) {
  if (!is.atomic (x)) {
    return (FALSE)
  }

  index <- attr (x, "na_index")

  if (is.null (index)) {
    return (TRUE)
  }

  return (.Call (
    "_allIndexedNA", x, index, PACKAGE = "declared"
  ))
}


`sanitize_na_index_` <- function (x) {
  if (is.atomic (x) && !valid_na_index (x)) {
    # The positional map cannot be repaired selectively once it is stale.
    attr (x, "na_index") <- NULL
    attr (x, "na_values") <- NULL
    attr (x, "na_range") <- NULL
  }

  return (x)
}
