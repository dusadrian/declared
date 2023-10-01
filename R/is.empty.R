#' @title
#' Test the presence of empty (undeclared) missing values
#'
#' @description
#' Functions that indicate which elements are empty `NA` missing values, in
#' contrast to declared missing values.
#'
#' @details
#' All missing values, declared or undeclared, as stored as regular `NA`
#' values, therefore the base function `is_na()` does not differentiate
#' between them.
#'
#' These functions are specifically adapted to objects of class `"declared"`,
#' to return a truth value only for those elements that are completely missing
#' with no reason.
#'
#' @return
#' A logical vector.
#'
#' @examples
#'
#' x <- declared(
#'     c(1:2, -91),
#'     labels = c(Good = 1, Bad = 2, Missing = -91),
#'     na_values = -91
#' )
#'
#' x
#'
#' is.empty(x) # FALSE FALSE FALSE
#'
#' anyNAempty(x) # FALSE
#'
#' x <- c(x, NA)
#'
#' is.empty(x) # FALSE FALSE FALSE  TRUE
#'
#' anyNAempty(x) # TRUE
#'
#' @param x A vector
#'
#' @export
`is.empty` <- function (x) {
  if (is.null (x) || !is.atomic (x)) {
    stopError_ ("'x' should be an atomic vector.")
  }

  empty <- is.na (x)

  if (is.declared (x)) {
    na_index <- attr (x, "na_index")
    if (!is.null (na_index)) {
      empty[na_index] <- FALSE
    }
  }

  return (empty)
}

#' @rdname is.empty
#' @export
`anyNAempty` <- function (x) {
  return (anyNA (undeclare (x)))
}
