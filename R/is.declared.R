#' @rdname declared
#'
#' @export
`is.declared` <- function (x) {
  inherits (x, "declared")
}

#' @rdname declared
#' @export
`anyNAdeclared` <- function (x) {
  if (is.declared (x)) {
    if (length(attr(x, "na_index")) > 0) {
      return (TRUE)
    }
  }

  return (FALSE)
}
