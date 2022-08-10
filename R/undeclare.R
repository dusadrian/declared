#' @rdname declared
#'
#' @param drop Logical, drop all attributes
#'
#' @export
`undeclare` <- function(x, drop = FALSE, ...) {
  UseMethod("undeclare")
}


#' @export
`undeclare.default` <- function(x, drop = FALSE, ...) {
  if (isTRUE(drop)) {
    attributes(x) <- NULL
  }

  return(x)
}


#' @export
`undeclare.declared` <- function(x, drop = FALSE, ...) {
  na_index <- attr(x, "na_index")
  attrx <- attributes(x)

  # this is necessary to replace those values
  # (because of the "[<-.declared" method)
  attributes(x) <- NULL # or x <- unclass(x), but I find this cleaner
  if (!is.null(na_index)) {
    # x <- ifelse(!is.na(missingValues), missingValues, x)
    x[na_index] <- names(na_index)
  }

  x <- coerceMode_(x)

  attrx$na_index <- NULL
  attrx$na_values <- NULL
  attrx$na_range <- NULL

  if (isFALSE(drop)) {
    attributes(x) <- attrx
  }
  return(x)
}


#' @export
`undeclare.data.frame` <- function(x, drop = FALSE, ...) {
  declared <- vapply(x, is.declared, logical(1))
  x[declared] <- lapply(x[declared], undeclare, drop = drop)

  return(x)
}
