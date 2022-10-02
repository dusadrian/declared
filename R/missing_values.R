#' @title
#' Get / Declare missing values
#'
#' @description
#' Functions to extract information about the declared missing values, or to
#' declare such values if they are present in the data.
#'
#' @return
#'
#' \code{missing_values()} will return a vector of one or more values.
#'
#' \code{missing_range()} will return a numeric vector of length 2.
#'
#' @examples
#' x <- declared(c(-2, 1:5, -1),
#'     labels = c(Good = 1, Bad = 5, DK = -1, NotApplicable = -2),
#'     na_values = c(-1, -2)
#' )
#' x
#'
#' missing_values(x)
#'
#' missing_range(x) <- c(-10, -7)
#'
#' missing_range(x)
#'
#' @param x A vector.
#'
#' @param value Any vector of values that should be declared as missing
#' (for `missing_values`) or a numeric vector of length two giving the
#' (inclusive) extents of the range of missing values (for `missing_range`).
#'
#' @export
`missing_values` <- function (x) {
  UseMethod ("missing_values")
}


#' @export
`missing_values.default` <- function (x) {
  # return nothing
  NULL
}


#' @export
`missing_values.haven_labelled_spss` <- function (x) {
  attr (x, "na_values", exact = TRUE)
}


#' @export
`missing_values.declared` <- function (x) {
  attr (x, "na_values", exact = TRUE)
}


#' @export
`missing_values.data.frame` <- function (x) {
  lapply (x, missing_values)
}


#' @rdname missing_values
#'
#' @export
`missing_values<-` <- function (x, value) {
  UseMethod ("missing_values<-")
}


#' @export
`missing_values<-.default` <- function (x, value) {
  # do nothing
  x
}


#' @export
`missing_values<-.declared` <- function (x, value) {
  declared (undeclare (x),
           labels = attr (x, "labels", exact = TRUE),
           na_values = value,
           na_range = attr (x, "na_range", exact = TRUE),
           label = attr (x, "label", exact = TRUE)
  )
}
