#' @rdname missing_values
#'
#' @inherit missing_values examples
#'
#' @export
`missing_range` <- function (x) {
  UseMethod ("missing_range")
}


#' @export
`missing_range.default` <- function (x) {
  # return nothing
  NULL
}


#' @export
`missing_range.haven_labelled_spss` <- function (x) {
  attr (x, "na_range", exact = TRUE)
}


#' @export
`missing_range.declared` <- function (x) {
  attr (x, "na_range", exact = TRUE)
}


#' @export
`missing_range.data.frame` <- function (x) {
  lapply (x, missing_range)
}



#' @rdname missing_values
#'
#' @export
`missing_range<-` <- function (x, value) {
  UseMethod ("missing_range<-")
}


#' @export
`missing_range<-.default` <- function (x, value) {
  # do nothing
  x
}


#' @export
`missing_range<-.declared` <- function (x, value) {
  if (!is.null (value) && (length (value) != 2 || !is.numeric (value))) {
    stopError_ ("`value` should be a numeric vector of length 2.")
  }

  value <- sort (value)

  declared (undeclare (x),
           labels = attr (x, "labels", exact = TRUE),
           na_values = attr (x, "na_values", exact = TRUE),
           na_range = value,
           label = attr (x, "label", exact = TRUE)
  )
}
