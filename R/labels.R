#' @name labels
#' @family labelling functions
#' @title Get / Declare value labels
#'
#' @description
#' Functions to extract information about the declared variable / value labels,
#' or to declare such values if they are present in the data.
#'
#' @details
#' The function `labels()` is a adaptation of the base function to the objects
#' of class `declared`. In addition to the regular arguments, it has an
#' additional logical one called `prefixed`, to retrieve the value labels
#' prefixed with their values.
#'
#' @return
#' \code{labels()} will return a named vector.
#'
#' \code{label()} will return a single character string.
#'
#' @examples
#' x <- declared(
#'     c(-2, 1:5, -1),
#'     labels = c("Good" = 1, "Bad" = 5, "DK" = -1),
#'     na_values = c(-1, -2),
#'     label = "Test variable"
#' )
#' x
#'
#' labels(x)
#'
#' labels(x, prefixed = TRUE)
#'
#' labels(x) <- c("Good" = 1, "Bad" = 5, "DK" = -1, "Not applicable" = -2)
#'
#' label(x)
#'
#' label(x) <- "This is a proper label"
#'
#' x
#'
#' @param x Any vector of values that should be declared as missing
#' (for `labels`) or a numeric vector of length two giving the (inclusive)
#' extents of the range of missing values (for `label`).
#'
#' @export
label <- function (x) {
  UseMethod ("label")
}


#' @export
label.default <- function (x) {
  attr (x, "label", exact = TRUE)
}


#' @export
label.haven_labelled_spss <- function (x) {
  attr (x, "label", exact = TRUE)
}


#' @export
label.declared <- function (x) {
  attr (x, "label", exact = TRUE)
}


#' @export
label.data.frame <- function (x) {
  lapply (x, label)
}


#' @rdname labels
#' @param ... Other arguments, for internal use.
#' @param value The variable label, or a list of (named) variable labels
#' @export
`label<-` <- function (x, ..., value) {
  UseMethod ("label<-")
}


#' @export
`label<-.declared` <- function (x, ..., value) {
  if (!is.null (value) && length (value) > 1) {
    stopError_ ("`value` should be a single character string or NULL.")
  }

  if (is.null (value)) {
    attr (x, "label") <- NULL
  }
  else {
    attr (x, "label") <- as.character (value)
  }

  return (x)
}


#' @export
`label<-.haven_labelled_spss` <- function (x, ..., value) {
  if (!is.null (value) && length (value) > 1) {
    stopError_ ("`value` should be a single character string or NULL.")
  }

  if (is.null (value)) {
    attr (x, "label") <- NULL
  }
  else {
    attr (x, "label") <- as.character (value)
  }

  return (x)
}



# the following two functions are deliberately taken from
# package Hmisc to ensure functionality upon a namespace collision

#' @export
`label<-.default` <- function (x, ..., value) {
  if (is.list (value)) {
    stopError_ ("cannot assign a list to be a object label")
  }

  if (!is.null (value) && length (value) != 1L) {
    stopError_ ("value must be character vector of length 1")
  }

  attr (x, "label") <- value

  if (! inherits (x, "labelled")) {
    class (x) <- c ("labelled", class (x))
  }

  return (x)
}


#' @export
`label<-.data.frame` <- function (x, self = TRUE, ..., value) {
  if (is.list (value)) {
    self <- FALSE
  }

  if (self) {
    if (!is.null (value) && length (value) > 1) {
      stopError_ ("`value` should be a single character string or NULL.")
    }

    xc <- class (x)
    xx <- unclass (x)
    if (is.null (value)) {
      attr (xx, "label") <- NULL
    }
    else {
      attr (xx, "label") <- as.character (value)
    }

    class (xx) <- xc
    return (xx)
  }
  else {
    if (length (value) != length (x)) {
      stopError_ ("value must have the same length as x")
    }

    for (i in seq (along.with = x)) {
      label(x[[i]]) <- value[[i]]
    }
  }

  return (x)
}






#' @export
labels.declared <- function (object, prefixed = FALSE, ...) {
    labels <- attr (object, "labels", exact = TRUE)
    if (prefixed) {
        names (labels) <- paste0 ("[", labels, "] ", names (labels))
    }

    return (labels)
}


#' @export
labels.haven_labelled_spss <- function (object, prefixed = FALSE, ...) {
    labels <- attr (object, "labels", exact = TRUE)
    if (prefixed)
        names (labels) <- paste0 ("[", labels, "] ", names (labels))
    labels
}


#' @export
labels.data.frame <- function (object, prefixed = FALSE, ...) {
    lapply (object, labels, prefixed = prefixed)
}




#' @rdname labels
#' @export
`labels<-` <- function (x, value) {
  UseMethod ("labels<-")
}


#' @export
`labels<-.default` <- function (x, value) {
  # do nothing
  return (x)
}


#' @export
`labels<-.declared` <- function (x, value) {
    attr (x, "labels") <- value
    return (x)
}


#' @export
`labels<-.haven_labelled_spss` <- function (x, value) {
  attr (x, "labels") <- value
  return (x)
}
