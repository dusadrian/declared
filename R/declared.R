#' @title Labelled vectors with declared missing values
#' @description
#' The labelled vectors are mainly used to analyse social science data,
#' and the missing values declaration is an important step in the analysis.
#' @details
#' The `declared` objects are very similar to the `haven_labelled_spss` objects
#' from package \bold{haven}. It has exactly the same arguments, but it features
#' a fundamental difference in the treatment of (declared) missing values.
#'
#' In package \bold{haven}, existing values are treated as if they were missing.
#' By contrast, in package \bold{declared} the NA values are treated as if they
#' were existing values.
#'
#' This difference is fundamental and points to an inconsistency in package
#' \bold{haven}: while existing values can be identified as missing using the
#' function \code{is.na()}, they are in fact present in the vector and other
#' packages (most importantly the base ones) do not know these values should be
#' treated as missing.
#'
#' Consequently, the existing values are interpreted as missing only by package
#' \bold{haven}. Statistical procedures will use those values as if they were
#' valid values.
#'
#' Package \bold{declared} approaches the problem in exactly the opposite way:
#' instead of treating existing values as missing, it treats (certain) NA values
#' as existing. It does that by storing an attribute containing the indices of
#' those NA values which are to be treated as declared missing values, and it
#' refreshes this attribute each time the declared object is changed.
#'
#' This is a trade off and has important implications when subsetting datasets:
#' all declared variables get this attribute refreshed, which consumes some time
#' depending on the number of variables in the data.
#'
#' The generic function `as.declared()` attempts to coerce only the compatible
#' types of objects, namely `haven_labelled` and `factor`s. Dedicated class
#' methods can be written for any other type of object, and users are free to
#' write their own. To end of with a declared object, additional metadata is
#' needed such as value labels, which values should be treated as missing etc.
#'
#' The measurement level is optional and, for the moment, purely aesthetic. It
#' might however be useful to (automatically) determine if a declared object is
#' suitable for a certain statistical analysis, for instance regression requires
#' quantitative variables, while some declared objects are certainly categorical
#' despite using numbers to denote categories.
#'
#' It distinguishes between `"categorial"` and `"quantitative"` types of
#' variables, and additionally recognizes `"nominal"` and `"ordinal"` as
#' categorical, and similarly recognizes `"interval"`, `"ratio"`,
#' `"discrete"` and `"continuous"` as quantitative.
#' @return `declared()` and `as.declared()` return labelled vector of class
#' "declared". When applied to a data frame, `as.declared()` will return a
#' corresponding data frame with declared variables. `is.declared()` and
#' `anyNAdeclared` return a logical value.
#' @examples
#'
#' x <- declared(
#'     c(1:5, -1),
#'     labels = c(Good = 1, Bad = 5, DK = -1),
#'     na_values = -1
#' )
#'
#' x
#'
#' is.na(x)
#'
#' x > 0
#'
#' x == -1
#'
#' # Values are actually placeholder for categories,
#' # so labels work as if they were factors:
#' x == "DK"
#'
#'
#' # when newly added values are already declared as missing,
#' # they are automatically coerced
#' c(x, 2, -1)
#'
#' # switch NAs with their original values
#' undeclare(x)
#'
#' as.character(x)
#'
#' # Returning values instead of categories
#' as.character(x, values = TRUE)
#' @param x A numeric vector to label, or a declared labelled vector
#' (for `undeclare`)
#' @param labels A named vector or `NULL`. The vector should be the same type
#' as `x`. Unlike factors, labels don't need to be exhaustive: only a fraction
#' of the values might be labelled
#' @param na_values A vector of values that should also be considered as missing
#' @param na_range A numeric vector of length two giving the (inclusive) extents
#' of the range. Use \code{-Inf} and \code{Inf} if you want the range to be
#' open ended
#' @param label A short, human-readable description of the vector
#' @param measurement Optional, user specified measurement level
#' @param llevels Logical, when `x` is a factor only use those levels that have
#' labels
#' @param ... Other arguments used by various other methods
#' @name declared
NULL


#' @rdname declared
#' @export
declared <- function (
    x, labels = NULL, na_values = NULL, na_range = NULL, label = NULL,
    measurement = NULL, llevels = FALSE, ...
) {
  UseMethod ("declared")
}


#' @export
declared.default <- function (
    x, labels = NULL, na_values = NULL, na_range = NULL, label = NULL,
    measurement = NULL, llevels = FALSE, ...
) {
  if (is.factor (x)) {
    nms <- levels (x)
    if (is.null (labels)) {
      labels <- seq (length (nms))
      names (labels) <- nms
    }

    if (isTRUE (llevels)) {
      labels <- labels[!possibleNumeric_ (names (labels), each = TRUE)]
    }

    wnms <- which (is.element (na_values, nms))

    if (length (wnms) > 0) {
      for (i in wnms) {
        na_values[i] <- which (nms == na_values[i])
      }
      if (possibleNumeric_ (na_values)) {
        na_values <- asNumeric_ (na_values)
      }
    }

    x <- as.numeric (x)
  }

  xchar <- FALSE

  if (is.null (labels)) {
    labels <- attr (x, "labels", exact = TRUE)
  }

  if (!is.null (labels)) {
    nms <- names (labels)
    if (possibleNumeric_ (labels) && (possibleNumeric_ (x) | all (is.na (x)))) {
      labels <- asNumeric_ (labels)
    }
    else {
      labels <- as.character (labels, values = TRUE)

      if (length(setdiff(labels, na_values)) > 0) {
        xchar <- !possibleNumeric_ (setdiff(labels, na_values))
      }

      if (xchar) {
        x <- as.character (x, values = TRUE)
        na_range <- NULL
      }
    }
    names (labels) <- nms

    # 2023.05.08 rationale, ex. ESS10 (integrated data file) has a variable
    # called region, which has multiple duplicated values and labels:
    # deprecated codes
    #        "DEP"
    # this is a mistake of course, but it nevertheless prevents subsetting,
    # because of validate_declared()
    labels <- labels[!duplicated(labels)]
  }

  if (!is.null (na_values)) {
    if (possibleNumeric_ (na_values) & !xchar) {
      na_values <- asNumeric_ (na_values)
    }
    else {
      na_values <- as.character (na_values, values = TRUE)
    }
  }

  if ((possibleNumeric_ (x) | all (is.na (x))) & !xchar) {
    x <- asNumeric_ (x)
  }
  else {
    x <- as.character (x, values = TRUE)
  }

  attributes (x) <- NULL

  validate_declared (x, labels, label, na_values, na_range)

  na_range <- sort (na_range)
  misvals <- all_missing_values (x, na_values, na_range, labels)

  attr (x, "xchar") <- xchar
  missingValues (x)[is.element (x, misvals)] <- x[is.element (x, misvals)]

  attr (x, "na_values") <- na_values
  attr (x, "na_range") <- na_range
  attr (x, "labels") <- labels
  attr (x, "label") <- label

  attr (x, "measurement") <- check_measurement (measurement)

  return (x)
}
