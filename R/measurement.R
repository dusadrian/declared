#' @name measurement
#' @family labelling functions
#' @title Get / Set measurement levels for declared objects
#'
#' @description
#' Functions to extract information about the measurement levels of a variable
#' (if already present), or to specify such measurement levels.
#'
#' @details
#' This function creates an attribute called `"measurement"` to a declared
#' This object, as an optional feature, at this point for purely aesthetic
#' reasons. attribute might become useful in the future to (automatically)
#' determine if a declared object is suitable for a certain statistical
#' analysis, for instance regression requires quantitative variables, while some
#' declared objects are certainly categorical despite using numbers to denote
#' categories.
#'
#' It distinguishes between `"categorical"` and `"quantitative"` types of
#' variables, and additionally recognizes `"nominal"` and `"ordinal"` as
#' categorical, and similarly recognizes `"interval"`, `"ratio"`,
#' `"discrete"` and `"continuous"` as quantitative.
#'
#' The words `"qualitative"` is treated as a synonym for `"categorical"`,
#' and the words `"metric"` and `"numeric"` are treated as synonyms for
#' `"quantitative"`, respectively.
#'
#' @return
#' A character vector.
#'
#' @examples
#' x <- declared(
#'     c(-2, 1:5, -1),
#'     labels = c(Good = 1, Bad = 5, DK = -1),
#'     na_values = c(-1, -2),
#'     label = "Test variable"
#' )
#'
#' x
#'
#' measurement(x)
#'
#' # automatically recognized as categorical
#' measurement(x) <- "ordinal"
#'
#' measurement(x)
#'
#' # the same with
#' measurement(x) <- "categorical, ordinal"
#'
#' set.seed(1890)
#' x <- declared(
#'     sample(c(18:90, -91), 20, replace = TRUE),
#'     labels = c("No answer" = -91),
#'     na_values = -91,
#'     label = "Respondent's age"
#' )
#'
#' # automatically recognized as quantitative
#' measurement(x) <- "discrete"
#'
#' measurement(x)
#'
#' # the same with
#' measurement(x) <- "metric, discrete"
#'
#' @param x A declared vector.
#' @export
`measurement` <- function (x) {
    UseMethod ("measurement")
}


#' @export
`measurement.default` <- function (x) {
    # do nothing
    NULL
}


#' @export
`measurement.declared` <- function (x) {
    m <- attr (x, "measurement")
    if (is.null (m)) {
        m <- "Unspecified"
        l_m <- likely_measurement(x)
        if (!identical (l_m, "")) {
            m <- paste0 (m, ", but likely ", paste (l_m, collapse = " "))
        }
    }

    return (m)
}


#' @rdname measurement
#' @param value A single character string of measurement levels,
#' separated by commas.
#' @export
`measurement<-` <- function (x, value) {
  UseMethod ("measurement<-")
}


#' @export
`measurement<-.default` <- function (x, value) {
    # do nothing
    x
}


#' @export
`measurement<-.declared` <- function (x, value) {
    attr (x, "measurement") <- check_measurement (value)
    return (x)
}
