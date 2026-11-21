#' @rdname weighted
#' @order 10
#' @export
`wstandardize` <- function (x, wt = NULL, na.rm = TRUE) {

    if (inherits (x, "haven_labelled")) {
        x <- as.declared (x)
    }

    if (
        is.null (x) || !is.atomic (x) || !(
            is.numeric (x) || is.complex (x) || is.logical (x)
        )
    ) {
        warning ("'x' should be a numerical / logical vector: returning NA")
        return (NA_real_)
    }

    if (inherits (x, "declared")) {
        data <- drop_declared_missing_ (x, wt)
        x <- data$x
        wt <- data$wt
    }

    return (
        (x - wmean (x, wt = wt, na.rm = na.rm)) /
        wsd (x, wt = wt, na.rm = na.rm)
    )
}

#' @rdname declared_internal
#' @keywords internal
#' @export
`w_standardize` <- function (...) {
    wstandardize(...)
}
