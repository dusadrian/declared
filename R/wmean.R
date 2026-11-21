#' @rdname weighted
#' @order 2
#'
#' @param trim A fraction (0 to 0.5) of observations to be trimmed from each end
#' of x before the mean is computed. Values of trim outside that range are
#' taken as the nearest endpoint.
#'
#' @param na.rm Logical, should the empty missing values be removed?
#'
#' @export
`wmean` <- function (
    x, wt = NULL, trim = 0, na.rm = TRUE
) {

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

    if (is.null (wt)) {
        return (mean (x, trim = trim, na.rm = na.rm))
    }

    wt <- frequency_weights_ (x, wt)
    ok <- !is.na (x) & !is.na (wt)

    if (na.rm) {
        x <- x[ok]
        wt <- wt[ok]
    }
    else if (any (!ok)) {
        return (NA_real_)
    }

    sumwt <- sum (wt)

    if (sumwt == 0) {
        stopError_ ("'wt' must be non-negative and not all zero")
    }

    n <- length (x)

    if (trim > 0 & n) {
        if (is.complex (x)) {
            stopError_ ("Trimmed means are not defined for complex data")
        }

        if (trim >= 0.5) {
            return (wmedian (x, wt = wt))
        }

        lo <- floor (n * trim) + 1
        hi <- n + 1 - lo

        # when this will be implemented in the base package:
        # ox <- sort.list (x, partial = unique (c (lo, hi)))
        lohi <- order (x)[lo:hi]

        x <- x[lohi]
        wt <- wt[lohi]
    }

    return (weighted.mean (x, wt))
}

#' @rdname declared_internal
#' @keywords internal
#' @export
`w_mean` <- function (...) {
    wmean(...)
}
