#' @rdname weighted
#'
#' @param trim A fraction (0 to 0.5) of observations to be trimmed from each end
#' of x before the mean is computed. Values of trim outside that range are
#' taken as the nearest endpoint.
#'
#' @param na.rm Logical, should the empty missing values be removed?
#'
#' @export
`w_mean` <- function (
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
        na_index <- attr (x, "na_index")
        if (!is.null (na_index)) {
            wt <- wt[-na_index]
            x <- x[-na_index]
        }
    }

    if (is.null (wt)) {
        return (mean (x, na.rm = na.rm))
    }

    if (
        !is.null (wt) && !(
            is.atomic (wt) && all (is.finite (na.omit (wt)))
        )
    ) {
        stopError_ ("'wt' should be an atomic vector with finite values.")
    }

    if (length (x) != length (wt)) {
        stopError_ ("Lengths of 'x' and 'wt' differ.")
    }

    ok <- !is.na (x + wt)

    if (na.rm) {
        x <- x[ok]
        wt <- wt[ok]
    }
    else if (any (!ok)) {
        return (NA_real_)
    }

    sumwt <- sum (wt)

    if (any (wt < 0) || sumwt == 0) {
        stopError_ ("'wt' must be non-negative and not all zero")
    }

    n <- length (x)

    if (trim > 0 & n) {
        if (is.complex (x)) {
            stopError_ ("Trimmed means are not defined for complex data")
        }

        if (trim >= 0.5) {
            return (w_median (x, wt = wt))
        }

        lo <- floor (n * trim) + 1
        hi <- n + 1 - lo

        # when this will be implemented in the base package:
        # ox <- sort.list (x, partial = unique (c (lo, hi)))
        lohi <- order (wt * x)[lo:hi]

        x <- x[lohi]
        wt <- wt[lohi]
        sumwt <- sum (wt)
    }

    return (sum (wt * x)/sumwt)
}
