#' @rdname weighted
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
        na_index <- attr (x, "na_index")
        if (length (na_index)) {
            x <- x[-na_index]
            wt <- wt[-na_index] # if wt is NULL, the result is still NULL
        }
        attributes (x) <- NULL
    }

    if (is.null (wt)) {
        wt <- rep (1, length (x))
    }

    if (
        !is.null (wt) && !(
            is.atomic (wt) && all (is.finite (na.omit (wt)))
        )
    ) {
        stopError_ ("'wt' should be an atomic vector with finite values.")
    }

    return (
        (x - wmean (x, wt = wt, na.rm = na.rm)) /
        wsd (x, wt = wt, na.rm = na.rm)
    )
}

#' @export
`w_standardize` <- function (...) {
    wstandardize(...)
}
