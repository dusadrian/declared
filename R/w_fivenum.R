#' @rdname weighted
#' @export
`w_fivenum` <- function (
    x, wt = NULL, na.rm = FALSE
) {
    metacall <- as.list (match.call ())

    if (inherits (x, "haven_labelled")) {
        x <- as.declared (x)
    }

    if (!(is.atomic (x) && is.numeric (x))) {
        stopError_ ("'x' should be an atomic numerical vector.")
    }

    if (inherits (x, "declared")) {
        na_index <- attr (x, "na_index")
        if (length (na_index)) {
            x <- x[-na_index]
            wt <- wt[-na_index] # if wt is NULL, the result is still NULL
        }
        attributes (x) <- NULL
    }

    if (is.null(wt)) {
        fvn <- fivenum (x, na.rm = na.rm)
        names (fvn) <- c ("Min", "Q1", "Q2", "Q3", "Max")
        class (fvn) <- c ("fobject", class (fvn))
        return (fvn)
    }

    if (!(is.atomic (wt) && all (is.finite (na.omit (wt))))) {
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
        stopError_ ("Missing values and NaN's not allowed if `na.rm' is FALSE")
    }

    sumwt <- sum (wt)

    if (any (wt < 0) || sumwt == 0) {
        stopError_ ("'wt' must be non-negative and not all zero")
    }

    fvn <- fivenum (x * wt, na.rm = na.rm)
    names (fvn) <- c ("Min", "Q1", "Q2", "Q3", "Max")
    class (fvn) <- c ("fobject", class (fvn))
    return (fvn)
}
