#' @rdname weighted
#' @order 9
#' @details
#' For whole-number frequency weights, `wfivenum()` returns the minimum,
#' lower hinge, median, upper hinge and maximum of the repeated observations.
#' It locates these observations using cumulative weights. Zero-weight
#' observations are excluded.
#' @export
`wfivenum` <- function (
    x, wt = NULL, na.rm = FALSE
) {
    if (inherits (x, "haven_labelled")) {
        x <- as.declared (x)
    }

    if (
        is.null (x) || !(
            is.atomic (x) && is.numeric (x)
        )
    ) {
        stopError_ ("'x' should be an atomic numerical vector.")
    }

    if (inherits (x, "declared")) {
        data <- drop_declared_missing_ (x, wt)
        x <- data$x
        wt <- data$wt
    }

    if (is.null(wt)) {
        fvn <- fivenum (x, na.rm = na.rm)
        names (fvn) <- c ("Min", "Q1", "Q2", "Q3", "Max")
        class (fvn) <- c ("fobject", class (fvn))
        return (fvn)
    }

    wt <- frequency_weights_ (x, wt)
    ok <- !is.na (x) & !is.na (wt)

    if (na.rm) {
        x <- x[ok]
        wt <- wt[ok]
    }
    else if (any (!ok)) {
        stopError_ ("Missing values and NaN's not allowed if `na.rm' is FALSE")
    }

    sumwt <- sum (wt)

    if (sumwt == 0) {
        stopError_ ("'wt' must be non-negative and not all zero")
    }

    hinge <- floor ((sumwt + 3) / 2) / 2
    ranks <- c (1, hinge, (sumwt + 1) / 2, sumwt + 1 - hinge, sumwt)
    ends <- frequency_order_stats_ (x, wt, c (floor (ranks), ceiling (ranks)))
    fvn <- 0.5 * ends[1:5] + 0.5 * ends[6:10]
    fvn[c (1, 5)] <- range (x[wt > 0])
    names (fvn) <- c ("Min", "Q1", "Q2", "Q3", "Max")
    class (fvn) <- c ("fobject", class (fvn))
    return (fvn)
}

#' @rdname declared_internal
#' @keywords internal
#' @export
`w_fivenum` <- function (...) {
    wfivenum(...)
}
