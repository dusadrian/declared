#' @rdname weighted
#' @order 7
#' @param probs Numeric vector of probabilities with values in \[0,1\]
#' @details
#' The function `wquantile()` extensively borrowed ideas from packages
#' **`stats`** and **`Hmisc`**, to ensure a constant interpolation that would
#' produce the same quantiles if no weights are provided or if all weights are
#' equal to 1.
#'
#' Other arguments can be passed to the stats function `quantile()` via the
#' three dots `...` argument, and their extensive explanation is found in the
#' corresponding stats function's help page.
#' @export
`wquantile` <- function (
    x, wt = NULL, probs = seq (0, 1, 0.25), na.rm = TRUE, ...
) {

    if (inherits (x, "haven_labelled")) {
        x <- as.declared (x)
    }

    if (is.null (x) || !is.atomic (x) || !is.numeric (x)) {
        stopError_ ("'x' should be an atomic numerical vector.")
    }

    if (inherits (x, "declared")) {
        data <- drop_declared_missing_ (x, wt)
        x <- data$x
        wt <- data$wt
    }

    if (is.null (wt)) {
        qs <- quantile (x, probs = probs, na.rm = na.rm, ... = ...)
        class (qs) <- c ("fobject", class (qs))
        return (qs)
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

    if (any ((p.ok <- !is.na (probs)) & (probs < 0 | probs > 1))) {
        stopError_ ("probs outside [0,1]")
    }

    qs <- frequency_quantile_ (x, wt, probs[p.ok], ...)

    class (qs) <- c ("fobject", class (qs))
    return (qs)
}

#' @rdname declared_internal
#' @keywords internal
#' @export
`w_quantile` <- function (...) {
    wquantile(...)
}
