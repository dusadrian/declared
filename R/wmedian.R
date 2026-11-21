#' @rdname weighted
#' @order 3
#' @param ... Further arguments passed to or from other methods.
#' @export
`wmedian` <- function (x, wt = NULL, na.rm = TRUE, ...)
{
    if (inherits (x, "haven_labelled")) {
        x <- as.declared (x)
    }

    if (inherits (x, "declared")) {
        data <- drop_declared_missing_ (x, wt)
        x <- data$x
        wt <- data$wt
    }

    if (is.null (wt)) {
        return (median (x, na.rm = na.rm, ... = ...))
    }

    x <- wquantile (
        x, wt = wt, probs = 0.5, na.rm = na.rm, ... = ...
    )

    attributes (x) <- NULL

    return (x)
}

#' @rdname declared_internal
#' @keywords internal
#' @export
`w_median` <- function (...) {
    wmedian(...)
}
