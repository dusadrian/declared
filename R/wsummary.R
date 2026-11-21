#' @rdname weighted
#' @order 11
#' @export
`wsummary` <- function (x, wt = NULL, ...) {

    wsum <- wquantile (x, wt = wt, ... = ...)
    wmean <- wmean (x, wt = wt, ... = ...)
    wsum <- c (wsum[1:3], wmean, wsum[4:5])

    if (inherits (x, "haven_labelled")) {
        x <- as.declared (x)
    }

    empty <- is.empty (x)
    names (wsum) <- c ("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
    if (any (empty)) {
        count <- if (is.null (wt)) sum (empty) else round (sum (wt[empty], na.rm = TRUE))
        if (count > 0) wsum <- c (wsum, "NA's" = count)
    }

    class (wsum) <- c ("fobject", class (wsum))
    return (wsum)
}

#' @rdname declared_internal
#' @keywords internal
#' @export
`w_summary` <- function (...) {
    wsummary(...)
}
