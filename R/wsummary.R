#' @rdname weighted
#' @export
`wsummary` <- function (x, wt = NULL, ...) {

    wsum <- wquantile (x, wt = wt, ... = ...)
    wmean <- wmean (x, wt = wt, ... = ...)
    wsum <- c (wsum[1:3], wmean, wsum[4:5])

    if (inherits (x, "haven_labelled")) {
        x <- as.declared (x)
    }

    nas <- wtable (is.empty (x), wt = wt)
    rnms <- rownames (nas)

    names (wsum) <- c ("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")

    if (any (rnms == "TRUE")) {
        wsum <- c (wsum, "NA's" = attr (nas, "toprint")$fre[2])
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
