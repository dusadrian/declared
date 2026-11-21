#' @rdname weighted
#' @order 4
#' @details
#' The function `wmode()` returns the weighted mode of a variable. Unlike the
#' other functions where the prefix `w` signals a weighted version of the
#' base function with the same name, this has nothing to do with the base
#' function `mode()` which refers to the storage mode or type of an R object.
#' @export
`wmode` <- function (x, wt = NULL) {

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

    values <- sort (unique (x[!is.na (x)]))
    if (!length (values)) {
        return (NA_real_)
    }
    groups <- match (x, values)
    if (is.null (wt)) {
        counts <- tabulate (groups, length (values))
    }
    else {
        wt <- frequency_weights_ (x, wt)
        keep <- !is.na (groups) & !is.na (wt) & wt > 0
        if (!any (keep)) {
            return (NA_real_)
        }
        groups <- groups[keep]
        counts <- numeric (length (values))
        counts[unique (groups)] <- rowsum (wt[keep], groups, reorder = FALSE)
    }
    i <- which.max (counts)
    if (sum (counts == counts[i]) > 1) {
        message ("Multiple modes detected, only the first is returned.")
    }
    return (values[i])
}

#' @rdname declared_internal
#' @keywords internal
#' @export
`w_mode` <- function (...) {
    wmode(...)
}
