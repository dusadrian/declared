`w_standardize` <- function(x, wt = NULL, na.rm = TRUE) {

    if (inherits(x, "haven_labelled")) {
        x <- as_declared(x)
    }

    if (!(is.atomic(x) && (is.numeric(x) || is.complex(x) || is.logical(x)))) {
        warning("'x' should be a numerical / logical vector: returning NA")
        return(NA_real_)
    }

    if (inherits(x, "declared")) {
        na_index <- attr(x, "na_index")
        if (length(na_index)) {
            x <- x[-na_index]
            wt <- wt[-na_index] # if wt is NULL, the result is still NULL
        }
        attributes(x) <- NULL
    }

    if (is.null(wt)) {
        wt <- rep(1, length(x))
    }

    if (!(is.atomic(wt) && all(is.finite(na.omit(wt))))) {
        admisc::stopError("'wt' should be an atomic vector with finite values.")
    }
    
    return(
        (x - w_mean(x, wt = wt, na.rm = na.rm)) / w_sd(x, wt = wt, na.rm = na.rm)
    )
}
