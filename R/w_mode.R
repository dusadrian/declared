`w_mode` <- function(x, wt = NULL) {

    if (inherits(x, "haven_labelled")) {
        x <- as.declared(x)
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

    if (!(is.atomic(wt) && all(is.finite(na.omit(wt))))) {
        stopError_("'wt' should be an atomic vector with finite values.")
    }

    tbl <- w_table(x, wt = wt)
    wm <- which.max(tbl)
    
    fmode <- names(tbl[wm])
    if (possibleNumeric_(fmode)) {
        fmode <- asNumeric_(fmode)
    }

    if (length(tbl[tbl == max(tbl)]) > 1) {
        message("Multiple modes detected, only the first is returned.")
    }

    return(fmode)
}
