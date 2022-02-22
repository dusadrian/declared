`w_mean` <- function (
    x, wt = NULL, na.rm = TRUE
) {
    
    if (inherits(x, "haven_labelled")) {
        x <- as_declared(x)
    }
    
    if (!(is.atomic(x) && all(is.finite(x)))) {
        admisc::stopError("'x' should be an atomic vector with finite values.")
    }

    if (inherits(x, "declared")) {
        na_index <- attr(x, "na_index")
        if (!is.null(na_index)) {
            wt <- wt[-na_index]
            x <- x[-na_index]
        }
    }

    if (is.null(wt)) {
        return(mean(x, na.rm = na.rm))
    }
    
    if (!(is.atomic(wt) && all(is.finite(wt)))) {
        admisc::stopError("'wt' should be an atomic vector with finite values.")
    }

    if (length(x) != length(wt)) {
        admisc::stopError("Lengths of 'x' and 'wt' differ.")
    }
    

    ok <- !is.na(x + wt)
    
    if (na.rm) {
        x <- x[ok]
        wt <- wt[ok]
    }
    else if (any(!ok)) {
        return(NA)
    }

    sumwt <- sum(wt)

    if (any(wt < 0) || sumwt == 0) {
        admisc::stopError("'wt' must be non-negative and not all zero")
    }
    
    return(sum(wt * x/sumwt))
}
