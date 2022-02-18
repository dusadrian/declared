`w_var` <- function (
    x, wt = NULL, method = NULL, na.rm = TRUE
) {
    
    if (inherits(x, "haven_labelled")) {
        x <- as_declared(x)
    }
    
    if (!is.atomic(x) || !is.finite(x)) {
        admisc::stopError("'x' should be an atomic vector with finite values.")
    }
    
    if (!is.atomic(wt) || !all(is.finite(x))) {
        admisc::stopError("'wt' should be an atomic vector with finite values.")
    }

    if (inherits(x, "declared")) {
        na_index <- attr(x, "na_index")
        if (!is.null(na_index)) {
            wt <- wt[-na_index]
            x <- x[-na_index]
        }
    }

    if (is.null(wt)) {
        return(var(x, na.rm = na.rm))
    }

    if (length(x) != length(wt)) {
        admisc::stopError("Lengths of 'x' and 'wt' differ.")
    }

    sumwt <- sum(wt)

    if (any(wt < 0) || sumwt == 0) {
        admisc::stopError("'wt' must be non-negative and not all zero")
    }

    if (sumwt <= 1) {
        warning("The sum of all weights account to a single observed value.")
    }

    if (na.rm) {
        na <- is.na(x) | is.na(wt)
        x <- x[!na]
        wt <- wt[!na]
    }

    sumwt <- sum(wt)
    meanwt <- sum(x * wt/sumwt)
    
    if (!is.null(method)) {
        if (!is.element(method, c("unbiased", "ML"))) {
            admisc::stopError("Method should be either 'unbiased' or 'ML'.")
        }
        
        result <- sum((sqrt(wt / sumwt) * (x - meanwt)) ^ 2)
        
        if (method == "unbiased") {
            return(result / (1 - sum((wt/sumwt)^2)))
        }
        
        return(result)
    }

    return(sum(wt * ((x - meanwt)^2))/(sumwt - 1))
    
}
