#' @rdname weighted
#' @param method Character, specifying how the result is scaled, see 'Details' below.
#' @export
`w_var` <- function (
    x, wt = NULL, method = NULL, na.rm = TRUE
) {

    if (inherits(x, "haven_labelled")) {
        x <- as.declared(x)
    }

    if (!(is.atomic(x) && (is.numeric(x) || is.complex(x) || is.logical(x)))) {
        stopError_("'x' should be an atomic vector with finite values.")
    }

    if (inherits(x, "declared")) {
        na_index <- attr(x, "na_index")
        if (length(na_index)) {
            x <- x[-na_index]
            wt <- wt[-na_index] # if wt is NULL, the result is still NULL
        }
    }

    if (is.null(wt)) {
        return(var(x, na.rm = na.rm))
    }

    if (!(is.atomic(wt) && all(is.finite(na.omit(wt))))) {
        stopError_("'wt' should be an atomic vector with finite values.")
    }

    if (length(x) != length(wt)) {
        stopError_("Lengths of 'x' and 'wt' differ.")
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
        stopError_("'wt' must be non-negative and not all zero")
    }

    wmean <- sum(wt * x/sumwt)

    if (!is.null(method)) {
        if (!is.element(method, c("unbiased", "ML"))) {
            stopError_("Method should be either 'unbiased' or 'ML'.")
        }

        result <- sum((sqrt(wt / sumwt) * (x - wmean)) ^ 2)

        if (method == "unbiased") {
            return(result / (1 - sum((wt/sumwt)^2)))
        }

        return(result)
    }

    return(sum(wt * (x - wmean)^2)/(sumwt - 1))
}
