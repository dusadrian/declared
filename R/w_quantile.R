`w_quantile` <- function(
    x, wt = NULL, probs = seq(0, 1, 0.25), na.rm = TRUE, ...
) {

    metacall <- as.list(match.call())
    
    if (inherits(x, "haven_labelled")) {
        x <- as.declared(x)
    }
    
    if (!(is.atomic(x) && is.numeric(x))) {
        stopError_("'x' should be an atomic numerical vector.")
    }

    if (inherits(x, "declared")) {
        na_index <- attr(x, "na_index")
        if (length(na_index)) {
            x <- x[-na_index]
            wt <- wt[-na_index] # if wt is NULL, the result is still NULL
        }
    }

    if (is.null(wt)) {
        qs <- quantile(x, probs = probs, na.rm = na.rm, ... = ...)
        class(qs) <- c("w_quantile", class(qs))
        return(qs)
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
        stopError_("Missing values and NaN's not allowed if `na.rm' is FALSE")
    }
    
    if (any((p.ok <- !is.na(probs)) & (probs < 0 | probs > 1))) {
        stopError_("probs outside [0,1]")
    }

    if (na.p <- any(!p.ok)) {
        o.pr <- probs
        probs <- probs[p.ok]
        probs <- pmax(0, pmin(1, probs))
    }

    sumwt <- sum(wt)
    wt <- cumsum(tapply(wt, x, sum, na.rm = TRUE))
    x <- sort(unique(x))

    index <- 1 + (sumwt - 1) * probs

    left <- pmax(floor(index), 1)
    right <- pmin(left + 1, sumwt)

    index <- index%%1

    both <- approx(
        wt, x, xout = c(left, right),
        method = "constant", rule = 2, f = 1
    )$y
    lp <- length(probs)
    qs <- (1 - index) * both[seq(1, lp)] + index * both[-seq(1, lp)]

    qs <- coerceMode_(qs)

    names(qs) <- paste0(format(100 * probs, trim = TRUE), "%")

    class(qs) <- c("fobject", class(qs))
    return(qs)
}
