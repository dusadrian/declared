`w_median` <- function (x, wt = NULL, na.rm = TRUE, ...)
{
    metacall <- as.list(match.call())
    
    if (inherits(x, "haven_labelled")) {
        x <- as_declared(x)
    }

    if (inherits(x, "declared")) {
        na_index <- attr(x, "na_index")
        if (length(na_index)) {
            x <- x[-na_index]
            wt <- wt[-na_index] # if wt is NULL, the result is still NULL
        }
        attributes(x) <- NULL # if passed to w_quantile, to bypass this process
    }

    if (is.null(wt)) {
        return(median(x, na.rm = na.rm, ... = ...))
    }
    
    return(unname(
        w_quantile(
            x, wt = wt, probs = 0.5, na.rm = na.rm, ... = ...
        )
    ))
}
