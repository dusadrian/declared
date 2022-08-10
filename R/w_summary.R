#' @rdname weighted
#' @export
`w_summary` <- function(x, wt = NULL, ...) {

    wsum <- w_quantile(x, wt = wt, ... = ...)
    wmean <- w_mean(x, wt = wt, ... = ...)
    wsum <- c(wsum[1:3], wmean, wsum[4:5])

    if (inherits(x, "haven_labelled")) {
        x <- as.declared(x)
    }

    nas <- w_table(is.na(undeclare(x)), wt = wt)
    rnms <- rownames(nas)

    names(wsum) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")

    if (any(rnms == "TRUE")) {
        wsum <- c(wsum, "NA's" = nas$fre[2])
    }

    class(wsum) <- c("fobject", class(wsum))
    return(wsum)
}
