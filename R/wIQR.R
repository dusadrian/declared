#' @rdname weighted
#' @export
`wIQR` <- function (
    x, wt = NULL, na.rm = FALSE, ...
) {
    unname(unclass(diff (
        wquantile(
            x,
            wt = wt,
            probs = c(0.25, 0.75),
            na.rm = na.rm,
            names = FALSE,
            ... = ...
        )
    )))
}

#' @export
`w_IQR` <- function (...) {
    wIQR(...)
}
