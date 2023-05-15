#' @rdname weighted
#' @export
`w_IQR` <- function (
    x, wt = NULL, na.rm = FALSE, ...
) {
    unname(unclass(diff (
        w_quantile(
            x,
            wt = wt,
            probs = c(0.25, 0.75),
            na.rm = na.rm,
            names = FALSE,
            ... = ...
        )
    )))
}
