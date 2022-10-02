#' @rdname weighted
#' @export
`w_sd` <- function (
    x, wt = NULL, method = NULL, na.rm = TRUE
) {
    return (sqrt (w_var (x = x, wt = wt, method = method, na.rm = na.rm)))
}
