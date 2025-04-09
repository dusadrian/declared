#' @rdname weighted
#' @export
`wsd` <- function (
    x, wt = NULL, method = NULL, na.rm = TRUE
) {
    return (sqrt (wvar (x = x, wt = wt, method = method, na.rm = na.rm)))
}

#' @export
`w_sd` <- function (...) {
    wsd(...)
}
