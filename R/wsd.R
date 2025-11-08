#' @rdname weighted
#' @export
`wsd` <- function (
    x, wt = NULL, method = NULL, na.rm = TRUE
) {
    return (sqrt (wvar (x = x, wt = wt, method = method, na.rm = na.rm)))
}

#' @rdname declared_internal
#' @keywords internal
#' @export
`w_sd` <- function (...) {
    wsd(...)
}
