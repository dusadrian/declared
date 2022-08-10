#' @rdname declared
#'
#' @export
`is.declared` <- function(x) {
  inherits(x, "declared")
}
