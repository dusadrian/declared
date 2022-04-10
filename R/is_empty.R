`is_empty` <- function(x) {
    if (!is.atomic(x)) {
        stopError_("'x' should be an atomic vector.")
    }

    empty <- is.na(x)

    if (is.declared(x)) {
        na_index <- attr(x, "na_index")
        if (!is.null(na_index)) {
            empty[na_index] <- FALSE
        }
    }

    return(empty)
}

is.empty <- function(x) {
    is_empty(x)
}
