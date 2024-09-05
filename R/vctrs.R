# Dynamically exported, see onLoad.R
# using eval(parse()) to avoid the huge dependency tree of vctrs, haven,
# labelled and pillar; these functions will be registered when or if the
# package vctrs is loaded

`vec_ptype_abbr.declared` <- function (x, ...) {
    command <- "vctrs::vec_ptype_abbr(vctrs::vec_data(unclass (undeclare (x))))"
    return (
        paste0 (eval (parse (text = command)), "+lbl")
    )
}

`vec_ptype_full.declared` <- function (x, ...) {
    command <- "vctrs::vec_ptype_full(vctrs::vec_data(unclass (undeclare (x))))"
    return (
        paste0 (
            "declared<",
            eval (parse (text = command)),
            ">"
        )
    )
}

`vec_proxy.declared` <- function (x, ...) {
    return (undeclare (x, drop = TRUE))
}

`vec_restore.declared` <- function(x, to, ...) {
    attrs <- attributes(to)
    todate <- isTRUE(attrs$date)

    misvals <- all_missing_values (
        x,
        attrs$na_values,
        attrs$na_range,
        attrs$labels
    )

    na_index <- which (is.element (x, misvals))

    if (length(na_index) > 0) {
        declared_nas <- x[na_index]

        if (todate) {
            declared_nas <- as.numeric (declared_nas)
        }

        x[na_index] <- NA
        names (na_index) <- declared_nas
    } else {
        na_index <- NULL
    }

    attrs$na_index <- na_index
    if (possibleNumeric_ (x)) {
        x <- as.numeric (x)
        if (inherits (to, "integer")) {
            x <- as.integer (x)
        }
    }
    attributes(x) <- attrs
    return (x)
}

# `vec_ptype2.declared` <- function (x, y, ...) {
#     command <- paste (
#         "vctrs::vec_ptype2(unclass (undeclare (x)),",
#         "vctrs::vec_data(unclass (undeclare (y))), ...)"
#     )
#     eval (parse (text = command))
# }
