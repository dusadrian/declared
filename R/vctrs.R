# Dynamically exported, see onLoad.R
# using eval (parse ()) to avoid the huge dependency tree of vctrs, haven,
# labelled and pillar  these functions will be registered when or if the package
# vctrs is loaded

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

# `vec_ptype2.declared` <- function (x, y, ...) {
#     command <- paste (
#         "vctrs::vec_ptype2(unclass (undeclare (x)),",
#         "vctrs::vec_data(unclass (undeclare (y))), ...)"
#     )
#     eval (parse (text = command))
# }
