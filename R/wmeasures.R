#' @rdname weighted
#' @order 12
#' @param what A character vector or a named list of functions, identifying
#' what to compute. Available character values are `"n"`, `"NA"`, `"mode"`,
#' `"mean"`, `"median"`, `"range"`, `"var"`, `"sd"`, `"iqr"`, `"min"`, and
#' `"max"`.
#' @details
#' For `wmeasures()`, the `"n"` measure counts valid values, while `"NA"` counts
#' all missing values, both declared and empty.
#'
#' When `wmeasures()` is evaluated in the context of a data frame, such as with
#' `admisc::using()`, `.` can be used as a placeholder for all variables in the
#' current dataset.
#' @export
`wmeasures` <- function (
    x, what = c ("n", "mode", "mean", "median", "range", "var", "sd"),
    wt = NULL, na.rm = TRUE, ...
) {

    if (identical (substitute (x), quote (.))) {
        x <- currentDataset_ (parent.frame ())
    }

    what <- parseWhat_ (what)

    if (is.data.frame (x)) {
        result <- matrix (
            NA_real_, nrow = length (x), ncol = length (what),
            dimnames = list (names (x), names (what))
        )

        for (i in seq_along (x)) {
            result[i, ] <- calculateMeasures_ (
                x[[i]], what = what, wt = wt, na.rm = na.rm, ... = ...
            )
        }
        class (result) <- c ("wmeasures", class (result))
        return (result)
    }

    result <- calculateMeasures_ (
        x, what = what, wt = wt, na.rm = na.rm, ... = ...
    )

    class (result) <- c ("wmeasures", class (result))
    return (result)
}
