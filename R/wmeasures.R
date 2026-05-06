#' @rdname weighted
#' @param what A character vector or a named list of functions, identifying
#' what to compute.
#' @details In an expression evaluated in the context of a data frame, such as
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
        result <- do.call (rbind, lapply (
            x,
            function (y) {
                calculateMeasures_ (
                    y, what = what, wt = wt, na.rm = na.rm, ... = ...
                )
            }
        ))

        rownames (result) <- names (x)
        class (result) <- c ("wmeasures", class (result))
        return (result)
    }

    result <- calculateMeasures_ (
        x, what = what, wt = wt, na.rm = na.rm, ... = ...
    )

    class (result) <- c ("wmeasures", class (result))
    return (result)
}

`currentDataset_` <- function (envir) {

    objects <- as.list (envir, all.names = FALSE)
    is_column <- vapply (
        objects,
        function (x) {
            !is.function (x) && !is.environment (x)
        },
        TRUE
    )

    objects <- objects[is_column]

    if (!length (objects)) {
        stopError_ ("Could not find a current dataset.")
    }

    lengths <- vapply (objects, NROW, integer (1))

    if (length (unique (lengths)) != 1 || lengths[1] == 0) {
        stopError_ ("Could not find a current dataset.")
    }

    return (as.data.frame (objects, optional = TRUE))
}

`parseWhat_` <- function (what) {

    builtin <- list (
        n = function (x, wt = NULL, na.rm = TRUE, ...) sum (!is.empty (x)),
        mean = wmean,
        sd = wsd,
        median = wmedian,
        var = wvar,
        mode = wmode,
        min = function (x, wt = NULL, na.rm = TRUE, ...) {
            x <- drop_declared_missing_ (x, wt = wt)
            min (x$x, na.rm = na.rm)
        },
        max = function (x, wt = NULL, na.rm = TRUE, ...) {
            x <- drop_declared_missing_ (x, wt = wt)
            max (x$x, na.rm = na.rm)
        }
    )

    if (is.character (what)) {
        key <- tolower (what)
        choices <- list (
            n = "n",
            mean = "mean",
            average = "mean",
            sd = "sd",
            stdev = "sd",
            median = "median",
            var = "var",
            variance = "var",
            mode = "mode",
            min = "min",
            max = "max",
            range = c ("min", "max")
        )

        if (!all (key %in% names (choices))) {
            stopError_ ("Unknown measure name.")
        }

        what <- builtin[unlist (choices[key], use.names = FALSE)]
    }
    else if (is.function (what)) {
        what <- list (what)
    }

    if (!is.list (what) || !all (vapply (what, is.function, TRUE))) {
        stopError_ (
            "Argument `what` should be a character vector or a list of functions."
        )
    }

    nms <- names (what)

    if (is.null (nms)) {
        nms <- rep ("", length (what))
    }

    missing_names <- !nzchar (nms)

    if (any (missing_names)) {
        nms[missing_names] <- paste0 ("Measure", which (missing_names))
    }

    names (what) <- nms

    return (what)
}

`drop_declared_missing_` <- function (x, wt = NULL) {

    if (inherits (x, "haven_labelled")) {
        x <- as.declared (x)
    }

    if (inherits (x, "declared")) {
        na_index <- attr (x, "na_index")
        if (length (na_index)) {
            x <- x[-na_index]
            wt <- wt[-na_index]
        }
    }

    attributes (x) <- NULL

    return (list (x = x, wt = wt))
}

`calculateMeasures_` <- function (
    x, what, wt = NULL, na.rm = TRUE, ...
) {

    result <- vapply (
        what,
        function (fun) {
            callMeasure_ (fun, x = x, wt = wt, na.rm = na.rm, ... = ...)
        },
        numeric (1)
    )

    return (result)
}

`callMeasure_` <- function (fun, ...) {

    arguments <- list (...)
    fmls <- names (formals (fun))

    if (!is.null (fmls) && !("..." %in% fmls)) {
        arguments <- arguments[names (arguments) %in% fmls]
    }

    return (do.call (fun, arguments))
}

#' @rdname weighted
#' @export
`w_measures` <- function (...) {
    wmeasures(...)
}
