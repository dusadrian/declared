#' @rdname weighted
#' @param measures A character vector or a named list of functions, identifying
#' which measures to compute.
#' @export
`wmeasures` <- function (
    x, measures = c ("n", "mean", "sd", "median", "var", "mode", "range"),
    wt = NULL, na.rm = TRUE, ...
) {

    measures <- parseMeasures_ (measures)

    if (is.data.frame (x)) {
        result <- do.call (rbind, lapply (
            x,
            function (y) {
                calculateMeasures_ (
                    y, measures = measures, wt = wt, na.rm = na.rm, ... = ...
                )
            }
        ))

        rownames (result) <- names (x)
        class (result) <- c ("wmeasures", class (result))
        return (result)
    }

    result <- calculateMeasures_ (
        x, measures = measures, wt = wt, na.rm = na.rm, ... = ...
    )

    class (result) <- c ("wmeasures", class (result))
    return (result)
}

`parseMeasures_` <- function (measures) {

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

    if (is.character (measures)) {
        key <- tolower (measures)
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

        measures <- builtin[unlist (choices[key], use.names = FALSE)]
    }
    else if (is.function (measures)) {
        measures <- list (measures)
    }

    if (!is.list (measures) || !all (vapply (measures, is.function, TRUE))) {
        stopError_ (
            "Argument `measures` should be a character vector or a list of functions."
        )
    }

    nms <- names (measures)

    if (is.null (nms)) {
        nms <- rep ("", length (measures))
    }

    missing_names <- !nzchar (nms)

    if (any (missing_names)) {
        nms[missing_names] <- paste0 ("Measure", which (missing_names))
    }

    names (measures) <- nms

    return (measures)
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
    x, measures, wt = NULL, na.rm = TRUE, ...
) {

    result <- vapply (
        measures,
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
