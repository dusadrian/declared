#' @title declared internal functions
#' @description Functions to be used internally, only by developers and
#' contributors.
#' @name declared_internal
NULL

#' @rdname declared_internal
#' @keywords internal
#' @export
`format_declared` <- function (x, digits = getOption ("digits")) {
    if (is.null (x) || !is.atomic (x)) {
        stopError_ ("`x` has to be a vector.")
    }

    x <- sanitize_na_index_ (x)

    if (inherits (x, "Date") || isTRUE (attr (x, "date"))) {
        class (x) <- "Date"
        out <- as.character (x)
    } else if (is.numeric (x) && !is.null (attr (x, "decimals"))) {
        out <- format_decimals_ (unclass (x), attr (x, "decimals"))
    } else {
        out <- format (unclass (x), digits = digits)
    }

    na_index <- attr (x, "na_index")

    if (!is.null (na_index)) {
        out[na_index] <- paste0 ("NA(", names (na_index), ")")
    }

    # format again to make sure all elements have same width
    return (format (out, justify = "right"))
}

`format_decimals_` <- function (x, decimals) {
    decimals <- check_decimals (decimals)
    out <- formatC (x, digits = decimals, format = "f")
    if (decimals > 0) {
        out <- sub ("\\.?0+$", "", out)
    }
    out[out == "-0"] <- "0"
    return (out)
}

#' @rdname declared_internal
#' @keywords internal
#' @export
`order_declared` <- function (
    x, na.last = NA, decreasing = FALSE, method = c ("auto", "shell", "radix"),
    empty.last = na.last, ...) {

    if (!is.declared (x)) {
        stopError_ ("`x` has to be a vector of class `declared`.")
    }

    x <- sanitize_na_index_ (x)

    if (!identical (empty.last, NA)) {
        if (!(isTRUE (empty.last) | isFALSE (empty.last))) {
        stopError_ ("Argument `empty.last` should be either TRUE or FALSE.")
        }
    }

    method <- match.arg (method)

    x_indexes <- seq_along(x)

    na_index <- attr (x, "na_index")
    na_declared <- logical (length (x))
    na_declared[na_index] <- TRUE
    na_empty <- is.empty (x)

    declared_indexes <- c ()

    if (any (na_declared)) {
        x <- undeclare (x)
        nms <- names (na_index)
        if (possibleNumeric_ (nms)) {
        nms <- asNumeric_ (nms)
        }
        declared_indexes <- unname (
            na_index[order (nms, decreasing = decreasing, method = method)]
        )
    }

    attributes (x) <- NULL
    x_indexes <- x_indexes[!(is.na (x) | na_declared)]
    x <- x[!(is.na (x) | na_declared)]

    res <- c ()

    if (isFALSE (na.last)) {
        if (isFALSE (empty.last)) {
        res <- c (which (na_empty), declared_indexes)
        }

        if (isTRUE (empty.last)) {
        res <- c (declared_indexes, which (na_empty))
        }
    }


    res <- c (
        res,
        x_indexes[order (unclass (x), decreasing = decreasing, method = method)]
    )


    if (isTRUE (na.last)) {
        if (isTRUE (empty.last)) {
        res <- c (res, declared_indexes, which (na_empty))
        }

        if (isFALSE (empty.last)) {
        res <- c (res, which (na_empty), declared_indexes)
        }
    }

    return (res)
}

#' @rdname declared_internal
#' @keywords internal
#' @export
`value_labels` <- function (...) {
    .Deprecated(msg = "Function value_labels() is deprecated, use labels()\n")
    labels (...)
}

#' @rdname declared_internal
#' @keywords internal
#' @export
`variable_label` <- function (...) {
    .Deprecated(msg = "Function variable_label() is deprecated, use label()\n")
    label(...)
}


`likely_type` <- function (x) {
    type <- NULL
    others <- setdiff (class (x), "declared")

    if (length (others) > 0) {
        type <- others[1]
    }

    if (identical(type, "numeric")) {
        if (!anyTagged_ (x) && (is.integer (x) || wholeNumeric_ (x))) {
            type <- "integer"
        }
    }

    ## all declared objects have a second class
    # if (!is.null (type)) {
        return (paste0 ("<", type, ">"))
    # }

    # return (type)
}


`validate_declared` <- function (x = double(), labels = NULL, label = NULL,
                                na_values = NULL, na_range = NULL, ...) {

  if (
    !is.numeric (x) &&
    !is.character (x) &&
    !inherits (x, "Date") &&
    !all (is.na (x))
  ) {
    stopError_ ("`x` must be a numeric, character or a Date vector.")
  }

  if (!is.null (labels)) {
    if (is.null (names (labels))) {
      stopError_ ("`labels` must have names.")
    }

    if (any (duplicated (stats::na.omit (labels)))) {
      stopError_ ("Value `labels` must be unique.")
    }
  }

  if (
    !is.null (label) && !(
      is.atomic (label) && is.character (label) && length (label) == 1
    )
  ) {
    stopError_ ("`label` must be a character vector of length one.")
  }

  if (!is.null (na_values)) {
    if (any (is.na (na_values))) {
      stopError_ ("`na_values` should not contain NA values.")
    }
  }

  if (!is.null (na_range)) {
    type_ok <-  all (is.na (x)) ||
      (is.character (x) && is.character (na_range)) ||
      (is.numeric (x) && is.numeric (na_range))

    if (!type_ok || length (na_range) != 2 || anyDuplicated (na_range)) {
      stopError_ (
        "`na_range` must have two unique values of the same type as `x`."
      )
    }

    if (any (is.na (na_range))) {
      stopError_ ("`na_range` can not contain missing values.")
    }
  }
}

`check_measurement` <- function (x) {

    if (is.null (x)) {
        return (x)
    }

    x <- trimstr_ (tolower(unlist (strsplit (x, split = ","))))

    mlevels <- c (
        "categorical", "nominal", "ordinal",
        "quantitative", "interval", "ratio", "discrete", "continuous"
    )

    if (any (wx <- x == "qualitative")) {
        x[wx] <- "categorical"
    }

    if (any (wx <- x == "metric")) {
        x[wx] <- "quantitative"
    }

    if (any (wx <- x == "numeric")) {
        x[wx] <- "quantitative"
    }

    position <- pmatch (x, mlevels)

    if (any (is.na (position))) {
        stopError_ ("Unknown measurement level.")
    }

    position <- sort (position)

    first <- unique (ifelse (position < 4, 1, 4))

    if (length (first) > 1) {
        stopError_ (
            paste (
                "Measurement can not be categorical",
                "and quantitative at the same time."
            )
        )
    }
    cpos <- setdiff (position, first)

    if (length (cpos) > 1) {

        if (first == 4) { # quantitative
            # check for combinations of interval / ratio and
            # discrete / continuous
            if (any (cpos < 7)) {
                ir <- cpos[cpos < 7]
                if (length (ir) > 1) {
                stopError_ (sprintf (
                    "Measurement can not be both %s and %s at the same time.",
                    mlevels[ir[1]],
                    mlevels[ir[2]]
                ))
                }
            }

            if (any (cpos > 6)) {
                dc <- cpos[cpos > 6]
                if (length (dc) > 1) {
                stopError_ (sprintf (
                    "Measurement can not be both %s and %s at the same time.",
                    mlevels[dc[1]],
                    mlevels[dc[2]]
                ))
                }
            }
        }
        else {
            stopError_ (
                sprintf (
                    "Measurement can not be both %s and %s at the same time.",
                    mlevels[position[1]],
                    mlevels[position[2]]
                )
            )
        }

        return (
            paste (mlevels[unique (sort (c (first, cpos)))], collapse = ", ")
        )
    }
    else {
        return (paste (c (mlevels[first], mlevels[cpos]), collapse = ", "))
    }
}

`check_decimals` <- function (x) {
    if (is.null (x)) {
        return (x)
    }

    if (is.character (x) && possibleNumeric_ (x)) {
        x <- as.numeric (x)
    }

    if (
        !is.numeric (x) ||
        length (x) != 1 ||
        is.na (x) ||
        x < 0 ||
        x != floor (x)
    ) {
        stopError_ ("`decimals` should be a single non-negative integer.")
    }

    return (as.integer (x))
}


`likely_measurement` <- function (x) {

    if (inherits (x, "declared")) {
        x <- sanitize_na_index_ (x)
    }

    labels <- attr (x, "labels", exact = TRUE)
    na_values <- attr (x, "na_values", exact = TRUE)
    na_range <- attr (x, "na_range", exact = TRUE)

    x <- undeclare (x, drop = TRUE)
    numx <- possibleNumeric_ (x)

    if (length (labels) > 0) {

        # possibly a categorical variable
        # but even numeric variables can have labels (for missing values)
        # check unique labels except the missing values
        except_na <- setdiff (labels, na_values)
        numlabels <- possibleNumeric_ (labels)

        if (length (except_na) > 0 && !is.null (na_range)) {
            if (numlabels) {
                except_na <- asNumeric_ (except_na)
                except_na <- except_na[
                    except_na < na_range[1] | except_na > na_range[2]
                ]
            } else {
                # in principle this should not be possible, but just in case
                return ("") # character, so cannot determine the measurement level
            }
        }

        if (length (except_na) == 0) {
            if (numlabels & numx) {
                return ("quantitative")
            }
            else {
                # character, but cannot determine the measurement level
                return ("")
            }
        }

        return ("categorical")
    }

    if (numx) {
        return ("quantitative")
    }

    return ("") # character, but cannot determine the measurement level
}


`all_missing_values` <- function (
    x, na_values = NULL, na_range = NULL, labels = NULL
) {

    if (inherits (x, "declared")) {
        x <- sanitize_na_index_ (x)
    }

    ##########
    # Arguments na_values, na_range and labels can either be provided
    # by the user or, if the input is a haven_labelled(_spss) objects
    # they might already be in the attributes
    if (is.null (na_values)) {
        na_values <- attr (x, "na_values")
    }

    if (is.null (na_range)) {
        na_range <- attr (x, "na_range")
    }

    if (is.null (labels)) {
        labels <- attr (x, "labels", exact = TRUE)
    }
    ##########


    misvals <- c ()

    if (is.null (na_values) & is.null (na_range)) {
        return (misvals)
    }

    if (!is.null (na_values)) {
        misvals <- sort (na_values)
    }

    if (!is.null (na_range)) {
        if (is.declared (x)) x <- undeclare (x, drop = TRUE)
        x <- c (x, unname (unclass (labels)))
        na_range <- range (na_range)
        if (na_range[1] == na_range[2]) {
            stopError_ ("`na_range` must have two unique values.")
        }
        inside <- x >= na_range[1] & x <= na_range[2]
        misvals <- sort (unique (c (misvals, x[inside], na_range)))
    }

    return (misvals)
}


#' @rdname declared_internal
#' @export
`names_values` <- function (x, drop_na = FALSE, observed = TRUE) {

    if (!inherits (x, "declared") & !inherits (x, "haven_labelled_spss")) {
        stopError_ (
            "The input should be a declared / haven_labelled_spss vector."
        )
    }

    if (inherits (x, "declared")) {
        x <- sanitize_na_index_ (x)
    }

    attrx <- attributes (x)
    na_values <- getElement(attrx, "na_values")
    labels <- getElement(attrx, "labels")

    na_range <- attrx$na_range
    if (inherits (x, "declared")) {
        x <- undeclare (x, drop = TRUE)
    }
    else {
        attributes (x) <- NULL
    }

    x <- sort (unique (c (x, unname (labels))))
    missing <- is.element (x, na_values)
    if (length (na_range)) {
        missing <- missing | (x >= na_range[1] & x <= na_range[2])
    }
    xnotmis <- x[!missing]
    xmis <- x[missing]
    if (drop_na) {
        xmis <- NULL
    }
    else if (all (is.element (xmis, na_values))) {
        xmis <- na_values
    }

    if (isFALSE (observed) && is.numeric (labels) && is.numeric (xnotmis)) {
        endpoints <- sort (labels[is.element (labels, xnotmis)])
        # Fill a short integer response scale labelled only at its endpoints.
        if (length (endpoints) == 2 && all (is.finite (endpoints)) &&
            all (xnotmis == trunc (xnotmis)) && diff (endpoints) < 16 &&
            endpoints[2] < 16 && all (endpoints == range (xnotmis))) {
            xnotmis <- seq (endpoints[1], endpoints[2])
        }
    }

    names (xnotmis) <- xnotmis
    pos <- match (xnotmis, labels)
    named <- !is.na (pos) & nzchar (names (labels)[pos])
    names (xnotmis)[named] <- names (labels)[pos[named]]

    if (length (xmis)) {
        names (xmis) <- xmis
        pos <- match (xmis, labels)
        named <- !is.na (pos)
        names (xmis)[named] <- names (labels)[pos[named]]
    }

    result <- c (xnotmis, xmis)
    attr (result, 'missing') <- unname (xmis)

    return (result)
}

`combined_metadata_` <- function (dots) {
  na_values <- sort (unique (unlist (
    lapply (dots, function (x) attr (x, "na_values"))
  )))

  labels <- unlist (lapply (dots, function (x) {
    attr (x, "labels", exact = TRUE)
  }))

  duplicates <- duplicated (labels)
  if (any (duplicates) && any (
    names (labels)[duplicates] != names (labels)[match (labels[duplicates], labels)]
  )) {
    stopError_ ("Labels must be unique.")
  }
  labels <- sort (labels[!duplicates])
  ranges <- lapply (dots, attr, "na_range", exact = TRUE)

  decimals <- unique (unlist (lapply (dots, function (x) {
    attr (x, "decimals", exact = TRUE)
  })))
  if (length (decimals) == 0) {
    decimals <- NULL
  }
  else {
    decimals <- max (decimals)
  }

  measures <- unique (unlist (lapply (dots, attr, "measurement", exact = TRUE)))
  first <- which (vapply (dots, is.declared, TRUE))[1]
  return (list (
    labels = labels, na_values = na_values, na_range = combine_ranges_ (ranges),
    label = attr (dots[[first]], "label", exact = TRUE),
    measurement = if (length (measures) == 1) measures,
    decimals = decimals
  ))
}

`combine_ranges_` <- function (ranges) {
  ranges <- Filter (Negate (is.null), ranges)
  if (!length (ranges)) return (NULL)
  lower <- unname (unlist (lapply (ranges, min)))
  upper <- unname (unlist (lapply (ranges, max)))
  by_lower <- order (lower)
  end <- upper[by_lower[1]]
  for (i in by_lower[-1]) {
    if (lower[i] > end) {
      stopError_ ("Incompatible NA ranges.")
    }
    end <- max (end, upper[i])
  }
  return (c (lower[by_lower[1]], end))
}

`sanitize_na_index_` <- function (x) {
  if (is.atomic (x) && !valid_na_index (x)) {
    # The positional map cannot be repaired selectively once it is stale.
    attr (x, "na_index") <- NULL
    attr (x, "na_values") <- NULL
    attr (x, "na_range") <- NULL
  }

  return (x)
}

`table_factor_` <- function (x, categories) {
    # Group by the original codes, including codes with identical labels.
    codes <- match (undeclare (x, drop = TRUE), categories)
    return (structure (
        codes, levels = as.character (seq_along (categories)), class = "factor"
    ))
}

`frequency_weights_` <- function (x, wt) {
    if (!(is.numeric (wt) || is.logical (wt)) || any (is.infinite (wt))) {
        stopError_ ("'wt' should be an atomic vector with finite values.")
    }

    if (length (wt) != length (x)) {
        stopError_ ("Lengths of 'x' and 'wt' differ.")
    }

    if (any (wt < 0, na.rm = TRUE)) {
        stopError_ ("'wt' must be non-negative and not all zero")
    }

    return (as.double (wt))
}

`frequency_order_stats_` <- function (x, wt, ranks) {
    keep <- which (wt > 0)
    if (!length (keep)) {
        stopError_ ("'wt' must be non-negative and not all zero")
    }

    sorted <- keep[order (x[keep])]
    totals <- cumsum (wt[sorted])
    # A rank on a boundary belongs to the observation ending at that rank.
    i <- findInterval (ranks, totals, left.open = TRUE) + 1

    return (unname (x[sorted[pmin (i, length (sorted))]]))
}

`frequency_quantile_` <- function (
    x, wt, probs, type = 7L, names = TRUE, digits = 7,
    fuzz = if (type == 7L) 0 else 4 * .Machine$double.eps, ...
) {
    if (length (type) != 1L || !is.numeric (type) || !(type %in% 1:9)) {
        stopError_ ("'type' must be an integer in 1..9")
    }

    stopifnot (length (fuzz) == 1L, is.finite (fuzz), fuzz >= 0)
    total <- sum (wt)

    if (!is.finite (total)) stopError_ ("The sum of the weights must be finite.")

    # Use the same ranks and interpolation rules as stats::quantile().
    if (type <= 3) {
        index <- total * probs - if (type == 3) 0.5 else 0
        left <- floor (index * (1 + fuzz))
        fraction <- switch (as.integer (type),
            as.numeric (index > left),
            (1 + (index > left)) / 2,
            as.numeric (index != left | left %% 2 == 1)
        )
    }
    else {
        a <- c (0, 0.5, 0, 1, 1/3, 3/8)[type - 3]
        b <- if (type == 4) 1 else a
        index <- a + probs * (total - a + (1 - b))
        left <- floor (index * (1 + fuzz))
        fraction <- index - left
        fraction[abs (fraction) < fuzz] <- 0
    }

    right <- pmin (left + 1, total)
    both <- frequency_order_stats_ (x, wt, c (pmax (left, 1), pmax (right, 1)))
    i <- seq_along (probs)
    qs <- both[i]
    upper <- both[length (probs) + i]
    qs[fraction == 1] <- upper[fraction == 1]
    mix <- fraction > 0 & fraction < 1 & qs != upper
    qs[mix] <- (1 - fraction[mix]) * qs[mix] + fraction[mix] * upper[mix]

    if (length (qs) && all (is.finite (qs)) &&
        all (abs (qs) <= .Machine$integer.max & qs == trunc (qs))) {
        qs <- as.integer (qs)
    }

    if (names && length (qs)) {
        stopifnot (is.numeric (digits), digits >= 1)
        pct <- 100 * probs
        text <- if (length (pct) < 100) {
            formatC (pct, format = "fg", width = 1, digits = digits)
        }
        else {
            format (pct, trim = TRUE, digits = digits)
        }
        names (qs) <- paste0 (text, "%")
    }

    return (qs)
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
        n = function (x, wt = NULL, na.rm = TRUE, ...) sum (!is.na (x)),
        "NA" = function (x, wt = NULL, na.rm = TRUE, ...) sum (is.na (x)),
        mean = wmean,
        sd = wsd,
        median = wmedian,
        iqr = wIQR,
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
            na = "NA",
            mean = "mean",
            average = "mean",
            sd = "sd",
            stdev = "sd",
            median = "median",
            iqr = "iqr",
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

    if (!is.null (wt) && length (wt) != length (x)) {
        stopError_ ("Lengths of 'x' and 'wt' differ.")
    }

    x <- sanitize_na_index_ (x)
    na_index <- if (is.declared (x)) attr (x, "na_index")
    attributes (x) <- NULL
    if (length (na_index)) {
        x <- x[-na_index]
        wt <- wt[-na_index]
    }

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

`format_wmeasure_column_` <- function (x, digits = 3) {

    result <- rep ("", length (x))
    ok <- !is.na (x)

    if (!any (ok)) {
        return (result)
    }

    x <- round (as.numeric (x[ok]), digits)
    ndec <- numdec_ (x, each = TRUE, maxdec = digits)
    maxdec <- max (ndec, na.rm = TRUE)

    result[ok] <- sprintf (paste0 ("%.", maxdec, "f"), x)

    return (result)
}


# The following functions are copied from package admisc
# to achieve zero dependency.
`stopError_` <- function (message, enter = "\n") {

    message <- paste0 (
        "Error: ",
        unlist (
            strsplit (message, split = "\\n")
        )
    )

    for (i in seq (length (message))) {
        message[i] <- gsub (
            "Error: ",
            ifelse (i > 1, "       ", ""),
            paste (
                strwrap (message[i], exdent = 7),
                collapse = "\n"
            )
        )
    }

    cat (enter)

    stop (
        simpleError(
            paste0 (
                paste (message, collapse = "\n"),
                enter, enter
            )
        )
    )
}


`coerceMode_` <- function (x) {

    if (is.null (x) || !is.atomic (x)) {
        stopError_ ("The input is not atomic.")
    }

    if (
        !is.numeric (x) &&
        (possibleNumeric_ (x) || all (is.na (x)))
    ) {
        x <- asNumeric_ (x)
    }

    if (
        !is.integer (x) &&
        wholeNumeric_ (x) &&
        # some whole numbers might be too big to be represented in memory
        # as integers, in which case a warning will be captured
        # else, if nothing is captured (the result is null) everything is ok
        is.null (tryCatchWEM_ (as.integer (x)))
    ) {
        x <- as.integer (x)
    }

    return (x)
}


`possibleNumeric_` <- function (x, each = FALSE) {

    result <- rep (NA, length (x))
    nax <- is.na (x)

    if (all (nax)) {
        if (each) {
            return (result)
        }

        return (FALSE)
    }

    if (is.logical(x)) {
        if (each) {
            result <- logical (length (x))
            result[nax] <- NA
            return (result)
        }

        return (FALSE)
    }

    if (inherits (x, "haven_labelled") || inherits(x, "declared")) {
        num <- Recall (unclass (x), each = each)

        labels <- attr (x, "labels", exact = TRUE)
        if (!is.null (labels) && !each && num) {
            return (Recall (labels))
        }

        return (num)
    }

    if (is.numeric (x)) {
        if (each) {
            result[!nax] <- TRUE
            return (result)
        }

        return (TRUE)
    }

    if (is.factor (x)) {
        x <- as.character (x)
    }

    x <- gsub (
        "\u00a0", # multibyte space
        " ",
        gsub (
            "\u009d", # weird zero length character
            "",
            x
        )
    )
    multibyte <- grepl ("[^!-~ ]", x)

    if (any (multibyte)) {
        result[multibyte] <- FALSE
    }

    if (sum (nax) < length (x)) {
        eachx <- suppressWarnings (as.numeric (x[!nax & !multibyte]))
        result[!nax & !multibyte] <- !is.na(eachx)
    }

    if (each | length(x) == 1) {
        return (result)
    }

    return (all (result[!nax]))
}


`asNumeric_` <- function (x, levels = TRUE) {
    if (inherits (x, "declared")) {
        x <- sanitize_na_index_ (x)
        na_index <- attr (x, "na_index")
        attributes (x) <- NULL

        if (!is.null (na_index)) {
            # Non-numeric missing codes naturally become NA after coercion.
            x[na_index] <- suppressWarnings (as.numeric (names (na_index)))
        }

        return (Recall (x, levels = levels))
    }

    if (is.numeric (x)) {
        return (x)
    }

    if (is.factor (x)) {
        if (isTRUE (levels)) {
            return (suppressWarnings (as.numeric (levels (x)))[x])
        }
        return (as.numeric (x))
    }

    if (!all (is.na (x))) {
        x <- gsub ("\u00a0", " ", x) # multibyte space
    }

    result <- rep (NA, length (x))
    multibyte <- logical (length (x))
    if (!all (is.na (x))) {
        multibyte <- grepl ("[^!-~ ]", x)
    }

    attributes (x) <- NULL

    result[!multibyte] <- suppressWarnings (as.numeric (x[!multibyte]))

    return (result)
}


`wholeNumeric_` <- function (x, each = FALSE) {
    if (inherits (x, "haven_labelled") || inherits (x, "declared")) {
        return (Recall (unclass (x), each = each))
    }

    if (!possibleNumeric_ (x) & !each) {
        return (FALSE)
    }

    result <- logical (length (x))
    isna <- is.na (x)
    result[isna] <- NA

    if (all (isna) || is.logical (x)) {
        # each is certainly TRUE because if they are all missing or all logical
        # it would not be numeric, which means the only condition continuing the
        # function on line 636 is each = TRUE otherwise line 637 would stop
        return (result)
    }

    x <- asNumeric_ (x)
    # some characters might be recoded to NA when coerced to numeric
    isnax <- is.na (x)

    result[!isna & isnax] <- FALSE
    isna <- isna | isnax
    x <- x[!isna]

    result[!isna] <- abs (x - round (x)) < .Machine$double.eps^0.5

    if (each) {
        return (result)
    }

    return (all (result[!isna]))
}


`tryCatchWEM_` <- function (expr, capture = FALSE) {
    env <- new.env()
    env$toreturn <- list ()
    output <- withVisible (withCallingHandlers (
        tryCatch (expr, error = function (e) {
            env$toreturn$error <- e$message
            NULL
        }),
        warning = function (w) {
            env$toreturn$warning <- c (env$toreturn$warning, w$message)
            invokeRestart ("muffleWarning")
        },
        message = function (m) {
            env$toreturn$message <- paste (
                env$toreturn$message,
                m$message,
                sep = ""
            )
            invokeRestart ("muffleMessage")
        }
    ))

    if (capture && output$visible && !is.null (output$value)) {
        env$toreturn$output <- capture.output (output$value)
        env$toreturn$value <- output$value
    }

    if (length (env$toreturn) > 0) {
        return (env$toreturn)
    }
}


`padLeft_` <- function (x, n) {
    paste (c (rep (" ", n), x), collapse = "", sep = "")
}


`padRight_` <- function (x, n) {
    paste (c (x, rep (" ", n)), collapse = "", sep = "")
}


`padBoth_` <- function (x, n) {
    n1 <- ceiling(n/2)
    n2 <- floor (n/2)
    paste (c (rep (" ", n1), x, rep (" ", n2)), collapse = "", sep = "")
}


# `unlockEnvironment_` <- function (env) {
#     .Call ("_unlockEnvironment", env, PACKAGE = "declared")
# }


#' @rdname declared_internal
#' @keywords internal
#' @export
`makeTag_` <- function (...) {
    x <- as.character (c (...))

    x <- .Call ("_tag", x, PACKAGE = "declared")
    class (x) <- "double"

    return (x)
}


#' @rdname declared_internal
#' @keywords internal
#' @export
`hasTag_` <- function (x, tag = NULL) {
    if (!is.double (x)) {
        return (logical (length (x)))
    }

    if (
        !is.null (tag) && !(
            is.atomic (tag) && length (tag) == 1 && !is.na (tag)
        )
    ) {
        stopError_ ("`tag` should be a vector of length 1.")
    }

    if (!is.null (tag)) {
        tag <- as.character (tag)
    }

    return (.Call ("_hasTag", x, tag, PACKAGE = "declared"))
}


#' @rdname declared_internal
#' @keywords internal
#' @export
`getTag_` <- function (x) {
    if (is.double (x)) {
        x <- .Call ("_getTag", x, PACKAGE = "declared")
        if (!any (is.na (suppressWarnings (as.numeric (na.omit (x)))))) {
            x <- as.numeric (x)
        }
        return (x)
    }
    else {
        # stopError_ ("Unsuitable input to extract a tagged value.")
        return (rep (NA, length (x)))
    }
}


#' @rdname declared_internal
#' @keywords internal
#' @export
`anyTagged_` <- function (x) {
    if (is.data.frame (x)) {
        i <- 1
        tagged <- FALSE
        while (!tagged & i <= ncol (x)) {
            tagged <- Recall (x[[i]])
            i <- i + 1
        }
        return (tagged)
    }

    if (is.double (x)) {
        return (.Call ("_anyTagged", x, PACKAGE = "declared"))
    }

    return (FALSE)
}


`numdec_` <- function (x, each = FALSE, na.rm = TRUE, maxdec = 15) {

    maxdec <- min (15, maxdec)

    pN <- possibleNumeric_ (x, each = TRUE)

    # sum (pN), maybe each = TRUE and it's a vector
    if (sum (na.omit (pN)) == 0) {
        stopError_ ("'x' should contain at least one (possibly) numeric value.")
    }

    # asNumeric is important here because the (possible) number might arrive
    # as character through coercion, for instance c ("A", 1e-04)
    if (is.character (x)) {
        x <- asNumeric_ (x)
    }

    result <- rep (NA, length (x))
    wpN <- which (pN)

    x <- abs (x[wpN])
    x <- x - floor (x) # all numbers are now 0.something

    x <- sub ("0\\.", "",
        sub ("0+$", "", # erase trailing zeros
            format (x, scientific = FALSE, digits = max (7, maxdec))
        )
    )

    if (any (w9 <- grepl ("999999", x))) {
        # A floating point number like 234.1 might have been represented as
        # 0.0999999999999943 (after subtracting the floor)
        x[w9] <- sub (
            # last 0 becomes 1
            "0+", "1",
            # retains everthing <up to> the sequence
            sub ("(*)999999.*", "\\1", x[w9])
        )
    }

    if (any (w0 <- grepl ("000000", x))) {
        # ex. 0.00000000000000001, for all practical purposes this is equal to 0
        x[w0] <- sub ("(*)000000.*", "\\1", x[w0])
    }

    result[wpN] <- nchar (x)

    if (each) {
        return (pmin (result, maxdec))
    }

    return (min (maxdec, max (result, na.rm = na.rm)))
}


`trimstr_` <- function (x, what = " ", side = "both") {
    if (is.element (what, c ("*", "+"))) {
        what <- paste ("\\", what, sep = "")
    }

    what <- ifelse (
        identical (what, " "),
        paste0 ("[[:space:]|", "\u00a0", "]"), # plus the multibyte space
        what
    )

    pattern <- switch(side,
        both = paste ("^", what, "+|", what, "+$", sep = ""),
        left = paste ("^", what, "+", sep = ""),
        right = paste (what, "+$", sep = "")
    )

    gsub (pattern, "", x)
}


`getName_` <- function(x, object = FALSE) {
    result <- rep ("", length (x))
    x <- as.vector (gsub ("1-", "", gsub ("[[:space:]]", "", x)))

    condsplit <- unlist (strsplit (x, split = ""))

    startpos <- 0
    keycode <- ""

    if (any (condsplit == "]")) {
        startpos <- max (which (condsplit == "]"))
        keycode <- "]"
    }


    if (any (condsplit == "$")) {
        sp <- max (which (condsplit == "$"))
        if (sp > startpos) {
            startpos <- sp
            keycode <- "$"
        }
    }


    if (identical (keycode, "$")) {
        if (object) {
            return (substring (x, 1, min (which (condsplit == "$")) - 1))
        }

        # else
        result <- substring (x, startpos + 1)

    }
    else if (identical (keycode, "]")) {
        # ex. dd[,c("A","B")]
        # or dd[,c(A,B)] if the quotes have been removed before this function

        objname <- substring (x, 1, min (which (condsplit == "[")) - 1)

        if (object) {
            return (objname)
        }

        nms <- character(0)
        for (target in c ("names", "colnames")) {
        for (n in 1:2) {
            if (length (nms) == 0) {
                testnms <- tryCatchWEM_ (
                    nms <- eval.parent (
                        parse (
                            text = paste (target, "(", objname, ")", sep = "")
                        ),
                        n = n
                    )
                )
            }
        }
        }

        # else
        # keycode is "]"
        # this is a matrix or a list
        # determine where the indexing starts
        stindex <- max (which (condsplit == "["))

        stopindex <- ifelse (
            identical (condsplit[stindex - 1], "["),
            stindex - 2,
            stindex - 1
        )

        # ptn = possibly the name
        # ,c("A","B") or c(A, B)
        ptn <- gsub ("]", "", substr (x, stindex + 1, startpos))

        if (substring (ptn, 1, 1) == ",") {
            ptn <- substring (ptn, 2)
        }

        if (substring (ptn, 1, 2) == "c(") {
            # "A","B" or A,B
            ptn <- substring (ptn, 3, nchar(ptn) - 1)
        }

        ptn <- gsub ("'|\"|]|\ ", "", ptn)

        ptn <- unlist (strsplit (ptn, split = ","))
        if (length (ptn) == 1) {
            # try for something like [, 1:2]
            ptn <- unlist (strsplit (ptn, split = ":"))
        }

        # determine if what remains is a number or a name
        if (possibleNumeric_ (ptn)) {
            # it's a number (an index)
            # see if it has column names

            if (length (nms) > 0) {
                result <- nms[as.numeric (ptn)]
            }
        }
        else if (length (nms) > 0) {
            if (all (is.element (ptn, nms))) {
                return (ptn)
            }
        }
    }
    else {
        result <- x
    }

    return (gsub (",|'|\"|[[:space:]]", "", result))
}


check_date <- function (x) {
    xdate <- isTRUE (attr (x, "date"))
    attributes (x) <- NULL
    if (xdate) {
        x <- as.Date (x)
    }
    return (x)
}


`xtfrm_declared` <- function (x, decreasing = FALSE, na.last = TRUE, empty.last = na.last) {
    x <- sanitize_na_index_ (x)
    na_index <- attr (x, "na_index")
    na_declared <- logical (length (x))
    na_declared[na_index] <- TRUE
    na_empty <- is.empty (x)
    valid <- !is.na (x)

    z <- numeric (length (x))

    if (is.na (na.last)) {
        z[na_declared | na_empty] <- NA
        if (any (valid)) {
            val <- undeclare (x, drop = TRUE)
            r_v <- xtfrm (val)
            r_v <- r_v - min (r_v[valid]) + 1
            z[valid] <- r_v[valid]
        }
        return (z)
    }

    if (any (valid)) {
        val <- undeclare (x, drop = TRUE)
        r_v <- xtfrm (val)
        r_v <- r_v - min (r_v[valid]) + 1
        z[valid] <- r_v[valid]
    }

    if (any (na_declared)) {
        nms <- names (na_index)
        if (possibleNumeric_ (nms)) {
            nms <- asNumeric_ (nms)
        }
        r_m_vals <- xtfrm (nms)
        r_m_vals <- r_m_vals - min (r_m_vals) + 1
        z[na_index] <- r_m_vals
    }

    if (any (na_empty)) {
        z[na_empty] <- 1
    }

    if (isTRUE (na.last)) {
        if (isTRUE (empty.last)) {
            g_v <- 1
            g_m <- 2
            g_e <- 3
        }
        else {
            g_v <- 1
            g_e <- 2
            g_m <- 3
        }
    }
    else {
        if (isTRUE (empty.last)) {
            g_m <- 1
            g_e <- 2
            g_v <- 3
        }
        else {
            g_e <- 1
            g_m <- 2
            g_v <- 3
        }
    }

    shift <- 10 + length (x)

    if (isFALSE (decreasing)) {
        if (any (valid)) {
            z[valid] <- z[valid] + (g_v - 1) * shift
        }
        if (any (na_declared)) {
            z[na_declared] <- z[na_declared] + (g_m - 1) * shift
        }
        if (any (na_empty)) {
            z[na_empty] <- z[na_empty] + (g_e - 1) * shift
        }
    }
    else {
        if (any (valid)) {
            z[valid] <- z[valid] + (3 - g_v) * shift
        }
        if (any (na_declared)) {
            z[na_declared] <- z[na_declared] + (3 - g_m) * shift
        }
        if (any (na_empty)) {
            z[na_empty] <- z[na_empty] + (3 - g_e) * shift
        }
    }

    return (z)
}
