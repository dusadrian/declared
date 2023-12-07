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

    if (
        is.element("Date", class (x)) || isTRUE (attr (x, "date"))) {
        class (x) <- "Date"
        out <- as.character (x)
    }
    else {
        out <- format (unclass (x), digits = digits)
    }

    na_index <- attr (x, "na_index")

    if (!is.null (na_index)) {
        out[na_index] <- paste0 ("NA(", names (na_index), ")")
    }

    # format again to make sure all elements have same width
    return (format (out, justify = "right"))
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

    if (!is.null (type)) {
        return (paste0 ("<", type, ">"))
    }

    return (type)
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


`likely_measurement` <- function (x) {

    labels <- attr (x, "labels", exact = TRUE)
    na_values <- attr (x, "na_values")

    na_range <- attr (x, "na_range")
    if (!is.null(na_range)) {
        na_values <- sort(union(
            na_values,
            seq(na_range[1], na_range[2])
        ))
    }

    x <- undeclare (x, drop = TRUE)

    xnumeric <- possibleNumeric_ (x)
    uniquevals <- unique (x)

    if (length (labels) > 0) {

        # possibly a categorical variable
        # but even numeric variables can have labels (for missing values)
        # unique values excepting the missing values
        except_na <- setdiff (uniquevals, na_values)

        if (all (is.element (labels, na_values))) {
            if (xnumeric) {
                return ("quantitative")
            }
            else {
                # character, but cannot determine the measurement level
                return ("")
            }
        }

        return ("categorical")
    }

    if (xnumeric) {
        return ("quantitative")
    }

    return ("") # character, but cannot determine the measurement level
}


`all_missing_values` <- function (
    x, na_values = NULL, na_range = NULL, labels = NULL
) {

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

    if (is.numeric (x)) {
        if (!is.null (labels)) {
            x <- c (x, unname (unclass (labels)))
        }

        if (!is.null (na_range)) {
            na_range <- range (na_range)
            if (length (unique (na_range)) != 2) {
                stopError_ ("`na_range` must have two unique values.")
            }

            uniques <- sort (unique (x[x >= na_range[1] & x <= na_range[2]]))
            if (length (uniques) == 0) {
                uniques <- na_range
            }
            else {
                uniques <- sort (unique (c (uniques, na_range)))
            }

            misvals <- sort (unique (c (misvals, uniques)))
        }
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

    na_values <- attr (x, "na_values")
    attrx <- attributes (x)
    labels <- getElement(attrx, "labels")

    if (drop_na) {
        attrx$na_index <- NULL
        attrx$na_values <- NULL

        if (!is.null (labels)) {
            labels <- labels[!is.element (labels, na_values)]
            attrx$labels <- labels
        }
        attributes (x) <- NULL
    }
    else {
        x <- undeclare (x, drop = TRUE)
    }

    x <- c (x, unname (labels))
    x <- x[!duplicated (x)]
    xmis <- logical (length (x))

    na_values <- attrx$na_values
    na_range <- attrx$na_range


    if (!is.null (na_values)) {
        xmis <- xmis | is.element (x, na_values)
    }

    if (!is.null (na_range)) {
        xmis <- xmis | (x >= na_range[1] & x <= na_range[2])
    }


    xnotmis <- sort (x[!xmis])
    xmis <- sort (x[xmis])
    if (all (is.element (xmis, na_values))) {
        xmis <- na_values
    }

    if (isFALSE (observed) && is.numeric (labels)) {
        lnotmis <- length(xnotmis)
        labels_notmis <- labels[!is.element(labels, na_values)]
        if (labels_notmis[2] < 16) {
            # there should be an upper limit for tables of frequencies
            if (
                length (labels_notmis) == 2 &&
                xnotmis[1] == labels_notmis[1] &&
                xnotmis[lnotmis] == labels_notmis[2] &&
                lnotmis != (labels_notmis[2] + labels_notmis[1] == 0)
            ) {
                # Likert type response scale, with labels only for the
                # first and last values
                xnotmis <- seq(labels_notmis[1], labels_notmis[2])
            }
        }
    }

    wd <- which(duplicated(labels))
    if (length(wd) > 0) {
        labels <- labels[-wd]
    }

    if (length (xmis) > 0) {
        names (xmis) <- xmis
        for (i in seq (length (xmis))) {
            if (any (isel <- labels == xmis[i])) {
                names (xmis)[i] <- names (labels)[isel]
            }
        }
    }


    names (xnotmis) <- xnotmis
    if (length (xnotmis) > 0) {
        nms <- names (labels)
        for (i in seq (length (xnotmis))) {
            if (any (isel <- labels == xnotmis[i])) {
                names (xnotmis)[i] <- ifelse (
                    nzchar(nms[isel]), nms[isel], xnotmis[i]
                )
            }
        }
    }

    result <- c (xnotmis, xmis)
    attr (result, 'missing') <- unname (xmis)

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
    isna <- is.na (x)

    if (all (isna)) {
        if (each) {
            return (result)
        }
        return (FALSE)
    }

    if (is.logical (x)) {
        if (each) {
            result <- logical (length (x))
            result[isna] <- NA
            return (result)
        }
        return (FALSE)
    }

    if (inherits (x, "haven_labelled") || inherits (x, "declared")) {
        num <- Recall (unclass (x), each = each)

        labels <- attr (x, "labels", exact = TRUE)
        if (!is.null (labels) && !each && num) {
            return (Recall (labels))
        }

        return (num)
    }

    if (is.numeric (x)) {
        if (each) {
            result[!isna] <- TRUE
            return (result)
        }
        return (TRUE)
    }

    if (is.factor (x)) {
        x <- as.character (x)
    }

    x <- gsub ("\u00a0", " ", x) # multibyte space

    multibyte <- grepl ("[^!-~ ]", x)
    if (any (multibyte)) {
        isna[multibyte] <- TRUE
        result[multibyte] <- FALSE
        x[multibyte] <- NA
    }

    if (each) {
        x <- suppressWarnings (as.numeric (na.omit (x)))
        result[!isna] <- !is.na (x)
        return (result)
    }

    return (!any (is.na (suppressWarnings (as.numeric (na.omit (x))))))
}


`asNumeric_` <- function (x, levels = TRUE) {
    if (is.numeric (x)) {
        return (x)
    }

    if (is.factor (x)) {
        if (isTRUE (levels)) {
            return (suppressWarnings (as.numeric (levels (x)))[x])
        }
        return (as.numeric (x))
    }

    x <- gsub ("\u00a0", " ", x) # multibyte space

    result <- rep (NA, length (x))
    multibyte <- grepl ("[^!-~ ]", x)

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
        # each is certainly TRUE
        return (result)
    }

    x <- asNumeric_ (x)
    # some characters might be recoded to NA when coerced to numeric
    isnax <- is.na (x)

    result[!isna & isnax] <- FALSE
    isna <- isna | isnax
    x <- x[!isna]

    result[!isna] <- floor (x) == x

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


`unlockEnvironment_` <- function (env) {
     .Call ("_unlockEnvironment", env, PACKAGE = "declared")
}


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
`anyTagged_` <- function(x) {
    if (is.data.frame(x)) {
        i <- 1
        tagged <- FALSE
        while(!tagged & i <= ncol(x)) {
            tagged <- Recall(x[[i]])
            i <- i + 1
        }
        return(tagged)
    }

    if (is.double(x)) {
        return(.Call("_anyTagged", x, PACKAGE = "declared"))
    }

    return(FALSE)
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
