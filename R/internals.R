#' @title declared internal functions
#' @description Only to be used internally for developers and contributors.
#' @name declared_internal
NULL

#' @rdname declared_internal
#' @keywords internal
`format_declared` <- function(x, digits = getOption("digits")) {
  if (!is.atomic(x)) {
    stopError_("`x` has to be a vector.")
  }

  out <- format(unclass(x), digits = digits)
  na_index <- attr(x, "na_index")

  out[na_index] <- paste0("NA(", names(na_index), ")")

  # format again to make sure all elements have same width
  return(format(out, justify = "right"))
}

#' @rdname declared_internal
#' @keywords internal
`order_declared` <- function(
    x, na.last = NA, decreasing = FALSE, method = c("auto", "shell", "radix"),
    empty.last = na.last) {

  if (!is.declared(x)) {
    stopError_("`x` has to be a vector of class `declared`.")
  }

  if (!identical(empty.last, NA)) {
    if (!(isTRUE(empty.last) | isFALSE(empty.last))) {
      stopError_("Argument `empty.last` should be either TRUE or FALSE.")
    }
  }

  method <- match.arg(method)

  x_indexes <- seq_along(x)

  na_index <- attr(x, "na_index")
  na_declared <- logical(length(x))
  na_declared[na_index] <- TRUE
  na_empty <- is.empty(x)

  declared_indexes <- c()

  if (any(na_declared)) {
    x <- undeclare(x)
    nms <- names(na_index)
    if (possibleNumeric_(nms)) {
      nms <- asNumeric_(nms)
    }
    declared_indexes <- unname(na_index[order(nms, decreasing = decreasing, method = method)])
  }

  attributes(x) <- NULL
  x_indexes <- x_indexes[!(is.na(x) | na_declared)]
  x <- x[!(is.na(x) | na_declared)]

  res <- c()

  if (isFALSE(na.last)) {
    if (isFALSE(empty.last)) {
      res <- c(which(na_empty), declared_indexes)
    }

    if (isTRUE(empty.last)) {
      res <- c(declared_indexes, which(na_empty))
    }
  }


  res <- c(res, x_indexes[order(unclass(x), decreasing = decreasing, method = method)])


  if (isTRUE(na.last)) {
    if (isTRUE(empty.last)) {
      res <- c(res, declared_indexes, which(na_empty))
    }

    if (isFALSE(empty.last)) {
      res <- c(res, which(na_empty), declared_indexes)
    }
  }

  return(res)
}

#' @rdname declared_internal
#' @keywords internal
#' @export
`value_labels` <- function(...) {
  .Deprecated(msg = "Function value_labels() is deprecated, use labels()\n")
  labels(...)
}

#' @rdname declared_internal
#' @keywords internal
#' @export
`variable_label` <- function(...) {
  .Deprecated(msg = "Function variable_label() is deprecated, use label()\n")
  label(...)
}

`likely_type` <- function(x) {
    type <- NULL
    if (is.numeric(x)) {
        type <- "numeric"
        if (is.integer(x)) {
            type <- "integer"
        }
    }
    else if (is.character(x)) {
        type <- "character"
    }

    if (!is.null(type)) {
        return(paste0("<", type, ">"))
    }

    return(type)
}


`check_measurement` <- function(x) {

    if (is.null(x)) {
        return(x)
    }

    x <- trimstr_(tolower(unlist(strsplit(x, split = ","))))

    if (is.null(x)) {
        return(x)
    }

    mlevels <- c(
        "categorical", "nominal", "ordinal",
        "quantitative", "interval", "ratio", "discrete", "continuous"
    )

    if (any(x == "qualitative")) {
        x[x == "qualitative"] <- "categorical"
    }

    if (any(x == "metric")) {
        x[x == "metric"] <- "quantitative"
    }

    if (any(x == "numeric")) {
        x[x == "numeric"] <- "quantitative"
    }

    position <- pmatch(x, mlevels)

    if (any(is.na(position))) {
        stopError_("Unknown measurement level.")
    }

    position <- sort(position)

    first <- unique(ifelse(position < 4, 1, 4))

    if (length(first) > 1) {
        stopError_(
            "Measurement can not be categorical and quantitative at the same time."
        )
    }

    cpos <- setdiff(position, first)
    if (length(cpos) > 1) {

        if (first == 4) { # quantitative
            # check for combinations of interval / ratio and discrete / continuous
            if (any(cpos < 7)) {
                ir <- cpos[cpos < 7]
                if (length(ir) > 1) {
                    stopError_(
                        sprintf(
                            "Measurement can not be both %s and %s at the same time.",
                            mlevels[ir[1]],
                            mlevels[ir[2]]
                        )
                    )
                }
            }

            if (any(cpos > 6)) {
                dc <- cpos[cpos > 6]
                if (length(dc) > 1) {
                    stopError_(
                        sprintf(
                            "Measurement can not be both %s and %s at the same time.",
                            mlevels[dc[1]],
                            mlevels[dc[2]]
                        )
                    )
                }
            }
        }
        else {
            stopError_(
                sprintf(
                    "Measurement can not be both %s and %s at the same time.",
                    mlevels[position[1]],
                    mlevels[position[2]]
                )
            )
        }

        return(paste(mlevels[unique(sort(c(first, cpos)))], collapse = ", "))
    }
    else {
        if (cpos == first) {
            return(mlevels[first])
        }
        else {
            return(paste(c(mlevels[first], mlevels[cpos]), collapse = ", "))
        }
    }
}


`likely_measurement` <- function(x) {

    labels <- attr(x, "labels", exact = TRUE)
    na_values <- attr(x, "na_values")
    x <- undeclare(x, drop = TRUE)

    xnumeric <- possibleNumeric_(x)
    uniquevals <- unique(x)

    if (length(labels) > 0) {

        # possibly a categorical variable
        # but even numeric variables can have labels (for missing values)
        # unique values excepting the missing values
        except_na <- setdiff(uniquevals, na_values)

        if (all(is.element(labels, na_values))) {
            if (xnumeric) {
                return("quantitative")
            }
            else {
                return("") # character, but cannot determine the measurement level
            }
        }

        return("categorical")
    }

    if (xnumeric) {
        return("quantitative")
    }

    return("") # character, but cannot determine the measurement level
}


`all_missing_values` <- function(
    x, na_values = NULL, na_range = NULL, labels = NULL
) {

    ##########
    # Arguments na_values, na_range and labels can either be provided
    # by the user or, if the input is a haven_labelled(_spss) objects
    # they might already be in the attributes
    if (is.null(na_values)) {
        na_values <- attr(x, "na_values")
    }

    if (is.null(na_range)) {
        na_range <- attr(x, "na_range")
    }

    if (is.null(labels)) {
        labels <- attr(x, "labels", exact = TRUE)
    }
    ##########


    misvals <- c()

    if (is.null(na_values) & is.null(na_range)) {
        return(misvals)
    }

    if (!is.null(na_values)) {
        misvals <- sort(na_values)
    }

    if (is.numeric(x)) {
        if (!is.null(labels)) {
            x <- c(x, unname(unclass(labels)))
        }

        if (!is.null(na_range)) {
            uniques <- sort(unique(x[x >= na_range[1] & x <= na_range[2]]))
            if (length(uniques) == 0) {
                uniques <- na_range
            }
            else {
                uniques <- sort(unique(c(uniques, na_range)))
            }

            misvals <- sort(unique(c(misvals, uniques)))
        }
    }

    return(misvals)
}


`names_values` <- function(x, drop_na = FALSE) {

    if (!inherits(x, "declared") & !inherits(x, "haven_labelled_spss")) {
        stopError_("The input should be a declared / haven_labelled_spss vector.")
    }

    attrx <- attributes(x)

    if (drop_na) {
        attr(x, "na_index") <- NULL
        attr(x, "na_values") <- NULL
    }

    x <- undeclare(x, drop = TRUE)

    # attrx[["labels"]] is the equivalent of attr(x, "labels", exact = TRUE)
    labels <- attrx[["labels"]]
    x <- c(x, unname(labels))
    x <- x[!duplicated(x)]
    xmis <- logical(length(x))

    na_values <- attrx$na_values
    na_range <- attrx$na_range


    if (!is.null(na_values)) {
        xmis <- xmis | is.element(x, na_values)
    }

    if (!is.null(na_range)) {
        xmis <- xmis | (x >= na_range[1] & x <= na_range[2])
    }


    xnotmis <- sort(x[!xmis])
    xmis <- sort(x[xmis])

    if (length(xmis) > 0) {
        names(xmis) <- xmis
        for (i in seq(length(xmis))) {
            if (any(isel <- labels == xmis[i])) {
                names(xmis)[i] <- names(labels)[isel]
            }
        }
    }


    names(xnotmis) <- xnotmis
    if (length(xnotmis) > 0) {
        nms <- names(labels)
        for (i in seq(length(xnotmis))) {
            if (any(isel <- labels == xnotmis[i])) {
                # names(xnotmis)[i] <- ifelse(nms[isel] == "", xnotmis[i], nms[isel])
                names(xnotmis)[i] <- ifelse(nzchar(nms[isel]), nms[isel], xnotmis[i])
            }
        }
    }

    result <- c(xnotmis, xmis)
    attr(result, 'missing') <- unname(xmis)

    return(result)
}


# The following functions are copied from package admisc to achieve zero dependency.
`stopError_` <- function(message, enter = "\n") {

    message <- paste0(
        "Error: ",
        unlist(
            strsplit(message, split = "\\n")
        )
    )

    for (i in seq(length(message))) {
        message[i] <- gsub(
            "Error: ",
            ifelse(i > 1, "       ", ""),
            paste(
                strwrap(message[i], exdent = 7),
                collapse = "\n"
            )
        )
    }

    cat(enter)

    stop(
        simpleError(
            paste0(
                paste(message, collapse = "\n"),
                enter, enter
            )
        )
    )
}


`coerceMode_` <- function(x) {

    if (!is.atomic(x)) {
        stopError_("The input is not atomic.")
    }

    if (
        !is.numeric(x) &&
        (possibleNumeric_(x) || all(is.na(x)))
    ) {
        x <- asNumeric_(x)
    }

    if (
        !is.integer(x) &&
        wholeNumeric_(x) &&
        # some whole numbers might be too big to be represented in memory
        # as integers, in which case a warning will be captured
        # otherwise, if nothing is captured (the result is null) everything is ok
        is.null(tryCatchWEM_(as.integer(x)))
    ) {
        x <- as.integer(x)
    }

    return(x)
}


`possibleNumeric_` <- function(x, each = FALSE) {

    result <- rep(NA, length(x))
    isna <- is.na(x)

    if (all(isna)) {
        if (each) {
            return(result)
        }
        return(FALSE)
    }

    if (is.logical(x)) {
        if (each) {
            result <- logical(length(x))
            result[isna] <- NA
            return(result)
        }
        return(FALSE)
    }

    if (inherits(x, "haven_labelled") || inherits(x, "declared")) {
        num <- Recall(unclass(x), each = each)

        labels <- attr(x, "labels", exact = TRUE)
        if (!is.null(labels) && !each && num) {
            return(Recall(labels))
        }

        return(num)
    }

    if (is.numeric(x)) {
        if (each) {
            result[!isna] <- TRUE
            return(result)
        }
        return(TRUE)
    }

    if (is.factor(x)) {
        x <- as.character(x)
    }

    multibyte <- grepl("[^!-~ ]", x)
    if (any(multibyte)) {
        isna[multibyte] <- TRUE
        result[multibyte] <- FALSE
        x[multibyte] <- NA
    }

    if (each) {
        x <- suppressWarnings(as.numeric(na.omit(x)))
        result[!isna] <- !is.na(x)
        return(result)
    }

    return(!any(is.na(suppressWarnings(as.numeric(na.omit(x))))))
}


`asNumeric_` <- function(x, levels = TRUE) {
    if (is.numeric(x)) {
        return(x)
    }

    if (is.factor(x)) {
        if (isTRUE(levels)) {
            return(suppressWarnings(as.numeric(levels(x)))[x])
        }
        return(as.numeric(x))
    }

    result <- rep(NA, length(x))
    multibyte <- grepl("[^!-~ ]", x)

    attributes(x) <- NULL

    result[!multibyte] <- suppressWarnings(as.numeric(x[!multibyte]))

    return(result)
}


`wholeNumeric_` <- function(x, each = FALSE) {
    if (inherits(x, "haven_labelled") || inherits(x, "declared")) {
        return(Recall(unclass(x), each = each))
    }

    if (!possibleNumeric_(x) & !each) {
        return(FALSE)
    }

    result <- logical(length(x))
    isna <- is.na(x)
    result[isna] <- NA

    if (all(isna) || is.logical(x)) {
        if (each) {
            return(result)
        }
        return(FALSE)
    }

    x <- asNumeric_(x)
    # some characters might be recoded to NA when coerced to numeric
    isnax <- is.na(x)

    result[!isna & isnax] <- FALSE
    isna <- isna | isnax
    x <- x[!isna]

    result[!isna] <- floor(x) == x

    if (each) {
        return(result)
    }

    return(all(result[!isna]))
}


`tryCatchWEM_` <- function(expr, capture = FALSE) {
    toreturn <- list()
    output <- withVisible(withCallingHandlers(
        tryCatch(expr, error = function(e) {
            toreturn$error <<- e$message
            NULL
        }),
        warning = function(w) {
            toreturn$warning <<- c(toreturn$warning, w$message)
            invokeRestart("muffleWarning")
        },
        message = function(m) {
            toreturn$message <<- paste(toreturn$message, m$message, sep = "")
            invokeRestart("muffleMessage")
        }
    ))

    if (capture && output$visible && !is.null(output$value)) {
        toreturn$output <- capture.output(output$value)
        toreturn$value <- output$value
    }

    if (length(toreturn) > 0) {
        return(toreturn)
    }
}


`padLeft_` <- function(x, n) {
    paste(c(rep(" ", n), x), collapse = "", sep = "")
}


`padRight_` <- function(x, n) {
    paste(c(x, rep(" ", n)), collapse = "", sep = "")
}


`padBoth_` <- function(x, n) {
    n1 <- ceiling(n/2)
    n2 <- floor(n/2)
    paste(c(rep(" ", n1), x, rep(" ", n2)), collapse = "", sep = "")
}


`unlockEnvironment_` <- function(env) {
     .Call("_unlockEnvironment", env, PACKAGE = "declared")
}


`makeTag_` <- function(...) {
    x <- as.character(c(...))

    x <- .Call("_tag", x, PACKAGE = "declared")
    class(x) <- "double"

    return(x)
}


`hasTag_` <- function(x, tag = NULL) {
    if (!is.double(x)) {
        return(logical(length(x)))
    }

    if (!is.null(tag) && !is.atomic(tag) && (length(tag) > 1 || is.na(tag))) {
        stopError_("`tag` should be a vector of length 1.")
    }

    if (!is.null(tag)) {
        tag <- as.character(tag)
    }

    return(.Call("_hasTag_", x, tag, PACKAGE = "declared"))
}


`getTag_` <- function(x) {
    if (is.double(x)) {
        x <- .Call("_getTag_", x, PACKAGE = "declared")
        if (!any(is.na(suppressWarnings(as.numeric(na.omit(x)))))) {
            x <- as.numeric(x)
        }
        return(x)
    }
    else {
        # stopError_("Unsuitable input to extract a tagged value.")
        return(rep(NA, length(x)))
    }
}


`numdec_` <- function(x, each = FALSE, na.rm = TRUE, maxdec = 15) {

    pN <- possibleNumeric_(x, each = TRUE)

    # sum(pN), maybe each = TRUE and it's a vector
    if (sum(na.omit(pN)) == 0) {
        stopError_("'x' should contain at least one (possibly) numeric value.")
    }

    result <- rep(0, length(x))
    x <- asNumeric_(x)
    attributes(x) <- NULL
    result[is.na(x)] <- NA
    hasdec <- (x %% 1) - .Machine$double.eps^0.5 > 0

    if (any(hasdec, na.rm = TRUE)) {
        wdec <- which(hasdec)
        x[wdec] <- abs(x[wdec])
        x[wdec] <- trimstr_(formatC(x[wdec] - floor(x[wdec]), digits = maxdec))
        result[wdec] <- nchar(asNumeric_(gsub("^0.", "", x[wdec])))
    }

    if (each) {
        return(result)
    }

    return(max(result, na.rm = na.rm))
}


`trimstr_` <- function(x, what = " ", side = "both") {
    irv <- c(194, 160)
    multibyte_space <- rawToChar(as.raw(irv))

    if (is.element(what, c("*", "+"))) {
        what <- paste("\\", what, sep = "")
    }

    what <- ifelse(
        identical(what, " "),
        paste0("[[:space:]|", multibyte_space, "]"),
        what
    )

    pattern <- switch(side,
        both = paste("^", what, "+|", what, "+$", sep = ""),
        left = paste("^", what, "+", sep = ""),
        right = paste(what, "+$", sep = "")
    )

    gsub(pattern, "", x)
}
