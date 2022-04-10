# the following functions are copied from package admisc
# to eliminate all dependencies

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

`asNumeric_` <- function(x) {
    if (is.numeric(x)) {
        return(x)
    }

    if (is.factor(x)) {
        return(suppressWarnings(as.numeric(levels(x)))[x])
    }

    result <- rep(NA, length(x))
    multibyte <- grepl("[^!-~ ]", x)

    if (inherits(x, "haven_labelled")) {
        attributes(x) <- NULL
    }
    
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
    if (is.element(what, c("*", "+"))) what <- paste("\\", what, sep = "")
    what <- ifelse(what == " ", "[[:space:]]", what)
    pattern <- switch(side,
    both = paste("^", what, "+|", what, "+$", sep = ""),
    left = paste("^", what, "+", sep = ""),
    right = paste(what, "+$", sep = "")
    )
    gsub(pattern, "", x)
}
