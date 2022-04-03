# internal
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


`is_declared` <- function(x) {
    inherits(x, "declared")
}


`is.declared` <- function(x) {
    inherits(x, "declared")
}


`as_declared` <- function(x, ...) {
    UseMethod("as_declared")
}


`as.declared` <- function(x, ...) {
    UseMethod("as_declared")
}


`as_declared.default` <- function(x, ...) {
    return(declared(x))
}


`as_declared.haven_labelled` <- function(x, ...) {

    dots <- list(...)

    na_values <- attr(x, "na_values")
    na_range <- attr(x, "na_range")
    labels <- attr(x, "labels", exact = TRUE)
    label <- attr(x, "label", exact = TRUE)
    format_spss <- attr(x, "format.spss") # necessary for DDIwR::convert

    if (!inherits(x, "haven_labelled_spss")) {
        tagged <- admisc::hasTag(x)
        attributes(x) <- NULL

        if (any(tagged)) {
            x[tagged] <- admisc::getTag(x[tagged])
        }

        if (!is.null(labels)) {
            nms <- names(labels)
            tagged <- admisc::hasTag(labels)

            if (any(tagged)) {
                labels[tagged] <- admisc::getTag(labels[tagged])
                na_values <- sort(unname(labels[tagged]))
            }

            labels <- admisc::coerceMode(labels)
            names(labels) <- nms
        }

        misvals <- na_values
    }
    else {
        misvals <- all_missing_values(unclass(x))
    }

    attributes(x) <- NULL
    missingValues(x)[is.element(x, misvals)] <- x[is.element(x, misvals)]

    attr(x, "na_values") <- na_values
    attr(x, "na_range") <- na_range
    attr(x, "labels") <- labels
    attr(x, "label") <- label
    attr(x, "format.spss") <- format_spss

    return(x)
}


`as_declared.factor` <- function(x, ...) {
    return(declared(x, ... = ...))
}


`as_declared.data.frame` <- function(x, ...) {
    x[] <- lapply(x, as_declared, ...)
    class(x) <- "data.frame"
    return(x)
}


`undeclare` <- function(x, drop = FALSE, ...) {
    UseMethod("undeclare")
}


`undeclare.default` <- function(x, drop = FALSE, ...) {
    if (isTRUE(drop)) {
        attributes(x) <- NULL
    }

    return(x)
}


`undeclare.declared` <- function(x, drop = FALSE, ...) {
    na_index <- attr(x, "na_index")
    attrx <- attributes(x)
    
    # this is necessary to replace those values
    # (because of the "[<-.declared" method)
    attributes(x) <- NULL # or x <- unclass(x), but I find this cleaner
    if (!is.null(na_index)) {
        # x <- ifelse(!is.na(missingValues), missingValues, x)
        x[na_index] <- names(na_index)
    }
    
    x <- admisc::coerceMode(x)
    
    attrx$na_index <- NULL
    attrx$na_values <- NULL
    attrx$na_range <- NULL

    if (isFALSE(drop)) {
        attributes(x) <- attrx
    }
    return(x)
}


`undeclare.data.frame` <- function(x, drop = FALSE, ...) {
    declared <- vapply(x, is_declared, logical(1))
    x[declared] <- lapply(x[declared], undeclare, drop = drop)
    
    return(x)
}


`validate_declared` <- function(x = double(), labels = NULL, label = NULL,
                                na_values = NULL, na_range = NULL, ...) {

    if (!is.numeric(x) && !is.character(x) && !all(is.na(x))) {
        admisc::stopError("`x` must be a numeric or a character vector.")
    }

    if (!is.null(labels)) {
        if (is.null(names(labels))) {
            admisc::stopError("`labels` must have names.")
        }

        if (any(duplicated(stats::na.omit(labels)))) {
            admisc::stopError("`labels` must be unique.")
        }

        if (is.factor(x)) {
            if (!identical(labels, levels(x))) {
                admisc::stopError("`x` is a factor, and `labels` are different its levels.")
            }
        }
    }

    if (
        !is.null(label) &&
        (!is.atomic(label) || !is.character(label) || length(label) != 1)
    ) {
        admisc::stopError("`label` must be a character vector of length one.")
    }

    if (!is.null(na_values)) {
        if (any(is.na(na_values))) {
            admisc::stopError("`na_values` should not contain NA values.")
        }
    }

    if (!is.null(na_range)) {
        type_ok <-  (is.character(x) && is.character(na_range)) ||
                    (is.numeric(x) && is.numeric(na_range))

        if (!type_ok || length(na_range) != 2) {
            admisc::stopError("`na_range` must be a vector of length two of the same type as `x`.")
        }

        if (any(is.na(na_range))) {
            admisc::stopError("`na_range` can not contain missing values.")
        }
    }
}


`declared` <- function(x = double(), labels = NULL, na_values = NULL,
                          na_range = NULL, label = NULL, ...) {
    if (inherits(x, "haven_labelled")) {
        return(as_declared(x))
    }

    if (is.factor(x)) {
        nms <- levels(x)
        if (is.null(labels)) {
            labels <- seq(length(nms))
            names(labels) <- nms
        }

        wnms <- which(is.element(na_values, nms))
        if (length(wnms) > 0) {
            for (i in wnms) {
                na_values[i] <- which(nms == na_values[i])
            }
            if (admisc::possibleNumeric(na_values)) {
                na_values <- admisc::asNumeric(na_values)
            }
        }
    }

    attributes(x) <- NULL
    
    validate_declared(x, labels, label, na_values, na_range)

    
    misvals <- all_missing_values(x, na_values, na_range, labels)

    if (!is.null(na_range)) {
        if (!is.atomic(na_range) || length(na_range) != 2 ) {
            admisc::stopError("The 'na_range' argument should be an atomic vector of length 2.")
        }
        na_range <- sort(na_range)
    }

    missingValues(x)[is.element(x, misvals)] <- x[is.element(x, misvals)]

    attr(x, "na_values") <- na_values
    attr(x, "na_range") <- na_range
    attr(x, "labels") <- labels
    attr(x, "label") <- label

    return(x)
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
}


`missingValues` <- function(x) {
    
    mv <- rep(NA, length(x))
    
    if (is_declared(x)) {
        misvals <- attr(x, "na_index")
        mv[as.numeric(names(misvals))] <- misvals
    }

    return(mv)
}


`missingValues<-` <- function(x, value) {
    
    class(x) <- setdiff(class(x), "declared")
    other_classes <- setdiff(class(x), c("integer", "double", "character", "numeric", "complex", "haven_labelled", "haven_labelled_spss", "vctrs_vctr"))
    
    notna <- !is.na(value)
    x[notna] <- NA
    x <- admisc::coerceMode(x)

    if (any(notna)) {
        na_index <- which(notna)
        names(na_index) <- value[notna]
        attr(x, "na_index") <- na_index
    }
    
    structure(x, class = c("declared", other_classes, class(x)))
}


`[.declared` <- function(x, i, ...) {
    attrx <- attributes(x)
    x <- undeclare(x)
    x <- NextMethod()
    # attrx$label, if not existing, takes from attrx$labels
    # attrx[["label"]] is something like attr(x, "label", exact = TRUE)
    declared(x, attrx[["labels"]], attrx$na_values, attrx$na_range, attrx[["label"]])
}


`[<-.declared` <- function(x, i, value) {
    attrx <- attributes(x)
    value <- undeclare(value)
    x <- undeclare(x)
    x <- NextMethod()
    declared(x, attrx[["labels"]], attrx$na_values, attrx$na_range, attrx[["label"]])
}

`c.declared` <- function(...) {
    dots <- list(...)
    declared <- unlist(lapply(dots, is_declared))
    na_values <- sort(unique(unlist(
        lapply(dots, function(x) attr(x, "na_values"))
    )))

    labels <- unlist(lapply(dots, function(x) {
        attr(x, "labels", exact = TRUE)
    }))

    duplicates <- duplicated(labels)

    if (length(wduplicates <- which(duplicates)) > 0) {
        for (i in seq(length(wduplicates))) {
            if (length(unique(names(
                labels[labels == labels[wduplicates[i]]]
            ))) > 1) {
                admisc::stopError("Labels must be unique.")
            }
        }
    }

    labels <- sort(labels[!duplicates])

    na_range <- lapply(dots, function(x) attr(x, "na_range", exact = TRUE))
    nulls <- unlist(lapply(na_range, is.null))

    if (all(nulls)) {
        na_range <- NULL
    }
    else {
        if (sum(nulls) == length(na_range) - 1) {
            na_range <- unlist(na_range)
        }
        else {
            compatible <- logical(length(na_range))
            if (!is.null(na_range)) {
                for (i in seq(1, length(na_range) - 1)) {
                    nai <- na_range[[i]]
                    if (is.null(nai)) {
                        compatible[i] <- TRUE
                    }
                    else {
                        for (j in seq(2, length(na_range))) {
                            naj <- na_range[[j]]
                            if (is.null(naj)) {
                                compatible[j] <- TRUE
                            }
                            else {
                                if (any(is.element(seq(nai[1], nai[2]), seq(naj[1], naj[2]))) > 0) {
                                    compatible[i] <- TRUE
                                    compatible[j] <- TRUE
                                }
                            }
                        }
                    }
                }
            }

            if (any(!compatible)) {
                admisc::stopError("Incompatible NA ranges.")
            }

            na_range <- range(unlist(na_range))
        }
    }

    dots <- unlist(lapply(dots, function(x) {
        if (is_declared(x)) x <- undeclare(x)
        attributes(x) <- NULL
        return(x)
    }))

    return(declared(
        dots,
        labels = labels,
        na_values = na_values,
        na_range = na_range,
        label = attr(dots[[which(declared)[1]]], "label", exact = TRUE)
    ))
}


`format_declared` <- function(x, digits = getOption("digits")) {
    if (!is.atomic(x)) {
        admisc::stopError("`x` has to be a vector.")
    }

    out <- format(unclass(x), digits = digits)
    na_index <- attr(x, "na_index")

    out[na_index] <- paste0("NA(", names(na_index), ")")

    # format again to make sure all elements have same width
    return(format(out, justify = "right"))
}


`print.declared` <- function(x, ...) {
    label <- variable_label(x)
    if (!is.null(label)) {
        label <- paste("", label)
    }

    cat(paste0("<declared", likely_type(x), "[", length(x), "]>", label, "\n"))
    if (length(x) > 0) {
        print(noquote(format_declared(x)), ...)

        na_values <- attr(x, "na_values")
        if (!is.null(na_values)) {
            cat(paste0("Missing values: ", paste(na_values, collapse = ", "), "\n"))
        }

        na_range <- attr(x, "na_range")
        if (!is.null(na_range)) {
            cat(paste0(
                "Missing range:  [",
                paste(na_range, collapse = ", "),
                "]\n"
            ))
        }

        labels <- attr(x, "labels", exact = TRUE)

        if (length(labels) == 0) {
            return(invisible(x))
        }

        cat("\nLabels:", "\n", sep = "")

        print(
            data.frame(
                value = unname(labels),
                label = names(labels),
                row.names = NULL
            ),
            row.names = FALSE
        )

        return(invisible(x))
    }
}


`==.declared` <- function(e1, e2) {
    le1 <- attr(e1, "labels", exact = TRUE)
    e1 <- unclass(undeclare(e1))
    e2 <- unclass(undeclare(e2))
    if (admisc::possibleNumeric(e1) && admisc::possibleNumeric(e2)) {
        e1 <- admisc::asNumeric(e1)
        e2 <- admisc::asNumeric(e2)
        return(abs(e1 - e2) < .Machine$double.eps^0.5)
        # return(admisc::aeqb(e1, e2)
    }
    else {
        if (length(e2) == 1 && is.element(e2, names(le1)) && !is.element(e2, e1)) {
            e2 <- le1[names(le1) == e2]
        }
        return(e1 == e2)
    }
}


`!=.declared` <- function(e1, e2) {e1 <- unclass(undeclare(e1))
    le1 <- attr(e1, "labels", exact = TRUE)
    e1 <- unclass(undeclare(e1))
    e2 <- unclass(undeclare(e2))
    if (admisc::possibleNumeric(e1) && admisc::possibleNumeric(e2)) {
        e1 <- admisc::asNumeric(e1)
        e2 <- admisc::asNumeric(e2)
        return(abs(e1 - e2) > .Machine$double.eps^0.5)
        # return(admisc::aneqb(e1, e2)
    }
    else {
        if (length(e2) == 1 && is.element(e2, names(le1)) && !is.element(e2, e1)) {
            e2 <- le1[names(le1) == e2]
        }
        return(e1 != e2)
    }
}


`<=.declared` <- function(e1, e2) {
    e1 <- unclass(undeclare(e1))
    e2 <- unclass(undeclare(e2))
    if (admisc::possibleNumeric(e1) && admisc::possibleNumeric(e2)) {
        e1 <- admisc::asNumeric(e1)
        e2 <- admisc::asNumeric(e2)
        return(e1 < (e2 + .Machine$double.eps^0.5))
        # return(admisc::alteb(e1, e2)
    }
    else {
        return(e1 <= e2)
    }
}


`<.declared` <- function(e1, e2) {
    e1 <- unclass(undeclare(e1))
    e2 <- unclass(undeclare(e2))
    if (admisc::possibleNumeric(e1) && admisc::possibleNumeric(e2)) {
        e1 <- admisc::asNumeric(e1)
        e2 <- admisc::asNumeric(e2)
        return(e1 < (e2 - .Machine$double.eps^0.5))
        # return(admisc::altb(e1, e2)
    }
    else {
        return(e1 < e2)
    }
}


`>=.declared` <- function(e1, e2) {
    e1 <- unclass(undeclare(e1))
    e2 <- unclass(undeclare(e2))
    if (admisc::possibleNumeric(e1) && admisc::possibleNumeric(e2)) {
        e1 <- admisc::asNumeric(e1)
        e2 <- admisc::asNumeric(e2)
        return((e1 + .Machine$double.eps^0.5) > e2)
        # return(admisc::agteb(e1, e2)
    }
    else {
        return(e1 >= e2)
    }
}


`>.declared` <- function(e1, e2) {
    e1 <- unclass(undeclare(e1))
    e2 <- unclass(undeclare(e2))
    if (admisc::possibleNumeric(e1) && admisc::possibleNumeric(e2)) {
        e1 <- admisc::asNumeric(e1)
        e2 <- admisc::asNumeric(e2)
        return((e1 - .Machine$double.eps^0.5) > e2)
        # return(admisc::agtb(e1, e2))
    }
    else {
        return(e1 > e2)
    }
}


`names<-.declared` <- function(x, value) {
    attr(x, "names") <- value
    x
}


`duplicated.declared` <- function(x, incomparables = FALSE, ...) {
    x <- unclass(undeclare(x))
    NextMethod()
}


`unique.declared` <- function(x, incomparables = FALSE, ...) {
    x[!duplicated(x)]
}


`head.declared` <- function(x, n = 6L, ...) {
    lx <- length(x)
    if (n < 0) {
        n <- lx - abs(n)
    }
    n <- min(n, length(x))
    if (n < 1) {
        return(x[0])
    }
    x[seq(n)]
}


`tail.declared` <- function(x, n = 6L, ...) {
    if (n < 1) {
        n <- 6L
    }
    lx <- length(x)
    n <- min(n, lx)
    x[seq(lx - n + 1, lx)]
}


`na.omit.declared` <- function (object, ...)  {
    attrx <- attributes(object)
    attrx$na_index <- NULL
    object <- unclass(object)
    object <- NextMethod()
    attrx$na.action <- attr(object, "na.action")
    nms <- attrx$names
    if (!is.null(nms) && !is.null(attrx$na.action)) {
        nms <- nms[-attr(object, "na.action")]
        attrx$names <- nms
    }
    attributes(object) <- attrx
    return(object)
}


`na.fail.declared` <- function (object, ...)  {
    object <- unclass(object)
    NextMethod()
}


`na.exclude.declared` <- function (object, ...)  {
    attrx <- attributes(object)
    attrx$na_index <- NULL
    object <- unclass(object)
    object <- NextMethod()
    attrx$na.action <- attr(object, "na.action")
    nms <- attrx$names
    if (!is.null(nms) && !is.null(attrx$na.action)) {
        nms <- nms[-attr(object, "na.action")]
        attrx$names <- nms
    }
    attributes(object) <- attrx
    return(object)
}


`mean.declared` <- function(x, ...) {
    na_index <- attr(x, "na_index")
    if (!is.null(na_index)) {
        x <- x[-na_index]
    }
    x <- unclass(x)
    NextMethod()
}


`median.declared` <- function(x, na.rm = FALSE, ...) {
    na_index <- attr(x, "na_index")
    if (!is.null(na_index)) {
        x <- x[-na_index]
    }
    x <- unclass(x)
    NextMethod()
}


`summary.declared` <- function(object, ...) {
    na_index <- attr(object, "na_index")
    if (!is.null(na_index)) {
        object[na_index] <- NA
    }
    object <- unclass(object)
    NextMethod()
}


# Arithmetic operations
`+.declared` <- function(e1, e2) {
    attributes(e1) <- NULL
    callist <- list(e1 = e1)
    if (!missing(e2)) {
        if (is_declared(e2)) {
            attributes(e2) <- NULL
        }
        callist$e2 <- e2
    }
    do.call(.Primitive("+"), callist)
}


`-.declared` <- function(e1, e2) {
    attributes(e1) <- NULL
    callist <- list(e1 = e1)
    if (!missing(e2)) {
        if (is_declared(e2)) {
            attributes(e2) <- NULL
        }
        callist$e2 <- e2
    }
    do.call(.Primitive("-"), callist)
}


`*.declared` <- function(e1, e2) {
    attributes(e1) <- NULL
    callist <- list(e1 = e1)
    if (!missing(e2)) {
        if (is_declared(e2)) {
            attributes(e2) <- NULL
        }
        callist$e2 <- e2
    }
    do.call(.Primitive("*"), callist)
}


`/.declared` <- function(e1, e2) {
    attributes(e1) <- NULL
    callist <- list(e1 = e1)
    if (!missing(e2)) {
        if (is_declared(e2)) {
            attributes(e2) <- NULL
        }
        callist$e2 <- e2
    }
    do.call(.Primitive("/"), callist)
}


`^.declared` <- function(e1, e2) {
    attributes(e1) <- NULL
    callist <- list(e1 = e1)
    if (!missing(e2)) {
        if (is_declared(e2)) {
            attributes(e2) <- NULL
        }
        callist$e2 <- e2
    }
    do.call(.Primitive("^"), callist)
}


`%%.declared` <- function(e1, e2) {
    attributes(e1) <- NULL
    callist <- list(e1 = e1)
    if (!missing(e2)) {
        if (is_declared(e2)) {
            attributes(e2) <- NULL
        }
        callist$e2 <- e2
    }
    do.call(.Primitive("%%"), callist)
}


`%/%.declared` <- function(e1, e2) {
    attributes(e1) <- NULL
    callist <- list(e1 = e1)
    if (!missing(e2)) {
        if (is_declared(e2)) {
            attributes(e2) <- NULL
        }
        callist$e2 <- e2
    }
    do.call(.Primitive("%/%"), callist)
}


`sqrt.declared` <- function(x) {
    attributes(x) <- NULL
    do.call(.Primitive("sqrt"), list(x))
}


`abs.declared` <- function(x) {
    attributes(x) <- NULL
    do.call(.Primitive("abs"), list(x))
}
