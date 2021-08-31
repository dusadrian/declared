# internal
`all_missing_values` <-
function(x, na_values = NULL, na_range = NULL, labels = NULL) {

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


`as_declared` <- function(x, ...) {
    UseMethod("as_declared")
}


`as_declared.default` <- function(x, ...) {
    return(x)
}


`as_declared.haven_labelled` <- function(x, ...) {

    dots <- list(...)
    if (is.element("haven", names(dots))) {
        haven <- dots$haven
    }
    else {
        haven <- eval(parse(text = "requireNamespace('haven', quietly = TRUE)"))
    }

    if (haven) {
        if (eval(parse(text = "any(haven::is_tagged_na(x))"))) {
            admisc::stopError("Tagged NAs are not supported.")
        }
    }

    misvals <- all_missing_values(unclass(x))

    na_values <- attr(x, "na_values")
    na_range <- attr(x, "na_range")
    labels <- attr(x, "labels", exact = TRUE)
    label <- attr(x, "label", exact = TRUE)
    format_spss <- attr(x, "format.spss") # necessary for DDIwR::convert
    # attrx <- attributes(x)

    attributes(x) <- NULL
    missingValues(x)[is.element(x, misvals)] <- x[is.element(x, misvals)]

    attr(x, "na_values") <- na_values
    attr(x, "na_range") <- na_range
    attr(x, "labels") <- labels
    attr(x, "label") <- label
    attr(x, "format.spss") <- format_spss

    # attrx$class <- class(x)
    # attributes(x) <- attrx

    return(x)
}


`as_declared.factor` <- function(x, ...) {
    # TO DO, but for the moment, do nothing
    return(x)
}


`as_declared.data.frame` <- function(x, ...) {
    haven <- eval(parse(text = "requireNamespace('haven', quietly = TRUE)"))
    x[] <- lapply(x, as_declared, haven = haven, ...)
    class(x) <- "data.frame"
    return(x)
}


`undeclare` <- function(x, ...) {
    UseMethod("undeclare")
}


`undeclare.default` <- function(x, ...) {
    return(x)
}


`undeclare.declared` <- function(x, ...) {
    na_index <- attr(x, "na_index")
    attrx <- attributes(x)
    
    # this is necessary to replace those values
    # (because of the "[<-.declared" method)
    attributes(x) <- NULL # or x <- unclass(x), but I find this cleaner
    if (!is.null(na_index)) {
        # x <- ifelse(!is.na(missingValues), missingValues, x)
        x[na_index] <- likely_mode(names(na_index))
    }
    
    attrx$na_index <- NULL
    attrx$na_values <- NULL
    attrx$na_range <- NULL

    attributes(x) <- attrx
    return(x)
}


`undeclare.data.frame` <- function(x, ...) {
    declared <- vapply(x, is_declared, logical(1))
    x[declared] <- lapply(x[declared], undeclare)
    
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

    attributes(x) <- NULL
    
    validate_declared(x, labels, label, na_values, na_range)

    if (!all(is.na(x))) {
        misvals <- all_missing_values(x, na_values, na_range, labels)

        if (!is.null(na_range)) {
            na_range <- sort(na_range)
        }

        missingValues(x)[is.element(x, misvals)] <- x[is.element(x, misvals)]
    }

    attr(x, "na_values") <- na_values
    attr(x, "na_range") <- na_range
    attr(x, "labels") <- labels
    attr(x, "label") <- label

    return(x)
}


`likely_mode` <- function(x) {
    if (admisc::possibleNumeric(x) || all(is.na(x))) {
        x <- admisc::asNumeric(x)
        if (admisc::wholeNumeric(x) & !is.integer(x)) {
            x <- as.integer(x)
        }
    }

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
    
    x <- likely_mode(x)

    if (any(!is.na(value))) {
        x[!is.na(value)] <- NA
        na_index <- which(!is.na(value))
        value <- value[!is.na(value)]
        names(na_index) <- value
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


`==.declared` <- function(e1, e2) {
    e1 <- unclass(undeclare(e1))
    e2 <- unclass(undeclare(e2))
    if (admisc::possibleNumeric(e1) && admisc::possibleNumeric(e2)) {
        e1 <- admisc::asNumeric(e1)
        e2 <- admisc::asNumeric(e2)
        return(abs(e1 - e2) < .Machine$double.eps^0.5)
        # return(admisc::aeqb(e1, e2)
    }
    else {
        return(e1 == e2)
    }
}

`!=.declared` <- function(e1, e2) {e1 <- unclass(undeclare(e1))
    e1 <- unclass(undeclare(e1))
    e2 <- unclass(undeclare(e2))
    if (admisc::possibleNumeric(e1) && admisc::possibleNumeric(e2)) {
        e1 <- admisc::asNumeric(e1)
        e2 <- admisc::asNumeric(e2)
        return(abs(e1 - e2) > .Machine$double.eps^0.5)
        # return(admisc::aneqb(e1, e2)
    }
    else {
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
    x[seq(n)]
}

`tail.declared` <- function(x, n = 6L, ...) {
    lx <- length(x)
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
