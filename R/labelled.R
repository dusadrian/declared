`na_values.declared` <- function(x, value) {
    missing_values(x)
}

`na_values<-.declared` <- function(x, value) {
    declared(undeclare(x),
        labels = attr(x, "labels", exact = TRUE),
        na_values = value,
        na_range = attr(x, "na_range", exact = TRUE),
        label = attr(x, "label", exact = TRUE)
    )
}

`na_range.declared` <- function(x, value) {
    missing_range(x)
}

`na_range<-.declared` <- function(x, value) {
    declared(undeclare(x),
        labels = attr(x, "labels", exact = TRUE),
        na_values = attr(x, "na_values", exact = TRUE),
        na_range = value,
        label = attr(x, "label", exact = TRUE)
    )
}

`val_labels.declared` <- function(x, prefixed = FALSE) {
    labels <- attr(x, "labels", exact = TRUE)
    if (prefixed) {
        names(labels) <- paste0("[", labels, "] ", names(labels))
    }

    return(labels)
}

`val_labels<-.declared` <- function(x, value) {
    attr(x, "labels") <- value
    return(x)
}

`var_label.declared` <- function(x) {
    attr(x, "label", exact = TRUE)
}

`var_label<-.declared` <- function(x, value) {
    if (!is.null(value) && length(value) > 1) {
        cat("\n")
        stop(simpleError("`value` should be a single character string or NULL.\n\n"))
    }

    if (is.null(value)) {
        attr(x, "label") <- NULL
    }
    else {
        attr(x, "label") <- as.character(value)
    }

    return(x)
}

`drop_unused_value_labels.declared` <- function(x) {
    labels <- value_labels(x)
    value_labels(x) <- labels[is.element(labels, unique(undeclare(x)))]
    return(x)
}


`val_label.declared` <- function(x, v, prefixed = FALSE) {
    if (length(v) != 1) {
        stop("`v` should be a single value", call. = FALSE, domain = "R-labelled")
    }

    labels <- value_labels(x)

    if (v %in% labels) {
        if (prefixed) {
            return(paste0("[", v, "] ", names(labels)[labels == v]))
        }
        else {
            return(names(labels)[labels == v])
        }
    }
    else {
        return(NULL)
    }
}

`val_label<-.declared` <- function(x, v, value) {
    if (length(v) != 1) {
        stop("`v` should be a single value", call. = FALSE, domain = "R-labelled")
    }

    if (length(value) > 1) {
        stop("`value` should be a single character string or NULL",
        call. = FALSE, domain = "R-labelled")
    }

    labels <- value_labels(x)

    if (is.null(value)) {
        if (v %in% labels) {
            labels <- labels[labels != v]
        }
    }
    else {
        if (v %in% labels) {
            names(labels)[labels == v] <- value
        }
        else {
            names(v) <- value
            labels <- c(labels, v)
        }
    }

    if (length(labels) == 0)
        labels <- NULL

    value_labels(x) <- labels
    x
}

`sort_val_labels.declared` <- function(x, according_to = c("values",
    "labels"), decreasing = FALSE) {

    according_to <- match.arg(according_to)
    
    labels <- value_labels(x)
    
    if (!is.null(labels)) {
        if (according_to == "values") {
            labels <- sort(labels, decreasing = decreasing)
        }
        
        if (according_to == "labels") {
            labels <- labels[order(names(labels), decreasing = decreasing)]
        }

        value_labels(x) <- labels
    }

    return(x)
}

`nolabel_to_na.declared` <- function(x) {
    allval <- unique(x)
    allval <- allval[!is.na(allval)]
    nolabel <- allval[!allval %in% value_labels(x)]
    if (length(nolabel) > 0) {
        x[x %in% nolabel] <- NA
    }
    x
}

`val_labels_to_na.declared` <- function(x) {
    labels <- value_labels(x)

    if (length(labels) > 0) {
        x[is.element(x, labels)] <- NA
    }

    value_labels(x) <- NULL
    return(x)
}

`remove_labels.declared` <- function(x, user_na_to_na = FALSE, keep_var_label = FALSE) {
    if (!keep_var_label) {
        variable_label(x) <- NULL
    }

    value_labels(x) <- NULL
    attr(x, "format.spss") <- NULL
    return(x)
}

`remove_user_na.declared` <- function(x, user_na_to_na = FALSE) {
    attr(x, "na_index") <- NULL
    missing_values(x) <- NULL
    missing_range(x) <- NULL
    return(x)
}

`to_factor.declared` <- function(x, levels = c("labels", "values",
    "prefixed"), ordered = FALSE, nolabel_to_na = FALSE,
    sort_levels = c("auto", "none", "labels", "values"), decreasing = FALSE,
    drop_unused_labels = FALSE, user_na_to_na = FALSE, strict = FALSE,
    unclass = FALSE,
    ...) {

    x <- undeclare(x)
    vl <- variable_label(x)
    levels <- match.arg(levels)
    sort_levels <- match.arg(sort_levels)
    
    if (strict) {
        allval <- unique(x)
        allval <- allval[!is.na(allval)]
        nolabel <- allval[!allval %in% value_labels(x)]

        if (length(nolabel) > 0) {
            if (unclass) {
                x <- unclass(x)
            }
            return(x)
        }
    }

    labels <- value_labels(x)
    if (nolabel_to_na) {
        allval <- unique(x)
        allval <- allval[!is.na(allval)]
        nolabel <- allval[!allval %in% labels]
        if (length(nolabel) > 0) {
            x[x %in% nolabel] <- NA
        }
    }

    allval <- unique(x)
    allval <- allval[!is.na(allval)]
    nolabel <- sort(allval[!is.element(allval, labels)])
    # if there are some values with no label
    if (length(nolabel) > 0) {
        names(nolabel) <- as.character(nolabel)
        levs <- c(labels, nolabel)
    }
    else {
        levs <- labels
    }

    if (sort_levels == "auto" & length(nolabel) > 0) {
        sort_levels <- "values"
    }

    if (sort_levels == "labels") {
        levs <- levs[order(names(levs), decreasing = decreasing)]
    }

    if (sort_levels == "values") {
        levs <- sort(levs, decreasing = decreasing)
    }

    if (levels == "labels") {
        labs <- names(levs)
    }

    if (levels == "values") {
        labs <- unname(levs)
    }

    if (levels == "prefixed") {
        labs <- eval(parse(text = "names_prefixed_by_values(levs)"))
    }

    levs <- unname(levs)
    x <- factor(x, levels = levs, labels = labs, ordered = ordered, ...)
    
    if (drop_unused_labels) {
        x <- droplevels(x)
    }

    variable_label(x) <- vl

    return(x)
}

`to_character.declared` <- function(x, levels = c("labels", "values",
    "prefixed"), nolabel_to_na = FALSE, user_na_to_na = FALSE, ...) {
    vl <- variable_label(x)
    levels <- match.arg(levels)
    x <- as.character(eval(parse(text = "to_factor(x, levels = levels, nolabel_to_na = nolabel_to_na, user_na_to_na = FALSE)")))
    variable_label(x) <- vl
    x
}

`copy_labels.declared` <- function(from, to, .strict = TRUE) {
    if (mode(from) != mode(to) & .strict) {
        stop("`from` and `to` should be of same type", call. = FALSE, domain = "R-labelled")
    }

    variable_label(to) <- variable_label(from)

    if (mode(from) == mode(to)) {
        value_labels(to) <- value_labels(from)
        missing_range(to) <- missing_range(from)
        missing_values(to) <- missing_values(from)
    }

    return(to)
}
