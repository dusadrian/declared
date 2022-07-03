`value_labels` <- function(...) {
    .Deprecated(msg = "Function value_labels() is deprecated, use labels()\n")
    labels(...)
}

`variable_label` <- function(...) {
    .Deprecated(msg = "Function variable_label() is deprecated, use label()\n")
    label(...)
}

`labels.declared` <- function(object, prefixed = FALSE, ...) {
    labels <- attr(object, "labels", exact = TRUE)
    if (prefixed) {
        names(labels) <- paste0("[", labels, "] ", names(labels))
    }

    return(labels)
}

`labels.haven_labelled_spss` <- function(object, prefixed = FALSE, ...) {
    labels <- attr(object, "labels", exact = TRUE)
    if (prefixed)
        names(labels) <- paste0("[", labels, "] ", names(labels))
    labels
}

`labels.data.frame` <- function(object, prefixed = FALSE, ...) {
    lapply(object, labels, prefixed = prefixed)
}

`labels<-` <- function(x, value) {
    UseMethod("labels<-")
}

`labels<-.default` <- function(x, value) {
    # do nothing
    x
}

`labels<-.declared` <- function(x, value) {
    attr(x, "labels") <- value
    return(x)
}

`label` <- function(x) {
    UseMethod("label")
}

`label.default` <- function(x) {
    attr(x, "label", exact = TRUE)
}

`label.haven_labelled_spss` <- function(x) {
    attr(x, "label", exact = TRUE)
}

`label.declared` <- function(x) {
    attr(x, "label", exact = TRUE)
}

`label.data.frame` <- function(x) {
    lapply(x, label)
}

`label<-` <- function(x, value) {
    UseMethod("label<-")
}

`label<-.declared` <- function(x, value) {
    if (!is.null(value) && length(value) > 1) {
        stopError_("`value` should be a single character string or NULL.")
    }

    if (is.null(value)) {
        attr(x, "label") <- NULL
    }
    else {
        attr(x, "label") <- as.character(value)
    }

    return(x)
}

# the following functions are deliberately taken from package Hmisc
# to lower the risk of namespace collision

`label<-.default` <- function(x, value) {
    if(is.list(value)) {
        stopError_("cannot assign a list to be a object label")
    }

    if (length(value) != 1L) {
        stopError_("value must be character vector of length 1")
    }

    attr(x, "label") <- value

    if (! inherits(x, "labelled")) {
        class(x) <- c("labelled", class(x))
    }

    return(x)
}

`label<-.data.frane` <- function(x, self = TRUE, ..., value) {
    if (is.list(value)) {
        self <- FALSE
    }

    if (self) {
        if (!is.null(value) && length(value) > 1) {
            stopError_("`value` should be a single character string or NULL.")
        }

        xc <- class(x)
        xx <- unclass(x)
        if (is.null(value)) {
            attr(xx, "label") <- NULL
        }
        else {
            attr(xx, "label") <- as.character(value)
        }
        
        class(xx) <- xc
        return(xx)
    }
    else {
        if(length(value) != length(x)) {
            stopError_("value must have the same length as x")
        }

        for (i in seq(along.with = x)) {
            label(x[[i]]) <- value[[i]]
        }
    }

    return(x)
}
