`value_labels` <- function(x, prefixed = FALSE) {
    UseMethod("value_labels")
}

`value_labels.default` <- function(x, prefixed = FALSE) {
    # do nothing
    NULL
}

`value_labels.haven_labelled_spss` <- function(x, prefixed = FALSE) {
    labels <- attr(x, "labels", exact = TRUE)
    if (prefixed)
        names(labels) <- paste0("[", labels, "] ", names(labels))
    labels
}

`value_labels.declared` <- function(x, prefixed = FALSE) {
    labels <- attr(x, "labels", exact = TRUE)
    if (prefixed) {
        names(labels) <- paste0("[", labels, "] ", names(labels))
    }

    return(labels)
}

`value_labels.data.frame` <- function(x, prefixed = FALSE) {
    lapply(x, value_labels, prefixed = prefixed)
}

`value_labels<-` <- function(x, value) {
  UseMethod("value_labels<-")
}

`value_labels<-.default` <- function(x, value) {
    # do nothing
    x
}

`value_labels<-.declared` <- function(x, value) {
    attr(x, "labels") <- value
    return(x)
}

`variable_label` <- function(x) {
    UseMethod("variable_label")
}

`variable_label.default` <- function(x) {
    # do nothing
    x
}

`variable_label.haven_labelled_spss` <- function(x) {
    attr(x, "label", exact = TRUE)
}

`variable_label.declared` <- function(x) {
    attr(x, "label", exact = TRUE)
}

`variable_label.data.frame` <- function(x) {
    lapply(x, variable_label)
}

`variable_label<-` <- function(x, value) {
  UseMethod("variable_label<-")
}

`variable_label<-.default` <- function(x, value) {
    # do nothing
    x
}

`variable_label<-.declared` <- function(x, value) {
    if (!is.null(value) && length(value) > 1) {
        admisc::stopError("`value` should be a single character string or NULL.")
    }

    if (is.null(value)) {
        attr(x, "label") <- NULL
    }
    else {
        attr(x, "label") <- as.character(value)
    }

    return(x)
}
