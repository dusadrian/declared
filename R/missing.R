
`missing_values` <- function(x) {
    UseMethod("missing_values")
}

`missing_values.default` <- function(x) {
    # return nothing
    NULL
}

`missing_values.haven_labelled_spss` <- function(x) {
  attr(x, "na_values", exact = TRUE)
}

`missing_values.declared` <- function(x) {
    attr(x, "na_values", exact = TRUE)
}

`missing_values.data.frame` <- function(x) {
  lapply(x, missing_values)
}

`missing_values<-` <- function(x, value) {
  UseMethod("missing_values<-")
}

`missing_values<-.default` <- function(x, value) {
    # do nothing
    x
}

`missing_values<-.declared` <- function(x, value) {
    declared(undeclare(x),
        labels = attr(x, "labels", exact = TRUE),
        na_values = value,
        na_range = attr(x, "na_range", exact = TRUE),
        label = attr(x, "label", exact = TRUE)
    )
}

`missing_range` <- function(x) {
  UseMethod("missing_range")
}

`missing_range.default` <- function(x) {
    # return nothing
    NULL
}

`missing_range.haven_labelled_spss` <- function(x) {
    attr(x, "na_range", exact = TRUE)
}

`missing_range.declared` <- function(x) {
    attr(x, "na_range", exact = TRUE)
}

`missing_range.data.frame` <- function(x) {
    lapply(x, missing_range)
}

`missing_range<-` <- function(x, value) {
    UseMethod("missing_range<-")
}

`missing_range<-.default` <- function(x, value) {
    # do nothing
    x
}

`missing_range<-.declared` <- function(x, value) {
    if (length(value) != 2 || !is.numeric(value)) {
        cat("\n")
        stop("`value` should be a numeric vector of length 2.\n\n", call. = FALSE)
    }

    value <- sort(value)

    declared(undeclare(x),
        labels = attr(x, "labels", exact = TRUE),
        na_values = attr(x, "na_values", exact = TRUE),
        na_range = value,
        label = attr(x, "label", exact = TRUE)
    )
}



# the following functions are solely for internal use purposes

`missingValues` <- function(x) {
    
    mv <- rep(NA, length(x))
    
    if (is.declared(x)) {
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

    if (!all(is.na(x))) {
        x <- coerceMode_(x)
    }

    if (any(notna)) {
        na_index <- which(notna)
        names(na_index) <- value[notna]
        attr(x, "na_index") <- na_index
    }
    
    structure(x, class = c("declared", other_classes, class(x)))
}
