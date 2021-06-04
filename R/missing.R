
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

#' @export
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
