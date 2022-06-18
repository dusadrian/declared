`measurement` <- function(x) {
    UseMethod("measurement")
}

`measurement.default` <- function(x) {
    # do nothing
    NULL
}

`measurement.declared` <- function(x) {
    m <- attr(x, "measurement")
    if (is.null(m)) {
        m <- "Unspecified"
        l_m <- likely_measurement(x)
        if (!identical(l_m, "")) {
            m <- paste0(m, ", but likely ", paste(l_m, collapse = " "))
        }
    }

    return(m)
}


`measurement<-` <- function(x, value) {
  UseMethod("measurement<-")
}

`measurement<-.default` <- function(x, value) {
    # do nothing
    x
}

`measurement<-.declared` <- function(x, value) {
    attr(x, "measurement") <- check_measurement(value)
    return(x)
}
