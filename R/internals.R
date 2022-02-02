`c_declared` <- function(dots, recursive = FALSE, use.names = TRUE) {
    
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

    declared(
        dots,
        labels = labels,
        na_values = na_values,
        na_range = na_range,
        label = attr(dots[[which(declared)[1]]], "label", exact = TRUE)
    )
}

`order_declared` <- function(x, na.last = NA, decreasing = FALSE, method = c("auto",
    "shell", "radix"), na_values.last = na.last) {
    
    if (!is_declared(x)) {
        cat("\n")
        stop("`x` has to be a vector of class `declared`.\n\n", call. = FALSE)
    }

    method <- match.arg(method)
    
    x_indexes <- seq_along(x)

    na_index <- attr(x, "na_index")
    declared <- logical(length(x))
    declared[na_index] <- TRUE
    truena <- x_indexes[is.na(x) & !declared]
    
    declared_indexes <- c()

    if (any(declared)) {
        x <- undeclare(x)
        declared_indexes <- unname(na_index[order(names(na_index), decreasing = decreasing, method = method)])
    }

    attributes(x) <- NULL
    x_indexes <- x_indexes[!(is.na(x) | declared)]
    x <- x[!(is.na(x) | declared)]

    res <- c()
    if (isFALSE(na.last)) {
        res <- truena
    }

    if (isFALSE(na_values.last)) {
        res <- c(res, declared_indexes)
    }

    res <- c(res, x_indexes[order(unclass(x), decreasing = decreasing, method = method)])
    
    if (isTRUE(na_values.last)) {
        res <- c(res, declared_indexes)
    }
    
    if (isTRUE(na.last)) {
        res <- c(res, truena)
    }

    return(res)
}

`names_values` <- function(x) {

    if (!inherits(x, "declared") & !inherits(x, "haven_labelled_spss")) {
        cat("\n")
        stop("The input should be a declared / haven_labelled_spss vector.\n\n", call. = FALSE)
    }

    attrx <- attributes(x)
    x <- undeclare(x)
    attributes(x) <- NULL
    
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
                names(xnotmis)[i] <- ifelse(nms[isel] == "", xnotmis[i], nms[isel])
            }
        }
    }

    result <- c(xnotmis, xmis)
    attr(result, 'missing') <- unname(xmis)

    return(result)
}

`to_labels` <- function(x) {

    if (!inherits(x, "declared")) {
        cat("\n")
        stop("The input should be a declared vector.\n\n", call. = FALSE)
    }

    labels <- names_values(x)

    x <- undeclare(x)

    attributes(x) <- NULL

    x[is.element(x, labels)] <- names(labels)[match(x[is.element(x, labels)], labels)]

    return(x)
}

`plus_declared` <- function(e1, e2) {
    
}
