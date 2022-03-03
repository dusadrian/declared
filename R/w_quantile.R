`w_quantile` <- function(
    x, wt = NULL, probs = seq(0, 1, 0.25), na.rm = TRUE, ...
) {

    metacall <- as.list(match.call())
    
    if (inherits(x, "haven_labelled")) {
        x <- as_declared(x)
    }
    
    if (!(is.atomic(x) && is.numeric(x))) {
        admisc::stopError("'x' should be an atomic numerical vector.")
    }

    if (inherits(x, "declared")) {
        na_index <- attr(x, "na_index")
        if (length(na_index)) {
            x <- x[-na_index]
            wt <- wt[-na_index] # if wt is NULL, the result is still NULL
        }
    }

    if (is.null(wt)) {
        qs <- quantile(x, probs = probs, na.rm = na.rm, ... = ...)
        class(qs) <- c("w_quantile", class(qs))
        return(qs)
    }
    
    if (!(is.atomic(wt) && all(is.finite(wt)))) {
        admisc::stopError("'wt' should be an atomic vector with finite values.")
    }

    if (length(x) != length(wt)) {
        admisc::stopError("Lengths of 'x' and 'wt' differ.")
    }

    ok <- !is.na(x + wt)
    
    if (na.rm) {
        x <- x[ok]
        wt <- wt[ok]
    }
    else if (any(!ok)) {
        admisc::stopError("Missing values and NaN's not allowed if `na.rm' is FALSE")
    }
    
    if (any((p.ok <- !is.na(probs)) & (probs < 0 | probs > 1))) {
        admisc::stopError("probs outside [0,1]")
    }

    if (na.p <- any(!p.ok)) {
        o.pr <- probs
        probs <- probs[p.ok]
        probs <- pmax(0, pmin(1, probs))
    }

    sumwt <- sum(wt)
    wt <- cumsum(tapply(wt, x, sum, simplify = TRUE))
    x <- sort(unique(x))

    index <- 1 + (sumwt - 1) * probs

    left <- pmax(floor(index), 1)
    right <- pmin(left + 1, sumwt)

    index <- index%%1

    both <- approx(
        wt, x, xout = c(left, right),
        method = "constant", rule = 2, f = 1
    )$y
    lp <- length(probs)
    qs <- (1 - index) * both[seq(1, lp)] + index * both[-seq(1, lp)]

    qs <- admisc::coerceMode(round(qs, 3))

    names(qs) <- paste0(format(100 * probs, trim = TRUE), "%")

    class(qs) <- c("w_quantile", class(qs))
    # class(qs) <- c("admisc_fobject", class(qs)) # next version
    return(qs)

}



print.w_quantile <- function(x, startend = TRUE, ...) {

    class(x) <- setdiff(class(x), "w_quantile")

    if (is.matrix(x)) {
        # for ex. via using() with split.by
        rnms <- rownames(x)
        
        max.nchar.rnms <- max(nchar(encodeString(rnms)), na.rm = TRUE)
        for (i in seq(length(rnms))) {
            if (nchar(rnms[i]) < max.nchar.rnms) {
                rnms[i] <- pad_left(rnms[i], max.nchar.rnms - nchar(rnms[i]))
            }
        }

        rownames(x) <- rnms
    }
    else if (is.atomic(x)) {
        x <- matrix(x, nrow = 1, dimnames = list("", names(x)))
    }

    nax <- is.na(x)

    pN <- apply(x, 2, admisc::possibleNumeric)
    nms <- colnames(x)

    for (c in seq(ncol(x))) {
        xc <- x[, c]
        if (pN[c]) {
            max.nchar.nc <- max(nchar(xc), na.rm = TRUE)
            ndec <- countdec(xc)
            x[, c] <- sprintf(
                paste0("%", max.nchar.nc, ".", ndec, "f"),
                admisc::asNumeric(xc)
            )
        }

        if (admisc::possibleNumeric(nms[c])) {
            # since this is a column name, most likely it is a whole number
            # e.g. the value of a declared object instead of the label
            nmsc <- sprintf(
                paste0("%", max.nchar.nc, ".", ndec, "f"),
                admisc::asNumeric(nms[c])
            )

            if (grepl("[.]", nmsc)) {
                nmsc <- paste(
                    unlist(strsplit(nmsc, split = "[.]"))[1],
                    paste(rep(" ", ndec), collapse = "")
                )
            }

            nms[c] <- nmsc
        }
    }

    x[nax] <- ""


    max.nchars <- max(nchar(c(encodeString(nms), x)), na.rm = TRUE)
    for (i in seq(length(nms))) {
        if (nchar(nms[i]) < max.nchars) {
            nms[i] <- pad_both(nms[i], max.nchars - nchar(nms[i]))
        }
    }

    for (i in seq(length(x))) {
        if (nchar(x[i]) < max.nchars) {
            x[i] <- pad_both(x[i], max.nchars - nchar(x[i]))
        }
    }

    colnames(x) <- nms

    cat(ifelse(startend, "\n", ""))
    print(noquote(x))
    cat(ifelse(startend, "\n", ""))
}


`countdec` <- function(x, each = FALSE, na.rm = TRUE) {

    pN <- admisc::possibleNumeric(x, each = each)

    if ((each & sum(pN) == 0) || (!each & !pN)) {
        stopError("'x' values should be numeric.")
    }

    result <- rep(NA, length(x))
    x <- admisc::asNumeric(x)
    hasdec <- admisc::agtb(x %% 1, 0)

    if (any(hasdec, na.rm = TRUE)) {
        if (each) {
            for (i in seq(length(x))) {
                if (pN[i]) {
                    result[i] <- 0
                    if (hasdec[i]) {
                        xi <- format(x[i], scientific = FALSE)
                        result[i] <- nchar(unlist(strsplit(xi, split = "[.]"))[2])
                    }
                }
            }

            return(result)
        }

        if (na.rm) {
            x <- na.omit(x)
        }

        if (any(is.na(x))) {
            return(NA)
        }
        
        x <- format(x, scientific = FALSE)
        return(nchar(unlist(strsplit(x, split = "[.]"))[2]))
    }

    if (each) {
        result[pN] <- 0
        return(result)
    }

    if ((each & sum(pN) == length(x)) || pN) {
        return(0)
    }

    return(NA)
}


`pad_left` <- function(x, n) {
    paste(c(rep(" ", n), x), collapse = "", sep = "")
}

`pad_right` <- function(x, n) {
    paste(c(x, rep(" ", n)), collapse = "", sep = "")
}

`pad_both` <- function(x, n) {
    n1 <- ceiling(n/2)
    n2 <- floor(n/2)
    paste(c(rep(" ", n1), x, rep(" ", n2)), collapse = "", sep = "")
}
