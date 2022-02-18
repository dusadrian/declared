`w_table` <- function(
    x, wt = NULL, values = TRUE, valid = NA, observed = FALSE
) {
    
    if (inherits(x, "haven_labelled")) {
        x <- as_declared(x)
    }
    
    if (!is.atomic(x)) {
        admisc::stopError("'x' should be an atomic vector.")
    }

    vals <- NULL
    vallab <- NULL
    na_values <- NULL

    if (is.na(valid)) {
        valid <- any(is.na(x))
    }
    
    if (inherits(x, "declared")) {
        vallab <- names_values(x) # arranges missing values at the end
        na_values <- attr(vallab, "missing")
        # x <- factor(to_labels(x), levels = names(vallab))
        # sometimes (e.g. ISCO codifications in ESS) there are identical labels
        # with different values, and factor() complains with overlapping levels
        x <- factor(
            paste(to_labels(x), undeclare(x), sep = "_-_"),
            levels = paste(names(vallab), vallab, sep = "_-_")
        )
    }
    else {
        values <- FALSE
        lvls <- levels(as.factor(x))
        vallab <- seq(length(lvls))
        names(vallab) <- lvls
    }

    if (is.null(wt)) {
        wt <- rep(1, length(x))
    }

    
    if (!is.atomic(wt) || !is.finite(x)) {
        admisc::stopError("'wt' should be an atomic vector with finite values.")
    }

    if (length(x) != length(wt)) {
        admisc::stopError("Lengths of 'x' and 'wt' differ.")
    }
    
    # tbl <- table(x)
    tbl <- round(tapply(wt, x, sum, simplify = TRUE), 0)
    tblzero <- is.na(tbl)
    
    if (observed) {
        tbl <- tbl[!tblzero]
        vallab <- vallab[!tblzero]
    }
    else {
        tbl[tblzero] <- 0
    }

    labels <- names(tbl)
    labels <- unlist(lapply(strsplit(labels, split = "_-_"), "[[", 1))
    if (any(is.na(x))) {
        tbl <- c(tbl, sum(is.na(x)))
        labels <- c(labels, NA)
    }

    res <- data.frame(fre = as.vector(tbl))

    res$rel <- prop.table(res$fre)
    res$per <- res$rel * 100

    if (valid & (length(missing) > 0 | any(is.na(labels)))) {
        vld <- res$fre
        nalabels <- is.element(vallab, na_values)
        vld[nalabels] <- NA

        if (!observed) {
            vld[!nalabels & res$fre == 0] <- 0
        }

        vld[!nalabels & res$fre != 0] <- 100 * prop.table(vld[!nalabels & res$fre != 0])
        
        res$vld <- NA
        res$vld[seq(length(vld))] <- vld
        res$cpd <- NA
        res$cpd[seq(length(vld))] <- cumsum(vld)
    }
    else {
        valid <- FALSE
        res$cpd <- cumsum(res$per)
    }

    attr(res, "labels") <- labels
    attr(res, "values") <- as.vector(vallab)
    attr(res, "show_values") <- values
    attr(res, "na_values") <- na_values
    attr(res, "valid") <- valid
    class(res) <- c("w_table", "data.frame")
    return(res)
}



`print.w_table` <- function(x, force = FALSE, startend = TRUE, ...) {
    
    irv <- c(194, 180)
    tick <- unlist(strsplit(rawToChar(as.raw(irv)), split = ""))

    show_values <- attr(x, "show_values")
    valid <- attr(x, "valid")
    dots <- list(...)
    if (is.element("show_values", names(dots))) {
        show_values <- dots$show_values
    }

    values <- gsub(paste(tick, collapse = "|"), "'", attr(x, "values"))
    labels <- gsub(paste(tick, collapse = "|"), "'", attr(x, "labels"))
    na_values <- gsub(paste(tick, collapse = "|"), "'", attr(x, "na_values"))

    first_missing <- Inf
    if (any(is.element(values, na_values))) {
        first_missing <- which(is.element(values, na_values))[1]
    }

    if (first_missing == Inf && is.na(labels[length(labels)])) {
        first_missing <- length(labels)
    }
    
    rnms <- labels
    
    if (show_values) {
        values <- formatC(as.character(values), digits = max(nchar(values)) - 1, flag = " ")
        labels[!is.na(labels)][values == labels[!is.na(labels)]] <- ""
        rnms[!is.na(labels)] <- paste(labels[!is.na(labels)], values, sep = " ")
    }

    rnms[is.na(labels)] <- "NA"
    
    max.nchar.cases <- max(nchar(encodeString(rnms)))
    # rnms <- sprintf(paste0("% ", max.nchar.cases, "s"), rnms)
    for (i in seq(length(rnms))) {
        if (nchar(rnms[i]) < max.nchar.cases) {
            rnms[i] <- paste(c(rep(" ", max.nchar.cases - nchar(rnms[i])), rnms[i]), collapse = "", sep = "")
        }
    }

    sums <- colSums(x[, 1:3])

    fres <- formatC(as.character(c(x$fre, sums[1])), format = "s")
    fres <- paste(sprintf(paste("%", max(3, nchar(sums[1])), "s", sep = ""), fres), "")
    # fres <- format(c(paste(rep(1, max(4, nchar(sums[1]))), collapse = ""), fres), justify = "centre")[-1]
    x$rel <- formatC(x$rel, digits = 3, format = "f")
    rel <- sprintf("% 5s", x$rel)
    x$per <- formatC(x$per, digits = 1, format = "f")
    if (valid & is.element("vld", names(x))) {
        x$vld <- formatC(x$vld, digits = 1, format = "f")
        vld <- sprintf(paste("% 5s", sep = ""), x$vld)
    }
    per <- sprintf("% 5s", c(x$per, sums[3]))
    cpd <- formatC(x$cpd, digits = 1, format = "f")
    cpd <- sprintf(paste("% 5s", sep = ""), cpd)
    
    miseparator <- paste(
        c(
            rep(" ", ifelse(max.nchar.cases > 5, max.nchar.cases - 5, 0)),
            rep("-", min(max.nchar.cases, 5) + 1 * (sums[1] >= 1000)),
            "\n"
        ),
        collapse = ""
    )
    separator <- paste(
        c(
            rep(" ", max.nchar.cases + 1),
            rep("-", nchar(sums[1])),
            ifelse(
                nchar(sums[1]) < 3, 
                paste(
                    rep("-", 3 - nchar(sums[1])),
                    collapse = ""
                ),
                ""
            ),
            sprintf("-------------------%s\n", ifelse(valid, "------", ""))
        ),
        collapse = ""
    )

    if (nrow(x) > 100 & !force) {
        admisc::stopError("It looks like a lot of categories. If you really want to print it, use:\nprint(x, force = TRUE)")
    }

    cat(ifelse(startend, "\n", ""))

    cat(
        paste(
            rep(
                " ",
                max.nchar.cases + ifelse(nchar(sums[1]) > 4, nchar(sums[1]) - 4, 0)
            ),
            collapse = ""
        ),
        sprintf(ifelse(
            sums[1] < 1000,
            "fre    rel   per   %scpd\n",
            " fre    rel   per   %scpd\n"
        ), ifelse(valid, "vld   ", ""))
    )

    cat(separator)

    for (i in seq(nrow(x))) {
        if (first_missing == i) {
            cat(miseparator)
        }

        if (valid) {
            if (i < first_missing) {
                cat(rnms[i], fres[i], rel[i], per[i], vld[i], cpd[i], "\n")
            }
            else {
                cat(rnms[i], fres[i], rel[i], per[i], "\n")
            }
        }
        else {
            cat(rnms[i], fres[i], rel[i], per[i], cpd[i], "\n")
        }
    }

    cat(separator)

    # cat(paste(rep(" ", max.nchar.cases), sep = ""), " ", sprintf(paste("% ", max(4, nchar(sums[1])), "s", sep = ""), sums[1]), " 1.000 100.0\n", sep = "")
    cat(
        paste(
            rep(" ", max.nchar.cases),
            sep = ""
        ),
        " ",
        fres[length(fres)],
        " 1.000 100.0\n",
        sep = ""
    )

    cat(ifelse(startend, "\n", ""))
}




`frtable` <- function(...) {
    .Deprecated(msg = "Function frtable() is deprecated, and has been renamed to w_table()\n")
    w_table(...)
}
