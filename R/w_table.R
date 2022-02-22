`w_table` <- function(
    x, y = NULL, wt = NULL, values = TRUE, valid = TRUE, observed = FALSE,
    margin = NULL
) {
    
    if (inherits(x, "haven_labelled")) {
        x <- as_declared(x)
    }
    
    if (!is.atomic(x)) {
        admisc::stopError("'x' should be an atomic vector.")
    }

    xvallab <- yvallab <- NULL
    xna_values <- yna_values <- NULL
    xvalues <- yvalues <- TRUE
    crosstab <- !is.null(y)

    valid <- isTRUE(valid) && any(is.na(x))
    
    if (inherits(x, "declared")) {
        xvallab <- names_values(x) # arranges missing values at the end
        xna_values <- attr(xvallab, "missing")
        # x <- factor(to_labels(x), levels = names(xvallab))
        # sometimes (e.g. ISCO codifications in ESS) there are identical labels
        # with different values, and factor() complains with overlapping levels
        x <- factor(
            paste(to_labels(x), undeclare(x), sep = "_-_"),
            levels = paste(names(xvallab), xvallab, sep = "_-_")
        )
    }
    else {
        xvalues <- FALSE
        lvls <- levels(as.factor(x))
        xvallab <- seq(length(lvls))
        names(xvallab) <- lvls
    }
return(xvallab)
    xy <- list(x = x)

    if (crosstab) {
        if (!is.atomic(y)) {
            admisc::stopError("'y' should be an atomic vector.")
        }

        if (length(x) != length(y)) {
            admisc::stopError("Lengths of 'x' and 'y' differ.")
        }

        if (inherits(y, "declared")) {
            yvallab <- names_values(y)
            yna_values <- attr(yvallab, "missing")
            y <- factor(
                paste(to_labels(y), undeclare(y), sep = "_-_"),
                levels = paste(names(yvallab), yvallab, sep = "_-_")
            )
        }
        else {
            yvalues <- FALSE
            lvls <- levels(as.factor(y))
            yvallab <- seq(length(lvls))
            names(yvallab) <- lvls
        }

        xy$y <- y
    }

    if (is.null(wt)) {
        wt <- rep(1, length(x))
    }
    
    
    if (!(is.atomic(wt) && all(is.finite(wt)))) {
        admisc::stopError("'wt' should be an atomic vector with finite values.")
    }

    if (length(x) != length(wt)) {
        admisc::stopError("Lengths of 'x' and 'wt' differ.")
    }

    
    
    tbl <- as.matrix(tapply(wt, xy, sum))
    dimnames(tbl) <- unname(dimnames(tbl))
    
    tbl[is.na(tbl)] <- 0
    rs <- rowSums(tbl)
    cs <- colSums(tbl)
    
    if (isTRUE(observed)) {
        if (crosstab) {
            xvallab <- xvallab[rs > 0]
            yvallab <- yvallab[cs > 0]
            tbl <- tbl[rs > 0, , drop = FALSE]
            tbl <- tbl[, cs > 0, drop = FALSE]
            rs <- rs[rs > 0]
            cs <- cs[cs > 0]
        }
        else {
            tbl <- tbl[rs > 0, , drop = FALSE]
            xvallab <- xvallab[rs > 0]
        }
    }

    if (crosstab) {
        res <- round(tbl, 0)
        
        if (length(margin)) {
            if (!is.numeric(margin) || !is.element(margin, 0:2)) {
                admisc::stopError("'margin' should be a number between 0, 1 and 2.")
            }

            res <- switch(margin + 1,
                prop.table(res),
                prop.table(res, 1),
                prop.table(res, 2)
            )
        }
        
        if (isTRUE(values)) {
            rownames(res) <- gsub("_-_", " ", rownames(res))
            colnames(res) <- gsub("_-_", " ", colnames(res))
        }
        else {
            labels <- rownames(res)
            labels <- unlist(lapply(strsplit(labels, split = "_-_"), "[[", 1))
            rownames(res) <- labels
            labels <- colnames(res)
            labels <- unlist(lapply(strsplit(labels, split = "_-_"), "[[", 1))
            colnames(res) <- labels
        }
        
        if (is.null(margin) || margin != 1) {
            res <- rbind(res, Total = colSums(res))
        }

        if (is.null(margin) || margin != 2) {
            res <- cbind(res, Total = rowSums(res))
        }

        if (length(margin)) {
            res <- round(100 * res, 1)
        }
    }
    else {
        labels <- rownames(tbl)
        labels <- unlist(lapply(strsplit(labels, split = "_-_"), "[[", 1))
        
        if (any(is.na(x))) {
            tbl <- c(tbl, sum(is.na(x)))
            labels <- c(labels, NA)
        }

        res <- data.frame(fre = round(as.vector(tbl), 0))

        res$rel <- prop.table(res$fre)
        res$per <- res$rel * 100

        if (valid & (length(missing) > 0 | any(is.na(labels)))) {
            vld <- res$fre
            nalabels <- is.element(xvallab, xna_values)
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
        attr(res, "values") <- as.vector(xvallab)
        attr(res, "show_values") <- values & xvalues
        attr(res, "na_values") <- xna_values
        attr(res, "valid") <- valid
        class(res) <- c("w_table", "data.frame")
    }

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
