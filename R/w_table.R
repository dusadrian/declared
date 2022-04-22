`w_table` <- function(
    x, y = NULL, wt = NULL, values = FALSE, valid = TRUE, observed = TRUE,
    margin = NULL
) {
    
    if (inherits(x, "haven_labelled")) {
        x <- as.declared(x)
    }
    
    if (!is.atomic(x)) {
        stopError_("'x' should be an atomic vector.")
    }

    xvallab <- yvallab <- NULL
    xna_values <- yna_values <- NULL
    xvalues <- yvalues <- TRUE
    crosstab <- !is.null(y)

    valid <- isTRUE(valid) && any(is.na(x))
    
    if (inherits(x, "declared")) {
        xvallab <- names_values(x) # arranges missing values at the end
        xna_values <- attr(xvallab, "missing")
        # x <- factor(as.character(x), levels = names(xvallab))
        # sometimes (e.g. ISCO codifications in ESS) there are identical labels
        # with different values, and factor() complains with overlapping levels
        
        xvalues <- !identical(names(xvallab), as.character(xvallab))
        # print(head(paste(as.character(x), undeclare(x), sep = "_-_")))
        
        x <- factor(
            paste(
                as.character(x),
                undeclare(x, drop = TRUE),
                sep = "_-_"
            ),
            levels = paste(names(xvallab), xvallab, sep = "_-_")
        )
    }
    else {
        xvalues <- FALSE
        lvls <- levels(as.factor(x))
        xvallab <- seq(length(lvls))
        names(xvallab) <- lvls
    }

    xy <- list(x = x)
    if (crosstab) {
        if (!is.atomic(y)) {
            stopError_("'y' should be an atomic vector.")
        }

        if (length(x) != length(y)) {
            stopError_("Lengths of 'x' and 'y' differ.")
        }

        if (inherits(y, "declared")) {
            yvallab <- names_values(y)
            yna_values <- attr(yvallab, "missing")
            y <- factor(
                paste(
                    as.character(y),
                    undeclare(y, drop = TRUE),
                    sep = "_-_"
                ),
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
    
    
    if (!(is.atomic(wt) && all(is.finite(na.omit(wt))))) {
        stopError_("'wt' should be an atomic vector with finite values.")
    }

    if (length(x) != length(wt)) {
        stopError_("Lengths of 'x' and 'wt' differ.")
    }

    
    orig <- round(tapply(wt, xy, sum, na.rm = TRUE), 0)
    orig[is.na(orig)] <- 0
    tbl <- as.matrix(orig)
    dimnames(tbl) <- unname(dimnames(tbl))
    
    rs <- rowSums(tbl)
    cs <- colSums(tbl)
    
    if (isTRUE(observed)) {
        if (crosstab) {
            xvallab <- xvallab[rs > 0]
            yvallab <- yvallab[cs > 0]
            orig <- orig[rs > 0, , drop = FALSE]
            orig <- orig[, cs > 0, drop = FALSE]
            tbl <- tbl[rs > 0, , drop = FALSE]
            tbl <- tbl[, cs > 0, drop = FALSE]
            rs <- rs[rs > 0]
            cs <- cs[cs > 0]
        }
        else {
            orig <- orig[rs > 0]
            tbl <- tbl[rs > 0, , drop = FALSE]
            xvallab <- xvallab[rs > 0]
        }
    }

    if (crosstab) {
        toprint <- tbl
        
        if (length(margin)) {
            if (!is.numeric(margin) || !is.element(margin, 0:2)) {
                stopError_("'margin' should be a number between 0, 1 and 2.")
            }

            toprint <- switch(margin + 1,
                proportions(toprint),
                proportions(toprint, 1),
                proportions(toprint, 2)
            )
        }
        
        if (is.null(margin) || margin != 1) {
            toprint <- rbind(toprint, Total = colSums(toprint))
        }

        if (is.null(margin) || margin != 2) {
            toprint <- cbind(toprint, Total = rowSums(toprint))
        }

        if (length(margin)) {
            toprint <- round(100 * toprint, 1)
        }

        attr(toprint, "xvalues") <- isTRUE(values) & xvalues
        attr(toprint, "yvalues") <- isTRUE(values) & yvalues

        # class(toprint) <- c("w_table", "matrix")
    }
    else {
        labels <- rownames(tbl)
        labels <- unlist(lapply(strsplit(labels, split = "_-_"), "[[", 1))
        
        if (any(is.na(x))) {
            tbl <- c(tbl, sum(is.na(x)))
            labels <- c(labels, NA)
        }

        if (isTRUE(observed)) {
            tbl <- tbl[tbl > 0]
        }

        toprint <- data.frame(fre = tbl)

        toprint$rel <- proportions(toprint$fre)
        toprint$per <- toprint$rel * 100

        if (valid & (length(missing) > 0 | any(is.na(labels)))) {
            vld <- toprint$fre
            nalabels <- is.element(xvallab, xna_values)
            vld[nalabels] <- NA
            vld[is.na(labels)] <- NA
            
            lna <- seq(length(nalabels))

            vld[seq(sum(!nalabels))] <- 100 * proportions(
                vld[seq(sum(!nalabels))]
            )
            
            toprint$vld <- NA
            toprint$vld[seq(length(vld))] <- vld
            toprint$cpd <- NA
            toprint$cpd[seq(length(vld))] <- cumsum(vld)
        }
        else {
            valid <- FALSE
            toprint$cpd <- cumsum(toprint$per)
        }

        attr(toprint, "labels") <- labels
        attr(toprint, "values") <- as.vector(xvallab)
        attr(toprint, "show_values") <- values & xvalues
        attr(toprint, "na_values") <- xna_values
        attr(toprint, "valid") <- valid
    }
    
    if (is.matrix(orig)) {
        rownames(orig) <- names(xvallab)
        colnames(orig) <- names(yvallab)
    }
    else {
        names(orig) <- names(xvallab)
    }

    
    
    attr(orig, "toprint") <- toprint
    class(orig) <- c("w_table", class(orig))
    return(orig)
}
