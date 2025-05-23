#' Weighted summaries
#' @name weighted
#'
#' @title Compute weighted summaries for declared objects
#'
#' @description
#' Functions to compute weighted tables or summaries, based on a vector of
#' frequency weights. These are reimplementations of various existing functions,
#' adapted to objects of class \code{"declared"} (see Details below)
#'
#' @returns
#' A vector of (weighted) values.
#'
#' @details
#' A frequency table is usually performed for a categorical variable, displaying
#' the frequencies of the respective categories. Note that general variables
#' containing text are not necessarily factors, despite having a small number of
#' characters.
#'
#' A general table of frequencies, using the base function `table()`, ignores
#' the defined missing values (which are all stored as NAs). The
#' reimplementation of this function in `wtable()` takes care of this detail,
#' and presents frequencies for each separately defined missing values. Similar
#' reimplementations for the other functions have the same underlying objective.
#'
#' It is also possible to perform a frequency table for numerical variables, if
#' the number of values is limited (an arbitrary and debatable upper limit of 15
#' is used). An example of such variable can be the number of children, where
#' each value can be interpreted as a class, containing a single value (for
#' instance 0 meaning the category of people with no children).
#'
#' Objects of class `declared` are not pure categorical variables (R factors)
#' but they are nevertheless interpreted as if they were factors, to allow
#' producing frequency tables. Given the high similarity with package
#' **`haven`**, objects of class `haven_labelled_spss` are automatically
#' coerced to objects of class `declared` and treated accordingly.
#'
#' The argument `values` makes sense only when the input is of family class
#' `declared`, otherwise for regular (base R) factors the values are
#' just a sequence of numbers.
#'
#' The later introduced argument `observed` is useful in situations when a
#' variable has a very large number of potential values, and a smaller subset of
#' actually observed ones. As an example, the variable \dQuote{Occupation} has
#' hundreds of possible values in the ISCO08 codelist, and not all of them might
#' be actually observed. When activated, this argument restricts the printed
#' frequency table to the subset of observed values only.
#'
#' The argument `method` can be one of `"unbiased"` or `"ML"`.
#'
#' When this is set to `"unbiased"`, the result is an unbiased estimate
#' using Bessel's correction. When this is set to `"ML"`, the result is the
#' maximum likelihood estimate for a Gaussian distribution.
#'
#' The argument `wt` refers only to frequency weights. Users should be
#' aware of the differences between frequency weights, analytic weights,
#' probability weights, design weights, post-stratification weights etc. For
#' purposes of inferential testing, Thomas Lumley's package **`survey`**
#' should be employed.
#'
#' If no frequency weights are provided, the result is identical to the
#' corresponding base functions.
#'
#' The function `wquantile()` extensively borrowed ideas from packages
#' **`stats`** and **`Hmisc`**, to ensure a constant interpolation that would
#' produce the same quantiles if no weights are provided or if all
#' weights are equal to 1.
#'
#' Other arguments can be passed to the stats function `quantile()` via the
#' three dots `...` argument, and their extensive explanation is found in the
#' corresponding stats function's help page.
#'
#' For all functions, the argument `na.rm` refers to the empty missing values
#' and its default is set to TRUE. The declared missing values are automatically
#' eliminated from the summary statistics, even if this argument is deactivated.
#'
#' The function `wmode()` returns the weighted mode of a variable. Unlike the
#' other functions where the prefix `w` signals a weighted version of the
#' base function with the same name, this has nothing to do with the base
#' function `mode()` which refers to the storage mode / type of an R object.
#'
#' @examples
#' set.seed(215)
#'
#' # a pure categorical variable
#' x <- factor(sample(letters[1:5], 215, replace = TRUE))
#' wtable(x)
#'
#'
#' # simulate number of children
#' x <- sample(0:4, 215, replace = TRUE)
#' wtable(x)
#'
#' # simulate a Likert type response scale from 1 to 7
#' values <- sample(c(1:7, -91), 215, replace = TRUE)
#' x <- declared(values, labels = c("Good" = 1, "Bad" = 7))
#' wtable(x)
#'
#'
#' # Defining missing values
#' missing_values(x) <- -91
#' wtable(x)
#'
#'
#' # Defined missing values with labels
#' values <- sample(c(1:7, -91, NA), 215, replace = TRUE)
#' x <- declared(
#'     values,
#'     labels = c("Good" = 1, "Bad" = 7, "Don't know" = -91),
#'     na_values = -91
#' )
#'
#' wtable(x)
#'
#' # Including the values in the table of frequencies
#' wtable(x, values = TRUE)
#'
#'
#' # An example involving multiple variables
#' DF <- data.frame(
#'     Area = declared(
#'         sample(1:2, 215, replace = TRUE, prob = c(0.45, 0.55)),
#'         labels = c(Rural = 1, Urban = 2)
#'     ),
#'     Gender = declared(
#'         sample(1:2, 215, replace = TRUE, prob = c(0.55, 0.45)),
#'         labels = c(Males = 1, Females = 2)
#'     ),
#'     Age = sample(18:90, 215, replace = TRUE),
#'     Children = sample(0:5, 215, replace = TRUE)
#' )
#'
#' wtable(DF$Gender)
#'
#' wsd(DF$Age)
#'
#'
#' # Weighting: observed proportions
#' op <- proportions(with(DF, table(Gender, Area)))
#'
#' # Theoretical proportions: 53% Rural, and 50.2% Females
#' tp <- rep(c(0.53, 0.47), each = 2) * rep(c(0.498, 0.502), 2)
#'
#' # Corrections by strata
#' fweights <- tp / op
#'
#' DF$fweight <- fweights[match(10 * DF$Area + DF$Gender, c(11, 12, 21, 22))]
#'
#' with(DF, wtable(Gender, wt = fweight))
#'
#' with(DF, wmean(Age, wt = fweight))
#'
#' with(DF, wquantile(Age, wt = fweight))
#' @author Adrian Dusa
#'
#' @param x A numeric vector for summaries, or declared / factor for frequency
#' tables
#'
#' @param y An optional variable, to create crosstabs; must have the same length
#' as x
#'
#' @param wt A numeric vector of frequency weights
#'
#' @param values Logical, print the values in the table rows
#'
#' @param valid Logical, print separate percent distribution for valid values,
#' if any missing values are present; for cross tables, use valid values only
#'
#' @param observed Logical, print the observed categories only
#'
#' @param margin Numeric, indicating the margin to calculate crosstab
#' proportions: 0 from the total, 1 from row totals and 2 from column totals
#'
#' @param vlabel Logical, print the variable label, if existing
#' @export
`wtable` <- function (
    x, y = NULL, wt = NULL, values = TRUE, valid = TRUE, observed = TRUE,
    margin = NULL, vlabel = FALSE
) {

    funargs <- lapply(
        lapply(
            match.call(), deparse)[-1],
            function(x) gsub("'|\"|[[:space:]]", "", x
        )
    )

    nmx <- getName_ (funargs$x)

    if (inherits (x, "haven_labelled")) {
        x <- as.declared (x)
    }

    if (is.null (x) || !is.atomic (x)) {
        stopError_ ("'x' should be an atomic vector.")
    }

    xvallab <- yvallab <- NULL
    xna_values <- yna_values <- NULL
    xvalues <- yvalues <- TRUE
    crosstab <- !is.null (y)

    if (!crosstab) {
        valid <- isTRUE (valid) && any (is.na (x))
    }

    xlabel <- attr (x, "label", exact = TRUE)
    allnax <- all (is.na (x))

    if (inherits (x, "declared")) {
        allnax <- all (is.empty (x))
        if (!allnax) {
            # names_values () arranges missing values at the end
            xvallab <- names_values (
                x,
                drop_na = isTRUE (valid) & crosstab,
                observed = observed
            )

            xna_values <- attr (xvallab, "missing")
            # x <- factor (as.character (x), levels = names (xvallab))
            # sometimes (e.g. ISCO codifications in ESS) there are identical labels
            # with different values, and factor () complains with overlapping levels

            xvalues <- !identical (names (xvallab), as.character (xvallab))
            # print (head(paste (as.character (x), undeclare (x), sep = "_-_")))
            x <- factor (
                paste (
                    undeclare (x, drop = TRUE),
                    as.character (undeclare (x)),
                    sep = "_-_"
                ),
                levels = paste (xvallab, names (xvallab), sep = "_-_")
            )
        }
    }
    else {
        xvalues <- FALSE
        if (!allnax) {
            lvls <- levels (as.factor (x))
            xvallab <- seq (length (lvls))
            names (xvallab) <- lvls
        }
    }

    xy <- list (x = x)
    if (crosstab) {
        if (!is.atomic (y)) {
            stopError_ ("'y' should be an atomic vector.")
        }

        if (length (x) != length (y)) {
            stopError_ ("Lengths of 'x' and 'y' differ.")
        }

        ylabel <- attr (y, "label", exact = TRUE)
        allnay <- all (is.na (y))

        nmy <- getName_ (funargs$y)

        ncharx <- nchar (nmx)
        nchary <- nchar (nmy)
        if (length(c (ncharx + nchary)) > 0) {
            nchars <- max (nchar (c (nmx, nmy)))
        }

        if (length(ncharx) > 0 && ncharx > 0) {
            nmx <- padLeft_ (nmx, nchars - ncharx)
        }

        if (length(nchary) > 0 && nchary > 0) {
            nmy <- padLeft_ (nmy, nchars - nchary)
        }

        if (inherits (y, "declared")) {
            allnay <- all (is.empty (y))
            if (!allnay) {
                yvallab <- names_values (
                    y,
                    drop_na = crosstab && isTRUE (valid),
                    observed = observed
                )
                yna_values <- attr (yvallab, "missing")

                y <- factor (
                    paste (
                        undeclare (y, drop = TRUE),
                        as.character (undeclare (y)),
                        sep = "_-_"
                    ),
                    levels = paste (yvallab, names (yvallab), sep = "_-_")
                )
            }
        }
        else {
            yvalues <- FALSE
            if (!allnay) {
                lvls <- levels (as.factor (y))
                yvallab <- seq (length (lvls))
                names (yvallab) <- lvls
            }
        }

        xy$y <- y
    }

    if (is.null (wt)) {
        wt <- rep (1, length (x))
    }


    if (
        !is.null (wt) && !(
            is.atomic (wt) && all (is.finite (na.omit (wt)))
        )
    ) {
        stopError_ ("'wt' should be an atomic vector with finite values.")
    }

    if (length (x) != length (wt)) {
        stopError_ ("Lengths of 'x' and 'wt' differ.")
    }


    orig <- round (tapply (wt, xy, sum, na.rm = TRUE), 0)
    orig[is.na (orig)] <- 0
    tbl <- as.matrix(orig)
    dimnames (tbl) <- unname (dimnames (tbl))

    rs <- rowSums (tbl)
    cs <- colSums (tbl)

    if (isTRUE (observed)) {
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

        if (length (margin)) {
            if (!is.numeric (margin) || !is.element (margin, 0:2)) {
                stopError_ ("'margin' should be a number between 0, 1 and 2.")
            }

            toprint <- switch(margin + 1,
                proportions (toprint),
                proportions (toprint, 1),
                proportions (toprint, 2)
            )
        }

        if (is.null (margin) || margin != 1) {
            toprint <- rbind (toprint, Total = colSums (toprint))
        }

        if (is.null (margin) || margin != 2) {
            toprint <- cbind (toprint, Total = rowSums (toprint))
        }

        if (length (margin)) {
            toprint <- round(100 * toprint, 1)
        }

        if (isTRUE (vlabel)) {
            if (!is.null (xlabel) & !is.null (ylabel)) {
                attr (toprint, "xlabel") <- paste(nmx, xlabel, sep = ": ")
                attr (toprint, "ylabel") <- paste(nmy, ylabel, sep = ": ")
            } else {
                message("Variable label(s) not available.")
            }
        }
        attr (toprint, "xvalues") <- isTRUE (values) & xvalues
        attr (toprint, "yvalues") <- isTRUE (values) & yvalues

        # class (toprint) <- c ("wtable", "matrix")
    }
    else {
        labels <- NULL
        if (nrow(tbl) > 0) {
            labels <- rownames (tbl)
            labels <- sapply (strsplit (labels, split = "_-_"), tail, 1)
        }

        if (any (is.na (x))) {
            tbl <- c (tbl, sum (is.na (x)))
            labels <- c (labels, NA)
        }

        if (isTRUE (observed)) {
            tbl <- tbl[tbl > 0]
        }

        toprint <- data.frame (fre = tbl)

        toprint$rel <- proportions (toprint$fre)
        toprint$per <- toprint$rel * 100

        if (valid & (length (missing) > 0 | any (is.na (labels)))) {
            vld <- toprint$fre
            nalabels <- is.element (xvallab, xna_values)
            vld[nalabels] <- NA
            vld[is.na (labels)] <- NA

            lna <- seq (length (nalabels))

            vld[seq (sum (!nalabels))] <- 100 * proportions (
                vld[seq (sum (!nalabels))]
            )

            toprint$vld <- NA
            toprint$vld[seq (length (vld))] <- vld
            toprint$cpd <- NA
            toprint$cpd[seq (length (vld))] <- cumsum (vld)
        }
        else {
            valid <- FALSE
            toprint$cpd <- cumsum (toprint$per)
        }

        if (isTRUE (vlabel)) {
            attr (toprint, "xlabel") <- paste(nmx, xlabel, sep = ": ")
        }
        attr (toprint, "labels") <- labels
        attr (toprint, "values") <- as.vector (xvallab)
        attr (toprint, "show_values") <- values & xvalues
        attr (toprint, "na_values") <- xna_values
        attr (toprint, "valid") <- valid
    }

    if (is.matrix(orig)) {
        rownames (orig) <- names (xvallab)
        colnames (orig) <- names (yvallab)
    }
    else {
        names (orig) <- names (xvallab)
    }



    attr (orig, "toprint") <- toprint
    class (orig) <- c ("wtable", class (orig))
    return (orig)
}

#' @export
`w_table` <- function (...) {
    wtable(...)
}