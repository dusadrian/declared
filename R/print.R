#' @export
`print.declared` <- function (x, ...) {
    label <- label (x)
    if (!is.null (label)) {
        label <- paste ("", label)
    }

    measurement <- attr (x, "measurement")

    # type <- likely_type (x)
    cat (
        paste0 (
            "<declared",
            # ifelse (is.null (measurement), "", paste0 (", ", measurement)),
            # ifelse (is.null (type), "", paste0 (", ", type)),
            likely_type (x),
            "[", length (x), "]>",
            label,
            "\n"
        )
    )
    if (length (x) > 0) {
        print (noquote (format_declared (x)), ...)

        na_values <- attr (x, "na_values")
        if (!is.null (na_values)) {
            cat (paste0 (
                "Missing values: ", paste (na_values, collapse = ", "), "\n"
            ))
        }

        na_range <- attr (x, "na_range")
        if (!is.null (na_range)) {
            cat (paste0 (
                "Missing range:  [",
                paste (na_range, collapse = ", "),
                "]\n"
            ))
        }

        labels <- attr (x, "labels", exact = TRUE)

        if (length (labels) == 0) {
            return (invisible(x))
        }

        cat ("\nLabels:", "\n", sep = "")

        print (
            data.frame (
                value = unname (labels),
                label = names (labels),
                row.names = NULL
            ),
            row.names = FALSE
        )

        return (invisible(x))
    }
}


#' @export
`print.w_table` <- function (x, force = FALSE, startend = TRUE, ...) {
    toprint <- attr (x, "toprint")
    achar <- rawToChar (as.raw (c (195, 130)))
    irv <- c (194, 180)
    tick <- unlist (strsplit (rawToChar (as.raw (irv)), split = ""))
    tick <- c (paste0 (achar, "'"), paste0 (achar, tick), tick)

    if (x[1] != as.matrix(toprint)[1]) {
        # this means the original table was altered. e.g. proportions (tbl)
        class (x) <- setdiff (class (x), c ("w_table", "array"))
        names (dimnames (x)) <- NULL
        attr (x, "toprint") <- NULL
        rownames (x) <- gsub (paste (tick, collapse = "|"), "'", rownames (x))
        if (length (dimnames (x)) == 2) {
            colnames (x) <- gsub (
                paste (tick, collapse = "|"), "'", colnames (x)
            )
        }
        print (x)
    }
    else {
        x <- toprint

        if (is.matrix(x)) { # crosstab

            rnms <- strsplit (rownames (x), split = "_-_")

            xlabels <- unlist (lapply (rnms, "[[", 1))
            max.nchar.xlabels <- max (nchar (encodeString (xlabels)))
            for (i in seq (length (xlabels))) {
                if (nchar (xlabels[i]) < max.nchar.xlabels) {
                    xlabels[i] <- padLeft_ (
                        xlabels[i], max.nchar.xlabels - nchar (xlabels[i])
                    )
                }
            }

            if (attr (x, "xvalues")) {
                # -length (rnms), because of "Total" which does not have a value
                xvalues <- unlist (lapply (rnms[-length (rnms)], "[[", 2))
                xvalues <- c (xvalues, "")

                max.nchar.xvalues <- max (nchar (encodeString (xvalues)))

                for (i in seq (length (xvalues) - 1)) {
                    if (nchar (xvalues[i]) < max.nchar.xvalues) {
                        xvalues[i] <- padLeft_ (
                            xvalues[i], max.nchar.xvalues - nchar (xvalues[i])
                        )
                    }
                }

                rnms <- paste (xlabels, xvalues)
            }
            else {
                rnms <- xlabels
            }

            rownames (x) <- rnms

            cnms <- colnames (x)
            if (attr (x, "yvalues")) {
                cnms <- gsub ("_-_", " ", cnms)
            } else {
                cnms <- unlist (lapply (
                    strsplit (cnms, split = "_-_"),
                    "[[",
                    1
                ))
            }

            max.nchar.cols <- max (nchar (c (encodeString (cnms), x)))
            for (i in seq (length (cnms))) {
                if (nchar (cnms[i]) < max.nchar.cols) {
                    cnms[i] <- padBoth_ (
                        cnms[i], max.nchar.cols - nchar (cnms[i])
                    )
                }
            }
            colnames (x) <- cnms

            for (i in seq (length (x))) {
                if (nchar (x[i]) < max.nchar.cols) {
                    x[i] <- padBoth_ (x[i], max.nchar.cols - nchar (x[i]))
                }
            }

            class (x) <- setdiff (class (x), c ("w_table", "array"))
            attr (x, "xvalues") <- NULL
            attr (x, "yvalues") <- NULL
            rownames (x) <- gsub (
                paste (tick, collapse = "|"), "'", rownames (x)
            )
            colnames (x) <- gsub (
                paste (tick, collapse = "|"), "'", colnames (x)
            )
            cat (ifelse (startend, "\n", ""))
            print (noquote (x))
            cat (ifelse (startend, "\n", ""))
        }
        else {
            show_values <- attr (x, "show_values")
            valid <- attr (x, "valid")
            dots <- list (...)
            if (is.element ("show_values", names (dots))) {
                show_values <- dots$show_values
            }

            values <- gsub (
                paste (tick, collapse = "|"), "'", attr (x, "values")
            )
            labels <- gsub (
                paste (tick, collapse = "|"), "'", attr (x, "labels")
            )
            na_values <- gsub (
                paste (tick, collapse = "|"), "'", attr (x, "na_values")
            )

            first_missing <- Inf
            if (any (is.element (values, na_values))) {
                first_missing <- which (is.element (values, na_values))[1]
            }

            if (first_missing == Inf && is.na (labels[length (labels)])) {
                first_missing <- length (labels)
            }

            rnms <- labels

            if (show_values) {
                values <- formatC (
                    as.character (values),
                    digits = max (nchar (values)) - 1,
                    flag = " "
                )
                labels[!is.na (labels)][values == labels[!is.na (labels)]] <- ""
                rnms[!is.na (labels)] <- paste (labels[!is.na (labels)], values)
            }

            rnms[is.na (labels)] <- "NA"

            max.nchar.cases <- max (nchar (encodeString (rnms)))
            # rnms <- sprintf (paste0 ("% ", max.nchar.cases, "s"), rnms)
            for (i in seq (length (rnms))) {
                if (nchar (rnms[i]) < max.nchar.cases) {
                    rnms[i] <- padLeft_ (
                        rnms[i], max.nchar.cases - nchar (rnms[i])
                    )
                    # rnms[i] <- paste (
                    #     c (
                    #      rep (" ", max.nchar.cases - nchar (rnms[i])), rnms[i]
                    #     ),
                    #     collapse = "", sep = ""
                    # )
                }
            }

            sums <- colSums (x[, 1:3])

            fres <- formatC (as.character (c (x$fre, sums[1])), format = "s")
            fres <- paste (
                sprintf (
                    paste ("%", max (3, nchar (sums[1])), "s", sep = ""),
                    fres
                ),
                ""
            )
            
            x$rel <- formatC (x$rel, digits = 3, format = "f")
            rel <- sprintf ("% 5s", x$rel)
            x$per <- formatC (x$per, digits = 1, format = "f")
            if (valid & is.element ("vld", names (x))) {
                x$vld <- formatC (x$vld, digits = 1, format = "f")
                vld <- sprintf (paste ("% 5s", sep = ""), x$vld)
            }
            per <- sprintf ("% 5s", c (x$per, sums[3]))
            cpd <- formatC (x$cpd, digits = 1, format = "f")
            cpd <- sprintf (paste ("% 5s", sep = ""), cpd)

            miseparator <- paste (
                c (
                    rep (
                        " ",
                        ifelse (max.nchar.cases > 5, max.nchar.cases - 5, 0)
                    ),
                    rep (
                        "-",
                        min (max.nchar.cases, 5) + 1 * (sums[1] >= 1000)
                    ),
                    "\n"
                ),
                collapse = ""
            )
            separator <- paste (
                c (
                    rep (" ", max.nchar.cases + 1),
                    rep ("-", nchar (sums[1])),
                    ifelse (
                        nchar (sums[1]) < 3,
                        paste (
                            rep ("-", 3 - nchar (sums[1])),
                            collapse = ""
                        ),
                        ""
                    ),
                    sprintf (
                        "-------------------%s\n",
                        ifelse (valid, "------", "")
                    )
                ),
                collapse = ""
            )

            if (nrow (x) > 100 & !force) {
                stopError_ (paste (
                    "It looks like a lot of categories. If you really want to",
                    "print it, use:\nprint(x, force = TRUE)"
                ))
            }

            cat (ifelse (startend, "\n", ""))

            cat (
                paste (
                    rep (
                        " ",
                        max.nchar.cases + ifelse (
                            nchar (sums[1]) > 4, nchar (sums[1]) - 4, 0
                        )
                    ),
                    collapse = ""
                ),
                sprintf (ifelse (
                    sums[1] < 1000,
                    "fre    rel   per   %scpd\n",
                    " fre    rel   per   %scpd\n"
                ), ifelse (valid, "vld   ", ""))
            )

            cat (separator)

            for (i in seq (nrow (x))) {
                if (first_missing == i) {
                    cat (miseparator)
                }

                if (valid) {
                    if (i < first_missing) {
                        cat (
                            rnms[i], fres[i], rel[i], per[i], vld[i], cpd[i],
                            "\n"
                        )
                    }
                    else {
                        cat (rnms[i], fres[i], rel[i], per[i], "\n")
                    }
                }
                else {
                    cat (rnms[i], fres[i], rel[i], per[i], cpd[i], "\n")
                }
            }

            cat (separator)

            cat (
                paste (
                    rep (" ", max.nchar.cases),
                    sep = ""
                ),
                " ",
                fres[length (fres)],
                " 1.000 100.0\n",
                sep = ""
            )

            cat (ifelse (startend, "\n", ""))
        }
    }
}


#' @export
`print.fobject` <- function (x, startend = TRUE, ...) {

    class (x) <- setdiff (class (x), "fobject")

    x <- matrix(
        if (possibleNumeric_ (x)) round(asNumeric_ (x), 3) else x,
        nrow = 1,
        dimnames = list ("", names (x))
    )

    nax <- is.na (x)

    pN <- apply (x, 2, possibleNumeric_)
    nms <- colnames (x)

    cx <- x

    for (c in seq (ncol(x))) {
        xc <- x[, c]
        max.nchar.nc <- max (nchar (xc), na.rm = TRUE)
        ndec <- 0

        if (pN[c]) {
            ndec <- min (numdec_ (xc), 3)
            x[, c] <- sprintf (
                paste0 ("%", max.nchar.nc, ".", ndec, "f"),
                asNumeric_ (xc)
            )
        }

        # if (possibleNumeric_ (nms[c])) {
        #     # since this is a column name, most likely it is a whole number
        #     # e.g. the value of a declared object instead of the label
        #     nmsc <- sprintf (
        #         paste0 ("%", max.nchar.nc, ".", ndec, "f"),
        #         asNumeric_ (nms[c])
        #     )

        #     if (grepl ("[.]", nmsc)) {
        #         nmsc <- paste (
        #             unlist (strsplit (nmsc, split = "[.]"))[1],
        #             paste (rep (" ", ndec), collapse = "")
        #         )
        #     }

        #     nms[c] <- nmsc
        # }
    }

    x[nax] <- ""


    max.nchars <- max (nchar (c (encodeString (nms), x)), na.rm = TRUE)

    for (i in seq (length (nms))) {
        if (nchar (nms[i]) < max.nchars) {
            nms[i] <- padBoth_ (nms[i], max.nchars - nchar (nms[i]))
        }
    }

    for (i in seq (length (x))) {
        if (nchar (x[i]) < max.nchars) {
            x[i] <- padBoth_ (x[i], max.nchars - nchar (x[i]))
        }
    }

    colnames (x) <- nms

    cat (ifelse (startend, "\n", ""))
    print (noquote (x))
    cat (ifelse (startend, "\n", ""))

}
