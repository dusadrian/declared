`.onLoad` <- function (...) {
    loc <- dirname(getNamespaceInfo("stats", "path"))
    suppressPackageStartupMessages(
        do.call (
            "library",
            list (
                "stats",
                lib.loc = loc,
                character.only = TRUE,
                warn.conflicts = FALSE
            )
        )
    )

    # if (unlockEnvironment_ (asNamespace("base"))) {

        env <- as.environment("package:base")
        do.call ("unlockBinding", list (sym = "print.data.frame", env = env))

        # this function is unchanged, but it needs to be re-written to access
        # the custom version of format.data.frame()
        env$`print.data.frame` <- function (
            x, ..., digits = NULL, quote = FALSE,
            right = TRUE, row.names = TRUE, max = NULL
        ) {
            n <- length (row.names (x))
            if (length (x) == 0L) {
                do.call ("cat", list (
                    sprintf (ngettext(n, "data frame with 0 columns and %d row",
                    "data frame with 0 columns and %d rows"), n),
                    "\n",
                    sep = "")
                )
            }
            else if (n == 0L) {
                print.default(names (x), quote = FALSE)
                do.call ("cat", list (
                    gettext("<0 rows> (or 0-length row.names)\n")
                    )
                )
            }
            else {
                if (is.null (max))
                    max <- getOption ("max.print", 99999L)
                if (!is.finite (max))
                    stop ("invalid 'max' / getOption (\"max.print\"): ",
                        max)
                omit <- (n0 <- max%/%length (x)) < n
                m <- as.matrix(format.data.frame(if (omit)
                    x[seq_len(n0), , drop = FALSE]
                else x, digits = digits, na.encode = FALSE))
                if (!isTRUE (row.names))
                    dimnames (m)[[1L]] <- if (isFALSE (row.names))
                        rep.int("", if (omit)
                        n0
                        else n)
                    else row.names
                do.call (
                    "print",
                    list (m, ..., quote = quote, right = right, max = max)
                )
                if (omit)
                    do.call (
                        "cat",
                        list (
                            " [ reached 'max' /",
                            "getOption (\"max.print\") -- omitted",
                            n - n0,
                            "rows ]\n"
                        )
                    )
            }
            invisible(x)
        }

        do.call ("unlockBinding", list (sym = "format.data.frame", env = env))

        env$`format.data.frame` <- function (x, ..., justify = "none")
        {
            nc <- length (x)
            if (!nc)
                return (x)
            nr <- .row_names_info(x, 2L)
            rval <- vector("list", nc)

            # -----------------------------------------------------
            # this function is also unchanged, except for this part:
            for (i in seq_len(nc)) {
                if (is.declared (x[[i]]) && any (is.na (x[[i]]))) {
                    # any (is.na ()) is necessary to guard against na.omit (),
                    # for instance:
                    rval[[i]] <- format_declared (x[[i]])
                }
                else {
                    rval[[i]] <- format (x[[i]], ..., justify = justify)
                }
            }
            # -----------------------------------------------------

            lens <- vapply (rval, NROW, 1)

            if (any (lens != nr)) {
                warning (
                    paste (
                        "corrupt data frame: columns will be",
                        "truncated or padded with NAs"
                    )
                )
                for (i in seq_len(nc)) {
                    len <- nrow (rval[[i]])
                    if (len == nr)
                        next
                    if (length (dim(rval[[i]])) == 2L) {
                        rval[[i]] <- if (len < nr)
                        rbind(rval[[i]], matrix(NA, nr - len, ncol(rval[[i]])))
                        else rval[[i]][seq_len(nr), ]
                    }
                    else {
                        rval[[i]] <- if (len < nr)
                        c (rval[[i]], rep.int(NA, nr - len))
                        else rval[[i]][seq_len(nr)]
                    }
                }
            }

            for (i in seq_len(nc)) {
                if (
                    is.character (rval[[i]]) &&
                    inherits (rval[[i]], "character")
                )
                    oldClass (rval[[i]]) <- "AsIs"
            }

            y <- as.data.frame.list (
                    rval, row.names = seq_len(nr), col.names = names (x),
                    optional = TRUE, fix.empty.names = FALSE, cut.names = TRUE
                )

            attr (y, "row.names") <- row.names (x)

            return (y)
        }

        do.call ("unlockBinding", list (sym = "rbind.data.frame", env = env))

        env$`rbind.data.frame` <- function (
            ...,
            deparse.level = 1,
            make.row.names = TRUE,
            stringsAsFactors = FALSE,
            factor.exclude = TRUE
        ) {
            match.names <- function (clabs, nmi) {
                if (identical (clabs, nmi)) NULL
                else if (
                    length (nmi) == length (clabs) && all (nmi %in% clabs)
                ) {
                        ## we need 1-1 matches here
                    m <- pmatch(nmi, clabs, 0L)
                        if (any (m == 0L))
                            stop ("names do not match previous names")
                        m
                } else stop ("names do not match previous names")
            }

            allargs <- list (...)
            allargs <- allargs[lengths(allargs) > 0L]
            if (length (allargs)) {
                ## drop any zero-row data frames, as they may not have proper
                ## column types (e.g. NULL).
                nr <- vapply (allargs, function (x)
                            if (is.data.frame(x)) .row_names_info(x, 2L)
                            else if (is.list (x)) length (x[[1L]])
                            # mismatched lists are checked later
                            else length (x), 1L)
            if (any (n0 <- nr == 0L)) {
                if (all (n0)) return (allargs[[1L]]) # pretty arbitrary
                allargs <- allargs[!n0]
            }
            }
            n <- length (allargs)
            if (n == 0L)
            return (list2DF())
            nms <- names (allargs)
            if (is.null (nms))
            nms <- character (n)
            cl <- NULL
            perm <- rows <- vector("list", n)
            if (make.row.names) {
                rlabs <- rows
                env <- new.env()
                # result with 1:nrow (.) row names? [efficiency!]
                env$autoRnms <- TRUE
                Make.row.names <- function (nmi, ri, ni, nrow) {
                    if (nzchar(nmi)) {
                    if (env$autoRnms) env$autoRnms <- FALSE
                    if (ni == 0L) character ()  # PR#8506
                    else if (ni > 1L) paste (nmi, ri, sep = ".")
                    else nmi
                    }
                    else if (
                        env$autoRnms && nrow > 0L && identical (ri, seq_len(ni))
                    )
                    as.integer (seq.int(from = nrow + 1L, length.out = ni))
                    else {
                        if (
                            env$autoRnms && (nrow > 0L ||
                            !identical (ri, seq_len(ni)))
                        ) {
                            env$autoRnms <- FALSE
                        }
                        ri
                    }
                }
            }
            smartX <- isTRUE (factor.exclude)

            ## check the arguments, develop row and column labels
            nrow <- 0L
            value <- clabs <- NULL
            all.levs <- list ()
            for(i in seq_len(n)) { ## check and treat arg [[ i ]]  -- part 1
                xi <- allargs[[i]]
                nmi <- nms[i]
                ## coerce matrix to data frame
                if (is.matrix(xi)) allargs[[i]] <- xi <-
                    as.data.frame(xi, stringsAsFactors = stringsAsFactors)
                if (inherits (xi, "data.frame")) {
                    if (is.null (cl))
                    cl <- oldClass (xi)
                    ri <- attr (xi, "row.names")
                    ni <- length (ri)
                    if (is.null (clabs)) ## first time
                    clabs <- names (xi)
                    else {
                            if (length (xi) != length (clabs))
                                stop (
                                  "numbers of columns of arguments do not match"
                                )
                        pi <- match.names (clabs, names (xi))
                        if ( !is.null (pi) ) perm[[i]] <- pi
                    }
                    rows[[i]] <- seq.int(from = nrow + 1L, length.out = ni)
                    if (make.row.names) rlabs[[i]] <- Make.row.names (
                        nmi, ri, ni, nrow
                    )
                    nrow <- nrow + ni
                    if (is.null (value)) { ## first time ==> setup once:
                        value <- unclass (xi)
                        nvar <- length (value)

                        ## code for declared
                        lxi <- vector("list", nvar)
                        xideclared <- vapply (xi, is.declared, TRUE)
                        names (lxi) <- names (xideclared)
                        lxi[xideclared] <- lapply(xi[xideclared], function (x) {
                            list (
                                labels = attr (x, "labels", exact = TRUE),
                                na_values = attr (x, "na_values"),
                                na_range = attr (x, "na_range")
                            )
                        })
                        ## code for declared

                        all.levs <- vector("list", nvar)
                        has.dim <- facCol <- ordCol <- logical (nvar)
                        if (smartX) NA.lev <- ordCol
                        for(j in seq_len(nvar)) {
                        xj <- value[[j]]
                                facCol[j] <- fac <-
                                    if (!is.null (lj <- levels (xj))) {
                                        all.levs[[j]] <- lj
                                        TRUE # turn categories into factors
                                    } else
                                        is.factor (xj)
                        if (fac) {
                        ordCol[j] <- is.ordered(xj)
                        if (smartX && !NA.lev[j])
                            NA.lev[j] <- anyNA(lj)
                        }
                        has.dim[j] <- length (dim(xj)) == 2L
                        }
                    }
                    else for(j in seq_len(nvar)) {
                        xij <- xi[[j]]
                        if (is.null (pi) || is.na (jj <- pi[[j]])) jj <- j
                        if (facCol[jj]) {
                            if (length (lij <- levels (xij))) {
                                all.levs[[jj]] <- unique(c(all.levs[[jj]], lij))
                                if (ordCol[jj])
                                    ordCol[jj] <- is.ordered(xij)
                                if (smartX && !NA.lev[jj])
                                    NA.lev[jj] <- anyNA(lij)
                            } else if (is.character (xij))
                                all.levs[[jj]] <- unique(c(all.levs[[jj]], xij))
                        }

                        ## code for declared
                        if (xideclared[jj]) {
                            labels <- c (
                                lxi[[jj]]$labels,
                                attr (xij, "labels", exact = TRUE)
                            )
                            labels <- labels[!duplicated (labels)]

                            lxi[[jj]]$labels <- sort (labels)

                            lxi[[jj]]$na_values <- sort (
                                unique (
                                    c (
                                        lxi[[jj]]$na_values,
                                        attr (xij, "na_values")
                                    )
                                )
                            )

                            na_range <- sort (
                                unique (
                                    c (
                                        lxi[[jj]]$na_range,
                                        attr (xij, "na_range")
                                    )
                                )
                            )

                            if (!is.null (na_range)) {
                                lxi[[jj]]$na_range <- range(na_range)
                            }
                        }
                        ## code for declared
                    }
                } ## end{data.frame}
                else if (is.list (xi)) {
                    ni <- range(lengths(xi))
                    if (ni[1L] == ni[2L])
                    ni <- ni[1L]
                    else stop (
                        paste (
                            "invalid list argument: all variables",
                            "should have the same length"
                        )
                    )
                        ri <- seq_len(ni)
                    rows[[i]] <- seq.int(from = nrow + 1L, length.out = ni)
                    if (make.row.names) rlabs[[i]] <- Make.row.names (
                        nmi, ri, ni, nrow
                    )
                    nrow <- nrow + ni
                    if (length (nmi <- names (xi)) > 0L) {
                        if (is.null (clabs))
                            clabs <- nmi
                        else {
                            if (length (xi) != length (clabs)) {
                                stop (
                                  "numbers of columns of arguments do not match"
                                )
                            }
                            pi <- match.names (clabs, nmi)
                            if ( !is.null (pi) ) perm[[i]] <- pi
                        }
                    }
                }
                else if (length (xi)) { # 1 new row
                    rows[[i]] <- nrow <- nrow + 1L
                        if (make.row.names)
                    rlabs[[i]] <- if (nzchar(nmi)) nmi else as.integer (nrow)
                }
            } # for(i .)

            nvar <- length (clabs)
            if (nvar == 0L)
            nvar <- max (lengths(allargs)) # only vector args
            if (nvar == 0L)
            return (list2DF())
            pseq <- seq_len(nvar)
            if (is.null(value)) { # this happens if there has been no data frame
            value <- list ()
            value[pseq] <- list(logical(nrow)) # OK for coercion except to raw.
                all.levs <- vector("list", nvar)
            has.dim <- facCol <- ordCol <- logical (nvar)
            if (smartX) NA.lev <- ordCol
            }
            names (value) <- clabs
            for(j in pseq)
            if (length (lij <- all.levs[[j]]))
                    value[[j]] <-
                factor (as.vector (value[[j]]), levels = lij,
                    exclude = if (smartX) {
                            if (!NA.lev[j]) NA # else NULL
                        } else factor.exclude,
                    ordered = ordCol[j])

            if (any (has.dim)) { # some col's are matrices or d.frame's
                jdim <- pseq[has.dim]
                if (!all (df <- vapply (
                    jdim, function (j) inherits (value[[j]],"data.frame"), NA
                ))) {
                    ## Ensure matrix columns can be filled in  for(i ...) below
                    rmax <- max (unlist (rows))
                    for(j in jdim[!df]) {
                dn <- dimnames (vj <- value[[j]])
                rn <- dn[[1L]]
                if (length (rn) > 0L) length (rn) <- rmax
                pj <- dim(vj)[2L]
                length (vj) <- rmax * pj
                value[[j]] <- array(vj, c (rmax, pj), list (rn, dn[[2L]]))
                }
                }
            }

            for(i in seq_len(n)) { ## add arg [[i]] to result  (part 2)
            xi <- unclass (allargs[[i]])
            if (!is.list (xi))
                if ((ni <- length (xi)) != nvar) {
                if (ni && nvar %% ni != 0)
                    warning (
                        gettextf(
                            paste (
                                "number of columns of result, %d, is not a",
                                "multiple of vector length %d of arg %d"
                            ),
                            nvar, ni, i),
                        domain = NA
                    )
                xi <- rep_len(xi, nvar)
                    }
            ri <- rows[[i]]
            pi <- perm[[i]]
            if (is.null (pi)) pi <- pseq
            for(j in pseq) {
                jj <- pi[j]
                    xij <- xi[[j]]
                if (has.dim[jj]) {
                value[[jj]][ri,	 ] <- xij
                        ## copy rownames
                        if (!is.null (r <- rownames (xij)) &&
                        !(inherits (xij, "data.frame") &&
                            .row_names_info(xij) <= 0))
                            rownames (value[[jj]])[ri] <- r
                } else {
                        ## coerce factors to vectors, in case lhs is character
                        ## or level set has changed
                        value[[jj]][ri] <- if (is.factor (xij)) {
                            as.vector (xij)
                        } else {
                            xij
                        }
                        ## copy names if any
                        if (!is.null (nm <- names (xij))) {
                            names (value[[jj]])[ri] <- nm
                        }
                    }
            }
            }
            rlabs <- if (make.row.names && !env$autoRnms) {
                rlabs <- unlist (rlabs)
                if (anyDuplicated (rlabs))
                    make.unique (as.character (rlabs), sep = "")
                else
                    rlabs
                } # else NULL

            ## code for declared
            if (any (xideclared)) {
                for (i in which (xideclared)) {
                    value[[i]] <- declared (
                        undeclare (value[[i]], drop = TRUE),
                        label = attr (value[[i]], "label", exact = TRUE),
                        labels = lxi[[i]]$labels,
                        na_values = lxi[[i]]$na_values,
                        na_range = lxi[[i]]$na_range
                    )
                }
            }
            ## code for declared

            if (is.null (cl)) {
            as.data.frame(value, row.names = rlabs, fix.empty.names = TRUE,
                    stringsAsFactors = stringsAsFactors)
            } else {
            structure (value, class = cl,
                row.names = if (is.null (rlabs)) {
                        .set_row_names (nrow)
                    } else { rlabs }
                )
            }
        }

        do.call ("unlockBinding", list (sym = "order", env = env))

        env$order <- function (..., na.last = TRUE, decreasing = FALSE,
            method = c ("auto", "shell", "radix"), empty.last = na.last) {

            z <- list (...)
            decreasing <- as.logical (decreasing)

            if (
                length (z) == 1L &&
                is.numeric (x <- z[[1L]]) &&
                !is.object(x) &&
                length (x) > 0
            ) {
                if (
                    eval (parse (
                        text = ".Internal(sorted_fpass(x, decreasing, na.last))"
                    ))
                ) {
                    return (seq_along(x))
                }
            }

            method <- match.arg (method)

            if (any (vapply (z, function (x) {
                    is.object(x) && !is.declared (x)
                }, logical (1L)))) {
                z <- lapply (z, function (x) if (is.object(x))
                    as.vector (xtfrm(x))
                else x)
                return (do.call ("order", c (z, list (na.last = na.last,
                    decreasing = decreasing, method = method))))
            }

            if (method == "auto") {
                useRadix <- all (vapply (z, function (x) {
                    (
                        is.numeric (x) ||
                        is.factor (x) ||
                        is.logical (x)
                    ) &&
                    is.integer (length (x))
                }, logical (1L)))
                method <- ifelse (useRadix, "radix", "shell")
            }


            if (length (z) == 1L && is.declared (x)) {
                return (order_declared (
                    x, na.last = na.last, decreasing = decreasing,
                    method = method, empty.last = empty.last
                ))
            }

            if (method != "radix" && !is.na (na.last)) {
                return (eval (parse (
                    text = ".Internal(order (na.last, decreasing, ...))"
                )))
            }

            if (method == "radix") {
                decreasing <- rep_len(as.logical (decreasing), length (z))
                return (eval (parse (
                    text = paste (
                        ".Internal(radixsort (",
                        "na.last, decreasing, FALSE, TRUE, ...",
                        "))"
                    )
                )))
            }

            if (any (diff((l.z <- lengths(z)) != 0L))) {
                stop ("argument lengths differ")
            }

            na <- vapply (z, is.na, rep.int(NA, l.z[1L]))

            ok <- if (is.matrix(na)) {
                rowSums(na) == 0L
            }
            else {
                !any (na)
            }

            if (all (!ok)) {
                return (integer())
            }

            z[[1L]][!ok] <- NA

            ans <- do.call ("order", c (z, list (decreasing = decreasing)))

            ans[ok[ans]]
        }

        do.call ("unlockBinding", list (sym = "as.factor", env = env))

        env$as.factor <- function (
            x,
            levels = c ("labels", "values", "both"),
            drop_na = TRUE,
            nolabels = FALSE,
            ordered = FALSE,
            ...
        ) {
            if (is.declared (x)) {
                levels <- match.arg (levels)
                labels <- attr (x, "labels", exact = TRUE)
                nv <- names_values (x, drop_na = drop_na)

                if (isFALSE (drop_na)) {
                    x <- undeclare (x)
                }

                if (levels == "labels") {
                    if (isTRUE (nolabels)) {
                        nv <- nv[is.element (nv, labels)]
                        x[!is.element (drop(undeclare (x)), labels)] <- NA
                    }

                    x <- factor (
                        as.character (x),
                        levels = names (nv),
                        ordered = ordered
                    )
                }
                else if (levels == "values") {
                    attributes (x) <- NULL
                    x <- factor (
                        x,
                        levels = unname (nv),
                        ordered = ordered
                    )
                }
                else if (levels == "both") {
                    nms <- paste0 ("[", nv, "] ", names (nv))

                    x <- factor (
                        nms[match(x, nv)],
                        levels = nms,
                        ordered = ordered
                    )
                }

                return (x)
            }
            else {
                if (is.factor (x))
                    x
                else if (!is.object(x) && is.integer (x)) {
                    levels <- sort.int(unique.default(x))
                    f <- match(x, levels)
                    levels (f) <- as.character (levels)
                    if (!is.null (nx <- names (x)))
                        names (f) <- nx
                    class (f) <- "factor"
                    f
                }
                else factor (x)
            }
        }

        do.call ("unlockBinding", list (sym = "drop", env = env))

        env$drop <- function (x) {
            if (is.declared (x)) {
                attributes (x) <- NULL
                return (x)
            }

            eval (parse (text = ".Internal(drop(x))"))
        }

        do.call ("unlockBinding", list (sym = "match", env = env))

        env$match <- function (
            x, table, nomatch = NA_integer_, incomparables = NULL
        ) {
            x <- undeclare (x, drop = TRUE)
            table <- undeclare (table, drop = TRUE)

            eval (parse (
                text = ".Internal(match(x, table, nomatch, incomparables))"
            ))
        }

        do.call ("unlockBinding", list (sym = "%in%", env = env))

        env$`%in%` <- function (x, table) {
            x <- undeclare (x, drop = TRUE)
            table <- undeclare (table, drop = TRUE)

            eval (parse (
                text = ".Internal(match(x, table, 0L, NULL))"
            )) > 0L
        }

        do.call ("unlockBinding", list (sym = "is.element", env = env))

        env$is.element <- function (el, set) {
            match(
                as.vector (undeclare (el, drop = TRUE)),
                as.vector (undeclare (set, drop = TRUE)),
                0L
            ) > 0L
        }

        do.call ("unlockBinding", list (sym = "sum", env = env))

        env$sum <- function (..., na.rm = FALSE) {
            dots <- lapply (list (...), function(x) {
                if (inherits (x, "declared")) {
                    na_index <- attr (x, "na_index")
                    if (!is.null (na_index)) {
                        x <- x[-na_index]
                    }
                    xdate <- isTRUE (attr (x, "date"))
                    attributes (x) <- NULL
                    if (xdate) {
                        x <- as.Date (x)
                    }
                }
                return (x)
            })

            do.call(.Primitive ("sum"), c(dots, na.rm = na.rm))
        }
    # }

    # if (unlockEnvironment_ (asNamespace("stats"))) {

        env <- as.environment("package:stats")

        do.call ("unlockBinding", list (sym = "sd", env = env))

        env$sd <- function (x, na.rm = FALSE) {
            if (is.declared (x)) {
                na_index <- attr (x, "na_index")
                if (!is.null (na_index)) {
                    x <- x[-na_index]
                }
                class (x) <- setdiff (class (x), "declared")
            }

            sqrt (
                var (
                    if (is.vector(x) || is.factor (x)) x else as.double(x),
                    na.rm = na.rm
                )
            )
        }

        do.call ("unlockBinding", list (sym = "var", env = env))

        env$var <- function (x, y = NULL, na.rm = FALSE, use) {
            if (is.declared (x)) {
                na_index <- attr (x, "na_index")
                if (!is.null (na_index)) {
                    x <- x[-na_index]
                }
                class (x) <- setdiff (class (x), "declared")
            }

            if (missing(use)) {
                use <- ifelse (na.rm, "na.or.complete", "everything")
            }

            na.method <- pmatch(
                use,
                c (
                    "all.obs", "complete.obs", "pairwise.complete.obs",
                    "everything", "na.or.complete"
                )
            )

            if (is.na (na.method)) {
                stop ("invalid 'use' argument")
            }

            if (!is.null (x)) {
                if (is.data.frame (x)) {
                    x <- as.matrix (x)
                }
                else {
                    stopifnot(is.atomic (x))
                }
            }

            if (!is.null(y)) {
                if (is.data.frame (y)) {
                    y <- as.matrix (y)
                }
                else {
                    stopifnot(is.atomic (y))
                }
            }

            eval (parse (
                text = '.Call (stats:::C_cov, x, y, na.method, FALSE)'
            ))
            # .Call (C_cov, x, y, na.method, FALSE)
        }

        do.call ("unlockBinding", list (sym = "fivenum", env = env))

        env$fivenum <- function (x, na.rm = FALSE) {
            if (is.declared (x)) {
                na_index <- attr (x, "na_index")
                if (!is.null (na_index)) {
                    x <- x[-na_index]
                }
                class (x) <- setdiff (class (x), "declared")
            }

            xna <- is.na (x)
            if (any (xna)) {
                if (na.rm)
                    x <- x[!xna]
                else return (rep.int(NA, 5))
            }
            x <- sort (x)
            n <- length (x)
            if (n == 0)
                rep.int(NA, 5)
            else {
                n4 <- floor ((n + 3)/2)/2
                d <- c (1, n4, (n + 1)/2, n + 1 - n4, n)
                0.5 * (x[floor (d)] + x[ceiling(d)])
            }
        }
    # }

    if (!is.element("package:graphics", search())) {
        suppressPackageStartupMessages(library("graphics", warn.conflicts = FALSE))
    }
    env <- as.environment("package:graphics")

    do.call ("unlockBinding", list (sym = "dotchart", env = env))

    env$dotchart <- function(
        x, labels = NULL, groups = NULL, gdata = NULL, offset = 1/8,
        ann = par("ann"), xaxt = par("xaxt"), frame.plot = TRUE, log = "",
        cex = par("cex"), pt.cex = cex,
        pch = 21, gpch = 21, bg = par("bg"),
        color = par("fg"), gcolor = par("fg"), lcolor = "gray",
        xlim = range(x[is.finite(x)]),
        main = NULL, xlab = NULL, ylab = NULL, ...
    ) {
        ## old-style "graphics" design-bug: ("mar"), ("mai"), ("mar", "mai")
        ##			    all fail, just the following, ("mai", "mar") is ok:
        opar <- par("mai", "mar", "mgp", "cex", "yaxs")
        on.exit(par(opar))
        par(cex = cex, yaxs = "i")

        if(!is.numeric(x))
            stop("'x' must be a numeric vector or matrix")
        n <- length(x)
        if (is.matrix(x)) {
            if (is.null(labels))
                labels <- rownames(x)
            if (is.null(labels))
                labels <- as.character(seq_len(nrow(x)))
            labels <- rep_len(labels, n)
            if (is.null(groups))
                groups <- col(x, as.factor = TRUE)
            glabels <- levels(groups)
        } else {
            if (is.null(labels)) labels <- names(x)
            if (inherits(x, "wtable")) {
                x <- as.numeric(x)
            }
            glabels <- if(!is.null(groups)) levels(groups)
            if (!is.vector(x)) { # e.g. a table
                warning("'x' is neither a vector nor a matrix: using as.numeric(x)")
                x <- as.numeric(x)
            }
        }

        plot.new() # for strwidth()

        linch <-
        if(!is.null(labels)) max(strwidth(labels, "inch"), na.rm = TRUE) else 0
        if (is.null(glabels)) {
            ginch <- 0
            goffset <- 0
        }
        else {
            ginch <- max(strwidth(glabels, "inch"), na.rm = TRUE)
            goffset <- offset
        }
        nmai <- opar[["mai"]]
        if(ann)
            nm.2 <- nmai[2L]
        if (!(is.null(labels) && is.null(glabels))) {
            ## The intention seems to be to balance the whitespace
            ## on each side (2 & 4) of the labels+plot.
            yi <- if(is.null(ylab) || !ann) 0 else offset
            nm.2 <- nmai[4L] + max(yi + linch + goffset, ginch) + 1/16
            if (nmai[2L] <	nm.2) { ## add space for ylab + glabels on left margin
                nmai[2L] <- nm.2
                par(mai = nmai)
            }
        }

        if (is.null(groups)) {
            o <- seq_len(n)
            y <- o
            ylim <- c(0, n + 1)
        }
        else {
            o <- sort.list(as.numeric(groups), decreasing = TRUE)
            x      <- x     [o]
            groups <- groups[o]
            color  <- rep_len(color,  length(groups))[o]
            lcolor <- rep_len(lcolor, length(groups))[o]
            pch    <- rep_len(pch,    length(groups))[o]
            of.1 <- cumsum(c(0, diff(as.numeric(groups)) != 0))
            y <- seq_len(n) + 2 * of.1
            ylim <- range(0, y + 2)
        }

        plot.window(xlim = xlim, ylim = ylim, log = log)
        #    xmin <- par("usr")[1L]
        lheight <- par("csi")
        if (!is.null(labels)) {
            loffset <- (linch + 0.1)/lheight
            mtext(labels[o], side = 2, line = loffset, at = y, adj = 0,
                    col = color, las = 2, cex = cex, ...)
        }
        abline(h = y, lty = "dotted", col = lcolor)
        points(x, y, pch = pch, col = color, bg = bg, cex = pt.cex/cex)
        if (!is.null(groups)) {
            gpos <- rev(cumsum(rev(tapply(groups, groups, length)) + 2) - 1)
            ginch <- max(strwidth(glabels, "inch"), na.rm = TRUE)
            goffset <- (max(linch+offset, ginch, na.rm = TRUE) + 1/16)/lheight
                mtext(glabels, side = 2, line = goffset, at = gpos,
                        adj = 0, col = gcolor, las = 2, cex = cex, ...)
            if (!is.null(gdata)) {
                abline(h = gpos, lty = "dotted")
                points(gdata, gpos, pch = gpch, col = gcolor, bg = bg,
                            cex = pt.cex/cex, ...)
            }
        }
        axis(1, xaxt=xaxt) # FIXME? add '...' or use localAxis() as plot.default()
        if(frame.plot)
            box()
        if(ann) {
            title(main=main, xlab=xlab, ...) # with default "mgp"
            ## y-axis label must be left of the (regular + group) labels:
            mgp <- par("mgp")
            par(mgp = c(max(mgp[1], nm.2 / lheight - 1.5), mgp[-1]))
            title(ylab=ylab, ...)
        }
        invisible()
    }



    register_S3_method("labelled", "na_values", "declared")
    register_S3_method("labelled", "na_values<-", "declared")
    register_S3_method("labelled", "na_range", "declared")
    register_S3_method("labelled", "na_range<-", "declared")
    register_S3_method("labelled", "val_labels", "declared")
    register_S3_method("labelled", "val_labels<-", "declared")
    register_S3_method("labelled", "var_label", "declared")
    register_S3_method("labelled", "var_label<-", "declared")
    register_S3_method("labelled", "drop_unused_value_labels", "declared")
    register_S3_method("labelled", "val_label", "declared")
    register_S3_method("labelled", "val_label<-", "declared")
    register_S3_method("labelled", "sort_val_labels", "declared")
    register_S3_method("labelled", "nolabel_to_na", "declared")
    register_S3_method("labelled", "val_labels_to_na", "declared")
    register_S3_method("labelled", "remove_labels", "declared")
    register_S3_method("labelled", "remove_user_na", "declared")
    register_S3_method("labelled", "to_factor", "declared")
    register_S3_method("labelled", "to_character", "declared")
    register_S3_method("labelled", "copy_labels", "declared")

    register_S3_method("haven", "as_factor", "declared")
    register_S3_method("haven", "zap_labels", "declared")
    register_S3_method("haven", "zap_missing", "declared")

    register_S3_method("pillar", "pillar_shaft", "declared")
    register_S3_method("pillar", "format", "pillar_shaft_declared_num")
    register_S3_method("pillar", "format", "pillar_shaft_declared_chr")

    register_S3_method("vctrs", "vec_ptype_abbr", "declared")
    register_S3_method("vctrs", "vec_ptype_full", "declared")
    register_S3_method("vctrs", "vec_proxy", "declared")
    register_S3_method("vctrs", "vec_restore", "declared")
    # register_S3_method("vctrs", "vec_ptype2", "declared")

    register_S3_method("vroom", "output_column", "declared")

    register_S3_method("stats", "weighted.mean", "declared")

    invisible()
}

# function copied from package haven
`register_S3_method` <- function (pkg, generic, class, fun = NULL) {
    stopifnot(is.character (pkg), length (pkg) == 1)
    stopifnot(is.character (generic), length (generic) == 1)
    stopifnot(is.character (class), length (class) == 1)

    if (is.null (fun)) {
        fun <- get(paste0 (generic, ".", class), envir = parent.frame())
    }
    else {
        stopifnot(is.function (fun))
    }

    if (is.element (pkg, loadedNamespaces())) {
        registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }

    # Always register hook in case package is later unloaded & reloaded
    setHook(
        packageEvent(pkg, "onLoad"),
        function (...) {
            registerS3method(generic, class, fun, envir = asNamespace(pkg))
        }
    )
}
