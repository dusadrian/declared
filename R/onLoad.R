# Save the bindings supplied by the running R version for delegation and unload.
package_bindings_ <- new.env (parent = emptyenv ())
package_hooks_ <- new.env (parent = emptyenv ())

`.onUnload` <- function (libpath) {
    for (name in ls (package_bindings_)) {
        binding <- package_bindings_[[name]]
        env <- binding$env
        if (identical (env[[name]], binding$replacement)) {
            if (bindingIsLocked (name, env)) unlockBinding (name, env)
            env[[name]] <- binding$original
            if (binding$locked) lockBinding (name, env)
        }
        rm (list = name, envir = package_bindings_)
    }
    for (event in ls (package_hooks_)) {
        ours <- package_hooks_[[event]]
        keep <- function (hook) !any (vapply (ours, identical, TRUE, y = hook))
        setHook (event, Filter (keep, getHook (event)), action = "replace")
        rm (list = event, envir = package_hooks_)
    }
    return (invisible (NULL))
}

`.onLoad` <- function (...) {
    if (length (package_bindings_)) return (invisible (NULL))
    loaded <- FALSE
    on.exit (if (!loaded) .onUnload (NULL))
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

    loc <- dirname(getNamespaceInfo("graphics", "path"))
    suppressPackageStartupMessages(
        do.call (
            "library",
            list (
                "graphics",
                lib.loc = loc,
                character.only = TRUE,
                warn.conflicts = FALSE
            )
        )
    )

    env <- as.environment("package:base")

    original_rbind <- env[["rbind.data.frame"]]
    install_binding_ (env, "rbind.data.frame", function (
        ..., deparse.level = 1, make.row.names = TRUE,
        stringsAsFactors = FALSE, factor.exclude = TRUE
    ) {
        args <- list (...)
        sources <- vector ("list", length (args))
        for (i in seq_along (args)) {
            part <- args[[i]]
            if (!is.list (part)) next
            cols <- which (vapply (part, is.declared, TRUE))
            if (!length (cols)) next

            sources[[i]] <- list (positions = cols, names = names (part),
                columns = lapply (part[cols], sanitize_na_index_))
            for (j in cols) {
                x <- part[[j]]
                values <- undeclare (x, drop = TRUE)
                names (values) <- names (x)
                part[[j]] <- values
            }
            args[[i]] <- part
        }
        result <- do.call (original_rbind, c (args, list (
            deparse.level = deparse.level, make.row.names = make.row.names,
            stringsAsFactors = stringsAsFactors, factor.exclude = factor.exclude
        )))
        if (!length (result) || !any (lengths (sources))) return (result)

        # Base R discards zero-row arguments when rows are available and
        # returns the first nonempty-column argument when all have zero rows.
        if (nrow (result)) {
            sources <- Filter (function (source) {
                return (length (source$columns) && length (source$columns[[1]]) > 0)
            }, sources)
        }
        else {
            sources <- sources[which (lengths (args) > 0)[1]]
        }
        columns <- vector ("list", length (result))
        for (source in sources) {
            positions <- source$positions
            if (length (source$names) && !identical (source$names, names (result))) {
                positions <- match (source$names[positions], names (result))
            }
            for (k in seq_along (positions)) {
                j <- positions[k]
                columns[[j]] <- c (columns[[j]], source$columns[k])
            }
        }
        for (j in which (lengths (columns) > 0)) {
            inputs <- unname (columns[[j]])
            values <- result[[j]]
            if (isTRUE (attr (inputs[[1]], "date"))) values <- as.Date (values)
            result[[j]] <- do.call (declared, c (
                list (x = values), combined_metadata_ (inputs)
            ))
        }
        return (result)
    })

    original_order <- env[["order"]]
    install_binding_ (env, "order", function (..., na.last = TRUE, decreasing = FALSE,
        method = c ("auto", "shell", "radix"), empty.last = na.last) {

        z <- list (...)
        declared <- vapply (z, is.declared, TRUE)
        method <- match.arg (method)
        if (!any (declared)) {
            return (original_order (..., na.last = na.last, decreasing = decreasing,
                method = method))
        }
        if (length (z) == 1L) {
            return (order_declared (z[[1L]], decreasing = decreasing,
                na.last = na.last, empty.last = empty.last, method = method))
        }
        directions <- rep_len (as.logical (decreasing), length (z))
        for (i in which (declared)) {
            z[[i]] <- xtfrm_declared (z[[i]], decreasing = directions[i],
                na.last = na.last, empty.last = empty.last)
        }
        return (do.call (original_order, c (unname (z), list (
            na.last = na.last, decreasing = decreasing, method = method
        ))))
    })

    original_as_factor <- env[["as.factor"]]
    install_binding_ (env, "as.factor", function (
        x,
        levels = c ("labels", "values", "both"),
        drop_na = TRUE,
        nolabels = FALSE,
        ordered = FALSE,
        ...
    ) {
        if (is.declared (x)) {
            x <- sanitize_na_index_ (x)
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
            return (original_as_factor (x))
        }
    })
    original_drop <- env[["drop"]]
    install_binding_ (env, "drop", function (x) {
        if (is.declared (x)) {
            attributes (x) <- NULL
            return (x)
        }

        return (original_drop (x))
    })

    # R versions with mtfrm() dispatch inside compiled match() and %in% too.
    if (!exists ("mtfrm", baseenv (), inherits = FALSE)) {
        original_match <- env[["match"]]
        declared_match <- function (x, table, nomatch = NA_integer_, incomparables = NULL) {
            if (is.declared (x)) x <- undeclare (x, drop = TRUE)
            if (is.declared (table)) table <- undeclare (table, drop = TRUE)
            return (original_match (x, table, nomatch, incomparables))
        }
        install_binding_ (env, "match", declared_match)
        install_binding_ (env, "%in%", function (x, table) {
            return (declared_match (x, table, 0L) > 0L)
        })
    }
    original_is_element <- env[["is.element"]]
    install_binding_ (env, "is.element", function (el, set) {
        if (is.declared (el)) el <- undeclare (el, drop = TRUE)
        if (is.declared (set)) set <- undeclare (set, drop = TRUE)
        return (original_is_element (el, set))
    })


    env <- as.environment("package:stats")

    install_binding_ (env, "sd", function (x, na.rm = FALSE) {
        if (is.declared (x)) x <- drop_declared_missing_ (x)$x
        return (stats::sd (x, na.rm = na.rm))
    })

    install_binding_ (env, "var", function (x, y = NULL, na.rm = FALSE, use) {
        if (is.declared (x) || is.declared (y)) {
            if (!is.null (y) && NROW (x) != NROW (y)) {
                stop ("incompatible dimensions")
            }
            omit <- c (attr (sanitize_na_index_ (x), "na_index"),
                       attr (sanitize_na_index_ (y), "na_index"))
            if (is.declared (x)) attributes (x) <- NULL
            if (is.declared (y)) attributes (y) <- NULL
            if (length (omit)) {
                x <- if (is.null (dim (x))) x[-omit] else x[-omit, , drop = FALSE]
                y <- if (is.null (dim (y))) y[-omit] else y[-omit, , drop = FALSE]
            }
        }
        return (stats::var (x, y, na.rm = na.rm, use = use))
    })

    install_binding_ (env, "fivenum", function (x, na.rm = FALSE) {
        if (is.declared (x)) x <- drop_declared_missing_ (x)$x
        return (stats::fivenum (x, na.rm = na.rm))
    })

    env <- as.environment("package:graphics")

    install_binding_ (env, "dotchart", function (x, labels = NULL, ...) {
        if (inherits (x, "wtable") && !is.matrix (x)) {
            if (is.null (labels)) labels <- names (x)
            x <- as.numeric (x)
        }
        return (graphics::dotchart (x, labels = labels, ...))
    })

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

    loaded <- TRUE
    return (invisible (NULL))
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
    event <- packageEvent (pkg, "onLoad")
    hook <- function (...) {
        registerS3method (generic, class, fun, envir = asNamespace (pkg))
    }
    setHook (event, hook)
    package_hooks_[[event]] <- c (package_hooks_[[event]], list (hook))
    return (invisible (NULL))
}
