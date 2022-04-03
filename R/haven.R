`as_haven` <- function(x, ...) {
    UseMethod("as_haven")
}


`as_haven.default` <- function(x, ...) {
    return(x)
}


`as_haven.declared` <- function(x, ...) {
    na_index <- attr(x, "na_index")
    attrx <- attributes(x)

    # this is necessary to replace those values
    # (because of the "[<-.declared" method)
    attributes(x) <- NULL # or x <- unclass(x), but I find this cleaner

    if (admisc::possibleNumeric(x) || all(is.na(x))) {
        x <- as.numeric(x)
    }

    if (!is.null(na_index)) {
        # x[na_index] <- admisc::coerceMode(names(na_index))

        #------------------------------------------
        # detour until ReadStat deals with integers
        all_na_values <- names(na_index)
        if (is.numeric(x)) {
            all_na_values <- as.numeric(all_na_values)
        }
        x[na_index] <- all_na_values
        #------------------------------------------
    }

    #------------------------------------------
    # detour until ReadStat deals with integers
    na_values <- attrx$na_values
    pN_na_values <- admisc::possibleNumeric(na_values)
    labels <- attrx$labels
    pN_labels <- admisc::possibleNumeric(labels)
    all_num <- is.numeric(x)

    if (!is.null(na_values)) {
        all_num <- all_num & pN_na_values
    }

    if (!is.null(labels)) {
        all_num <- all_num & pN_labels
    }

    if (all_num) {

        if (!is.null(na_values)) {
            na_values <- as.numeric(na_values)
            names(na_values) <- names(attrx$na_values)
            attrx$na_values <- na_values
        }

        if (!is.null(labels)) {
            labels <- as.numeric(labels)
            names(labels) <- names(attrx$labels)
            attrx$labels <- labels
        }
    }
    else {
        x <- as.character(x)

        if (!is.null(na_values)) {
            na_values <- as.character(na_values)
            names(na_values) <- names(attrx$na_values)
            attrx$na_values <- na_values
        }

        if (!is.null(labels)) {
            labels <- as.character(labels)
            names(labels) <- names(attrx$labels)
            attrx$labels <- labels
        }
    }

    #------------------------------------------

    attrx$na_index <- NULL
    # attrx$class <- c("haven_labelled_spss", "haven_labelled", "vctrs_vctr", setdiff(attrx$class, "declared"))

    #------------------------------------------
    # detour until ReadStat deals with integers
    attrx$class <- unique(c(
            "haven_labelled_spss", "haven_labelled", "vctrs_vctr",
            setdiff(attrx$class, c("declared", "double", "integer")),
            class(x)
    ))
    #------------------------------------------

    attributes(x) <- attrx
    return(x)
}


`as_haven.data.frame` <- function(x, ..., only_declared = TRUE) {
    if (only_declared) {
        xdeclared <- vapply(x, is_declared, logical(1))
        x[xdeclared] <- lapply(x[xdeclared], as_haven, ...)
    } else {
        x[] <- lapply(x, as_haven, ...)
    }
    
    class(x) <- c("tbl", "tbl_df", "data.frame")
    return(x)
}


`as_factor.declared` <- function(
    x, levels = c("default", "labels", "values", "both"), ordered = FALSE, ...
) {
    as.factor(x, levels = levels, ordered = ordered, ... = ...)
}

`zap_labels.declared` <- function(x) {
    attr(x, "labels") <- NULL
    attr(x, "na_index") <- NULL
    attr(x, "na_values") <- NULL
    attr(x, "na_range") <- NULL
    class(x) <- NULL

    return(x)
}

`zap_missing.declared` <- function(x) {
    attr(x, "na_index") <- NULL
    attr(x, "na_values") <- NULL
    attr(x, "na_range") <- NULL

    return(x)
}


# using eval(parse()) to avoid the huge dependency tree of vctrs, haven, labelled and pillar

`vec_ptype_abbr.declared` <- function(x, ...) {
    command <- "paste0(vctrs::vec_ptype_abbr(vctrs::vec_data(unclass(undeclare(x)))), '+lbl')"
    eval(parse(text = command))
}

`vec_ptype_full.declared` <- function(x, ...) {
    command <- "paste0('declared<', vctrs::vec_ptype_full(vctrs::vec_data(unclass(undeclare(x)))), '>')"
    eval(parse(text = command))
}

`vec_ptype2.declared` <- function(x, y, ...) {
    command <- "vctrs::vec_ptype2(unclass(undeclare(x)), vctrs::vec_data(unclass(undeclare(y))), ...)"
    eval(parse(text = command))
}
