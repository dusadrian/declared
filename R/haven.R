`as_factor.declared` <- function(x, levels = c("default", "labels", "values", "both"), ordered = FALSE, ...) {
    levels <- match.arg(levels)
    label <- attr(x, "label", exact = TRUE)
    labels <- attr(x, "labels")

    if (levels %in% c("default", "both")) {
        if (levels == "both") {
            names(labels) <- paste0("[", labels, "] ", names(labels))
            attr(x, "labels") <- labels
        }

        vals <- sort(unique(x), na.last = TRUE)
        x <- factor(to_labels(undeclare(x)), levels = to_labels(vals), ordered = ordered)

    }
    else if (levels == "labels") {
        levs <- unname(labels)
        labs <- names(labels)
        x <- factor(to_labels(undeclare(x)), levels = sort(unique(labs)), ordered = ordered)
    }
    else if (levels == "values") {
        levels <- unique(undeclare(sort(x, na.last = TRUE)))
        
        x <- factor(undeclare(x), levels, ordered = ordered)
    }

    structure(x, label = label)
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


# using eval(parse()) to avoid the dependency tree of vctrs, haven, labelled and pillar

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
