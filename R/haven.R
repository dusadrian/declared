#' @title Coerce to haven / labelled objects
#' @description
#' Convert declared labelled objects to haven labelled objects
#' @details
#' This is a function that reverses the process of `as.declared()`, making
#' a round trip between `declared` and `haven_labelled_spss` classes.
#'
#' @return A labelled vector of class "haven_labelled_spss".
#' @examples
#'
#' x <- declared(
#'     c(1:5, -1),
#'     labels = c(Good = 1, Bad = 5, DK = -1),
#'     na_values = -1
#' )
#'
#' x
#'
#' as.haven(x)
#' @param x A declared labelled vector
#' @param ... Other arguments used by various methods
#' @export

#' @name as.haven
#' @export
# See https://stackoverflow.com/questions/18512528/
#     how-to-export-s3-method-so-it-is-available-in-namespace
`as.haven` <- function (x, ...) {
    UseMethod ("as.haven")
}

#' @export
`as.haven.default` <- function (x, ...) {

    dots <- list (...)
    interactive <- !isFALSE(dots$interactive)

    if (interactive) {
        msg <- "There is no automatic class method conversion for this type of"
        if (!is.null (dots$vname_)) {
            msg <- paste0 (dots$vname_, ": ", msg, " variable.")
        }
        else {
            msg <- paste (msg, "object.")
        }
        message (msg)
    }

    return (x)
}

#' @export
`as.haven.haven_labelled` <- function (x, ...) {
    # do nothing
    return (x)
}

#' @export
`as.haven.haven_labelled_spss` <- function (x, ...) {
    # do nothing
    return (x)
}



#' @export
`as.haven.declared` <- function (x, ...) {
    na_index <- attr (x, "na_index")
    attrx <- attributes (x)

    # this is necessary to replace those values
    # (because of the "[<-.declared" method)
    attributes (x) <- NULL # or x <- unclass (x), but I find this cleaner

    if (possibleNumeric_ (x) || all (is.na (x))) {
        x <- as.double(x)
        attr (x, "class") <- "double"
    }

    if (!is.null (na_index)) {
        # x[na_index] <- coerceMode_ (names (na_index))

        #------------------------------------------
        # detour until ReadStat deals with integers
        all_na_values <- names (na_index)
        if (is.numeric (x)) {
            all_na_values <- as.numeric (all_na_values)
        }
        x[na_index] <- all_na_values
        #------------------------------------------
    }

    #------------------------------------------
    # detour until ReadStat deals with integers
    na_values <- attrx$na_values
    pN_na_values <- possibleNumeric_ (na_values)
    labels <- attrx$labels
    pN_labels <- possibleNumeric_ (labels)
    all_num <- is.numeric (x)

    if (!is.null (na_values)) {
        all_num <- all_num & pN_na_values
    }

    if (!is.null (labels)) {
        all_num <- all_num & pN_labels
    }

    if (all_num) {

        if (!is.null (na_values)) {
            na_values <- as.numeric (na_values)
            names (na_values) <- names (attrx$na_values)
            attrx$na_values <- na_values
        }

        if (!is.null (labels)) {
            labels <- as.numeric (labels)
            names (labels) <- names (attrx$labels)
            attrx$labels <- labels
        }
    }
    else {
        x <- as.character (x)

        if (!is.null (na_values)) {
            na_values <- as.character (na_values)
            names (na_values) <- names (attrx$na_values)
            attrx$na_values <- na_values
        }

        if (!is.null (labels)) {
            labels <- as.character (labels)
            names (labels) <- names (attrx$labels)
            attrx$labels <- labels
        }
    }
    #------------------------------------------

    attrx$na_index <- NULL
    # attrx$class <- c (
    #     "haven_labelled_spss", "haven_labelled", "vctrs_vctr",
    #     setdiff (attrx$class, "declared")
    # )

    #------------------------------------------
    # detour until ReadStat deals with integers
    attrx$class <- unique (c (
            "haven_labelled_spss", "haven_labelled", "vctrs_vctr",
            setdiff (
                attrx$class,
                c ("declared", "double", "integer", "character")
            ),
            class (x)
    ))
    #------------------------------------------

    attributes (x) <- attrx
    return (x)
}

#' @export
`as.haven.data.frame` <- function (
    x, ..., only_declared = TRUE, interactive = FALSE
) {
    if (only_declared) {
        xdeclared <- vapply (x, is.declared, logical (1))
        if (isFALSE (interactive)) {
            x[xdeclared] <- lapply (
                x[xdeclared], as.haven, interactive = FALSE, ... = ...
            )
        }
        else {
            nms <- names (x)[xdeclared]
            for (i in seq (length (nms))) {
                x[[nms[i]]] <- as.haven (
                    x[[nms[i]]], vname_ = nms[i], ... = ...
                )
            }
        }
    } else {
        if (isFALSE (interactive)) {
            x[] <- lapply (x, as.haven, interactive = FALSE, ... = ...)
        }
        else {
            nms <- names (x)
            for (i in seq (length (nms))) {
                x[[i]] <- as.haven (x[[i]], vname_ = nms[i], ... = ...)
            }
        }
    }

    class (x) <- c ("tbl", "tbl_df", "data.frame")
    return (x)
}





# Dynamically exported, see onLoad.R
# using eval (parse ()) to avoid the huge dependency tree of vctrs, haven,
# labelled and pillar these functions will be registered when or if the package
# haven is loaded

`as_factor.declared` <- function (
    x, levels = c ("default", "labels", "values", "both"), ordered = FALSE, ...
) {
  haven::as_factor (
    as.haven (x),
    levels = levels,
    ordered = ordered,
    ... = ...
  )
}

`zap_labels.declared` <- function (x) {
    attr (x, "labels") <- NULL
    attr (x, "na_index") <- NULL
    attr (x, "na_values") <- NULL
    attr (x, "na_range") <- NULL
    class (x) <- NULL

    return (x)
}

`zap_missing.declared` <- function (x) {
    attr (x, "na_index") <- NULL
    attr (x, "na_values") <- NULL
    attr (x, "na_range") <- NULL

    return (x)
}
