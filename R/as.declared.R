#' @rdname declared
#'
#' @export
`as.declared` <- function (x, ...) {
  UseMethod ("as.declared")
}


#' @export
`as.declared.default` <- function (x, ...) {
  dots <- list (...)
  interactive <- isTRUE (dots$interactive)

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
`as.declared.haven_labelled` <- function (x, ...) {

  dots <- list (...)

  na_values <- attr (x, "na_values")
  na_range <- attr (x, "na_range")
  labels <- attr (x, "labels", exact = TRUE)
  label <- attr (x, "label", exact = TRUE)

  #---------------------------------------
  # necessary for DDIwR::convert
  format_spss <- attr (x, "format.spss")
  format_stata <- attr (x, "format.stata")
  format_sas <- attr (x, "format.sas")
  #---------------------------------------

  if (!inherits (x, "haven_labelled_spss")) {
    tagged <- hasTag_ (x)
    attributes (x) <- NULL

    if (any (tagged)) {
      x[tagged] <- getTag_ (x[tagged])
    }

    if (!is.null (labels)) {
      nms <- names (labels)
      tagged <- hasTag_ (labels)

      if (any (tagged)) {
        labels[tagged] <- getTag_ (labels[tagged])
        na_values <- sort (unname (labels[tagged]))
      }

      labels <- coerceMode_ (labels)
      names (labels) <- nms
    }

    misvals <- na_values
  }
  else {
    misvals <- all_missing_values (unclass (x))
  }

  attributes (x) <- NULL
  pnx <- possibleNumeric_ (setdiff(x, na_values)) | all (is.na (x))

  charlabels <- FALSE
  if (!is.null (labels)) {
    if (length (setdiff (labels, na_values)) > 0) {
      charlabels <- !possibleNumeric_ (setdiff(labels, na_values))
    }
  }

  attr (x, "xchar") <- !pnx | charlabels
  missingValues (x)[is.element (x, misvals)] <- x[is.element (x, misvals)]

  attr (x, "na_values") <- na_values
  attr (x, "na_range") <- na_range
  attr (x, "labels") <- labels
  attr (x, "label") <- label
  attr (x, "format.spss") <- format_spss
  attr (x, "format.stata") <- format_stata
  attr (x, "format.sas") <- format_sas

  return (x)
}


#' @export
`as.declared.factor` <- function (x, ...) {
  return (declared (x, ... = ...))
}


#' @export
`as.declared.data.frame` <- function (x, ..., interactive = FALSE) {
  if (isFALSE (interactive)) {
    x[] <- lapply (x, as.declared, interactive = FALSE, ... = ...)
  }
  else {
    nms <- names (x)
    for (i in seq (length (nms))) {
      x[[i]] <- as.declared (x[[i]], vname_ = nms[i], ... = ...)
    }
  }

  return (x)
}
