#' @export
`as.character.declared` <- function (
    x, drop_na = TRUE, values = FALSE, nolabels = FALSE, ...
) {

    allabels <- names_values (x, drop_na = drop_na)
    labels <- labels (x)
    attrx <- attributes (x)

    if (isFALSE (drop_na)) {
        x <- undeclare (x)
    }
    attributes (x) <- NULL

    if (isTRUE (attrx$date)) {
        if (is.null (attrx$na_index) & !is.null(labels)) {
            # in Date objects, only the missing values can possibly have labels
            attrx$na_index <- rep(1, length(labels)) # positions don't matter
            names (attrx$na_index) <- labels # the labels do matter
        }
        missing <- is.element (allabels, names(attrx$na_index))
        nms <- names (allabels)
        if (any(!missing)) {
            nms[!missing] <- as.character (as.Date (as.numeric (nms[!missing])))
        }
        allabels[!missing] <- as.character (as.Date (allabels[!missing]))
        names (allabels) <- nms

        xmissing <- is.element (x, allabels[missing])
        x[!xmissing] <- as.character (as.Date (x[!xmissing]))
    }

    if (isTRUE (values)) {
        return (as.character (x))
    }

    x <- names (allabels)[match (x, allabels)]

    if (isTRUE (nolabels)) {
        x[!is.element (x, names (labels))] <- NA
    }

    return (x)
}

#' @export
`[.declared` <- function (x, i, ...) {
  attrx <- attributes (x)
  x <- undeclare (x)
  x <- NextMethod()
  # attrx$label, if not existing, takes from attrx$labels
  # attrx[["label"]] is something like attr (x, "label", exact = TRUE)
  declared (
    x, attrx[["labels"]], attrx$na_values, attrx$na_range, attrx[["label"]]
  )
}

#' @export
`[<-.declared` <- function (x, i, value) {
  attrx <- attributes (x)
  value <- undeclare (value)
  x <- undeclare (x)
  xdate <- isTRUE (attrx$date)
  attributes (x) <- NULL
  x <- NextMethod()
  if (xdate) {
    x <- as.Date (x)
  }
  declared (
    x, attrx[["labels"]], attrx$na_values, attrx$na_range, attrx[["label"]]
  )
}

#' @export
`[.labels_df` <- function (x, i, ...) {
  result <- NextMethod()
  attr(result, "print_as_df") <- attr(x, "print_as_df")
  class(result) <- class(x)
  return(result)
}

#' @export
`c.declared` <- function (...) {
  dots <- list (...)
  declared <- unlist (lapply (dots, is.declared))
  na_values <- sort (unique (unlist (
    lapply (dots, function (x) attr (x, "na_values"))
  )))

  labels <- unlist (lapply (dots, function (x) {
    attr (x, "labels", exact = TRUE)
  }))

  duplicates <- duplicated (labels)

  if (length (wduplicates <- which (duplicates)) > 0) {
    for (i in wduplicates) {
      if (length (unique (names (labels[labels == labels[i]]))) > 1) {
        stopError_ ("Labels must be unique.")
      }
    }
  }

  labels <- sort (labels[!duplicates])

  na_range <- lapply (dots, function (x) attr (x, "na_range", exact = TRUE))
  nulls <- unlist (lapply (na_range, is.null))

  if (all (nulls)) {
    na_range <- NULL
  }
  else {
    if (sum (nulls) == length (na_range) - 1) {
      na_range <- unlist (na_range)
    }
    else {
      compatible <- logical (length (na_range))
      if (!is.null (na_range)) {
        for (i in seq (1, length (na_range) - 1)) {
          nai <- na_range[[i]]
          if (is.null (nai)) {
            compatible[i] <- TRUE
          }
          else {
            for (j in seq (2, length (na_range))) {
              naj <- na_range[[j]]
              if (is.null (naj)) {
                compatible[j] <- TRUE
              }
              else {
                if (
                  any (
                    is.element (seq (nai[1], nai[2]), seq (naj[1], naj[2]))
                  ) > 0
                ) {
                  compatible[i] <- TRUE
                  compatible[j] <- TRUE
                }
              }
            }
          }
        }
      }

      if (any (!compatible)) {
        stopError_ ("Incompatible NA ranges.")
      }

      na_range <- range(unlist (na_range))
    }
  }

  dots <- unlist (lapply (dots, function (x) {
    if (is.declared (x)) x <- undeclare (x)
    attributes (x) <- NULL
    return (x)
  }))

  return (declared (
    dots,
    labels = labels,
    na_values = na_values,
    na_range = na_range,
    label = attr (dots[[which (declared)[1]]], "label", exact = TRUE)
  ))
}

#' @export
`names<-.declared` <- function (x, value) {
  attr (x, "names") <- value
  x
}

#' @export
`sort.declared` <- function (x, decreasing = FALSE, ...) {
  return (x[order_declared (x, decreasing = decreasing, ... = ...)])
}

#' @export
`duplicated.declared` <- function (x, incomparables = FALSE, ...) {
  x <- unclass (undeclare (x))
  NextMethod()
}

#' @export
`unique.declared` <- function (x, incomparables = FALSE, ...) {
  x[!duplicated (x)]
}

#' @export
`head.declared` <- function (x, n = 6L, ...) {
  lx <- length (x)
  if (n < 0) {
    n <- lx - abs (n)
  }
  n <- min (n, lx)
  if (n < 1) {
    return (x[0])
  }
  x[seq (n)]
}

#' @export
`head.labels_df` <- function (x, n = 6L, ...) {
  lx <- length (x)
  if (n < 0) {
    n <- lx - abs (n)
  }
  n <- min (n, lx)
  if (n < 1) {
    result <- x[0]
  } else {
    result <- x[seq (n)]
  }
  attr(result, "print_as_df") <- attr(x, "print_as_df")
  class(result) <- class(x)
  return(result)
}

#' @export
`tail.declared` <- function (x, n = 6L, ...) {
  if (n < 1) {
    n <- 6L
  }
  lx <- length (x)
  n <- min (n, lx)
  x[seq (lx - n + 1, lx)]
}

#' @export
`tail.labels_df` <- function (x, n = 6L, ...) {
  if (n < 1) {
    n <- 6L
  }
  lx <- length (x)
  n <- min (n, lx)
  result <- x[seq (lx - n + 1, lx)]

  attr(result, "print_as_df") <- attr(x, "print_as_df")
  class(result) <- class(x)
  return(result)
}

#' @export
`na.omit.declared` <- function (object, ...)  {
  attrx <- attributes (object)
  attrx$na_index <- NULL
  object <- unclass (object)
  object <- NextMethod()
  attrx$na.action <- attr (object, "na.action")
  nms <- attrx$names
  if (!is.null (nms) && !is.null (attrx$na.action)) {
    nms <- nms[-attr (object, "na.action")]
    attrx$names <- nms
  }
  attributes (object) <- attrx
  return (object)
}

#' @export
`na.fail.declared` <- function (object, ...)  {
  object <- unclass (object)
  if (isTRUE (attr (object, "date"))) {
    attributes (object) <- NULL
    object <- as.Date (object)
  }
  NextMethod()
}

#' @export
`na.exclude.declared` <- function (object, ...)  {
  attrx <- attributes (object)
  attrx$na_index <- NULL
  object <- unclass (object)
  object <- NextMethod()
  attrx$na.action <- attr (object, "na.action")
  nms <- attrx$names
  if (!is.null (nms) && !is.null (attrx$na.action)) {
    nms <- nms[-attr (object, "na.action")]
    attrx$names <- nms
  }
  attributes (object) <- attrx
  return (object)
}

#' @export
`mean.declared` <- function (x, ...) {
  xdate <- isTRUE (attr (x, "date"))
  na_index <- attr (x, "na_index")
  if (!is.null (na_index)) {
    x <- x[-na_index]
  }
  x <- unclass (x)
  if (xdate) {
    attributes (x) <- NULL
    x <- as.Date (x)
  }
  NextMethod()
}

#' @export
`median.declared` <- function (x, na.rm = FALSE, ...) {
  xdate <- isTRUE (attr (x, "date"))
  na_index <- attr (x, "na_index")
  if (!is.null (na_index)) {
    x <- x[-na_index]
  }
  x <- unclass (x)
  if (xdate) {
    attributes (x) <- NULL
    x <- as.Date (x)
  }
  NextMethod()
}

#' @export
`summary.declared` <- function (object, ...) {
  object <- unclass (object)
  if (isTRUE (attr (object, "date"))) {
    attributes (object) <- NULL
    object <- as.Date (object)
  }
  NextMethod()
}

#' @export
`all.equal.declared` <- function (target, current, ...) {
  na_index <- attr (target, "na_index")
  target <- undeclare (target, drop = TRUE)
  if (is.declared (current)) {
    current <- undeclare (current, drop = TRUE)
  }

  allna <- TRUE

  if (!is.null (na_index)) {
    allna <- all.equal(target[na_index], current[na_index])
    target <- target[-na_index]
    current <- current[-na_index]
  }

  allv <- all.equal(target, current)

  if (isTRUE (allv)) {
    if (isTRUE (allna)) {
      return (TRUE)
    }
    return (paste ("Declared mising values", tolower(allna)))
  }

  return (allv)
}


# Math operations

#' @export
`abs.declared` <- function (x) {
  x <- check_date (x)
  .Primitive ("abs")(x)
}

#' @export
`sign.declared` <- function (x) {
  x <- check_date (x)
  .Primitive ("sign")(x)
}

#' @export
`sqrt.declared` <- function (x) {
  x <- check_date (x)
  .Primitive ("sqrt")(x)
}

#' @export
`floor.declared` <- function (x) {
  x <- check_date (x)
  .Primitive ("floor")(x)
}

#' @export
`ceiling.declared` <- function (x) {
  x <- check_date (x)
  .Primitive ("ceiling")(x)
}

#' @export
`trunc.declared` <- function (x, ...) {
  x <- check_date (x)
  .Primitive ("trunc")(x, ...)
}

#' @export
`round.declared` <- function (x, digits = 0) {
  x <- check_date (x)
  .Primitive ("round")(x, digits)
}

#' @export
`signif.declared` <- function (x, digits = 0) {
  x <- check_date (x)
  .Primitive ("signif")(x, digits)
}

#' @export
`exp.declared` <- function (x) {
  x <- check_date (x)
  .Primitive ("exp")(x)
}

#' @export
`log.declared` <- function (x, base = exp(1)) {
  x <- check_date (x)
  .Primitive ("log")(x, base)
}

#' @export
`expm1.declared` <- function (x) {
  x <- check_date (x)
  .Primitive ("expm1")(x)
}

#' @export
`log1p.declared` <- function (x) {
  x <- check_date (x)
  .Primitive ("log1p")(x)
}

#' @export
`cos.declared` <- function (x) {
  x <- check_date (x)
  .Primitive ("cos")(x)
}

#' @export
`sin.declared` <- function (x) {
  x <- check_date (x)
  .Primitive ("sin")(x)
}

#' @export
`tan.declared` <- function (x) {
  x <- check_date (x)
  .Primitive ("tan")(x)
}

#' @export
`cospi.declared` <- function (x) {
  x <- check_date (x)
  .Primitive ("cospi")(x)
}

#' @export
`sinpi.declared` <- function (x) {
  x <- check_date (x)
  .Primitive ("sinpi")(x)
}

#' @export
`tanpi.declared` <- function (x) {
  x <- check_date (x)
  .Primitive ("tanpi")(x)
}

#' @export
`acos.declared` <- function (x) {
  x <- check_date (x)
  .Primitive ("acos")(x)
}

#' @export
`asin.declared` <- function (x) {
  x <- check_date (x)
  .Primitive ("asin")(x)
}

#' @export
`atan.declared` <- function (x) {
  x <- check_date (x)
  .Primitive ("atan")(x)
}

#' @export
`lgamma.declared` <- function (x) {
  x <- check_date (x)
  .Primitive ("lgamma")(x)
}

#' @export
`gamma.declared` <- function (x) {
  x <- check_date (x)
  .Primitive ("gamma")(x)
}

#' @export
`digamma.declared` <- function (x) {
  x <- check_date (x)
  .Primitive ("digamma")(x)
}

#' @export
`trigamma.declared` <- function (x) {
  x <- check_date (x)
  .Primitive ("trigamma")(x)
}

#' @export
`cumsum.declared` <- function (x) {
  na_index <- attr (x, "na_index")
  x <- check_date (x)
  if (is.null (na_index)) {
    return(cumsum (x))
  }
  x[-na_index] <- cumsum (x[-na_index])
  return(x)
}

#' @export
`cumprod.declared` <- function (x) {
  na_index <- attr (x, "na_index")
  x <- check_date (x)
  if (is.null (na_index)) {
    return(cumprod (x))
  }
  x[-na_index] <- cumprod (x[-na_index])
  return(x)
}

#' @export
`cummax.declared` <- function (x) {
  na_index <- attr (x, "na_index")
  x <- check_date (x)
  if (is.null (na_index)) {
    return(cummax (x))
  }
  x[-na_index] <- cummax (x[-na_index])
  return(x)
}

#' @export
`cummin.declared` <- function (x) {
  na_index <- attr (x, "na_index")
  x <- check_date (x)
  if (is.null (na_index)) {
    return(cummin (x))
  }
  x[-na_index] <- cummin (x[-na_index])
  return(x)
}



# Arithmetic operations
#' @export
`+.declared` <- function (e1, e2) {
  e1 <- check_date (e1)
  if (!missing(e2)) {
    if (is.declared (e2)) {
      e2 <- check_date (e2)
    }
  }
  .Primitive ("+")(e1, e2)
}

#' @export
`-.declared` <- function (e1, e2) {
  e1 <- check_date (e1)
  if (!missing(e2)) {
    if (is.declared (e2)) {
      e2 <- check_date (e2)
    }
  }
  .Primitive ("-")(e1, e2)
}

#' @export
`*.declared` <- function (e1, e2) {
  e1 <- check_date (e1)
  if (!missing(e2)) {
    if (is.declared (e2)) {
      e2 <- check_date (e2)
    }
  }
  .Primitive ("*")(e1, e2)
}

#' @export
`/.declared` <- function (e1, e2) {
  e1 <- check_date (e1)
  if (!missing(e2)) {
    if (is.declared (e2)) {
      e2 <- check_date (e2)
    }
  }
  .Primitive ("/")(e1, e2)
}

#' @export
`^.declared` <- function (e1, e2) {
  e1 <- check_date (e1)
  if (!missing(e2)) {
    if (is.declared (e2)) {
      e2 <- check_date (e2)
    }
  }
  .Primitive ("^")(e1, e2)
}

#' @export
`%%.declared` <- function (e1, e2) {
  e1 <- check_date (e1)
  if (!missing(e2)) {
    if (is.declared (e2)) {
      e2 <- check_date (e2)
    }
  }
  .Primitive ("%%")(e1, e2)
}

#' @export
`%/%.declared` <- function (e1, e2) {
  e1 <- check_date (e1)
  if (!missing(e2)) {
    if (is.declared (e2)) {
      e2 <- check_date (e2)
    }
  }
  .Primitive ("%/%")(e1, e2)
}

#' @export
`&.declared` <- function (e1, e2) {
  e1 <- check_date (e1)
  if (!missing(e2)) {
    if (is.declared (e2)) {
      e2 <- check_date (e2)
    }
  }
  .Primitive ("&")(e1, e2)
}

#' @export
`|.declared` <- function (e1, e2) {
  e1 <- check_date (e1)
  if (!missing(e2)) {
    if (is.declared (e2)) {
      e2 <- check_date (e2)
    }
  }
  .Primitive ("|")(e1, e2)
}

#' @export
`!.declared` <- function (x) {
  x <- check_date (x)
  .Primitive ("!")(x)
}

#' @export
`==.declared` <- function (e1, e2) {
  le1 <- attr (e1, "labels", exact = TRUE)
  e1 <- undeclare (e1, drop = TRUE)

  if (!missing(e2)) {
    if (is.declared (e2)) {
      e2 <- undeclare (e2, drop = TRUE)
    }

    if (
      length (e2) == 1 && is.element (e2, names (le1)) && !is.element (e2, e1)
    ) {
      e2 <- le1[names (le1) == e2]
    }
  }

  .Primitive ("==")(e1, e2)
}

#' @export
`!=.declared` <- function (e1, e2) {e1 <- unclass (undeclare (e1))
le1 <- attr (e1, "labels", exact = TRUE)
e1 <- undeclare (e1)
attributes (e1) <- NULL

if (!missing(e2)) {
  if (is.declared (e2)) {
    e2 <- undeclare (e2)
    attributes (e2) <- NULL
  }

  if (
    length (e2) == 1 && is.element (e2, names (le1)) && !is.element (e2, e1)
  ) {
    e2 <- le1[names (le1) == e2]
  }
}


.Primitive ("!=")(e1, e2)
}

#' @export
`<.declared` <- function (e1, e2) {
  e1 <- undeclare (e1)
  attributes (e1) <- NULL

  if (!missing(e2)) {
    if (is.declared (e2)) {
      e2 <- undeclare (e2)
      attributes (e2) <- NULL
    }
  }

  .Primitive ("<")(e1, e2)
}

#' @export
`<=.declared` <- function (e1, e2) {
  e1 <- undeclare (e1)
  attributes (e1) <- NULL

  if (!missing(e2)) {
    if (is.declared (e2)) {
      e2 <- undeclare (e2)
      attributes (e2) <- NULL
    }
  }

  .Primitive ("<=")(e1, e2)
}

#' @export
`>=.declared` <- function (e1, e2) {
  e1 <- undeclare (e1)
  attributes (e1) <- NULL

  if (!missing(e2)) {
    if (is.declared (e2)) {
      e2 <- undeclare (e2)
      attributes (e2) <- NULL
    }
  }

  .Primitive (">=")(e1, e2)
}

#' @export
`>.declared` <- function (e1, e2) {
  e1 <- undeclare (e1)
  attributes (e1) <- NULL

  if (!missing(e2)) {
    if (is.declared (e2)) {
      e2 <- undeclare (e2)
      attributes (e2) <- NULL
    }
  }

  .Primitive (">")(e1, e2)
}

#' @export
`Arg.declared` <- function (z) {
  z <- check_date (z)
  .Primitive ("Arg")(z)
}

#' @export
`Conj.declared` <- function (z) {
  z <- check_date (z)
  .Primitive ("Conj")(z)
}

#' @export
`Im.declared` <- function (z) {
  z <- check_date (z)
  .Primitive ("Im")(z)
}

#' @export
`Mod.declared` <- function (z) {
  z <- check_date (z)
  .Primitive ("Mod")(z)
}

#' @export
`Re.declared` <- function (z) {
  z <- check_date (z)
  .Primitive ("Re")(z)
}




# TODO:
# anyDuplicated () ?
# cut() ?
# diff() ?
