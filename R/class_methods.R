#' @export
`as.character.declared` <- function(x, values = FALSE, ...) {

  labels <- names_values(x, drop_na = TRUE)

  # x <- undeclare(x, drop = TRUE)
  attributes(x) <- NULL

  if (isTRUE(values)) {
    return(as.character(x))
  }

  x[is.element(x, labels)] <- names(labels)[match(x[is.element(x, labels)], labels)]

  return(x)
}

#' @export
`[.declared` <- function(x, i, ...) {
  attrx <- attributes(x)
  x <- undeclare(x)
  x <- NextMethod()
  # attrx$label, if not existing, takes from attrx$labels
  # attrx[["label"]] is something like attr(x, "label", exact = TRUE)
  declared(x, attrx[["labels"]], attrx$na_values, attrx$na_range, attrx[["label"]])
}

#' @export
`[<-.declared` <- function(x, i, value) {
  attrx <- attributes(x)
  value <- undeclare(value)
  x <- undeclare(x)
  x <- NextMethod()
  declared(x, attrx[["labels"]], attrx$na_values, attrx$na_range, attrx[["label"]])
}

#' @export
`c.declared` <- function(...) {
  dots <- list(...)
  declared <- unlist(lapply(dots, is.declared))
  na_values <- sort(unique(unlist(
    lapply(dots, function(x) attr(x, "na_values"))
  )))

  labels <- unlist(lapply(dots, function(x) {
    attr(x, "labels", exact = TRUE)
  }))

  duplicates <- duplicated(labels)

  if (length(wduplicates <- which(duplicates)) > 0) {
    for (i in seq(length(wduplicates))) {
      if (length(unique(names(
        labels[labels == labels[wduplicates[i]]]
      ))) > 1) {
        stopError_("Labels must be unique.")
      }
    }
  }

  labels <- sort(labels[!duplicates])

  na_range <- lapply(dots, function(x) attr(x, "na_range", exact = TRUE))
  nulls <- unlist(lapply(na_range, is.null))

  if (all(nulls)) {
    na_range <- NULL
  }
  else {
    if (sum(nulls) == length(na_range) - 1) {
      na_range <- unlist(na_range)
    }
    else {
      compatible <- logical(length(na_range))
      if (!is.null(na_range)) {
        for (i in seq(1, length(na_range) - 1)) {
          nai <- na_range[[i]]
          if (is.null(nai)) {
            compatible[i] <- TRUE
          }
          else {
            for (j in seq(2, length(na_range))) {
              naj <- na_range[[j]]
              if (is.null(naj)) {
                compatible[j] <- TRUE
              }
              else {
                if (any(is.element(seq(nai[1], nai[2]), seq(naj[1], naj[2]))) > 0) {
                  compatible[i] <- TRUE
                  compatible[j] <- TRUE
                }
              }
            }
          }
        }
      }

      if (any(!compatible)) {
        stopError_("Incompatible NA ranges.")
      }

      na_range <- range(unlist(na_range))
    }
  }

  dots <- unlist(lapply(dots, function(x) {
    if (is.declared(x)) x <- undeclare(x)
    attributes(x) <- NULL
    return(x)
  }))

  return(declared(
    dots,
    labels = labels,
    na_values = na_values,
    na_range = na_range,
    label = attr(dots[[which(declared)[1]]], "label", exact = TRUE)
  ))
}

#' @export
`names<-.declared` <- function(x, value) {
  attr(x, "names") <- value
  x
}

#' @export
`sort.declared` <- function(x, decreasing = FALSE, ...) {

  dots <- list(...)
  callist <- list(x = x, decreasing = decreasing)

  if (is.element("na.last", names(dots))) {
    callist$na.last <-  dots$na.last
  }

  if (is.element("method", names(dots))) {
    callist$method <-  dots$method
  }

  if (is.element("empty.last", names(dots))) {
    callist$empty.last <-  dots$empty.last
  }

  xorder <- do.call("order_declared", callist)

  return(x[xorder])
}

#' @export
`duplicated.declared` <- function(x, incomparables = FALSE, ...) {
  x <- unclass(undeclare(x))
  NextMethod()
}

#' @export
`unique.declared` <- function(x, incomparables = FALSE, ...) {
  x[!duplicated(x)]
}

#' @export
`head.declared` <- function(x, n = 6L, ...) {
  lx <- length(x)
  if (n < 0) {
    n <- lx - abs(n)
  }
  n <- min(n, length(x))
  if (n < 1) {
    return(x[0])
  }
  x[seq(n)]
}

#' @export
`tail.declared` <- function(x, n = 6L, ...) {
  if (n < 1) {
    n <- 6L
  }
  lx <- length(x)
  n <- min(n, lx)
  x[seq(lx - n + 1, lx)]
}

#' @export
`na.omit.declared` <- function (object, ...)  {
  attrx <- attributes(object)
  attrx$na_index <- NULL
  object <- unclass(object)
  object <- NextMethod()
  attrx$na.action <- attr(object, "na.action")
  nms <- attrx$names
  if (!is.null(nms) && !is.null(attrx$na.action)) {
    nms <- nms[-attr(object, "na.action")]
    attrx$names <- nms
  }
  attributes(object) <- attrx
  return(object)
}

#' @export
`na.fail.declared` <- function (object, ...)  {
  object <- unclass(object)
  NextMethod()
}

#' @export
`na.exclude.declared` <- function (object, ...)  {
  attrx <- attributes(object)
  attrx$na_index <- NULL
  object <- unclass(object)
  object <- NextMethod()
  attrx$na.action <- attr(object, "na.action")
  nms <- attrx$names
  if (!is.null(nms) && !is.null(attrx$na.action)) {
    nms <- nms[-attr(object, "na.action")]
    attrx$names <- nms
  }
  attributes(object) <- attrx
  return(object)
}

#' @export
`mean.declared` <- function(x, ...) {
  na_index <- attr(x, "na_index")
  if (!is.null(na_index)) {
    x <- x[-na_index]
  }
  x <- unclass(x)
  NextMethod()
}

#' @export
`median.declared` <- function(x, na.rm = FALSE, ...) {
  na_index <- attr(x, "na_index")
  if (!is.null(na_index)) {
    x <- x[-na_index]
  }
  x <- unclass(x)
  NextMethod()
}

#' @export
`summary.declared` <- function(object, ...) {
  na_index <- attr(object, "na_index")
  if (!is.null(na_index)) {
    object[na_index] <- NA
  }
  object <- unclass(object)
  NextMethod()
}

#' @export
`all.equal.declared` <- function(target, current, ...) {
  na_index <- attr(target, "na_index")
  target <- undeclare(target, drop = TRUE)
  if (is.declared(current)) {
    current <- undeclare(current, drop = TRUE)
  }

  allna <- TRUE

  if (!is.null(na_index)) {
    allna <- all.equal(target[na_index], current[na_index])
    target <- target[-na_index]
    current <- current[-na_index]
  }

  allv <- all.equal(target, current)

  if (isTRUE(allv)) {
    if (isTRUE(allna)) {
      return(TRUE)
    }
    return(paste("Declared mising values", tolower(allna)))
  }

  return(allv)
}



# Math operations

#' @export
`abs.declared` <- function(x) {
  attributes(x) <- NULL
  .Primitive("abs")(x)
}

#' @export
`sign.declared` <- function(x) {
  attributes(x) <- NULL
  .Primitive("sign")(x)
}

#' @export
`sqrt.declared` <- function(x) {
  attributes(x) <- NULL
  .Primitive("sqrt")(x)
}

#' @export
`floor.declared` <- function(x) {
  attributes(x) <- NULL
  .Primitive("floor")(x)
}

#' @export
`ceiling.declared` <- function(x) {
  attributes(x) <- NULL
  .Primitive("ceiling")(x)
}

#' @export
`trunc.declared` <- function(x, ...) {
  attributes(x) <- NULL
  .Primitive("trunc")(x, ...)
}

#' @export
`round.declared` <- function(x, digits = 0) {
  attributes(x) <- NULL
  .Primitive("round")(x, digits)
}

#' @export
`signif.declared` <- function(x, digits = 0) {
  attributes(x) <- NULL
  .Primitive("signif")(x, digits)
}

#' @export
`exp.declared` <- function(x) {
  attributes(x) <- NULL
  .Primitive("exp")(x)
}

#' @export
`log.declared` <- function(x, base = exp(1)) {
  attributes(x) <- NULL
  .Primitive("log")(x, base)
}

#' @export
`expm1.declared` <- function(x) {
  attributes(x) <- NULL
  .Primitive("expm1")(x)
}

#' @export
`log1p.declared` <- function(x) {
  attributes(x) <- NULL
  .Primitive("log1p")(x)
}

#' @export
`cos.declared` <- function(x) {
  attributes(x) <- NULL
  .Primitive("cos")(x)
}

#' @export
`sin.declared` <- function(x) {
  attributes(x) <- NULL
  .Primitive("sin")(x)
}

#' @export
`tan.declared` <- function(x) {
  attributes(x) <- NULL
  .Primitive("tan")(x)
}

#' @export
`cospi.declared` <- function(x) {
  attributes(x) <- NULL
  .Primitive("cospi")(x)
}

#' @export
`sinpi.declared` <- function(x) {
  attributes(x) <- NULL
  .Primitive("sinpi")(x)
}

#' @export
`tanpi.declared` <- function(x) {
  attributes(x) <- NULL
  .Primitive("tanpi")(x)
}

#' @export
`acos.declared` <- function(x) {
  attributes(x) <- NULL
  .Primitive("acos")(x)
}

#' @export
`asin.declared` <- function(x) {
  attributes(x) <- NULL
  .Primitive("asin")(x)
}

#' @export
`atan.declared` <- function(x) {
  attributes(x) <- NULL
  .Primitive("atan")(x)
}

#' @export
`lgamma.declared` <- function(x) {
  attributes(x) <- NULL
  .Primitive("lgamma")(x)
}

#' @export
`gamma.declared` <- function(x) {
  attributes(x) <- NULL
  .Primitive("gamma")(x)
}

#' @export
`digamma.declared` <- function(x) {
  attributes(x) <- NULL
  .Primitive("digamma")(x)
}

#' @export
`trigamma.declared` <- function(x) {
  attributes(x) <- NULL
  .Primitive("trigamma")(x)
}

#' @export
`cumsum.declared` <- function(x) {
  attributes(x) <- NULL
  .Primitive("cumsum")(x)
}

#' @export
`cumprod.declared` <- function(x) {
  attributes(x) <- NULL
  .Primitive("cumprod")(x)
}

#' @export
`cummax.declared` <- function(x) {
  attributes(x) <- NULL
  .Primitive("cummax")(x)
}

#' @export
`cummin.declared` <- function(x) {
  attributes(x) <- NULL
  .Primitive("cummin")(x)
}



# Arithmetic operations
#' @export
`+.declared` <- function(e1, e2) {
  attributes(e1) <- NULL
  if (!missing(e2)) {
    if (is.declared(e2)) {
      attributes(e2) <- NULL
    }
  }
  .Primitive("+")(e1, e2)
}

#' @export
`-.declared` <- function(e1, e2) {
  attributes(e1) <- NULL
  if (!missing(e2)) {
    if (is.declared(e2)) {
      attributes(e2) <- NULL
    }
  }
  .Primitive("-")(e1, e2)
}

#' @export
`*.declared` <- function(e1, e2) {
  attributes(e1) <- NULL
  if (!missing(e2)) {
    if (is.declared(e2)) {
      attributes(e2) <- NULL
    }
  }
  .Primitive("*")(e1, e2)
}

#' @export
`/.declared` <- function(e1, e2) {
  attributes(e1) <- NULL
  if (!missing(e2)) {
    if (is.declared(e2)) {
      attributes(e2) <- NULL
    }
  }
  .Primitive("/")(e1, e2)
}

#' @export
`^.declared` <- function(e1, e2) {
  attributes(e1) <- NULL
  if (!missing(e2)) {
    if (is.declared(e2)) {
      attributes(e2) <- NULL
    }
  }
  .Primitive("^")(e1, e2)
}

#' @export
`%%.declared` <- function(e1, e2) {
  attributes(e1) <- NULL
  if (!missing(e2)) {
    if (is.declared(e2)) {
      attributes(e2) <- NULL
    }
  }
  .Primitive("%%")(e1, e2)
}

#' @export
`%/%.declared` <- function(e1, e2) {
  attributes(e1) <- NULL
  if (!missing(e2)) {
    if (is.declared(e2)) {
      attributes(e2) <- NULL
    }
  }
  .Primitive("%/%")(e1, e2)
}


`%*%.declared` <- function(x, y) {
  attributes(x) <- NULL
  if (!missing(y)) {
    if (is.declared(y)) {
      attributes(y) <- NULL
    }
  }
  .Primitive("%*%")(x, y)
}

#' @export
`&.declared` <- function(e1, e2) {
  attributes(e1) <- NULL
  if (!missing(e2)) {
    if (is.declared(e2)) {
      attributes(e2) <- NULL
    }
  }
  .Primitive("&")(e1, e2)
}

#' @export
`|.declared` <- function(e1, e2) {
  attributes(e1) <- NULL
  if (!missing(e2)) {
    if (is.declared(e2)) {
      attributes(e2) <- NULL
    }
  }
  .Primitive("|")(e1, e2)
}

#' @export
`!.declared` <- function(x) {
  attributes(x) <- NULL
  .Primitive("!")(x)
}

#' @export
`==.declared` <- function(e1, e2) {
  le1 <- attr(e1, "labels", exact = TRUE)
  e1 <- undeclare(e1)
  attributes(e1) <- NULL

  if (!missing(e2)) {
    if (is.declared(e2)) {
      e2 <- undeclare(e2)
      attributes(e2) <- NULL
    }

    if (length(e2) == 1 && is.element(e2, names(le1)) && !is.element(e2, e1)) {
      e2 <- le1[names(le1) == e2]
    }
  }

  .Primitive("==")(e1, e2)
}

#' @export
`!=.declared` <- function(e1, e2) {e1 <- unclass(undeclare(e1))
le1 <- attr(e1, "labels", exact = TRUE)
e1 <- undeclare(e1)
attributes(e1) <- NULL

if (!missing(e2)) {
  if (is.declared(e2)) {
    e2 <- undeclare(e2)
    attributes(e2) <- NULL
  }

  if (length(e2) == 1 && is.element(e2, names(le1)) && !is.element(e2, e1)) {
    e2 <- le1[names(le1) == e2]
  }
}


.Primitive("!=")(e1, e2)
}

#' @export
`<.declared` <- function(e1, e2) {
  e1 <- undeclare(e1)
  attributes(e1) <- NULL

  if (!missing(e2)) {
    if (is.declared(e2)) {
      e2 <- undeclare(e2)
      attributes(e2) <- NULL
    }
  }

  .Primitive("<")(e1, e2)
}

#' @export
`<=.declared` <- function(e1, e2) {
  e1 <- undeclare(e1)
  attributes(e1) <- NULL

  if (!missing(e2)) {
    if (is.declared(e2)) {
      e2 <- undeclare(e2)
      attributes(e2) <- NULL
    }
  }

  .Primitive("<=")(e1, e2)
}

#' @export
`>=.declared` <- function(e1, e2) {
  e1 <- undeclare(e1)
  attributes(e1) <- NULL

  if (!missing(e2)) {
    if (is.declared(e2)) {
      e2 <- undeclare(e2)
      attributes(e2) <- NULL
    }
  }

  .Primitive(">=")(e1, e2)
}

#' @export
`>.declared` <- function(e1, e2) {
  e1 <- undeclare(e1)
  attributes(e1) <- NULL

  if (!missing(e2)) {
    if (is.declared(e2)) {
      e2 <- undeclare(e2)
      attributes(e2) <- NULL
    }
  }

  .Primitive(">")(e1, e2)
}

#' @export
`Arg.declared` <- function(z) {
  attributes(z) <- NULL
  .Primitive("Arg")(z)
}

#' @export
`Conj.declared` <- function(z) {
  attributes(z) <- NULL
  .Primitive("Conj")(z)
}

#' @export
`Im.declared` <- function(z) {
  attributes(z) <- NULL
  .Primitive("Im")(z)
}

#' @export
`Mod.declared` <- function(z) {
  attributes(z) <- NULL
  .Primitive("Mod")(z)
}

#' @export
`Re.declared` <- function(z) {
  attributes(z) <- NULL
  .Primitive("Re")(z)
}




# TODO:
# anyDuplicated() ?
# cut() ?
# diff() ?
