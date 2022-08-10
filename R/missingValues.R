
`missingValues` <- function(x) {

  mv <- rep(NA, length(x))

  if (is.declared(x)) {
    misvals <- attr(x, "na_index")
    mv[as.numeric(names(misvals))] <- misvals
  }

  return(mv)
}


`missingValues<-` <- function(x, value) {

  class(x) <- setdiff(class(x), "declared")
  other_classes <- setdiff(class(x), c("integer", "double", "character", "numeric", "complex", "haven_labelled", "haven_labelled_spss", "vctrs_vctr"))

  notna <- !is.na(value)
  x[notna] <- NA

  if (!all(is.na(x))) {
    x <- coerceMode_(x)
  }

  if (any(notna)) {
    na_index <- which(notna)
    names(na_index) <- value[notna]
    attr(x, "na_index") <- na_index
  }

  structure(x, class = c("declared", other_classes, class(x)))
}
