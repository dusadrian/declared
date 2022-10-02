`validate_declared` <- function (x = double(), labels = NULL, label = NULL,
                                na_values = NULL, na_range = NULL, ...) {

  if (!is.numeric (x) && !is.character (x) && !all (is.na (x))) {
    stopError_ ("`x` must be a numeric or a character vector.")
  }

  if (!is.null (labels)) {
    if (is.null (names (labels))) {
      stopError_ ("`labels` must have names.")
    }

    if (any (duplicated (stats::na.omit (labels)))) {
      stopError_ ("`labels` must be unique.")
    }
  }

  if (
    !is.null (label) &&
    (!is.atomic (label) || !is.character (label) || length (label) != 1)
  ) {
    stopError_ ("`label` must be a character vector of length one.")
  }

  if (!is.null (na_values)) {
    if (any (is.na (na_values))) {
      stopError_ ("`na_values` should not contain NA values.")
    }
  }

  if (!is.null (na_range)) {
    type_ok <-  all (is.na (x)) ||
      (is.character (x) && is.character (na_range)) ||
      (is.numeric (x) && is.numeric (na_range))

    if (!type_ok || length (na_range) != 2) {
      stopError_ (
        "`na_range` must be a vector of length two of the same type as `x`."
      )
    }

    if (any (is.na (na_range))) {
      stopError_ ("`na_range` can not contain missing values.")
    }
  }
}
