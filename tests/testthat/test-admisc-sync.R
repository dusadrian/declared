local_edition (3)

admisc_root <- "/Users/dusadrian/Lucru/_R/admisc"
declared_root <- normalizePath (file.path ("..", ".."), mustWork = TRUE)

top_level_function_names <- function (files) {
  result <- character()

  for (file in files) {
    expressions <- parse (file)
    for (expr in expressions) {
      if (
        is.call (expr) &&
        identical (expr[[1]], as.name ("<-")) &&
        is.call (expr[[3]]) &&
        identical (expr[[3]][[1]], as.name ("function"))
      ) {
        result <- c (result, as.character (expr[[2]]))
      }
    }
  }

  return (unique (result))
}

declared_internal_function_names <- function () {
  top_level_function_names (file.path (declared_root, "R", "internals.R"))
}

admisc_function_names <- function () {
  top_level_function_names (list.files (
    file.path (admisc_root, "R"),
    pattern = "[.]R$",
    full.names = TRUE
  ))
}

normalize_admisc_name <- function (name) {
  sub ("_$", "", name)
}

test_that("admisc-related internal helpers are classified", {
  skip_if_not(
    identical (Sys.getenv ("DECLARED_CHECK_ADMISC_SYNC"), "true"),
    "admisc sync check is opt-in"
  )
  skip_if_not (dir.exists (admisc_root), "admisc checkout is not available")

  synced_helpers <- c(
    "asNumeric_",
    "numdec_",
    "possibleNumeric_",
    "trimstr_",
    "wholeNumeric_"
  )
  known_unsynced_candidates <- c(
    "anyTagged_",
    "coerceMode_",
    "getName_",
    "getTag_",
    "hasTag_",
    "makeTag_",
    "padBoth_",
    "padLeft_",
    "padRight_",
    "stopError_",
    "tryCatchWEM_"
  )

  declared_names <- declared_internal_function_names()
  admisc_names <- admisc_function_names()
  declared_admisc_candidates <- declared_names[
    endsWith (declared_names, "_") &
      is.element (normalize_admisc_name (declared_names), admisc_names)
  ]

  expected_candidates <- c(
    synced_helpers,
    known_unsynced_candidates
  )
  expect_equal(
    sort (declared_admisc_candidates),
    sort (expected_candidates),
    info = paste (
      "A declared internal helper matches an admisc helper but is not",
      "classified as synced or intentionally package-specific."
    )
  )
})
