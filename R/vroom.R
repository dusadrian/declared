`output_column.declared` <- function(x) {
  as.character(undeclare(x), values = TRUE)
}
