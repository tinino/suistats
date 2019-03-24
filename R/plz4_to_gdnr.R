#' Transform PLZ4 to GDNR
#'
#' @param x Vector with PLZ4.
#'
#' @return Vector with municipality numbers according to BFS (GDNR).
#' @export
#'
#' @examples
plz4_to_gdnr <- function(x, true_match_only = FALSE) {
  lookup_table <- suistats::plz_fill_table

  if(true_match_only) {
    lookup_table <- lookup_table[lookup_table$true_match, ]
  }
  mapvalues(
    x = x,
    from = lookup_table[["PLZ4"]],
    to = lookup_table[["GDNR"]],
    warn_missing = FALSE,
    missing_to_na = TRUE # should define a placeholder instead T/F for missing for correct NA (NA_integer_ here)
  )
}
