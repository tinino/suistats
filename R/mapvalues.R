#' Map values
#'
#' Maps values of given input to another value, given the vectors \code{from} and \code{to}. This function is a modified version of \code{plyr::mapvalues}, but treats missing values more strict (by default).
#'
#' @param x Vector to be translated/recode.
#' @param from From-values.
#' @param to To-values. Note: same order and length as from!
#' @param warn_missing Warning, if missings occur (not every in \code{x} has a match in \code{from}). Default is \code{FALSE}.
#' @param missing_to_na If \code{TRUE} (default), values that do not occur in the from-vector will be transformed to \code{NA}. If \code{FALSE}, these values will remain as given in \code{x}, hence untouched.
#'
#' @return Vector of length \code{x} with mapped values.
#' @export
#'
#' @examples
#' mapvalues(x = letters, from = letters, to = rev(letters)) # recode letters from a-z by reverse order, thus output is z-a
mapvalues <- function (x, from, to, warn_missing = FALSE, missing_to_na = TRUE)
{
  if (length(from) != length(to)) {
    stop("`from` and `to` vectors are not the same length.")
  }
  if (!is.atomic(x)) {
    stop("`x` must be an atomic vector.")
  }
  if (is.factor(x)) {
    levels(x) <- mapvalues(levels(x), from, to, warn_missing)
    return(x)
  }
  mapidx <- match(x, from)
  mapidxNA <- is.na(mapidx)
  from_found <- sort(unique(mapidx))
  if (warn_missing && length(from_found) != length(from)) {
    message("The following `from` values were not present in `x`: ",
            paste(from[!(1:length(from) %in% from_found)], collapse = ", "))
  }
  if (missing_to_na) {
    return(to[match(x, from)])
  } else {
    x[!mapidxNA] <- to[mapidx[!mapidxNA]]
    return(x)
  }
}
