#' Switch from GDNR to other BFS-levels
#'
#' Maps values from municipality ID (\code{GDNR}) one to another, including zip-codes, BFS-IDs, Grossraumnummer,
#' ... check \code{names(bfs_regio_info)} for alle options.
#'
#' @param x Vector with values to be mapped.
#' @param to Output category.
#' @param ... Pass other settings to \code{mapvalues}.
#'
#' @return Vector of same length as input with the respective to-values.
#' @export
#' @author Tino Good, \email{onit.good+suistats@gmail.com}
#'
#' @examples
#' switch_level(x = 1000, to = "MSNR")
switch_level <- function(x, from = "GDNR", to, ...) {
  if (!all(to %in% names(suistats::bfs_regio_info))) {
    stop(sprintf("Both values, from and to, must indicate a column name in suistats::bfs_regio_info!\nThese are: %s",
                 toString(names(suistats::bfs_regio_info))))
  }
  mapvalues(x = x, from = suistats::bfs_regio_info[["GDNR"]], to = suistats::bfs_regio_info[[to]], ...)
}
