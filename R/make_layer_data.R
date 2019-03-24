#' Create Map-Layers for ggplot2
#'
#' Create a Swiss map for ggplot2 with desired borders/levels.
#'
#' @param agg_level Default is BFS-ID (GMDNR), Choose one of "GMDNR", "KTNR", "BZNR", "MSNR", "GRNR".
#'
#' @return A fortified dataframe that is ready for use with ggplot2.
#' @export
#'
#' @examples
#' make_layer_data(agg_level = "GMDNR")
make_layer_data <- function(agg_level = "GDNR") {
  #allowed_levels <- c("GDNR", "KTNR", "BZNR", "MSNR", "GRNR", "AMREG", "GD9T2012", "GD25T2012")
  allowed_levels <- names(suistats::bfs_regio_info)
  if (!agg_level %in% allowed_levels) {
    stop(sprintf("Wrong agg_level! Must be one of: %s", toString(allowed_levels)))
  }

  shp <- ch_shape_munip
  shp$GMDNR <- switch_level(x = shp$GMDNR, to = agg_level)

  out <- suppressWarnings(broom::tidy(shp, region = "GMDNR")) # bad solution, but somewhere we have factors.
  return(transform(out, id = as.numeric(id)))
}
