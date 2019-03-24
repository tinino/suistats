#' GPS to LV03
#'
#' Transforms GPS coordinates to Swiss LV03 coordinates, such that they can be used with the BFS maps.
#'
#' @param long Longitude, in degrees.
#' @param lat Latitude, in degrees.
#'
#' @return Named list with \code{long} and \code{lat}.
#' @export
#' @author Tino Good, \email{onit.good@gmail.com}
#' @examples
#' gps_to_lv03(long = 7.45, lat = 46.95)
gps_to_lv03 <- function(long, lat) {
  # calc difference to Bern:
  long_rel <- (long * 3600 - 26782.5)/10000
  lat_rel <- (lat * 3600 - 169028.66)/10000
  # now transform to get the approximation:

  long_out <- 600072.37 +
    211455.93 * long_rel -
    10938.51 * long_rel * lat_rel -
    .36 * long_rel * lat_rel^2 -
    44.54 * long_rel^3

  lat_out <- 200147.07 +
    308807.95 * lat_rel +
    3745.25 * long_rel^2 +
    76.63 * lat_rel^2 -
    194.56 * long_rel^2 * lat_rel +
    119.79 * lat_rel^3

  return(list(long = long_out, lat = lat_out))
}
