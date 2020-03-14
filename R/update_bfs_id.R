#' Merge Municipalities
#'
#' Merge municipalities af of a certain target year to enable combination with statistics or geodata as of target year (including maps etc.).
#'
#' @param bfs_id Vector with BFS IDs.
#' @param target_year Target year. Default ist most recent available (therefore \code{Inf}).
#'
#' @return Vector with BFS IDs as of \code{target_year}.
#' @export
#' @importFrom dplyr select left_join mutate transmute pull filter ends_with
#' @author Tino Good, \email{onit.good+suistats@gmail.com}
#'
#' @examples
#' update_bfs_id(bfs_id = 132, target_year = 2017) # still 132, since merger only happens in 2018:
#' update_bfs_id(bfs_id = 132, target_year = 2018) # now 295
#' update_bfs_id(bfs_id = 132) # default: most recent
update_bfs_id <- function(bfs_id, target_year = Inf) {
  reduce_mm <- function(x, y) {
    x %>% select(ends_with("commune_id")) %>%
      left_join(y, by = intersect(names(.), names(y))) %>%
      mutate(to_commune_id = ifelse(is.na(to_commune_id), from_commune_id, to_commune_id)) %>%
      transmute(from_commune_id = to_commune_id, year = unique(y$year))
  }
  mmut <- filter(suistats::municipality_mergers, year <= target_year)
  Reduce(f = reduce_mm, x = split(x = mmut, f = mmut$year), init = data.frame(from_commune_id = bfs_id, stringsAsFactors = FALSE)) %>%
    transmute(year, to_commune_id = from_commune_id, from_commune_id = bfs_id) %>%
    pull(to_commune_id)
}
