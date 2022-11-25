#' Title
#'
#' @param x
#' @param pos
#'
#' @return
#' @export
#'
#' @examples
alluvial_model <- function(x, pos) {

  # euler's constant
  e <- 2.71828

  # sigmoid curve model
  with(x, ifelse(
    pos_diff > 0,
    pos_diff / (1 + e ^ (-curve)) + x[[pos]],
    ifelse(
      pos_diff < 0,
      abs(pos_diff) / (1 + e ^ (curve)) + (x[[pos]] + pos_diff),
      x[[pos]]
    )
  ))
}
