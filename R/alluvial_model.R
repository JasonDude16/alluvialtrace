#' Alluvial model specification
#'
#' @param x data frame/tibble containing position information
#' @param pos character vector of column in df containing position information
#'
#' @return numeric vector of model specification
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
