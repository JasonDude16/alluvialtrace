#' Alluvial curve specification
#'
#' @param from integer specifying start point of sequence
#' @param to integer specifying end point of sequence
#' @param length.out positive integer specifying length of curve
#'
#' @return
#' @export
#'
#' @examples
alluvial_curve <- function(from = -6, to = 6, length.out = 49) {
  seq(from = from, to = to, length.out = length.out)
}
