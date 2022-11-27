#' Title
#'
#' @param p
#' @param filename
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
alluvial_save <- function(p, filename, ...) {

  if (ggplot2::is.ggplot(p)) {
    ggplot2::ggsave(filename, plot = p, ...)

  } else {
    htmlwidgets::saveWidget(p, file = filename)

  }

}
