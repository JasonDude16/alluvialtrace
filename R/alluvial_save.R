#' Save an alluvial plot
#'
#' @param p A `ggplot` or htmlwidget object.
#' @param filename Output file path.
#' @param ... Additional arguments passed to `ggplot2::ggsave()` for ggplot objects
#'   or `htmlwidgets::saveWidget()` for htmlwidget objects.
#'
#' @return Invisibly returns the saved object.
#' @export
alluvial_save <- function(p, filename, ...) {

  if (inherits(p, "ggplot")) {
    ggplot2::ggsave(filename, plot = p, ...)

  } else {
    htmlwidgets::saveWidget(p, file = filename, ...)

  }

  invisible(p)
}
