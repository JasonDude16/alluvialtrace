#' Title
#'
#' @param x
#' @param bar_width
#' @param hpad
#' @param col
#' @param xlabs
#' @param labels
#' @param ggtitle
#' @param y_scale
#' @param bar_clrs
#' @param flow_clrs
#' @param bar_alpha
#' @param flow_alpha
#' @param show.legend
#' @param auto_theme
#' @param remove_y_axis
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
alluvial_plot <- function(x, bar_width = 1L, hpad = 0, col = "y_from", xlabs = NULL, labels = NULL, ggtitle = NULL,
                          y_scale = "prop", bar_clrs = NULL, flow_clrs = NULL, bar_alpha = 1L, flow_alpha = 0.3,
                          show.legend = TRUE, auto_theme = FALSE, remove_y_axis = FALSE, ...) {

  if (class(x) != "alluvial_model") {
    stop("x must be an alluvial model object. First use `alluvial_prep()`, then pass the result to `x`")
  }

  x <- .compute_plot(x, bar_width, hpad)

  .alluvial_base(
    x,
    col = col,
    xlabs = xlabs,
    labels = labels,
    ggtitle = ggtitle,
    y_scale = y_scale,
    bar_clrs = bar_clrs,
    flow_clrs = flow_clrs,
    bar_alpha = bar_alpha,
    flow_alpha = flow_alpha,
    show.legend = show.legend,
    auto_theme = auto_theme,
    remove_y_axis = remove_y_axis,
    ...
  )

}
