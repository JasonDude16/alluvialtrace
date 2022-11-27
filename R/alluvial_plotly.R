#' Title
#'
#' @param x
#' @param highlight_col
#' @param highlight_on
#' @param opacityDim
#' @param dynamic
#' @param tooltip
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
#' @param trace_alpha
#' @param flow_alpha
#' @param show.legend
#' @param auto_theme
#' @param remove_y_axis
#' @param trace_lwd
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
alluvial_plotly <- function(x, highlight_col = "black", highlight_on = "plotly_hover", opacityDim = 0.2,
                            dynamic = FALSE, tooltip = x$ID, bar_width = 1L, hpad = 0, col = "y_from",
                            xlabs = NULL, labels = NULL, ggtitle = NULL, y_scale = "prop", bar_clrs = NULL,
                            flow_clrs = NULL, bar_alpha = 1L, trace_alpha = 0.3, flow_alpha = 0.7,
                            show.legend = TRUE, auto_theme = FALSE, remove_y_axis = FALSE, trace_lwd = 1, ...) {

  if (class(x) != "alluvial_model") {
    stop("x must be an alluvial model object. First use `alluvial_prep()`, then pass the result to x")
  }

  if (x$type == "flow") {
    stop("Plotting flows with plotly is not currently supported")
  }

  # removing flows from trace-level data because geom_ribbon, which is implemented by flows,
  # is not currently supported by plotly
  if (!is.null(x$flows)) {
    x$flows <- NULL
  }

  # all manipulations to model must happen prior to using highlight_key()
  x <- .compute_plot(x, bar_width, hpad)

  # replacing traces with a crosstalk::SharedData object which allows for plotly's interactivity
  x$traces <- plotly::highlight_key(x$traces, ~.data[[x$ID]])

  p <- .alluvial_plot_base(
    x,
    col = col,
    xlabs = xlabs,
    labels = labels,
    ggtitle = ggtitle,
    y_scale = y_scale,
    bar_clrs = bar_clrs,
    flow_clrs = flow_clrs,
    bar_alpha = bar_alpha,
    trace_alpha = trace_alpha,
    flow_alpha = flow_alpha,
    show.legend = show.legend,
    auto_theme = auto_theme,
    remove_y_axis = remove_y_axis,
    trace_lwd = trace_lwd
  )

  gg <- plotly::ggplotly(p, tooltip = tooltip)
  gg <- plotly::layout(gg, ...)

  plotly::highlight(gg, on = highlight_on, color = highlight_col, opacityDim = opacityDim, dynamic = dynamic)

}
