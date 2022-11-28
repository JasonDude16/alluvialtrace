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
alluvial_plot <- function(x, bar_width = 1L, hpad = 0, col = "y_from", xlabs = NULL, labels = NULL, ggtitle = NULL,
                          y_scale = "prop", bar_clrs = NULL, flow_clrs = NULL, bar_alpha = 1L, trace_alpha = 0.3,
                          flow_alpha = 0.7, show.legend = TRUE, auto_theme = FALSE, remove_y_axis = FALSE, trace_lwd = 1, ...) {

  if (class(x) != "alluvial_model") {
    stop("x must be an alluvial model object. First use `alluvial_prep()`, then pass the result to `x`")
  }

  x <- .compute_plot(x, bar_width, hpad)

  .alluvial_plot_base(
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
    trace_lwd = trace_lwd,
    ...
  )

}


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


.compute_plot <- function(x, bar_width = 1L, hpad = 0) {

  # this function calculates a user-adjusted bar width, adds white space between lines
  # and bars (if hpad > 0), and imputes NAs where the lines should not be plotted for both y_prop and y_freq

  x$bar_width <- x$bar_width * bar_width
  horizontal_ws <- (length(x$curve) -  x$bar_width) * hpad
  bar_adjust <-  x$bar_width + horizontal_ws

  x1 <- c(0, x$x_pos[-1] - floor(bar_adjust / 2))
  x2 <- c(x$x_pos[-length(x$x_pos)] + floor(bar_adjust / 2), x$x_pos[length(x$x_pos)])

  # dropping values behind bars
  if (x$type == "trace") {
    for (i in seq_along(x1)) {
      x$traces$y_prop <- ifelse(x$traces$x_axis > x1[i] & x$traces$x_axis < x2[i], NA, x$traces$y_prop)
      x$traces$y_count <- ifelse(x$traces$x_axis > x1[i] & x$traces$x_axis < x2[i], NA, x$traces$y_count)
    }
  }

  if (!is.null(x$flows)) {
    for (i in seq_along(x1)) {
      x$flows$ymin <- ifelse(x$flows$x_axis > x1[i] & x$flows$x_axis < x2[i], NA, x$flows$ymin)
    }
  }

  return(x)

}


.alluvial_plot_base <- function(x, col, xlabs, labels, ggtitle, y_scale, bar_clrs, flow_clrs, bar_alpha,
                                trace_alpha, flow_alpha, show.legend, auto_theme, remove_y_axis, trace_lwd, ...) {

  p <- ggplot2::ggplot() +
    ggplot2::geom_bar(
      data = x$bars,
      stat = "identity",
      mapping = ggplot2::aes(
        x = x_pos,
        y = .data[[paste0("y_", y_scale)]],
        fill = as.factor(y_value)
      ),
      width = x$bar_width,
      alpha = bar_alpha
    ) +
    ggplot2::ggtitle(ggtitle)

  if (x$type == "trace") {
    p <- p +
      ggplot2::geom_line(
        data = x$traces,
        mapping = ggplot2::aes(
          x = x_axis,
          y = .data[[paste0("y_", y_scale)]],
          col = as.factor(.data[[col]]),
          group = .data[[x$ID]]
        ),
        alpha = trace_alpha,
        na.rm = TRUE,
        lwd = trace_lwd
      )
  }

  if (!is.null(x$flows)) {
    p <- p +
      ggplot2::geom_ribbon(
        data = x$flows,
        mapping = ggplot2::aes(
          x_axis,
          ymin = ymin,
          ymax = ymax,
          group = interaction(y_from, y_to),
          fill = as.factor(.data[[col]])
        ),
        alpha = flow_alpha
      )
  }

  if (!is.null(labels)) {

    geom_text <- ggplot2::geom_text
    geom_label <- ggplot2::geom_label

    geom_fun <- get(paste0("geom_", labels$type))

    if (is.null(labels$where$steps)) {
      labels$where$steps <- x$steps
    }

    if (is.null(labels$where$values)) {
      labels$where$values <- unique(x$bars$y_value)
    }

    # using all values
    if (all(labels$where == "all")) {
      labs <- dplyr::filter(x$bars , step %in% x$steps, y_value %in% unique(x$bars$y_value))
    } else {
      labs <- dplyr::filter(x$bars , step %in% labels$where$steps, y_value %in% labels$where$values)
    }

    labs$y_prop <- round(labs$y_prop, 2)

    p <- p + geom_fun(
      data = labs,
      ggplot2::aes(
        x = x_pos,
        y = .data[[paste0(y_scale, "_lab_pos")]],
        label = .data[[paste0("y_", labels$what)]]
      ),
      ...
    )

  }

  # suppressing warning because the scale is actually continuous
  # and we're supplying discrete x-labels
  if (!is.null(xlabs)) {
    p <- p + suppressWarnings(ggplot2::scale_x_discrete(limits = x$x_pos, labels = xlabs))
  } else {
    p <- p + suppressWarnings(ggplot2::scale_x_discrete(limits = x$x_pos, labels = x$steps))
  }

  if (!is.null(bar_clrs)) {
    p <- p + ggplot2::scale_fill_manual(values = bar_clrs)
  }

  if (!is.null(flow_clrs)) {
    p <- p + ggplot2::scale_color_manual(values = flow_clrs)
  }

  if (auto_theme) {
    p <- p +
      ggplot2::theme_classic() +
      ggplot2::theme(
        axis.line.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(
          hjust = 0.5,
          vjust = .1,
          family = "Arial",
          face = "bold",
          size = 28
        ),
        axis.text.x = ggplot2::element_text(
          family = "Arial",
          face = "bold",
          size = 18,
          vjust = 3
        )
      )
  }

  if (remove_y_axis) {
    p <- p + ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank()
    )
  }

  # apparently legend.position only drops legends when they're also set to false in main plot
  if (!show.legend) {
    p <- p + ggplot2::theme(legend.position = "none")
  }

  return(p)

}
