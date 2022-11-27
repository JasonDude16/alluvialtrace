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
