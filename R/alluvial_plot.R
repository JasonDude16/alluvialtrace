#' Plot prepared alluvial data
#'
#' @param x An `alluvial_model` object returned by `alluvial_prep_trace()` or `alluvial_prep_flow()`.
#' @param bar_width Multiplier for the computed bar width.
#' @param hpad Horizontal padding between bars and lines/ribbons.
#' @param col Column name used to color traces or flows.
#' @param xlabs Optional x-axis labels.
#' @param labels Optional label specification list.
#' @param ggtitle Optional plot title.
#' @param y_scale Either `"prop"` for proportions or `"count"` for counts.
#' @param bar_clrs Optional manual fill colors for bars.
#' @param flow_clrs Optional manual colors for trace lines and flow ribbons.
#' @param bar_alpha Bar opacity.
#' @param trace_alpha Trace line opacity.
#' @param flow_alpha Flow ribbon opacity.
#' @param border_col Bar border color.
#' @param show.legend Logical; show the legend.
#' @param auto_theme Logical; apply the package's default theme.
#' @param remove_y_axis Logical; hide y-axis text and line.
#' @param trace_lwd Trace line width.
#' @param ... Additional arguments passed to label geoms.
#'
#' @return A `ggplot` object.
#' @export
alluvial_plot <- function(x, bar_width = 1L, hpad = 0, col = "y_from", xlabs = NULL, labels = NULL, ggtitle = NULL,
                          y_scale = "prop", bar_clrs = NULL, flow_clrs = NULL, bar_alpha = 1L, trace_alpha = 0.3,
                          flow_alpha = 0.7, border_col = "black", show.legend = TRUE, auto_theme = FALSE, remove_y_axis = FALSE,
                          trace_lwd = 1, ...) {

  if (!inherits(x, "alluvial_model")) {
    stop("x must be an alluvial model object. First use `alluvial_prep_trace()` or `alluvial_prep_flow()`, then pass the result to `x`")
  }
  .validate_plot_args(x, col, xlabs, labels, y_scale, bar_clrs, flow_clrs)

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
    border_col = border_col,
    ...
  )

}


#' Plot prepared trace data with plotly
#'
#' @param x An `alluvial_model` object returned by `alluvial_prep_trace()`.
#' @param highlight_col Highlight color.
#' @param highlight_on Plotly event used for highlighting.
#' @param opacityDim Opacity for non-highlighted traces.
#' @param dynamic Logical; use dynamic highlighting.
#' @param tooltip Tooltip fields passed to `plotly::ggplotly()`.
#' @inheritParams alluvial_plot
#' @param ... Additional arguments passed to `plotly::layout()`.
#'
#' @return A `plotly` object.
#' @export
alluvial_plotly <- function(x, highlight_col = "black", highlight_on = "plotly_hover", opacityDim = 0.2, dynamic = FALSE,
                            tooltip = x$ID, bar_width = 1L, hpad = 0, col = "y_from", xlabs = NULL, labels = NULL, ggtitle = NULL,
                            y_scale = "prop", bar_clrs = NULL, flow_clrs = NULL, bar_alpha = 1L, trace_alpha = 0.3, flow_alpha = 0.7,
                            border_col = "black", show.legend = TRUE, auto_theme = FALSE, remove_y_axis = FALSE,
                            trace_lwd = 1, ...) {

  if (!inherits(x, "alluvial_model")) {
    stop("x must be an alluvial model object. First use `alluvial_prep_trace()`, then pass the result to x")
  }
  .validate_plot_args(x, col, xlabs, labels, y_scale, bar_clrs, flow_clrs)
  .validate_plotly_tooltip(x, tooltip)

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
  x$traces$.alluvial_tooltip <- .make_plotly_tooltip(x$traces, tooltip)

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
    trace_lwd = trace_lwd,
    border_col = border_col
  )

  gg <- plotly::ggplotly(p, tooltip = "text")
  gg <- plotly::layout(gg, ...)

  plotly::highlight(gg, on = highlight_on, color = highlight_col, opacityDim = opacityDim, dynamic = dynamic)

}


.compute_plot <- function(x, bar_width = 1L, hpad = 0) {

  # this function calculates a user-adjusted bar width, adds white space between
  # lines and bars (if hpad > 0), and imputes NAs where lines or ribbons should
  # not be plotted.

  x$bar_width <- x$bar_width * bar_width
  horizontal_ws <- (length(x$curve) -  x$bar_width) * hpad
  bar_adjust <-  x$bar_width + horizontal_ws

  x1 <- x$x_pos - (bar_adjust / 2)
  x2 <- x$x_pos + (bar_adjust / 2)

  if (x$type == "trace") {
    x$traces <- .compute_visible_traces(x, x1, x2)
  }

  if (!is.null(x$flows)) {
    x$flows <- .compute_visible_flows(x, x1, x2)
  }

  return(x)

}


.compute_visible_traces <- function(x, x1, x2) {
  traces <- x$traces
  traces$y_prop <- NA_real_
  traces$y_count <- NA_real_
  edge_rows <- list()

  for (i in seq_len(x$n_curves)) {
    visible_start <- x2[i]
    visible_end <- x1[i + 1L]
    in_segment <- traces$x_axis > visible_start & traces$x_axis < visible_end
    segment_span <- visible_end - visible_start

    if (any(in_segment)) {
      traces$curve[in_segment] <- .curve_from_x(traces$x_axis[in_segment], visible_start, segment_span, x$curve)
      traces$y_prop[in_segment] <- x$model(traces[in_segment, ], "pos_start")
      traces$y_count[in_segment] <- traces$y_prop[in_segment] * x$N
    }

    segment_rows <- traces[ceiling(traces$x_axis / length(x$curve)) == i, ]
    segment_rows <- segment_rows[!duplicated(segment_rows[[x$ID]]), ]
    if (nrow(segment_rows) > 0L) {
      start_rows <- segment_rows
      start_rows$x_axis <- visible_start
      start_rows$curve <- min(x$curve)
      start_rows$curve_index <- 0L
      start_rows$y_prop <- start_rows$pos_start
      start_rows$y_count <- start_rows$y_prop * x$N

      end_rows <- segment_rows
      end_rows$x_axis <- visible_end
      end_rows$curve <- max(x$curve)
      end_rows$curve_index <- length(x$curve) + 1L
      end_rows$y_prop <- end_rows$pos_end
      end_rows$y_count <- end_rows$y_prop * x$N

      edge_rows[[length(edge_rows) + 1L]] <- rbind(start_rows, end_rows)
    }
  }

  if (length(edge_rows) > 0L) {
    traces <- rbind(traces, do.call(rbind, edge_rows))
  }

  traces[order(traces[[x$ID]], traces$x_axis), ]
}


.compute_visible_flows <- function(x, x1, x2) {
  flows <- x$flows
  flow_y_cols <- intersect(
    c("ymin", "ymax", "ymin_prop", "ymax_prop", "ymin_count", "ymax_count"),
    names(flows)
  )
  flows[flow_y_cols] <- purrr::map(flows[flow_y_cols], ~ rep(NA_real_, length(.x)))
  edge_rows <- list()

  for (i in seq_len(x$n_curves)) {
    visible_start <- x2[i]
    visible_end <- x1[i + 1L]
    in_segment <- flows$x_axis > visible_start & flows$x_axis < visible_end
    segment_span <- visible_end - visible_start

    if (any(in_segment)) {
      flows$curve[in_segment] <- .curve_from_x(flows$x_axis[in_segment], visible_start, segment_span, x$curve)
      flows$ymin_prop[in_segment] <- x$model(flows[in_segment, ], "pos_start")
      flows$ymax_prop[in_segment] <- x$model(flows[in_segment, ], "pos_end")
      flows$ymin_count[in_segment] <- flows$ymin_prop[in_segment] * x$N
      flows$ymax_count[in_segment] <- flows$ymax_prop[in_segment] * x$N
      flows$ymin[in_segment] <- flows$ymin_prop[in_segment]
      flows$ymax[in_segment] <- flows$ymax_prop[in_segment]
    }

    segment_rows <- flows[ceiling(flows$x_axis / length(x$curve)) == i, ]
    segment_rows <- segment_rows[!duplicated(segment_rows[c("y_from", "y_to", "x_from", ".id")]), ]
    if (nrow(segment_rows) > 0L) {
      start_rows <- segment_rows
      start_rows$x_axis <- visible_start
      start_rows$curve <- min(x$curve)
      start_rows$curve_index <- 0L
      start_rows$ymin_prop <- start_rows$pos_start
      start_rows$ymax_prop <- start_rows$pos_end
      start_rows$ymin_count <- start_rows$ymin_prop * x$N
      start_rows$ymax_count <- start_rows$ymax_prop * x$N
      start_rows$ymin <- start_rows$ymin_prop
      start_rows$ymax <- start_rows$ymax_prop

      end_rows <- segment_rows
      end_rows$x_axis <- visible_end
      end_rows$curve <- max(x$curve)
      end_rows$curve_index <- length(x$curve) + 1L
      end_rows$ymin_prop <- end_rows$pos_start + end_rows$pos_diff
      end_rows$ymax_prop <- end_rows$pos_end + end_rows$pos_diff
      end_rows$ymin_count <- end_rows$ymin_prop * x$N
      end_rows$ymax_count <- end_rows$ymax_prop * x$N
      end_rows$ymin <- end_rows$ymin_prop
      end_rows$ymax <- end_rows$ymax_prop

      edge_rows[[length(edge_rows) + 1L]] <- rbind(start_rows, end_rows)
    }
  }

  if (length(edge_rows) > 0L) {
    flows <- rbind(flows, do.call(rbind, edge_rows))
  }

  flows[order(flows$x_from, flows$y_from, flows$y_to, flows$x_axis), ]
}


.curve_from_x <- function(x_axis, visible_start, segment_span, curve) {
  curve_min <- min(curve)
  curve_max <- max(curve)
  curve_min + ((x_axis - visible_start) / segment_span) * (curve_max - curve_min)
}


.alluvial_plot_base <- function(x, col, xlabs, labels, ggtitle, y_scale, bar_clrs, flow_clrs, bar_alpha, border_col,
                                trace_alpha, flow_alpha, show.legend, auto_theme, remove_y_axis, trace_lwd, ...) {

  flow_col <- .flow_col(x, col)
  x$bars$.fill_key <- .fill_key("bar", x$bars$y_value)
  if (!is.null(x$flows)) {
    x$flows$.fill_key <- .fill_key("flow", x$flows[[flow_col]])
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_bar(
      data = x$bars,
      stat = "identity",
      mapping = ggplot2::aes(
        x = x_pos,
        y = .data[[paste0("y_", y_scale)]],
        fill = .data[[".fill_key"]]
      ),
      col = border_col,
      width = x$bar_width,
      alpha = bar_alpha
    ) +
    ggplot2::ggtitle(ggtitle)

  if (x$type == "trace") {
    has_tooltip <- inherits(x$traces, "SharedData") || ".alluvial_tooltip" %in% names(x$traces)
    if (has_tooltip) {
      line_mapping <- ggplot2::aes(
        x = x_axis,
        y = .data[[paste0("y_", y_scale)]],
        col = as.factor(.data[[col]]),
        group = .data[[x$ID]],
        text = .data[[".alluvial_tooltip"]]
      )
    } else {
      line_mapping <- ggplot2::aes(
        x = x_axis,
        y = .data[[paste0("y_", y_scale)]],
        col = as.factor(.data[[col]]),
        group = .data[[x$ID]]
      )
    }

    line_layer <- suppressWarnings(ggplot2::geom_line(
        data = x$traces,
        mapping = line_mapping,
        alpha = trace_alpha,
        na.rm = TRUE,
        lwd = trace_lwd
      ))
    p <- p + line_layer
  }

  if (!is.null(x$flows)) {
    p <- p +
      ggplot2::geom_ribbon(
        data = x$flows,
        mapping = ggplot2::aes(
          x_axis,
          ymin = .data[[paste0("ymin_", y_scale)]],
          ymax = .data[[paste0("ymax_", y_scale)]],
          group = interaction(x_from, y_from, y_to),
          fill = .data[[".fill_key"]]
        ),
        alpha = flow_alpha
      )
  }

  if (!is.null(labels)) {

    geom_fun <- switch(
      labels$type,
      text = ggplot2::geom_text,
      label = ggplot2::geom_label
    )

    if (identical(labels$where, "all")) {
      labs <- dplyr::filter(x$bars , step %in% x$steps, y_value %in% unique(x$bars$y_value))
    } else {
      if (is.null(labels$where$steps)) {
        labels$where$steps <- x$steps
      }

      if (is.null(labels$where$values)) {
        labels$where$values <- unique(x$bars$y_value)
      }

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

  fill_values <- .manual_fill_values(x, bar_clrs, flow_clrs)
  if (is.null(fill_values)) {
    p <- p + ggplot2::scale_fill_discrete(labels = .fill_label)
  } else {
    p <- p + ggplot2::scale_fill_manual(values = fill_values, labels = .fill_label)
  }

  if (!is.null(xlabs)) {
    p <- p + ggplot2::scale_x_continuous(breaks = x$x_pos, labels = xlabs)
  } else {
    p <- p + ggplot2::scale_x_continuous(breaks = x$x_pos, labels = x$steps)
  }

  if (!is.null(flow_clrs) && !is.null(x$traces) && col %in% names(x$traces)) {
    p <- p + ggplot2::scale_color_manual(values = .manual_color_values(x, col, flow_clrs))
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


.validate_plot_args <- function(x, col, xlabs, labels, y_scale, bar_clrs, flow_clrs) {
  if (!y_scale %in% c("prop", "count")) {
    stop("`y_scale` must be either \"prop\" or \"count\".", call. = FALSE)
  }
  if (!is.character(col) || length(col) != 1L || is.na(col)) {
    stop("`col` must be a single column name.", call. = FALSE)
  }
  col_sources <- c(
    if (!is.null(x$traces)) names(x$traces),
    if (!is.null(x$flows)) names(x$flows),
    names(x$bars)
  )
  if (!col %in% col_sources) {
    stop("`col` must name a column in the prepared alluvial model.", call. = FALSE)
  }
  if (!is.null(xlabs) && length(xlabs) != length(x$steps)) {
    stop("`xlabs` must have the same length as `x$steps`.", call. = FALSE)
  }
  if (!is.null(labels)) {
    if (is.null(labels$type) || !labels$type %in% c("text", "label")) {
      stop("`labels$type` must be either \"text\" or \"label\".", call. = FALSE)
    }
    if (is.null(labels$what) || !labels$what %in% c("prop", "count", "perc")) {
      stop("`labels$what` must be one of \"prop\", \"count\", or \"perc\".", call. = FALSE)
    }
    if (is.null(labels$where)) {
      labels$where <- "all"
    }
    if (!identical(labels$where, "all") && !is.list(labels$where)) {
      stop("`labels$where` must be \"all\" or a list with optional `steps` and `values`.", call. = FALSE)
    }
  }
  .validate_palette(bar_clrs, "bar_clrs")
  .validate_palette(flow_clrs, "flow_clrs")
}


.validate_palette <- function(x, arg) {
  if (!is.null(x) && (!is.character(x) || anyNA(x))) {
    stop("`", arg, "` must be a character vector of colors.", call. = FALSE)
  }
}


.validate_plotly_tooltip <- function(x, tooltip) {
  if (is.null(tooltip)) {
    return(invisible(NULL))
  }
  if (!is.character(tooltip) || anyNA(tooltip)) {
    stop("`tooltip` must be a character vector of trace column names.", call. = FALSE)
  }
  missing_tooltips <- setdiff(tooltip, names(x$traces))
  if (length(missing_tooltips) > 0) {
    stop("`tooltip` columns were not found in trace data: ", paste(missing_tooltips, collapse = ", "), call. = FALSE)
  }
}


.make_plotly_tooltip <- function(data, tooltip) {
  if (is.null(tooltip) || length(tooltip) == 0L) {
    return(rep("", nrow(data)))
  }
  tooltip_data <- data[tooltip]
  purrr::pmap_chr(tooltip_data, function(...) {
    values <- list(...)
    paste(paste(tooltip, unlist(values), sep = ": "), collapse = "<br>")
  })
}


.flow_col <- function(x, col) {
  if (!is.null(x$flows) && col %in% names(x$flows)) {
    return(col)
  }
  "y_from"
}


.fill_key <- function(prefix, value) {
  paste(prefix, as.character(value), sep = "::")
}


.fill_label <- function(x) {
  sub("^[^:]+::", "", x)
}


.manual_fill_values <- function(x, bar_clrs, flow_clrs) {
  bar_keys <- unique(x$bars$.fill_key)
  flow_keys <- if (!is.null(x$flows)) unique(x$flows$.fill_key) else character()
  values <- c(
    .palette_for_keys(bar_clrs, bar_keys, "bar_clrs"),
    .palette_for_keys(flow_clrs, flow_keys, "flow_clrs")
  )
  if (length(values) == 0L) {
    return(NULL)
  }
  values
}


.manual_color_values <- function(x, col, flow_clrs) {
  if (is.null(flow_clrs) || is.null(x$traces) || !col %in% names(x$traces)) {
    return(flow_clrs)
  }
  trace_values <- unique(as.character(x$traces[[col]]))
  .palette_for_values(flow_clrs, trace_values, "flow_clrs")
}


.palette_for_keys <- function(colors, keys, arg) {
  if (is.null(colors) || length(keys) == 0L) {
    return(character())
  }
  values <- .palette_for_values(colors, .fill_label(keys), arg)
  names(values) <- keys
  values
}


.palette_for_values <- function(colors, values, arg) {
  values <- as.character(values)
  if (is.null(names(colors))) {
    if (length(colors) < length(values)) {
      stop("`", arg, "` must provide at least ", length(values), " colors.", call. = FALSE)
    }
    colors <- colors[seq_along(values)]
    names(colors) <- values
    return(colors)
  }

  missing_values <- setdiff(values, names(colors))
  if (length(missing_values) > 0) {
    stop("`", arg, "` is missing colors for: ", paste(missing_values, collapse = ", "), call. = FALSE)
  }
  colors[values]
}
