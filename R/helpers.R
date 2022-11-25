.compute_props <- function(data, steps, type, n_steps, id = NULL, weights = NULL, fctr_order) {

  # steps should always be a factor
  data[steps] <- purrr::map(data[steps], as.factor)

  # to implement the alluvial sorting algorithm we need to optimally sort levels
  # for both cases: step 1 -> step 2.1 and step 2.2 -> step 3.1, where 2.1 is optimally
  # sorted for 1, and 2.2 is optimally sorted for 3.1. We do this by duplicating the factor
  # levels, sorting by steps for both cases and computing positions for every id
  if (n_steps > 2) {
    n_calcs <- (n_steps - 2) * 2 + 2

    steps_from <- rep(steps[-length(steps)], each = 2)
    steps_to <- rep(lead(steps)[-length(steps)], each = 2)

    tmp <- paste0(rep(steps[-c(1, length(steps))], each = 2), c(".1", ".2"))
    keep <- seq(1, length(tmp), by = 2)
    new_nms <- c(steps[1], tmp, steps[length(steps)])

    order1 <- rep(c("y_from", "y_to"), n_calcs / 2)
    order2 <- rep(c("y_to", "y_from"), n_calcs / 2)

    # creating a grouping variable (used for flow calcs)
    group <- rep(1:(n_calcs / 2), each = 2)

  } else if (n_steps == 2) {
    steps_from <- rep(steps[1], 2)
    steps_to <- rep(steps[2], 2)
    order1 <- c("y_from", "y_to")
    order2 <- c("y_to", "y_from")
    new_nms <- steps

    # creating a grouping variable (used for flow calcs)
    group <- 1

  }

  ll <- list(steps_from, steps_to, order1, order2, new_nms, group)

  if (!is.null(weights) && type == "flow") {
    return(.compute_flow_props(data, weights, ll, fctr_order))
  }

  if (!is.null(id) && type == "trace") {
    return(.compute_trace_props(data, id, ll, fctr_order))
  }

}

.compute_flow_props <- function(data, weights, ll, fctr_order) {

  df <- purrr::pmap_dfr(ll, function(steps_from, steps_to, order1, order2, new_nms, group, .df = data, .wt = weights) {
    .df %>%
      dplyr::ungroup() %>%
      dplyr::rename(freq = .data[[.wt]]) %>%
      dplyr::mutate(N = sum(freq)) %>%
      dplyr::group_by(.data[[steps_from]], .data[[steps_to]]) %>%
      dplyr::summarise(prop = sum(freq) / N[1]) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        value = paste(.data[[steps_from]], .data[[steps_to]], sep = "_"),
        x_from = new_nms,
        group = group
      ) %>%
      tidyr::separate(col = "value", into = c("y_from", "y_to"), sep = "_", remove = F) %>%
      dplyr::mutate(
        y_from = as.factor(y_from),
        y_to = as.factor(y_to)
      ) %>%
      dplyr::arrange(desc(.data[[order1]]), desc(.data[[order2]])) %>%
      dplyr::select(-.data[[steps_from]], -.data[[steps_to]], -value)
  })

  # if (!is.null(fctr_order)) {
  #   df <- df %>%
  #     mutate(
  #       y_from = forcats::fct_relevel(y_from, fctr_order),
  #       y_to = forcats::fct_relevel(y_to, fctr_order)
  #     ) %>%
  #     arrange(y_from, y_to)
  # }

  df <- df %>%
    dplyr::group_by(x_from) %>%
    dplyr::mutate(prop_cumsum = cumsum(prop)) %>%
    dplyr::ungroup()

  return(df)

}

.compute_trace_props <- function(data, id, ll, fctr_order) {

  df <- purrr::pmap_dfr(ll, function(steps_from, steps_to, order1, order2, new_nms, group, .df = data, .id = id) {
    # for each step from/to combo calculate the frequency and proportion and create a nested tibble
    # with all ids listed in their respective step from/to category.
    .df %>%
      dplyr::arrange(.data[[.id]]) %>%
      dplyr::mutate(N = dplyr::n()) %>%
      dplyr::group_by(.data[[steps_from]], .data[[steps_to]]) %>%
      dplyr::summarise(
        freq = dplyr::n(),
        prop = dplyr::n() / N[1],
        {{ .id }} := list(.data[[.id]])
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        value = paste(.data[[steps_from]], .data[[steps_to]], sep = "_"),
        x_from = new_nms,
        group = group
      ) %>%
      tidyr::separate(col = "value", into = c("y_from", "y_to"), sep = "_", remove = F) %>%
      dplyr::mutate(
        y_from = as.factor(y_from),
        y_to = as.factor(y_to)
      ) %>%
      dplyr::arrange(desc(.data[[order1]]), desc(.data[[order2]])) %>%
      dplyr::select(-.data[[steps_from]], -.data[[steps_to]], -value)
  })

  # allowing for factor reordering on the y-axis. Doesn't currently work,
  # and will be difficult to implement because in some causes there will be
  # multiple factor levels
  # if (!is.null(fctr_order)) {
  #   df <- df %>%
  #     mutate(
  #       y_from = forcats::fct_relevel(y_from, fctr_order),
  #       y_to = forcats::fct_relevel(y_to, fctr_order)
  #     ) %>%
  #     arrange(y_from, y_to)
  # }

  # add cumulative sum
  df <- df %>%
    dplyr::group_by(x_from) %>%
    dplyr::mutate(prop_cumsum = cumsum(prop)) %>%
    dplyr::ungroup()

  return(df)

}

.compute_lines <- function(tbl_props, type, steps, curve, n_curves = NULL, id = NULL) {

  curve_index <- 1:length(curve)

  if (length(steps) > 2) {
    tmp <- paste0(rep(steps[-c(1, length(steps))], each = 2), c(".1", ".2"))
    keep <- seq(1, length(tmp), by = 2)
    filt_vars <- c(tmp[keep], steps[length(steps)])

  } else {
    filt_vars <- steps[2]

  }

  # computing start and end points at the group level
  # renaming prop_cumsum for clarity
  df_seq <- tbl_props %>%
    dplyr::group_by(x_from) %>%
    dplyr::rename(seq_end = prop_cumsum) %>%
    dplyr::mutate(
      seq_start = c(0, lag(seq_end)[-1]),
      seq_diff = seq_end - seq_start
    ) %>%
    dplyr::ungroup()

  if (type == "trace") {
    df_pos <- .compute_trace_lines(df_seq, filt_vars, curve, curve_index, id)
  }

  if (type == "flow") {
    df_pos <- .compute_flow_lines(df_seq, filt_vars, curve, curve_index, n_curves)
  }

  # getting back original names
  for (name in seq_along(unique(df_pos$x_from))) {
    var <- unique(df_pos$x_from)[name]
    df_pos$x_from[df_pos$x_from == var] <- steps[name]
  }

  # the last step converted the column back to a character
  # but it should be a factor
  df_pos <-df_pos %>% dplyr::mutate(x_from = as.factor(x_from))

  return(df_pos)

}

.compute_trace_lines <- function(df_seq, filt_vars, curve, curve_index, id){

  # computing start and end points for each person
  # we already have start and end points at the group level, and we have a list
  # of ids belonging to each to/from combo, so we can create a nested list of
  # positions for each person by creating a sequence from seq_start to seq_end
  # that's evenly spaced by frequency - 1 for each to/from combo
  vars <- list(df_seq$seq_start, df_seq$seq_end, df_seq$seq_diff, df_seq$freq)
  df_pos <-  dplyr::mutate(df_seq, pos = purrr::pmap(vars, ~ seq(..1, ..2, by = ..3 / (..4 - 1))))
  df_pos <-  dplyr::mutate(df_pos, pos = ifelse(is.na(pos), seq_end, pos))

  # note that position start refers to step - 1, and position end refers to the
  # current step (x_from), which is why we use filt_vars to only compute for the
  # steps that need computing. This ultimately yields positions for step 1 -> step 2.1,
  # step 2.2 -> step 3.1, step 3.2 -> 4.1, etc.
  df_pos %>%
    tidyr::unnest(cols = c(id, "pos")) %>%
    dplyr::group_by(.data[[id]]) %>%
    dplyr::mutate(
      pos_start = lag(pos),
      pos_end = pos,
      pos_diff = pos - pos_start
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(x_from %in% filt_vars) %>%
    dplyr::mutate(curve = list(curve),
           curve_index = list(curve_index)) %>%
    tidyr::unnest(cols = c("curve", "curve_index")) %>%
    dplyr::group_by(.data[[id]]) %>%
    dplyr::mutate(x_axis = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      y_from,
      y_to,
      x_from,
      seq_start,
      seq_end,
      pos_start,
      pos_end,
      pos_diff,
      curve,
      curve_index,
      x_axis,
      id
    )

}

.compute_flow_lines <- function(df_seq, filt_vars, curve, curve_index, n_curves) {

  # getting position difference
  # doesn't matter if seq_start or seq_end is used here
  df_seq <- df_seq %>%
    dplyr::group_by(y_from, y_to, group) %>%
    dplyr::mutate(
      pos_start = dplyr::lag(seq_start),
      pos_end = dplyr::lag(seq_end),
      pos_diff = seq_start - dplyr::lag(seq_start)
    ) %>%
    dplyr::ungroup()

  df_seq %>%
    dplyr::filter(x_from %in% filt_vars) %>%
    dplyr::mutate(
      curve = list(curve),
      curve_index = list(curve_index)
    ) %>%
    tidyr::unnest(cols = c("curve", "curve_index")) %>%
    dplyr::group_by(y_from, y_to, x_from) %>%
    dplyr::mutate(
      x_axis = 1:dplyr::n(),
      x_from = as.factor(x_from),
      x_axis = x_axis + ((as.numeric(x_from) * length(curve)) - length(curve)),
      x_from = as.character(x_from)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      y_from,
      y_to,
      x_from,
      seq_start,
      seq_end,
      pos_start,
      pos_end,
      pos_diff,
      curve,
      curve_index,
      x_axis
    )

}

.alluvial_vars <- function(data, steps, curve, type, weights, id = NULL) {

  # creating a set of variables for passing to other functions.
  # basically trying to do OOP in R
  if (type == "trace") {
    N <- nrow(unique(data[id]))
  }

  if (type == "flow") {
    N <- sum(data[weights])
  }

  n_steps <- length(steps)
  n_curves <- n_steps - 1
  x_axis <- 1:(n_curves * length(curve))
  x_pos <- c(0, (1:n_curves / n_curves) * length(x_axis))
  data_points <- length(curve) * nrow(data) * n_curves

  list(
    "N" = N,
    "data_points" = data_points,
    "steps" = steps,
    "n_steps" = n_steps,
    "n_curves" =  n_curves,
    "x_axis" = x_axis,
    "x_pos" = x_pos,
    "curve" = curve
  )
}

.compute_bars <- function(data, steps, type, x_pos, weights = NULL) {

  if (type == "trace") {
    bars <- .compute_trace_bars(data, steps)
  }

  if (type == "flow") {
    bars <- .compute_flow_bars(data, steps, weights)
  }

  # creating evenly spaced locations on the x-axis for plotting
  bars$x_pos <- NA
  for (i in seq_along(steps)) {
    bars$x_pos <- ifelse(bars$step == steps[i] & is.na(bars$x_pos), x_pos[i], bars$x_pos)
  }

  bars$x_pos <- as.numeric(bars$x_pos)

  return(bars)
}

.compute_trace_bars <- function(data, steps) {
  data %>%
    tidyr::pivot_longer(
      cols = steps,
      names_to = "step",
      values_to = "y_value"
    ) %$%
    table(y_value, step) %>%
    as.data.frame() %>%
    dplyr::rename(y_count = Freq) %>%
    dplyr::arrange(step, desc(y_value)) %>%
    dplyr::group_by(step) %>%
    dplyr:: mutate(
      y_prop = y_count / sum(y_count),
      y_perc = paste(round(y_prop, 3) * 100, "%")
    ) %>%
    dplyr::filter(y_prop > 0) %>%
    dplyr::mutate(
      prop_cumsum = cumsum(y_prop),
      freq_cumsum = cumsum(y_count),
      prop_lab_pos = prop_cumsum - (y_prop * 0.5),
      count_lab_pos = freq_cumsum - (y_count * 0.5)
    ) %>%
    dplyr::ungroup()
}

.compute_flow_bars <- function(data, steps, weights) {
  data %>%
    tidyr::pivot_longer(
      cols = steps,
      names_to = "step",
      values_to = "y_value"
    ) %>%
    dplyr::select(y_value, step, tidyr::everything()) %>%
    dplyr::rename(y_count = .data[[weights]]) %>%
    dplyr::group_by(y_value, step) %>%
    dplyr::summarise(y_count = sum(y_count)) %>%
    dplyr::arrange(step, desc(y_value)) %>%
    dplyr::group_by(step) %>%
    dplyr::mutate(
      y_prop = y_count / sum(y_count),
      y_perc = paste(round(y_prop, 3) * 100, "%")
    ) %>%
    dplyr::filter(y_prop > 0) %>%
    dplyr::mutate(
      prop_cumsum = cumsum(y_prop),
      freq_cumsum = cumsum(y_count),
      prop_lab_pos = prop_cumsum - (y_prop * 0.5),
      count_lab_pos = freq_cumsum - (y_count * 0.5)
    ) %>%
    dplyr::ungroup()
}

.alluvial_add_vars <- function(data, id, steps, alluvial_mod) {
  join_vars <- setdiff(colnames(data), colnames(data[steps]))
  data <-  dplyr::left_join(alluvial_mod, data[join_vars], by = id)
  return(data)
}

.alluvial_prep <- function(data, type, id = NULL, steps, weights = NULL, fctr_order = NULL, is.long = FALSE, values = NULL,
                          keep_vars = FALSE, curve = alluvial_curve(), res = 1L, model_fun = alluvial_model,
                          force = FALSE, compute_flows = FALSE) {

  # default model comes from alluvial_model(), but can be user-supplied
  model_fun <- match.fun(model_fun)

  if (is.long) {
    if (keep_vars) {
      steps_long <- steps
      dropped_vars <- setdiff(colnames(data), colnames(data[values]))
      long_dropped <- data[dropped_vars]
    }

    tmp <- tidyr::pivot_wider(
      data,
      id_cols = id,
      names_from = steps,
      values_from = values
      )
    steps <- unique(data[[steps]])
    data <- tmp

  }

  if (res < 1L) {
    down_scale <- ceiling(length(curve) * (1 - res))
    drop <- seq(2, (length(curve) - 1), length(curve) / down_scale)
    curve <- curve[-drop]
  }

  # getting vars *after* long format has been converted to wide and curve res has been adjusted
  vars <- .alluvial_vars(data, steps, curve, type, weights, id)

  if (vars$data_points > 1e5 && !force) {
    stop("Number of data points to calculate exceeeds 1e+05 (", data_points, "). Use Force = TRUE to continue.")
  }

  # TODO: keep vars at flow level
  # if (type == "flow") {
  #   if (keep_vars) {
  #     if (!any(colnames(data) %in% "id")) {
  #       data <- data %>% mutate(id = 1:n())
  #     }
  #   }
  # }

  tbl_props <- .compute_props(data, steps, type, vars$n_steps, id, weights, fctr_order)
  tbl_lines <- .compute_lines(tbl_props, type, steps, curve, vars$n_curves, id)
  bars <- .compute_bars(data, steps, type, vars$x_pos, weights)

  if (type == "trace") {
    alluvial_traces <- tbl_lines %>%
      dplyr::mutate(
        y_prop = model_fun(tbl_lines, "pos_start"),
        y_count = y_prop * vars$N
      )

    # TODO: add flows to trace
    if (compute_flows) {
      data_agg <- data %>%
        dplyr::group_by(across(steps)) %>%
        dplyr::summarise(n = dplyr::n()) %>%
        dplyr::ungroup()

      tbl_agg_props <- .compute_props(data_agg, steps, type = "flow", vars$n_steps, weights = "n")
      tbl_agg_lines <- .compute_lines(tbl_agg_props, type = "flow", steps, curve, vars$n_curves)

      alluvial_flows <- tbl_agg_lines %>%
        dplyr::mutate(
          ymin = model_fun(tbl_agg_lines, "pos_start"),
          ymax = model_fun(tbl_agg_lines, "pos_end")
        )

    }

    if (keep_vars) {
      warning("`alluvial_prep()` explodes a dataset. It is recommended to only include variables needed for plotting.")

      if (is.long) {
        alluvial_traces <- .alluvial_add_vars(long_dropped, id, steps_long, alluvial_traces)

      } else {
        alluvial_traces <- .alluvial_add_vars(data, id, steps, alluvial_traces)

      }

    }

  }

  if (type == "flow") {
    alluvial_flows <- tbl_lines %>%
      dplyr::mutate(
        ymin = model_fun(tbl_lines, "pos_start"),
        ymax = model_fun(tbl_lines, "pos_end"),
      )

    # TODO: keep vars at flow level (not working)
    # if (keep_vars) {
    #   alluvial_flows <- .alluvial_add_vars(data, id = "id", steps, alluvial_flows)
    # }
  }

  # when converting to plotly you can see the lines behind the bars, so
  # choosing bar widths that minimize how much the the lines extend behind bars.
  # the amount of line shown will depend on the bar width, number of data points
  # of the curve, horizontal padding (hpad), and number of steps.

  if (vars$n_steps > 3) {
    bar_width <- (length(vars$x_axis) / 14)
  } else {
    bar_width <- (length(vars$x_axis) / 12)
  }

  # adding vars from .alluvial_vars()
  mod <- c(
    vars,
    list(
      "model" = model_fun,
      "type" = type,
      "bar_width" = bar_width,
      "bars" = bars
    )
  )

  if (type == "trace") {
    mod <- c(
      mod,
      list(
        "ID" = id,
        "traces" = alluvial_traces,
        "trace_props" = tbl_props
        )
    )

    if (compute_flows) {
      mod <- c(mod, list("flows" = alluvial_flows))
    }
  }

  if (type == "flow") {
    mod <- c(
      mod,
      list(
        "flows" = alluvial_flows,
        "flow_props" = tbl_props
        )
    )
  }

  attr(mod, "class") <- "alluvial_model"

  return(mod)
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

  if (x$type == "flow") {
    for (i in seq_along(x1)) {
      x$flows$ymin <- ifelse(x$flows$x_axis > x1[i] & x$flows$x_axis < x2[i], NA, x$flows$ymin)
    }
  }

  return(x)

}

.alluvial_base <- function(x, col = "y_from", xlabs = NULL, labels = NULL, ggtitle = NULL, y_scale = "prop",
                           bar_clrs = NULL, flow_clrs = NULL, bar_alpha = 1L, flow_alpha = 0.3, show.legend = TRUE,
                           auto_theme = FALSE, remove_y_axis = FALSE, ...) {


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
        ggplot2::aes(
          x = x_axis,
          y = .data[[paste0("y_", y_scale)]],
          col = as.factor(.data[[col]]),
          group = .data[[x$ID]]
        ),
        alpha = flow_alpha,
        na.rm = TRUE
      )
  }

  if (x$type == "flow") {
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
        alpha = .7
      )
  }

  if (!is.null(labels)) {

    import::from(.from = "ggplot2", "geom_text")
    import::from(.from = "ggplot2", "geom_label")

    geom_fun <- match.fun(paste0("geom_", labels$type))

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
