.compute_bars <- function(data, steps, type, x_pos, y_fctr_order, weights = NULL) {

  if (type == "trace") {
    bars <- .compute_trace_bars(data, steps, y_fctr_order)
  }

  if (type == "flow") {
    bars <- .compute_flow_bars(data, steps, y_fctr_order, weights)
  }

  # creating evenly spaced locations on the x-axis for plotting
  bars$x_pos <- NA
  for (i in seq_along(steps)) {
    bars$x_pos <- ifelse(bars$step == steps[i] & is.na(bars$x_pos), x_pos[i], bars$x_pos)
  }

  bars$x_pos <- as.numeric(bars$x_pos)

  return(bars)
}

.compute_trace_bars <- function(data, steps, y_fctr_order) {
  data %>%
    tidyr::pivot_longer(
      cols = steps,
      names_to = "step",
      values_to = "y_value"
    ) %$%
    table(y_value, step) %>%
    as.data.frame() %>%
    dplyr::mutate(y_value = forcats::fct_relevel(y_value, y_fctr_order)) %>%
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

.compute_flow_bars <- function(data, steps, y_fctr_order, weights) {
  data %>%
    tidyr::pivot_longer(
      cols = steps,
      names_to = "step",
      values_to = "y_value"
    ) %>%
    dplyr::mutate(y_value = forcats::fct_relevel(y_value, y_fctr_order)) %>%
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
