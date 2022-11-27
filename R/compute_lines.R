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
      seq_start = c(0, dplyr::lag(seq_end)[-1]),
      seq_diff = seq_end - seq_start
    ) %>%
    dplyr::ungroup()

  if (type == "trace") {
    df_pos <- .compute_trace_lines(df_seq, filt_vars, curve, curve_index, id)
  }

  if (type == "flow") {
    df_pos <- .compute_flow_lines(df_seq, filt_vars, curve, curve_index, n_curves)
  }

  return(df_pos)

}

.compute_trace_lines <- function(df_seq, filt_vars, curve, curve_index, id){

  # computing start and end points for each person
  # we already have start and end points at the group level, and we have a list
  # of ids belonging to each to/from combo, so we can create a nested list of
  # positions for each person by creating a sequence from seq_start to seq_end
  # that's evenly spaced by frequency - 1 for each to/from combo.
  # If frequency = 1 we use seq_start
  vars <- list(df_seq$seq_start, df_seq$seq_end, df_seq$seq_diff, df_seq$freq)
  df_pos <- df_seq %>%
    dplyr::mutate(
      pos = purrr::pmap(vars, ~ seq(..1, ..2, by = ..3 / (..4 - 1))),
      pos = purrr::imap(pos, ~ifelse(is.nan(.x), seq_start[.y], .x))
    )

  # note that position start refers to step - 1, and position end refers to the
  # current step (x_from), which is why we use filt_vars to only compute for the
  # steps that need computing. This ultimately yields positions for step 1 -> step 2.1,
  # step 2.2 -> step 3.1, step 3.2 -> 4.1, etc.
  df_pos %>%
    tidyr::unnest(cols = c(id, "pos")) %>%
    dplyr::group_by(.data[[id]]) %>%
    dplyr::mutate(
      pos_start = dplyr::lag(pos),
      pos_end = pos,
      pos_diff = pos - pos_start
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(x_from %in% filt_vars) %>%
    dplyr::mutate(
      curve = list(curve),
      curve_index = list(curve_index)
    ) %>%
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
      x_axis,
      .id
    )

}
