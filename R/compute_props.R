.compute_props <- function(data, steps, type, n_steps, id = NULL, weights = NULL, y_fctr_order) {

  # to implement the alluvial sorting algorithm we need to optimally sort levels
  # for both cases: step 1 -> step 2.1 and step 2.2 -> step 3.1, where 2.1 is optimally
  # sorted for 1, and 2.2 is optimally sorted for 3.1. We do this by duplicating the factor
  # levels, sorting by steps for both cases and computing positions for every id
  if (n_steps > 2) {
    n_calcs <- (n_steps - 2) * 2 + 2

    steps_from <- rep(steps[-length(steps)], each = 2)
    steps_to <- rep(dplyr::lead(steps)[-length(steps)], each = 2)

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
    return(.compute_flow_props(data, weights, ll, y_fctr_order))
  }

  if (!is.null(id) && type == "trace") {
    return(.compute_trace_props(data, id, ll, y_fctr_order))
  }

}

.compute_flow_props <- function(data, weights, ll, y_fctr_order) {

  # for each step from/to combo calculate the frequency and proportion
  df <- purrr::pmap_dfr(ll, function(steps_from, steps_to, order1, order2, new_nms, group) {

    # each step can have different different levels, so we first identify the levels at a given
    # step and subset to only those levels, then fct_relevel()
    y_from_fctrs <- y_fctr_order[y_fctr_order %in% unique(levels(data[[steps_from]]))]
    y_to_fctrs <- y_fctr_order[y_fctr_order %in% unique(levels(data[[steps_to]]))]

    data %>%
      dplyr::ungroup() %>%
      dplyr::rename(freq = .data[[weights]]) %>%
      dplyr::mutate(
        N = sum(freq),
        .id = 1:nrow(.)
      ) %>%
      dplyr::group_by(.data[[steps_from]], .data[[steps_to]]) %>%
      dplyr::summarise(
        prop = sum(freq) / N[1],
        .id = .id[1]
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        value = paste(.data[[steps_from]], .data[[steps_to]], sep = "_"),
        x_from = new_nms,
        group = group
      ) %>%
      tidyr::separate(col = "value", into = c("y_from", "y_to"), sep = "_", remove = F) %>%
      dplyr::mutate(
        y_from = forcats::fct_relevel(y_from, y_from_fctrs),
        y_to = forcats::fct_relevel(y_to, y_to_fctrs)
      ) %>%
      dplyr::arrange(dplyr::desc(.data[[order1]]), dplyr::desc(.data[[order2]])) %>%
      dplyr::select(-.data[[steps_from]], -.data[[steps_to]], -value)
  })

  df <- df %>%
    dplyr::group_by(x_from) %>%
    dplyr::mutate(prop_cumsum = cumsum(prop)) %>%
    dplyr::ungroup()

  return(df)

}

.compute_trace_props <- function(data, id, ll, y_fctr_order) {

  # for each step from/to combo calculate the frequency and proportion and create a nested tibble
  # with all ids listed in their respective step from/to category.
  df <- purrr::pmap_dfr(ll, function(steps_from, steps_to, order1, order2, new_nms, group) {

    # each step can have different different levels, so we first identify the levels at a given
    # step and subset to only those levels, then fct_relevel()
    y_from_fctrs <- y_fctr_order[y_fctr_order %in% unique(levels(data[[steps_from]]))]
    y_to_fctrs <- y_fctr_order[y_fctr_order %in% unique(levels(data[[steps_to]]))]

    data %>%
      dplyr::ungroup() %>%
      dplyr::arrange(.data[[id]]) %>%
      dplyr::mutate(N = dplyr::n()) %>%
      dplyr::group_by(.data[[steps_from]], .data[[steps_to]]) %>%
      dplyr::summarise(
        freq = dplyr::n(),
        prop = dplyr::n() / N[1],
        {{ id }} := list(.data[[id]])
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        value = paste(.data[[steps_from]], .data[[steps_to]], sep = "_"),
        x_from = new_nms,
        group = group
      ) %>%
      tidyr::separate(col = "value", into = c("y_from", "y_to"), sep = "_", remove = F) %>%
      dplyr::mutate(
        y_from = forcats::fct_relevel(y_from, y_from_fctrs),
        y_to = forcats::fct_relevel(y_to, y_to_fctrs)
      ) %>%
      dplyr::arrange(desc(.data[[order1]]), desc(.data[[order2]])) %>%
      dplyr::select(-.data[[steps_from]], -.data[[steps_to]], -value)
  })

  # add cumulative sum
  df <- df %>%
    dplyr::group_by(x_from) %>%
    dplyr::mutate(prop_cumsum = cumsum(prop)) %>%
    dplyr::ungroup()

  return(df)

}
