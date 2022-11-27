.alluvial_vars <- function(data, steps, curve, type, weights, id = NULL) {

  # creating a set of variables for passing to other functions
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

.alluvial_prep_base <- function(data, type, id = NULL, steps, weights = NULL, y_fctr_order = NULL, values = NULL,
                           keep_vars = FALSE, curve = alluvial_curve(), res = 1L, model_fun = alluvial_model,
                           force = FALSE, add_flows = FALSE) {

  # default model comes from alluvial_model(), but can be user-supplied
  model_fun <- match.fun(model_fun)

  # ensure `steps` variables are factors
  data[steps] <- purrr::map(data[steps], as.factor)

  # if user doesn't supply y factor order we'll use the default ordering
  if (is.null(y_fctr_order)) {
    y_fctr_order <- unique(purrr::reduce(purrr::map(data[steps], levels), c))
  }

  # adjust resolution before getting alluvial vars and computing traces/flows
  if (res < 1L) {
    down_scale <- ceiling(length(curve) * (1 - res))
    drop <- seq(2, (length(curve) - 1), length(curve) / down_scale)
    curve <- curve[-drop]
  }

  # creating a list of useful vars
  vars <- .alluvial_vars(data, steps, curve, type, weights, id)

  if (vars$data_points > 1e5 && !force) {
    stop("Number of data points to calculate exceeeds 1e+05 (", data_points, "). Use Force = TRUE to continue.")
  }

  # main internal function calls for computing proportions, flows/traces, and bars
  tbl_prop <- .compute_props(data, steps, type, vars$n_steps, id, weights, y_fctr_order)
  tbl_line <- .compute_lines(tbl_prop, type, steps, curve, vars$n_curves, id)
  tbl_bar <- .compute_bars(data, steps, type, vars$x_pos, y_fctr_order, weights)

  # apply alluvial model to traces (default is sigmoid curve)
  if (type == "trace") {
    alluvial_traces <- tbl_line %>%
      dplyr::mutate(
        y_prop = model_fun(tbl_line, "pos_start"),
        y_count = y_prop * vars$N
      )

    # flow overlay for traces (better plotting resolution)
    if (add_flows) {
      data_agg <- data %>%
        dplyr::group_by(across(steps)) %>%
        dplyr::summarise(n = dplyr::n()) %>%
        dplyr::ungroup()

      tbl_agg_props <- .compute_props(data_agg, steps, "flow", vars$n_steps, id = NULL, "n", y_fctr_order)
      tbl_agg_lines <- .compute_lines(tbl_agg_props, "flow", steps, curve, vars$n_curves, id = NULL)

      alluvial_flows <- tbl_agg_lines %>%
        dplyr::mutate(
          ymin = model_fun(tbl_agg_lines, "pos_start"),
          ymax = model_fun(tbl_agg_lines, "pos_end")
        )
    }

    if (keep_vars) {
      message("`alluvial_prep()` explodes a dataset. It is recommended to only include variables needed for plotting.")
      alluvial_traces <- dplyr::left_join(alluvial_traces, data, by = id)
    }

  }

  # apply alluvial model to flows (default is sigmoid curve)
  if (type == "flow") {
    alluvial_flows <- tbl_line %>%
      dplyr::mutate(
        ymin = model_fun(tbl_line, "pos_start"),
        ymax = model_fun(tbl_line, "pos_end"),
      )
    # keep vars at flow level
    if (keep_vars) {
      if (!any(colnames(data) %in% ".id")) {
        data <- data %>% mutate(.id = 1:n())
        alluvial_flows <- dplyr::left_join(alluvial_flows, data, by = ".id")
      }
    }
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
      "bars" = tbl_bar
    )
  )

  if (type == "trace") {
    mod <- c(mod, list( "ID" = id, "traces" = alluvial_traces))

    if (add_flows) {
      mod <- c(mod, list("flows" = alluvial_flows))
    }
  }

  if (type == "flow") {
    mod <- c(mod, list("flows" = alluvial_flows))
  }

  attr(mod, "class") <- "alluvial_model"

  return(mod)
}
