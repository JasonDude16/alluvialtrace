test_that("trace models prepare and plot with flow overlays", {
  data("alluvialtrace_patient", package = "alluvialtrace")
  steps <- c("baseline", "month_1", "month_3")

  expect_message(
    model <- alluvial_prep_trace(
      alluvialtrace_patient,
      id = "patient_id",
      steps = steps,
      keep_vars = TRUE,
      add_flows = TRUE,
      y_fctr_order = c("Critical", "High", "Moderate", "Low")
    ),
    "explodes"
  )

  expect_s3_class(model, "alluvial_model")
  expect_identical(model$type, "trace")
  expect_true("arm" %in% names(model$traces))
  expect_gt(max(model$flows$ymax_count, na.rm = TRUE), 1)
  expect_lte(max(model$flows$ymax_prop, na.rm = TRUE), 1 + 1e-8)

  prop_plot <- alluvial_plot(model, col = "arm")
  count_plot <- alluvial_plot(model, col = "arm", y_scale = "count")

  expect_s3_class(prop_plot, "ggplot")
  expect_s3_class(count_plot, "ggplot")
  expect_no_error(ggplot2::ggplot_build(prop_plot))
  expect_no_error(ggplot2::ggplot_build(count_plot))
  expect_s3_class(prop_plot$scales$get_scales("x"), "ScaleContinuousPosition")
  expect_equal(prop_plot$scales$get_scales("y")$limits, c(0, 1))
  expect_equal(prop_plot$scales$get_scales("y")$expand, c(0, 0))
  expect_equal(count_plot$scales$get_scales("y")$limits, c(0, model$N))
  expect_equal(count_plot$scales$get_scales("y")$expand, c(0, 0))
})

test_that("labels support all bars and validate label specs", {
  data("alluvialtrace_patient", package = "alluvialtrace")
  model <- alluvial_prep_trace(
    alluvialtrace_patient,
    id = "patient_id",
    steps = c("baseline", "month_1", "month_3")
  )
  labels <- list(type = "text", what = "count", where = "all")

  expect_no_error(ggplot2::ggplot_build(alluvial_plot(model, labels = labels)))
  expect_error(
    alluvial_plot(model, labels = list(type = "bad", what = "count", where = "all")),
    "`labels$type`",
    fixed = TRUE
  )
  expect_error(
    alluvial_plot(model, labels = list(type = "text", what = "bad", where = "all")),
    "`labels$what`",
    fixed = TRUE
  )
})

test_that("plotly traces include explicit tooltips", {
  data("alluvialtrace_patient", package = "alluvialtrace")
  model <- alluvial_prep_trace(
    alluvialtrace_patient,
    id = "patient_id",
    steps = c("baseline", "month_1", "month_3"),
    keep_vars = TRUE
  )

  p <- alluvial_plotly(model, col = "arm", tooltip = c("patient_id", "arm", "risk"))
  texts <- unlist(lapply(p$x$data, `[[`, "text"), use.names = FALSE)

  expect_s3_class(p, "plotly")
  expect_true(any(grepl("patient_id:", texts, fixed = TRUE)))
  expect_error(alluvial_plotly(model, tooltip = "missing"), "`tooltip`")
})

test_that("plotly trace colors use the same manual palette as bars", {
  data("alluvialtrace_patient", package = "alluvialtrace")
  model <- alluvial_prep_trace(
    alluvialtrace_patient,
    id = "patient_id",
    steps = c("baseline", "month_1", "month_3"),
    add_flows = TRUE
  )
  clrs <- c("#111111", "#222222", "#333333", "#444444")

  p <- alluvial_plotly(model, bar_clrs = clrs, flow_clrs = clrs, col = "y_from")
  plotly_data <- p$x$data
  bar_layers <- plotly_data[vapply(plotly_data, function(x) identical(x$type, "bar"), logical(1))]
  line_layers <- plotly_data[vapply(plotly_data, function(x) identical(x$type, "scatter"), logical(1))]

  bar_colors <- stats::setNames(
    vapply(bar_layers, function(x) sub(",1\\)$", "", sub("^\\(bar::", "", x$name)), character(1)),
    vapply(bar_layers, function(x) sub(",1\\)$", "", sub("^\\(bar::", "", x$name)), character(1))
  )
  bar_colors[] <- vapply(bar_layers, function(x) sub(",1\\)$", "", sub("^rgba\\((.*),1\\)$", "\\1", x$marker$color)), character(1))

  for (line in line_layers) {
    line_name <- sub(",1\\)$", "", sub("^\\(", "", line$name))
    line_color <- sub(",0\\.3\\)$", "", sub("^rgba\\((.*),0\\.3\\)$", "\\1", line$line$color))
    expect_equal(line_color, bar_colors[[line_name]])
  }
})

test_that("unnamed palettes map shared bar and trace labels to the same colors", {
  data("alluvialtrace_patient", package = "alluvialtrace")
  model <- alluvial_prep_trace(
    alluvialtrace_patient,
    id = "patient_id",
    steps = c("baseline", "month_1", "month_3"),
    add_flows = TRUE
  )
  plotted <- alluvialtrace:::.compute_plot(model)
  flow_col <- alluvialtrace:::.flow_col(plotted, "y_from")
  plotted$bars$.fill_key <- alluvialtrace:::.fill_key("bar", plotted$bars$y_value)
  plotted$flows$.fill_key <- alluvialtrace:::.fill_key("flow", plotted$flows[[flow_col]])
  clrs <- c("#111111", "#222222", "#333333", "#444444")

  fill_values <- alluvialtrace:::.manual_fill_values(plotted, clrs, clrs)
  color_values <- alluvialtrace:::.manual_color_values(plotted, "y_from", clrs, fill_values)
  bar_values <- alluvialtrace:::.fill_label(names(fill_values)[startsWith(names(fill_values), "bar::")])
  trace_values <- names(color_values)
  common_values <- intersect(bar_values, trace_values)

  expect_true(length(common_values) > 0)
  for (value in common_values) {
    expect_equal(fill_values[[paste0("bar::", value)]], color_values[[value]])
  }
})

test_that("trace positions are centered within slots", {
  two_step <- data.frame(
    id = seq_len(4),
    first = c("A", "A", "B", "B"),
    second = c("C", "D", "C", "D")
  )

  model <- alluvial_prep_trace(two_step, "id", c("first", "second"))
  trace_starts <- unique(model$traces[c("id", "pos_start")])

  expect_equal(sort(trace_starts$pos_start), c(0.125, 0.375, 0.625, 0.875))
})

test_that("trace lines are hidden fully behind bars", {
  data("alluvialtrace_patient", package = "alluvialtrace")
  model <- alluvial_prep_trace(
    alluvialtrace_patient,
    id = "patient_id",
    steps = c("baseline", "month_1", "month_3")
  )
  plotted <- alluvialtrace:::.compute_plot(model, bar_width = 1, hpad = 0)
  half_width <- plotted$bar_width / 2

  for (center in plotted$x_pos) {
    inside_bar <- plotted$traces$x_axis > center - half_width & plotted$traces$x_axis < center + half_width
    expect_true(all(is.na(plotted$traces$y_prop[inside_bar])))
    expect_true(all(is.na(plotted$traces$y_count[inside_bar])))
  }
})

test_that("trace lines start and end at bar edges", {
  data("alluvialtrace_patient", package = "alluvialtrace")
  model <- alluvial_prep_trace(
    alluvialtrace_patient,
    id = "patient_id",
    steps = c("baseline", "month_1", "month_3")
  )
  plotted <- alluvialtrace:::.compute_plot(model, bar_width = 1, hpad = 0)
  half_width <- plotted$bar_width / 2

  for (i in seq_len(plotted$n_curves)) {
    edge_start <- plotted$x_pos[i] + half_width
    edge_end <- plotted$x_pos[i + 1L] - half_width
    start_rows <- plotted$traces[plotted$traces$x_axis == edge_start, ]
    end_rows <- plotted$traces[plotted$traces$x_axis == edge_end, ]

    expect_equal(start_rows$y_prop, start_rows$pos_start)
    expect_equal(end_rows$y_prop, end_rows$pos_end)
  }
})

test_that("trace input validation catches bad ids", {
  data("alluvialtrace_patient", package = "alluvialtrace")

  expect_error(
    alluvial_prep_trace(
      alluvialtrace_patient,
      id = "missing",
      steps = c("baseline", "month_1")
    ),
    "`id`"
  )

  expect_error(
    alluvial_prep_trace(
      data.frame(id = c(1, 1), baseline = c("low", "high"), followup = c("high", "low")),
      id = "id",
      steps = c("baseline", "followup")
    ),
    "unique"
  )

  expect_error(
    alluvial_prep_trace(
      data.frame(id = c(1, NA), baseline = c("low", "high"), followup = c("high", "low")),
      id = "id",
      steps = c("baseline", "followup")
    ),
    "missing"
  )

  expect_error(
    alluvial_prep_trace(
      data.frame(id = c(1, 2), baseline = c("low", NA), followup = c("high", "low")),
      id = "id",
      steps = c("baseline", "followup")
    ),
    "`steps`"
  )
})
