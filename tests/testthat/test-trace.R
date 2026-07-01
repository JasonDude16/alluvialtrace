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
