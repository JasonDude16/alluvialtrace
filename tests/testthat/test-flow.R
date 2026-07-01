test_that("flow models prepare and plot on prop and count scales", {
  ucb <- as.data.frame(UCBAdmissions)

  model <- alluvial_prep_flow(
    ucb,
    steps = c("Admit", "Gender", "Dept"),
    weights = "Freq"
  )

  expect_s3_class(model, "alluvial_model")
  expect_identical(model$type, "flow")
  expect_named(model$flows, c(
    "y_from", "y_to", "x_from", "seq_start", "seq_end",
    "pos_start", "pos_end", "pos_diff", "curve", "curve_index",
    "x_axis", ".id", "ymin_prop", "ymax_prop", "ymin_count",
    "ymax_count", "ymin", "ymax"
  ))
  expect_gt(max(model$flows$ymax_count, na.rm = TRUE), 1)
  expect_lte(max(model$flows$ymax_prop, na.rm = TRUE), 1 + 1e-8)

  prop_plot <- alluvial_plot(model)
  count_plot <- alluvial_plot(model, y_scale = "count")

  expect_s3_class(prop_plot, "ggplot")
  expect_s3_class(count_plot, "ggplot")
  expect_no_error(ggplot2::ggplot_build(prop_plot))
  expect_no_error(ggplot2::ggplot_build(count_plot))
  expect_s3_class(prop_plot$scales$get_scales("x"), "ScaleContinuousPosition")
})

test_that("flow ribbons are hidden consistently behind bars", {
  ucb <- as.data.frame(UCBAdmissions)
  model <- alluvial_prep_flow(ucb, c("Admit", "Gender", "Dept"), "Freq")

  plotted <- alluvialtrace:::.compute_plot(model, bar_width = 1, hpad = 0)
  flow_y_cols <- c("ymin", "ymax", "ymin_prop", "ymax_prop", "ymin_count", "ymax_count")

  expect_true(all(vapply(plotted$flows[flow_y_cols], function(x) any(is.na(x)), logical(1))))

  half_width <- plotted$bar_width / 2
  for (center in plotted$x_pos) {
    inside_bar <- plotted$flows$x_axis > center - half_width & plotted$flows$x_axis < center + half_width
    expect_true(all(is.na(plotted$flows$ymin_prop[inside_bar])))
    expect_true(all(is.na(plotted$flows$ymax_prop[inside_bar])))
  }
})

test_that("flow ribbons start and end at bar edges", {
  model <- alluvial_prep_flow(as.data.frame(UCBAdmissions), c("Admit", "Gender", "Dept"), "Freq")
  plotted <- alluvialtrace:::.compute_plot(model, bar_width = 1, hpad = 0)
  half_width <- plotted$bar_width / 2

  for (i in seq_len(plotted$n_curves)) {
    edge_start <- plotted$x_pos[i] + half_width
    edge_end <- plotted$x_pos[i + 1L] - half_width
    start_rows <- plotted$flows[plotted$flows$x_axis == edge_start, ]
    end_rows <- plotted$flows[plotted$flows$x_axis == edge_end, ]

    expect_equal(start_rows$ymin_prop, start_rows$pos_start)
    expect_equal(start_rows$ymax_prop, start_rows$pos_end)
    expect_equal(end_rows$ymin_prop, end_rows$pos_start + end_rows$pos_diff)
    expect_equal(end_rows$ymax_prop, end_rows$pos_end + end_rows$pos_diff)
  }
})

test_that("flow plot supports separate bar and ribbon palettes", {
  ucb <- as.data.frame(UCBAdmissions)
  model <- alluvial_prep_flow(ucb, c("Admit", "Gender", "Dept"), "Freq")

  p <- alluvial_plot(
    model,
    bar_clrs = c(Admitted = "#1b9e77", Rejected = "#d95f02", Male = "#7570b3", Female = "#e7298a",
                 A = "#66a61e", B = "#e6ab02", C = "#a6761d", D = "#666666", E = "#1f78b4", F = "#b2df8a"),
    flow_clrs = c(Admitted = "#111111", Rejected = "#999999", Male = "#333333", Female = "#777777")
  )

  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("flow values can contain underscores", {
  underscore_values <- data.frame(
    first = c("a_one", "a_one", "b_two"),
    second = c("c_three", "d_four", "c_three"),
    n = c(2, 3, 4)
  )

  model <- alluvial_prep_flow(underscore_values, c("first", "second"), "n")

  expect_contains(as.character(model$flows$y_from), "a_one")
  expect_contains(as.character(model$flows$y_to), "c_three")
})

test_that("flow input validation catches bad weights and steps", {
  ucb <- as.data.frame(UCBAdmissions)

  expect_error(
    alluvial_prep_flow(ucb, c("Admit", "Gender", "Dept"), weights = "Missing"),
    "`weights`"
  )

  expect_error(
    alluvial_prep_flow(transform(ucb, Freq = as.character(Freq)), c("Admit", "Gender", "Dept"), "Freq"),
    "numeric"
  )

  expect_error(
    alluvial_prep_flow(transform(ucb, Freq = c(-1, Freq[-1])), c("Admit", "Gender", "Dept"), "Freq"),
    "non-negative"
  )

  missing_step <- ucb
  missing_step$Admit[1] <- NA

  expect_error(
    alluvial_prep_flow(missing_step, c("Admit", "Gender", "Dept"), "Freq"),
    "`steps`"
  )
})

test_that("plot validation catches invalid flow plot arguments", {
  model <- alluvial_prep_flow(as.data.frame(UCBAdmissions), c("Admit", "Gender", "Dept"), "Freq")

  expect_error(alluvial_plot(model, y_scale = "bad"), "`y_scale`")
  expect_error(alluvial_plot(model, col = "missing"), "`col`")
  expect_error(alluvial_plot(model, xlabs = c("one", "two")), "`xlabs`")
  expect_error(alluvial_plot(model, flow_clrs = c("red")), "`flow_clrs`")
})
