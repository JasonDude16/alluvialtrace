test_that("label subsets are applied without changing unlabeled bars", {
  data("alluvialtrace_patient", package = "alluvialtrace")
  model <- alluvial_prep_trace(
    alluvialtrace_patient,
    id = "patient_id",
    steps = c("baseline", "month_1", "month_3")
  )
  labels <- list(
    type = "label",
    what = "perc",
    where = list(steps = "baseline", values = "Low")
  )

  p <- alluvial_plot(model, labels = labels)
  built <- ggplot2::ggplot_build(p)
  label_layers <- Filter(function(layer) "label" %in% names(layer), built$data)

  expect_no_error(ggplot2::ggplot_gtable(built))
  expect_length(label_layers, 1)
  expect_equal(nrow(label_layers[[1]]), 1)
  expect_equal(label_layers[[1]]$label, model$bars$y_perc[model$bars$step == "baseline" & model$bars$y_value == "Low"])
})

test_that("factor level labels can be shown on bars", {
  model <- alluvial_prep_flow(as.data.frame(UCBAdmissions), c("Admit", "Gender", "Dept"), "Freq")
  labels <- list(
    type = "text",
    what = "level",
    where = list(steps = "Admit", values = "Rejected")
  )

  p <- alluvial_plot(model, labels = labels)
  built <- ggplot2::ggplot_build(p)
  label_layers <- Filter(function(layer) "label" %in% names(layer), built$data)

  expect_length(label_layers, 1)
  expect_equal(nrow(label_layers[[1]]), 1)
  expect_equal(label_layers[[1]]$label, "Rejected")
})

test_that("plot title and label typography arguments are applied", {
  data("alluvialtrace_patient", package = "alluvialtrace")
  model <- alluvial_prep_trace(
    alluvialtrace_patient,
    id = "patient_id",
    steps = c("baseline", "month_1", "month_3")
  )
  labels <- list(type = "text", what = "count", where = "all")

  p <- alluvial_plot(
    model,
    ggtitle = "Styled",
    ggsubtitle = "Subtitle",
    labels = labels,
    title_size = 17,
    title_bold = FALSE,
    subtitle_size = 9,
    subtitle_bold = TRUE,
    label_size = 3,
    label_bold = TRUE,
    xlab_size = 11,
    xlab_bold = FALSE,
    center_title = TRUE
  )
  built <- ggplot2::ggplot_build(p)
  label_layers <- Filter(function(layer) "label" %in% names(layer), built$data)

  expect_equal(p$theme$plot.title$size, 17)
  expect_equal(p$theme$plot.title$face, "plain")
  expect_equal(p$theme$plot.title$hjust, 0.5)
  expect_equal(p$theme$plot.title.position, "plot")
  expect_equal(p$labels$subtitle, "Subtitle")
  expect_equal(p$theme$plot.subtitle$size, 9)
  expect_equal(p$theme$plot.subtitle$face, "bold")
  expect_equal(p$theme$axis.text.x$size, 11)
  expect_equal(p$theme$axis.text.x$face, "plain")
  expect_equal(label_layers[[1]]$size[1], 3)
  expect_equal(label_layers[[1]]$fontface[1], "bold")
})

test_that("plot arguments control legend, theme, axes, and manual scales", {
  model <- alluvial_prep_flow(as.data.frame(UCBAdmissions), c("Admit", "Gender", "Dept"), "Freq")
  p <- alluvial_plot(
    model,
    xlabs = c("Admission", "Gender", "Department"),
    show.legend = FALSE,
    theme_classic = TRUE,
    center_title = TRUE,
    remove_y_axis = TRUE,
    bar_clrs = c(
      Rejected = "#111111", Admitted = "#222222", F = "#333333", E = "#444444",
      D = "#555555", C = "#666666", B = "#777777", A = "#888888",
      Female = "#999999", Male = "#aaaaaa"
    ),
    flow_clrs = c(Male = "#bbbbbb", Female = "#cccccc", Admitted = "#dddddd", Rejected = "#eeeeee")
  )

  plot_warnings <- character()
  withCallingHandlers(
    ggplot2::ggplot_gtable(ggplot2::ggplot_build(p)),
    warning = function(w) {
      plot_warnings <<- c(plot_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_false(any(grepl("Removed .*geom_ribbon", plot_warnings)))
  expect_equal(p$labels$title, NULL)
  expect_equal(p$theme$legend.position, "none")
  expect_s3_class(p$theme$axis.text.y, "element_blank")
  expect_s3_class(p$scales$get_scales("fill"), "ScaleDiscrete")
  built <- ggplot2::ggplot_build(p)
  expect_equal(built$layout$panel_params[[1]]$x$get_labels(), c("Admission", "Gender", "Department"))
})

test_that("palette parsing and validation catches malformed color inputs", {
  model <- alluvial_prep_flow(as.data.frame(UCBAdmissions), c("Admit", "Gender", "Dept"), "Freq")

  expect_null(alluvialtrace:::.parse_palette(""))
  expect_null(alluvialtrace:::.parse_palette("  "))
  expect_equal(alluvialtrace:::.parse_palette("#111111; #222222\n#333333"), c("#111111", "#222222", "#333333"))
  expect_equal(alluvialtrace:::.parse_palette("A=#111111\nB=#222222"), c(A = "#111111", B = "#222222"))
  expect_equal(alluvialtrace:::.parse_palette("A=#111111, #222222"), c("A=#111111", "#222222"))

  expect_error(alluvial_plot(model, bar_clrs = c(Rejected = NA_character_)), "`bar_clrs`")
  expect_error(alluvial_plot(model, flow_clrs = 1:3), "`flow_clrs`")
  expect_error(alluvial_plot(model, bar_clrs = c(Rejected = "#111111")), "missing colors")
  expect_error(alluvial_plot(model, labels = list(type = "text", what = "bad", where = "all")), "`labels\\$what`")
})

test_that("plotly output rejects flow models and accepts null tooltip", {
  flow_model <- alluvial_prep_flow(as.data.frame(UCBAdmissions), c("Admit", "Gender", "Dept"), "Freq")
  trace_model <- alluvial_prep_trace(
    data.frame(id = 1:3, first = c("A", "A", "B"), second = c("B", "A", "B")),
    id = "id",
    steps = c("first", "second")
  )

  expect_error(alluvial_plotly(flow_model), "not currently supported")
  plotly_messages <- character()
  plotly_obj <- withCallingHandlers(
    alluvial_plotly(trace_model, tooltip = NULL),
    message = function(m) {
      plotly_messages <<- c(plotly_messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_s3_class(plotly_obj, "plotly")
  expect_false(any(grepl("Setting the `off` event", plotly_messages, fixed = TRUE)))

  plotly_plot <- plotly::plotly_build(alluvial_plotly(
    trace_model,
    ggtitle = "Title <A>",
    ggsubtitle = "Subtitle & detail",
    subtitle_bold = TRUE,
    tooltip = NULL
  ))
  expect_match(plotly_plot$x$layout$title$text, "<b>Title &lt;A&gt;</b>", fixed = TRUE)
  expect_match(plotly_plot$x$layout$title$text, "<sup><b>Subtitle &amp; detail</b></sup>", fixed = TRUE)
  expect_gte(plotly_plot$x$layout$margin$t, 95)

  label_plot <- plotly::plotly_build(alluvial_plotly(
    trace_model,
    labels = list(type = "text", what = "level", where = "all"),
    tooltip = NULL,
    xlab_bold = TRUE,
    label_bold = TRUE
  ))
  expect_equal(label_plot$x$layout$xaxis$ticktext, c("<b>first</b>", "<b>second</b>"))
  text_traces <- Filter(function(trace) !is.null(trace$mode) && grepl("text", trace$mode, fixed = TRUE), label_plot$x$data)
  expect_length(text_traces, 1)
  expect_true(all(grepl("^<b>.*</b>$", text_traces[[1]]$text)))
})

test_that("alluvial_save writes static and html outputs", {
  trace_model <- alluvial_prep_trace(
    data.frame(id = 1:3, first = c("A", "A", "B"), second = c("B", "A", "B")),
    id = "id",
    steps = c("first", "second")
  )
  static_plot <- alluvial_plot(trace_model)
  html_plot <- alluvial_plotly(trace_model)
  png_file <- tempfile(fileext = ".png")
  html_file <- tempfile(fileext = ".html")

  expect_identical(alluvial_save(static_plot, png_file, width = 4, height = 3, dpi = 72), invisible(static_plot))
  expect_true(file.exists(png_file))
  expect_gt(file.info(png_file)$size, 0)

  expect_identical(alluvial_save(html_plot, html_file, selfcontained = FALSE), invisible(html_plot))
  expect_true(file.exists(html_file))
  expect_gt(file.info(html_file)$size, 0)
})
