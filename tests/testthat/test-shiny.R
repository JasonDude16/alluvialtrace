test_that("alluvial_shiny validates input and returns an app object", {
  data("alluvialtrace_patient", package = "alluvialtrace")
  model <- alluvial_prep_trace(
    alluvialtrace_patient,
    id = "patient_id",
    steps = c("baseline", "month_1", "month_3"),
    add_flows = TRUE
  )

  expect_error(alluvial_shiny(list(), run = FALSE), "alluvial model")
  expect_s3_class(alluvial_shiny(model, run = FALSE), "shiny.appobj")
  expect_s3_class(alluvial_shiny(alluvial_prep_flow(as.data.frame(UCBAdmissions), c("Admit", "Gender", "Dept"), "Freq"), run = FALSE), "shiny.appobj")
})

test_that("shiny app parsers handle labels, palettes, and plot arguments", {
  data("alluvialtrace_patient", package = "alluvialtrace")
  model <- alluvial_prep_trace(
    alluvialtrace_patient,
    id = "patient_id",
    steps = c("baseline", "month_1", "month_3"),
    add_flows = TRUE
  )
  input <- list(
    show_labels = TRUE,
    label_all = FALSE,
    label_steps = c("baseline", "month_1"),
    label_values = c("Low", "High"),
    label_type = "text",
    label_what = "level",
    bar_width = 1,
    hpad = 0,
    col = "y_from",
    output_type = "static",
    xlabs = "Baseline, Month 1, Month 3",
    ggtitle = "Motor Arm",
    ggsubtitle = "Subtitle",
    y_scale = "prop",
    bar_clr_1 = "#111111",
    bar_clr_2 = "#222222",
    bar_clr_3 = "#333333",
    bar_clr_4 = "#444444",
    flow_clr_1 = "#aaaaaa",
    flow_clr_2 = "#bbbbbb",
    flow_clr_3 = "#cccccc",
    flow_clr_4 = "#dddddd",
    bar_alpha = 1,
    trace_alpha = 0.3,
    flow_alpha = 0.7,
    border_col = "black",
    show_legend = TRUE,
    theme_classic = TRUE,
    remove_y_axis = FALSE,
    center_title = TRUE,
    trace_lwd = 1,
    title_size = 24,
    title_bold = FALSE,
    subtitle_size = 12,
    subtitle_bold = TRUE,
    xlab_size = 13,
    xlab_bold = FALSE,
    label_size = 4,
    label_bold = TRUE,
    highlight_col = "black",
    highlight_on = "plotly_hover",
    highlight_off = "plotly_doubleclick",
    opacity_dim = 0.2,
    dynamic = FALSE,
    tooltip = "patient_id"
  )

  args <- alluvialtrace:::.alluvial_shiny_args(model, input)

  expect_equal(args$common$xlabs, c("Baseline", "Month 1", "Month 3"))
  expect_equal(args$common$labels$what, "level")
  expect_equal(args$common$labels$where$steps, c("baseline", "month_1"))
  expect_equal(args$common$bar_clrs, c(Critical = "#111111", High = "#222222", Low = "#333333", Moderate = "#444444"))
  expect_equal(args$common$flow_clrs, c(Critical = "#aaaaaa", High = "#bbbbbb", Low = "#cccccc", Moderate = "#dddddd"))
  expect_equal(args$common$y_scale, "prop")
  expect_equal(args$common$ggsubtitle, "Subtitle")
  expect_equal(args$common$title_size, 24)
  expect_false(args$common$title_bold)
  expect_equal(args$common$subtitle_size, 12)
  expect_true(args$common$subtitle_bold)
  expect_true(args$common$theme_classic)
  expect_true(args$common$center_title)
  expect_equal(args$common$xlab_size, 13)
  expect_false(args$common$xlab_bold)
  expect_equal(args$common$label_size, 4)
  expect_true(args$common$label_bold)
  expect_no_error(ggplot2::ggplot_build(do.call(alluvial_plot, c(list(x = model), args$common))))
})

test_that("shiny argument parser handles defaults, all labels, and invalid x labels", {
  data("alluvialtrace_patient", package = "alluvialtrace")
  model <- alluvial_prep_trace(
    alluvialtrace_patient,
    id = "patient_id",
    steps = c("baseline", "month_1", "month_3")
  )
  input <- list(
    show_labels = FALSE,
    label_all = TRUE,
    label_type = "text",
    label_what = "count",
    bar_width = 1,
    hpad = 0,
    col = "y_from",
    output_type = "static",
    xlabs = "Too few, labels",
    ggtitle = "",
    ggsubtitle = "",
    y_scale = "count",
    bar_alpha = 1,
    trace_alpha = 1,
    flow_alpha = 1,
    border_col = "gray30",
    show_legend = FALSE,
    theme_classic = NULL,
    remove_y_axis = TRUE,
    center_title = NULL,
    trace_lwd = 2,
    title_size = NULL,
    title_bold = NULL,
    subtitle_size = NULL,
    subtitle_bold = NULL,
    xlab_size = NULL,
    xlab_bold = NULL,
    label_size = NULL,
    label_bold = NULL,
    highlight_col = NULL,
    highlight_on = NULL,
    highlight_off = NULL,
    opacity_dim = NULL,
    dynamic = TRUE,
    tooltip = NULL
  )

  args <- alluvialtrace:::.alluvial_shiny_args(model, input)

  expect_null(args$common$labels)
  expect_null(args$common$xlabs)
  expect_null(args$common$ggtitle)
  expect_null(args$common$ggsubtitle)
  expect_named(args$common$bar_clrs, alluvialtrace:::.bar_color_values(model))
  expect_named(args$common$flow_clrs, alluvialtrace:::.flow_color_values(model, "y_from"))
  expect_equal(args$common$y_scale, "prop")
  expect_false(args$common$show.legend)
  expect_false(args$common$theme_classic)
  expect_false(args$common$center_title)
  expect_true(args$common$remove_y_axis)
  expect_equal(args$common$subtitle_size, 18)
  expect_false(args$common$subtitle_bold)
  expect_equal(args$plotly$highlight_col, "black")
  expect_equal(args$plotly$highlight_on, "plotly_hover")
  expect_equal(args$plotly$highlight_off, "plotly_doubleclick")
  expect_equal(args$plotly$opacityDim, 0.2)
  expect_false(args$plotly$dynamic)
  expect_equal(args$plotly$tooltip, model$ID)

  input$show_labels <- TRUE
  input$label_all <- TRUE
  args <- alluvialtrace:::.alluvial_shiny_args(model, input)
  expect_equal(args$common$labels$where, "all")

  input$output_type <- "plotly"
  input$label_type <- "label"
  args <- alluvialtrace:::.alluvial_shiny_args(model, input)
  expect_equal(args$common$labels$type, "text")

  args <- alluvialtrace:::.alluvial_shiny_args(model, input, xlabs = c("B", "M1", "M3"))
  expect_equal(args$common$xlabs, c("B", "M1", "M3"))
})

test_that("shiny UI choices match trace and flow model capabilities", {
  data("alluvialtrace_patient", package = "alluvialtrace")
  trace_model <- alluvial_prep_trace(
    alluvialtrace_patient,
    id = "patient_id",
    steps = c("baseline", "month_1", "month_3"),
    keep_vars = TRUE,
    add_flows = TRUE
  )
  flow_model <- alluvial_prep_flow(as.data.frame(UCBAdmissions), c("Admit", "Gender", "Dept"), "Freq")

  expect_contains(alluvialtrace:::.alluvial_plot_col_choices(trace_model), c("y_from", "arm", "risk"))
  expect_contains(alluvialtrace:::.alluvial_trace_choices(trace_model), c("patient_id", "arm", "risk"))
  expect_equal(alluvialtrace:::.alluvial_trace_choices(flow_model), character())
  expect_equal(alluvialtrace:::.alluvial_default_col(flow_model, alluvialtrace:::.alluvial_plot_col_choices(flow_model)), "y_from")
})

test_that("shiny UI conditionally shows trace and export controls", {
  data("alluvialtrace_patient", package = "alluvialtrace")
  trace_model <- alluvial_prep_trace(
    alluvialtrace_patient,
    id = "patient_id",
    steps = c("baseline", "month_1", "month_3")
  )
  trace_flow_model <- alluvial_prep_trace(
    alluvialtrace_patient,
    id = "patient_id",
    steps = c("baseline", "month_1", "month_3"),
    add_flows = TRUE
  )
  flow_model <- alluvial_prep_flow(as.data.frame(UCBAdmissions), c("Admit", "Gender", "Dept"), "Freq")

  trace_html <- as.character(shiny::tagList(alluvialtrace:::.alluvial_shiny_ui(trace_model)))
  trace_flow_html <- as.character(shiny::tagList(alluvialtrace:::.alluvial_shiny_ui(trace_flow_model)))
  flow_html <- as.character(shiny::tagList(alluvialtrace:::.alluvial_shiny_ui(flow_model)))
  trace_order <- regexpr(">Style<", trace_html)[[1]] < regexpr(">Labels<", trace_html)[[1]] &&
    regexpr(">Labels<", trace_html)[[1]] < regexpr(">Typography<", trace_html)[[1]] &&
    regexpr(">Typography<", trace_html)[[1]] < regexpr(">Geometry<", trace_html)[[1]]

  expect_match(trace_html, "alluvial-header")
  expect_match(trace_html, "alluvial-preview-control")
  expect_match(trace_html, ">Preview<")
  expect_match(trace_html, "alluvial-accordion")
  expect_match(trace_html, "alluvial-accordion-title")
  expect_match(trace_html, "alluvial-output-options")
  expect_match(trace_html, ">Width<")
  expect_match(trace_html, ">Height<")
  expect_match(trace_html, "Color column")
  expect_match(trace_html, "Edit colors")
  expect_match(trace_html, "Edit x labels")
  expect_match(trace_html, "theme_classic\\(\\)")
  expect_match(trace_html, "Center title")
  expect_false(grepl("open=\"open\"", trace_html, fixed = TRUE))
  expect_false(grepl("Flow ribbon alpha", trace_html, fixed = TRUE))
  expect_match(trace_flow_html, "Flow ribbon alpha")
  expect_false(grepl("Use package theme", trace_html, fixed = TRUE))
  expect_false(grepl("Y scale", trace_html, fixed = TRUE))
  expect_false(grepl(">DPI<", trace_html, fixed = TRUE))
  expect_false(grepl("Dynamic highlighting", trace_html, fixed = TRUE))
  expect_match(trace_html, ">Style<")
  expect_match(trace_html, ">Geometry<")
  expect_true(trace_order)
  expect_match(trace_html, "X label size")
  expect_match(trace_html, "Trace alpha")
  expect_match(trace_html, "Typography")
  expect_match(trace_html, "Labels")
  expect_match(trace_html, "Export")
  expect_match(trace_html, "Plotly")
  expect_match(trace_html, "Tooltip columns")
  expect_match(trace_html, "Save static")
  expect_match(trace_html, "Save HTML")
  expect_match(trace_html, "Interactive plotly")
  expect_match(trace_html, "input.output_type == &#39;static&#39;|input.output_type == 'static'")
  expect_match(trace_html, "input.output_type == &#39;plotly&#39;|input.output_type == 'plotly'")

  expect_false(grepl("Trace alpha", flow_html, fixed = TRUE))
  expect_match(flow_html, "alluvial-preview-control")
  expect_match(flow_html, ">Preview<")
  expect_match(flow_html, "Static ggplot")
  expect_false(grepl("Tooltip columns", flow_html, fixed = TRUE))
  expect_true(grepl("Save static", flow_html, fixed = TRUE))
  expect_false(grepl("Save HTML", flow_html, fixed = TRUE))
  expect_false(grepl("Interactive plotly", flow_html, fixed = TRUE))
})

test_that("shiny x-label modal helpers create and parse label mappings", {
  steps <- c("baseline", "month_1", "month_3")
  labels <- c("Baseline", "Month 1", "Month 3")
  controls <- alluvialtrace:::.alluvial_xlab_controls(steps, labels)
  controls_html <- as.character(shiny::tagList(controls))
  input <- list(draft_xlab_1 = "Base", draft_xlab_2 = "", draft_xlab_3 = "Month three")

  expect_s3_class(controls, "shiny.tag.list")
  expect_match(controls_html, "From")
  expect_match(controls_html, "To")
  expect_match(controls_html, "draft_xlab_1")
  expect_equal(
    alluvialtrace:::.xlabs_from_inputs(input, steps, labels),
    c("Base", "month_1", "Month three")
  )
})

test_that("shiny color picker helpers create stable named palettes", {
  data("alluvialtrace_patient", package = "alluvialtrace")
  model <- alluvial_prep_trace(
    alluvialtrace_patient,
    id = "patient_id",
    steps = c("baseline", "month_1", "month_3"),
    keep_vars = TRUE
  )
  values <- alluvialtrace:::.bar_color_values(model)
  ids <- alluvialtrace:::.color_input_ids(values, "bar_clr_")
  input <- as.list(stats::setNames(c("#010101", "#020202", "#030303", "#040404"), ids))

  expect_equal(values, c("Critical", "High", "Low", "Moderate"))
  expect_equal(ids, c(Critical = "bar_clr_1", High = "bar_clr_2", Low = "bar_clr_3", Moderate = "bar_clr_4"))
  expect_equal(
    alluvialtrace:::.palette_from_color_inputs(input, values, "bar_clr_"),
    c(Critical = "#010101", High = "#020202", Low = "#030303", Moderate = "#040404")
  )
  expect_contains(alluvialtrace:::.flow_color_values(model, "arm"), c("Control", "Intervention"))
  controls <- alluvialtrace:::.alluvial_color_controls("Colors", values, "bar_clr_", input)
  controls_html <- as.character(shiny::tagList(controls))

  expect_s3_class(controls, "shiny.tag.list")
  expect_match(controls_html, "type=\"color\"")
  expect_match(controls_html, "Shiny.setInputValue")
  expect_match(controls_html, "bar_clr_1")
})
