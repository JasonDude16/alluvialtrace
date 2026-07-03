test_that("alluvial_curve and alluvial_model land exactly on endpoints", {
  curve <- alluvial_curve(from = -4, to = 4, length.out = 9)
  expect_equal(length(curve), 9)
  expect_equal(curve[1], -4)
  expect_equal(curve[9], 4)

  x <- data.frame(
    curve = curve,
    pos_start = 0.2,
    pos_diff = 0.5
  )
  y <- alluvial_model(x, "pos_start")

  expect_equal(y[1], 0.2)
  expect_equal(y[length(y)], 0.7)
  expect_true(all(diff(y) > 0))

  flat <- transform(x, pos_diff = 0)
  expect_equal(alluvial_model(flat, "pos_start"), rep(0.2, length(curve)))
})

test_that("trace preparation handles two-step data and downsampled resolution", {
  data <- data.frame(
    id = paste0("id", 1:6),
    first = c("A", "A", "A", "B", "B", "B"),
    second = c("C", "C", "D", "C", "D", "D")
  )

  model <- alluvial_prep_trace(data, "id", c("first", "second"), res = 0.5)

  expect_identical(model$n_steps, 2L)
  expect_equal(model$n_curves, 1)
  expect_equal(model$N, nrow(data))
  expect_equal(length(model$curve), length(unique(model$traces$curve)))
  expect_true(all(model$traces$pos_start > 0 & model$traces$pos_start < 1))
  expect_true(all(model$traces$pos_end > 0 & model$traces$pos_end < 1))
  expect_equal(model$bars$y_prop[model$bars$step == "first"], c(0.5, 0.5))
})

test_that("custom model functions are used for traces and flows", {
  linear_model <- function(x, pos) {
    scaled <- (x$curve - min(x$curve)) / (max(x$curve) - min(x$curve))
    x[[pos]] + (x$pos_diff * scaled)
  }
  data <- data.frame(
    id = paste0("id", 1:4),
    first = c("A", "A", "B", "B"),
    second = c("B", "A", "A", "B")
  )
  flow_data <- data.frame(
    first = c("A", "A", "B", "B"),
    second = c("A", "B", "A", "B"),
    n = c(1, 2, 3, 4)
  )

  trace_model <- alluvial_prep_trace(data, "id", c("first", "second"), curve = c(0, 1), model_fun = linear_model)
  flow_model <- alluvial_prep_flow(flow_data, c("first", "second"), "n", curve = c(0, 1), model_fun = linear_model)

  trace_endpoints <- trace_model$traces[trace_model$traces$curve %in% c(0, 1), ]
  expect_equal(trace_endpoints$y_prop[trace_endpoints$curve == 0], trace_endpoints$pos_start[trace_endpoints$curve == 0])
  expect_equal(trace_endpoints$y_prop[trace_endpoints$curve == 1], trace_endpoints$pos_end[trace_endpoints$curve == 1])

  flow_endpoints <- flow_model$flows[flow_model$flows$curve %in% c(0, 1), ]
  expect_equal(flow_endpoints$ymin_prop[flow_endpoints$curve == 0], flow_endpoints$pos_start[flow_endpoints$curve == 0])
  expect_equal(flow_endpoints$ymin_prop[flow_endpoints$curve == 1], flow_endpoints$pos_start[flow_endpoints$curve == 1] + flow_endpoints$pos_diff[flow_endpoints$curve == 1])
})

test_that("factor order controls vertical bar ordering", {
  data <- data.frame(
    id = 1:4,
    first = c("low", "low", "high", "critical"),
    second = c("high", "critical", "low", "low")
  )

  model <- alluvial_prep_trace(
    data,
    id = "id",
    steps = c("first", "second"),
    y_fctr_order = c("critical", "high", "low")
  )
  first_bars <- model$bars[model$bars$step == "first", ]

  expect_equal(as.character(first_bars$y_value), c("low", "high", "critical"))
  expect_equal(first_bars$prop_cumsum, c(0.5, 0.75, 1))
})

test_that("preparation validates steps, ids, resolution, and size guardrails", {
  data <- data.frame(id = 1:3, first = c("A", "B", "C"), second = c("B", "C", "A"))

  expect_error(alluvial_prep_trace(data, "id", "first"), "at least two")
  expect_error(alluvial_prep_trace(data, "id", c("first", "second"), res = 0), "`res`")
  expect_error(alluvial_prep_trace(data, "id", c("first", "second"), res = 1.5), "`res`")
  expect_error(alluvial_prep_trace(data, id = c("id", "other"), steps = c("first", "second")), "`id`")
  expect_error(alluvial_prep_flow(data.frame(first = "A", second = "B", n = NA_real_), c("first", "second"), "n"), "non-missing")

  big <- data.frame(
    id = seq_len(2042),
    first = rep(c("A", "B"), length.out = 2042),
    second = rep(c("B", "A"), length.out = 2042)
  )
  expect_error(alluvial_prep_trace(big, "id", c("first", "second")), "exceeds")
  expect_no_error(alluvial_prep_trace(big, "id", c("first", "second"), force = TRUE))
})
