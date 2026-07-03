# alluvialtrace

`alluvialtrace` prepares and plots traceable alluvial diagrams from either
individual-level records or pre-aggregated flow counts.

The package has two data-prep paths:

- `alluvial_prep_trace()` for one row per entity, where each entity can be
  followed as a line.
- `alluvial_prep_flow()` for pre-aggregated counts, where each transition is
  drawn as a ribbon.

## Installation

```r
# install.packages("remotes")
remotes::install_github("JasonDude16/alluvialtrace")
```

## Trace-level alluvials

Use trace mode when each row is an entity with a stable identifier.

```r
library(alluvialtrace)

data(alluvialtrace_patient)

steps <- c("baseline", "month_1", "month_3")

trace_model <- alluvial_prep_trace(
  alluvialtrace_patient,
  id = "patient_id",
  steps = steps,
  keep_vars = TRUE,
  add_flows = TRUE,
  y_fctr_order = c("Critical", "High", "Moderate", "Low")
)

alluvial_plot(
  trace_model,
  col = "arm",
  theme_classic = TRUE,
  remove_y_axis = TRUE,
  center_title = TRUE,
  ggtitle = "Patient status over time"
)
```

Trace mode requires `id` values to be unique. The plotted lines are positioned
inside their computed slots rather than on stratum boundaries, so a single
patient in a transition is drawn through the middle of that transition.

## Flow-level alluvials

Use flow mode when data are already aggregated.

```r
library(dplyr)
library(alluvialtrace)

flow_data <- alluvialtrace_patient |>
  count(baseline, month_1, month_3, name = "n")

flow_model <- alluvial_prep_flow(
  flow_data,
  steps = c("baseline", "month_1", "month_3"),
  weights = "n",
  y_fctr_order = c("Critical", "High", "Moderate", "Low")
)

alluvial_plot(
  flow_model,
  y_scale = "count",
  theme_classic = TRUE,
  remove_y_axis = TRUE,
  center_title = TRUE
)
```

Both trace and flow models support `y_scale = "prop"` and `y_scale = "count"`.
Bars and ribbons use the same scale.

## Interactive traces

`alluvial_plotly()` supports interactive highlighting for trace-level models.
See the live demo at <https://jasondude16.github.io/alluvialtrace/>.

```r
motor_file <- system.file("extdata", "motor.RDS", package = "alluvialtrace")
data <- readRDS(motor_file)
data$Case <- stringr::str_replace(data$Case, "Case", "")
steps <- colnames(data)[1:4]

clrs <- c("#4F8CB7", "#4ead3b", "#b063c7", "#f09516", "#f25746")
xlabs <- c("BL", "24 Hours", "7-10 Days", "90 Days")
labels <- list(
  "type" = "text",
  "what" = "level",
  "where" = list("steps" = "Motor.Arm.Affected.BL", "values" = 0:4)
)

motor_traces <- alluvial_prep_trace(data, "Case", steps, add_flows = T, keep_vars = T)

alluvial_plotly(
  motor_traces,
  bar_clrs = rev(clrs),
  flow_clrs = rev(clrs),
  xlabs = xlabs,
  show.legend = FALSE,
  bar_width = 0.8,
  border_col = NA,
  hpad = 0,
  ggtitle = "Motor Arm",
  remove_y_axis = T,
  bar_alpha = 0.9,
  labels = labels,
  trace_alpha = .2,
  flow_alpha = .3,
  trace_lwd = 0.5,
  ggsubtitle = "(most affected arm)",
  title_bold = TRUE,
  xlab_bold = TRUE,
  subtitle_bold = TRUE,
  center_title = TRUE,
  theme_classic = TRUE,
  label_bold = FALSE,
  opacityDim = 1
)
```

Flow-level plotly output is not currently supported.

The GitHub Pages demo can be regenerated with:

```r
source("inst/dev/build_github_pages.R")
```
