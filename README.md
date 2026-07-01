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
  auto_theme = TRUE,
  remove_y_axis = TRUE,
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
  auto_theme = TRUE,
  remove_y_axis = TRUE
)
```

Both trace and flow models support `y_scale = "prop"` and `y_scale = "count"`.
Bars and ribbons use the same scale.

## Interactive traces

`alluvial_plotly()` supports interactive highlighting for trace-level models.

```r
alluvial_plotly(
  trace_model,
  col = "arm",
  tooltip = c("patient_id", "arm", "risk")
)
```

Flow-level plotly output is not currently supported.
