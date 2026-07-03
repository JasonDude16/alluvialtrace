#' Example patient status transitions
#'
#' A small synthetic trace-level data set for examples and tests. Each row is a
#' patient, and the ordered status columns describe how that patient moved
#' through three visits.
#'
#' @format A data frame with 18 rows and 6 variables:
#' \describe{
#'   \item{patient_id}{Unique patient identifier.}
#'   \item{baseline}{Status at baseline.}
#'   \item{month_1}{Status at month 1.}
#'   \item{month_3}{Status at month 3.}
#'   \item{arm}{Treatment arm.}
#'   \item{risk}{Baseline risk group.}
#' }
#' @examples
#' data(alluvialtrace_patient)
#'
#' trace_model <- alluvial_prep_trace(
#'   alluvialtrace_patient,
#'   id = "patient_id",
#'   steps = c("baseline", "month_1", "month_3")
#' )
"alluvialtrace_patient"
