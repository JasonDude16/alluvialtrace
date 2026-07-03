alluvialtrace_patient <- data.frame(
  patient_id = sprintf("P%02d", 1:18),
  baseline = c(
    "Critical", "Critical", "Critical", "High", "High", "High",
    "High", "Moderate", "Moderate", "Moderate", "Moderate",
    "Low", "Low", "Low", "Low", "Low", "Moderate", "High"
  ),
  month_1 = c(
    "High", "Critical", "High", "High", "Moderate", "High",
    "Critical", "Moderate", "Low", "Moderate", "High",
    "Low", "Moderate", "Low", "Low", "Moderate", "Low", "Moderate"
  ),
  month_3 = c(
    "Moderate", "High", "High", "Moderate", "Low", "Moderate",
    "High", "Low", "Low", "Moderate", "Moderate",
    "Low", "Low", "Low", "Moderate", "Moderate", "Low", "Low"
  ),
  arm = c(
    "Intervention", "Control", "Intervention", "Control", "Intervention",
    "Control", "Intervention", "Control", "Intervention", "Control",
    "Intervention", "Control", "Intervention", "Control", "Intervention",
    "Control", "Intervention", "Control"
  ),
  risk = c(
    "Elevated", "Elevated", "Elevated", "Elevated", "Elevated", "Elevated",
    "Elevated", "Standard", "Standard", "Standard", "Elevated",
    "Standard", "Standard", "Standard", "Standard", "Standard", "Standard",
    "Elevated"
  ),
  stringsAsFactors = FALSE
)

save(alluvialtrace_patient, file = "data/alluvialtrace_patient.rda", version = 2)
