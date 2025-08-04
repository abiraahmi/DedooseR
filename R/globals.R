# Global variables used in NSE contexts to suppress R CMD check warnings
#' @importFrom utils globalVariables
utils::globalVariables(c(
  # Variables used in coccur.R
  "Frequency", "Label", "Code1", "Code2",
  
  # Variables used in create_saturation_tracking.R
  "Excerpt Range", "Excerpt Date", "Resource Creator", "Resource Date",
  "Excerpt Creator", "coder_rank", "Media Title", "Code: Priority excerpt Applied",
  "Excerpt Copy", "Code: Heterogeniety Applied", "Applied", "Code",
  "Priority_Applied", "Heterogeneity_Applied", "Priority_Count", "Heterogeneity_Count",
  
  # Variables used in plot_saturation.R and set_saturation.R
  "Count", "Type"
))