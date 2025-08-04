# Script to test Excerpt viewer function for package
# Abi
# July 31st, 2025

# Load packages
library(tidyverse)
library(DedooseR)

#' Plot Saturation Tracking Bar Chart
#'
#' @param long_codes A data frame output from create_saturation_tracking()
#' @return A ggplot object showing stacked bar chart of Priority and Heterogeneity code counts
#' @examples
#' long_codes <- create_saturation_tracking("path/to/your/data.xlsx", c("coder1", "coder2"))
#' plot_saturation_tracking(long_codes)
plot_saturation <- function(long_codes) {
  # Check input
  if (is.null(long_codes) || !is.data.frame(long_codes)) {
    stop("Please provide a valid data frame from create_saturation_tracking().")
  }

  # Step 1: Reshape the data
  long_codes_melted <- long_codes %>%
    select(Code, Priority_Count, Heterogeneity_Count) %>%
    pivot_longer(cols = c(Priority_Count, Heterogeneity_Count),
                 names_to = "Type",
                 values_to = "Count") %>%
    filter(Count > 0)

  # Step 2: Create the bar chart
  p <- ggplot(long_codes_melted, aes(x = reorder(Code, Count), y = Count, fill = Type)) +
    geom_bar(stat = "identity", position = "stack") +
    theme_minimal() +
    coord_flip() +
    labs(
      title = "Priority and Heterogeneity Counts",
      x = "Code",
      y = "Count",
      fill = "Type"
    ) +
    scale_fill_manual(values = c("Priority_Count" = "#330662",
                                 "Heterogeneity_Count" = "#3CBBB1")) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  print(p)
  invisible(p)
}



