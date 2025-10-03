plot_compare_saturation <- function(compare_results, thresholds_list) {
  set_cols <- names(thresholds_list)

  # reshape to long format (one row per code per set)
  plot_data <- compare_results %>%
    dplyr::select(code, count, dplyr::all_of(set_cols)) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(set_cols),
      names_to = "Set",
      values_to = "Meets"
    ) %>%
    dplyr::filter(Meets == TRUE)   # drop FALSE cases entirely

  ggplot2::ggplot(plot_data, ggplot2::aes(x = reorder(code, count),
                                          y = count,
                                          fill = Set)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~ Set, ncol = 1, scales = "free_y") +
    ggplot2::labs(
      x = "Code",
      y = "Count",
      title = "Code Saturation Comparison by Threshold Set"
    ) +
    ggplot2::theme_minimal()
}
# Test

# Load packages
library(DedooseR)
library(tidyverse)
library(dplyr)
library(readxl)

# Clean data
excerpts <- read_xlsx("inst/raw_data/test_data.xlsx")
preferred_coders <- c("a", "l", "i", "r", "s", "v", "c", "n", "k")
excerpts <- clean_data(excerpts, preferred_coders)

# Count codes
code_counts <- count_codes(excerpts,
                           min_count = 10,
                           exclude = c("c_priority_excerpt", "c_heterogeneity", "c_program_implementation_unique_value_opportunity_from_dc",
                                       "c_ripple_impact_ripple_impact_who", "c_ripple_impact_ripple_missed"),
                           output = "tibble")

# Plot codes
plot_counts <- plot_counts(code_counts,
                           exclude_codes = c("c_priority_excerpt", "c_self_efficacy"),
                           metric = "n_media_titles",
                           min_prop = 0.40)

# Set saturation
saturation <- set_saturation(code_counts, min_count = 10, min_prop_media_titles = 0.25)


# Plot saturation

# Define thresholds
thresholds_list <- list(
  Set1 = list(code_count = 40, prop_media_title = 0.2),
  Set2 = list(code_count = 60, prop_media_title = 0.4)
)
# Apply thresholds
comp_saturation <- compare_saturation(code_counts, excerpts, thresholds_list)

# Plot
plot_compare_saturation(comp_saturation, thresholds_list)
