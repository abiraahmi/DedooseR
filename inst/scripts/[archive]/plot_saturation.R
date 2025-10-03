plot_saturation <- function(saturation_output,
                            metric = c("count", "prop_media_titles"),
                            top_n = NULL,
                            exclude_codes = NULL) {
  metric <- match.arg(metric)

  # Ensure tibble input (not kable)
  if (!is.data.frame(saturation_output)) {
    stop("Input must be a tibble from set_saturation(output_type = 'tibble').")
  }

  df <- saturation_output

  # Exclude codes if requested
  if (!is.null(exclude_codes)) {
    df <- df %>%
      dplyr::filter(!code %in% exclude_codes)
  }

  # Optional: select top_n codes
  if (!is.null(top_n)) {
    df <- df %>%
      dplyr::slice_max(order_by = .data[[metric]], n = top_n)
  }

  # Plot
  ggplot(df, aes(x = reorder(code, .data[[metric]]),
                 y = .data[[metric]])) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
      x = "Code",
      y = ifelse(metric == "count", "Frequency", "Proportion of Media Titles"),
      title = paste("Code Saturation by", metric)
    ) +
    theme_minimal(base_size = 14)
}

# test_script.R

library(DedooseR)
library(tidyverse)
library(dplyr)
library(readxl)

# Clean data
excerpts <- read_xlsx("inst/raw_data/test_data.xlsx")
preferred_coders <- c("a", "l", "i", "r", "s", "v", "c", "n", "k")
excerpts <- clean_data(excerpts, preferred_coders)

# Count codes
code_counts <- count_codes(excerpts, min_count = 10, output = "tibble")

# Plot codes
plot_counts <- plot_counts(code_counts,
                           exclude_codes = c("c_priority_excerpt", "c_self_efficacy"),
                           metric = "n_media_titles",
                           min_prop = 0.40)

# Set saturation
saturation <- set_saturation(code_counts,
                             min_count = 40)

# Plot saturation
plot_saturation <- plot_saturation(saturation, metric = "prop_media_titles", exclude_codes = c("c_knowledge_awareness", "c_self_efficacy"))
