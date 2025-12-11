plot_saturation <- function(code_counts,
                            total_media_titles = NULL,
                            min_count = 1,
                            min_prop_media_titles = NULL,
                            sort_by = c("count", "prop_media_titles")) {

  sort_by <- match.arg(sort_by)

  # Get filtered tibble from set_saturation
  df <- set_saturation(code_counts,
                       total_media_titles = total_media_titles,
                       min_count = min_count,
                       min_prop_media_titles = min_prop_media_titles,
                       output_type = "tibble")

  # Reorder factor levels for plotting
  if (sort_by == "count") {
    df <- df %>%
      dplyr::arrange(count)
    df$code <- factor(df$code, levels = df$code)
  } else {
    df <- df %>%
      dplyr::arrange(prop_media_titles)
    df$code <- factor(df$code, levels = df$code)
  }

  # Plot
  ggplot(df, aes(x = code, y = count, fill = prop_media_titles)) +
    geom_col() +
    coord_flip() +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    labs(
      title = "Saturation Plot",
      subtitle = paste0("min_count = ", min_count,
                        ", min_prop_media_titles = ",
                        ifelse(is.null(min_prop_media_titles), "none", min_prop_media_titles)),
      x = "Code",
      y = "Count",
      fill = "Proportion\nMedia Titles"
    ) +
    theme_minimal(base_size = 12)
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
plot_counts

# Set saturation
saturation <- set_saturation(code_counts, min_count = 10, min_prop_media_titles = 0.25)

# Plot saturation
saturation_plot <- plot_saturation(code_counts, min_count = 40, min_prop_media_titles = 0.25)
