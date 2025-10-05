set_saturation <- function(code_counts,
                           total_media_titles = NULL,
                           min_count = 1,
                           min_prop_media_titles = NULL,
                           output_type = c("tibble", "kable")) {
  output_type <- match.arg(output_type)

  # Handle both tibble or list input from create_code_summary()
  if (is.list(code_counts) && "table" %in% names(code_counts)) {
    code_counts <- code_counts$table
  }

  # Input validation
  if (!is.data.frame(code_counts)) {
    stop("`code_counts` must be a tibble or data frame (from create_code_summary()).")
  }
  if (!all(c("code", "count", "n_media_titles") %in% names(code_counts))) {
    stop("`code_counts` must contain columns `code`, `count`, and `n_media_titles`.")
  }

  # Determine denominator for proportions
  if (is.null(total_media_titles)) {
    total_media_titles <- max(code_counts$n_media_titles, na.rm = TRUE)
  }

  # Filter by count first
  df <- code_counts %>%
    dplyr::filter(.data$count >= min_count) %>%
    dplyr::mutate(
      prop_media_titles = round(.data$n_media_titles / total_media_titles, 2)
    ) %>%
    dplyr::select(.data$code, .data$count, .data$prop_media_titles)

  # Filter by proportion if requested
  if (!is.null(min_prop_media_titles)) {
    df <- df %>%
      dplyr::filter(.data$prop_media_titles >= min_prop_media_titles)
  }

  # Arrange
  df <- df %>%
    dplyr::arrange(dplyr::desc(.data$count))

  # Return in requested format
  caption_text <- paste(
    "Code Counts with Transcript Proportions (min_count =", min_count,
    ", min_prop_media_titles =",
    ifelse(is.null(min_prop_media_titles), "none", min_prop_media_titles),
    ")"
  )

  if (output_type == "kable") {
    knitr::kable(df, caption = caption_text, digits = 2)
  } else {
    df
  }
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

# Create code summary
code_summary <- create_code_summary(data_merged,
                                           table_min_count = 40,
                                           output = "tibble",
                                           plot = FALSE)

# Set saturation
saturation <- set_saturation(create_code_summary, min_count = 10, min_prop_media_titles = 0.25)
