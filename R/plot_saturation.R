#' Plot Code Saturation
#'
#' `plot_saturation()` creates a bar chart of codes that meet saturation
#' thresholds defined by \code{set_saturation()}. Bars represent code counts
#' across excerpts, and the fill color indicates the proportion of media titles
#' in which each code appears.
#'
#' @param code_counts A tibble or data frame output from \code{count_codes()}
#'   containing columns \code{code}, \code{count}, and \code{n_media_titles}.
#' @param total_media_titles Optional. Total number of media titles in the dataset.
#'   If \code{NULL}, defaults to the maximum of \code{n_media_titles}.
#' @param min_count Minimum number of excerpts for a code to be included
#'   (default = 1).
#' @param min_prop_media_titles Optional. Minimum proportion of media titles in which
#'   a code must appear. If \code{NULL}, no proportion filter is applied.
#' @param sort_by Whether to sort codes by \code{"count"} or by
#'   \code{"prop_media_titles"} (default = \code{"count"}).
#'
#' @return A \code{ggplot2} object showing a horizontal bar plot of code counts,
#'   with fill color corresponding to the proportion of media titles.
#'
#' @details
#' This function applies the same filtering logic as \code{set_saturation()}
#' to identify codes that meet minimum thresholds for frequency and coverage.
#' It then plots the results with \code{ggplot2}.
#'
#' @examples
#' \dontrun{
#' library(readxl)
#' library(dplyr)
#' library(tidyverse)
#'
#' # Load excerpts and clean
#' excerpts <- read_xlsx("inst/raw_data/test_data.xlsx")
#' preferred_coders <- c("a", "l", "i", "r", "s", "v", "c", "n", "k")
#' excerpts <- clean_data(excerpts, preferred_coders)
#'
#' # Count codes
#' code_counts <- count_codes(excerpts, min_count = 10, output = "tibble")
#'
#' # Plot saturation with thresholds
#' plot_saturation(code_counts, min_count = 10, min_prop_media_titles = 0.25)
#' }
#'
#' @export
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
