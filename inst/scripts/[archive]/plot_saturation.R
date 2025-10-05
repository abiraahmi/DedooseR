#' Plot Code Saturation
#'
#' `plot_saturation()` creates a bar chart of codes that meet saturation
#' thresholds defined by \code{set_saturation()}. Bars represent code counts,
#' and the fill color indicates the proportion of media titles in which each
#' code appears.
#'
#' @param saturation_df A tibble returned by \code{set_saturation()} containing
#'   columns \code{code}, \code{count}, and \code{prop_media_titles}.
#' @param sort_by Whether to sort codes by \code{"count"} or by
#'   \code{"prop_media_titles"} (default = \code{"count"}).
#'
#' @return A \code{ggplot2} object showing a horizontal bar plot of code counts,
#'   with fill color corresponding to the proportion of media titles.
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
#' # Apply saturation filter
#' saturation <- set_saturation(code_counts, min_count = 10, min_prop_media_titles = 0.25)
#'
#' # Plot results
#' plot_saturation(saturation, sort_by = "count")
#' }
#'
#' @export
plot_saturation <- function(saturation_df,
                            sort_by = c("count", "prop_media_titles")) {

  sort_by <- match.arg(sort_by)

  df <- saturation_df

  # Reorder factor levels for plotting
  if (sort_by == "count") {
    df <- df %>% dplyr::arrange(count)
    df$code <- factor(df$code, levels = df$code)
  } else {
    df <- df %>% dplyr::arrange(prop_media_titles)
    df$code <- factor(df$code, levels = df$code)
  }

  # Plot
  ggplot(df, aes(x = code, y = count, fill = prop_media_titles)) +
    geom_col() +
    coord_flip() +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    labs(
      x = "Code",
      y = "Count",
      title = "Code Saturation Plot")
}
