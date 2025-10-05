#' Plot code frequencies
#'
#' This function takes the output of [count_codes()] and produces a bar plot of
#' code frequencies using `ggplot2`. It supports excluding specific codes,
#' filtering by absolute count, or filtering by proportion of the maximum count.
#' The user can choose whether to plot the total excerpt frequency (`count`) or
#' the number of distinct media titles containing each code (`n_media_titles`).
#'
#' @param code_counts A tibble or data frame output from [count_codes()]. Must
#'   contain at least the columns `code`, `count`, and `n_media_titles`.
#' @param exclude_codes A character vector of code names to exclude from the plot.
#'   Defaults to `NULL` (no exclusions).
#' @param min_count An integer specifying the minimum absolute count for a code
#'   to be included. Defaults to `NULL`.
#' @param min_prop A numeric value between 0 and 1 specifying the minimum
#'   proportion (relative to the maximum of the chosen metric) required for a
#'   code to be included. Defaults to `NULL`.
#' @param metric Which metric to plot. One of `"count"` (excerpt frequency) or
#'   `"n_media_titles"` (media title coverage). Defaults to `"count"`.
#' @param fill_color A character string giving the fill color for the bars.
#'   Defaults to `"steelblue"`.
#'
#' @return A `ggplot` object showing code frequencies.
#'
#' @details
#' - Codes can be filtered either by absolute count (`min_count`) or by proportion
#'   of the maximum (`min_prop`). If both are supplied, *both filters are applied*.
#' - Codes listed in `exclude_codes` are removed prior to filtering.
#' - The `metric` argument controls whether the y-axis shows excerpt frequency or
#'   number of unique media titles containing the code.
#'
#' @examples
#' # Example data
#' df <- data.frame(
#'   media_title = c("Doc1", "Doc1", "Doc2", "Doc2", "Doc3"),
#'   c_theme1 = c(TRUE, FALSE, TRUE, FALSE, TRUE),
#'   c_theme2 = c(FALSE, TRUE, TRUE, TRUE, FALSE),
#'   c_theme3 = c(FALSE, FALSE, FALSE, TRUE, TRUE)
#' )
#'
#' code_counts <- count_codes(df)
#'
#' # Plot excerpt frequencies
#' plot_counts(code_counts)
#'
#' # Plot by media-title coverage
#' plot_counts(code_counts, metric = "n_media_titles")
#'
#' # Exclude one code
#' plot_counts(code_counts, exclude_codes = "c_theme2")
#'
#' # Filter by absolute count
#' plot_counts(code_counts, min_count = 2)
#'
#' # Filter by proportion (keep codes at least 50% of max)
#' plot_counts(code_counts, min_prop = 0.5)
#'
#' @importFrom dplyr filter arrange
#' @importFrom ggplot2 ggplot aes geom_col coord_flip labs theme_minimal
#' @export
plot_counts <- function(code_counts,
                        exclude_codes = NULL,
                        min_count = NULL,
                        min_prop = NULL,
                        metric = c("count", "n_media_titles"),
                        fill_color = "steelblue") {
  metric <- match.arg(metric)

  # Check input
  if (!is.data.frame(code_counts)) {
    stop("`code_counts` must be a tibble or data frame (from count_codes()).")
  }
  if (!all(c("code", "count", "n_media_titles") %in% names(code_counts))) {
    stop("`code_counts` must contain columns `code`, `count`, and `n_media_titles`.")
  }

  df <- code_counts

  # Exclude codes
  if (!is.null(exclude_codes)) {
    df <- dplyr::filter(df, !(code %in% exclude_codes))
  }

  # Filter by count (uses chosen metric)
  if (!is.null(min_count)) {
    df <- dplyr::filter(df, .data[[metric]] >= min_count)
  }

  # Filter by proportion of max (uses chosen metric)
  if (!is.null(min_prop)) {
    max_val <- max(df[[metric]], na.rm = TRUE)
    df <- dplyr::filter(df, .data[[metric]] >= min_prop * max_val)
  }

  # Arrange for plotting (lowest to highest, so bars flip nicely)
  df <- dplyr::arrange(df, .data[[metric]])

  # Plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = reorder(code, .data[[metric]]),
                                        y = .data[[metric]])) +
    ggplot2::geom_col(fill = fill_color) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = "Code",
      y = ifelse(metric == "count", "Excerpt Frequency", "Media Title Coverage"),
      title = paste("Code Frequencies by", ifelse(metric == "count", "Excerpts", "Media Titles"))
    ) +
    ggplot2::theme_minimal()

  return(p)
}
