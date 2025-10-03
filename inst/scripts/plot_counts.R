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
