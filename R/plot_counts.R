#' Plot Code Frequencies from Summarized Coding Data
#'
#' Creates a horizontal bar plot of code frequencies based on the output of `summarize_codes()`.
#' Users can optionally plot proportions instead of raw counts, filter by minimum frequency,
#' or exclude specific codes.
#'
#' @param summary_data A data frame or tibble containing at least two columns: `Code` and `total_preferred_coder`,
#' typically the output from `summarize_codes(output_type = "tibble")`.
#' @param plot_proportion Logical. If `TRUE`, plots code frequencies as proportions of the total count.
#' Defaults to `FALSE` (raw counts).
#' @param min_count Integer. Optional. Filters out codes with fewer than this number of occurrences.
#' @param min_proportion Numeric. Optional. Filters out codes with a proportion less than this value (e.g., 0.02).
#' @param exclude_codes Character vector. Optional. Codes to exclude from the plot (e.g., c("Other", "Heterogeneity")).
#'
#' @return A `ggplot2` object showing a horizontal bar plot of code frequencies or proportions.
#'
#' @examples
#' \dontrun{
#' summary_data <- summarize_codes(my_excerpts, preferred_coders, output_type = "tibble")
#' plot_counts(summary_data, plot_proportion = TRUE, min_proportion = 0.02)
#' plot_counts(summary_data, min_count = 30, exclude_codes = c("Other"))
#' }
#'
#' @importFrom dplyr filter mutate select %>%
#' @importFrom ggplot2 ggplot aes geom_col coord_flip labs
#' @export

plot_counts <- function(df_all_summary,
                        plot_proportion = FALSE,
                        min_count = NULL,
                        min_proportion = NULL,
                        exclude_codes = NULL) {
  # Ensure it's a tibble or data frame
  if (!tibble::is_tibble(df_all_summary)) {
    df_all_summary <- as.data.frame(df_all_summary)
  }

  # Sanity check: required columns
  if (!"Code" %in% colnames(df_all_summary) || !"total_preferred_coder" %in% colnames(df_all_summary)) {
    stop("`df_all_summary` must contain columns `Code` and `total_preferred_coder`.")
  }

  # Exclude certain codes
  if (!is.null(exclude_codes)) {
    df_all_summary <- df_all_summary %>% dplyr::filter(!Code %in% exclude_codes)
  }

  # Filter by count
  if (!is.null(min_count)) {
    df_all_summary <- df_all_summary %>% dplyr::filter(total_preferred_coder >= min_count)
  }

  # Filter by proportion
  if (!is.null(min_proportion)) {
    total <- sum(df_all_summary$total_preferred_coder, na.rm = TRUE)
    df_all_summary <- df_all_summary %>%
      dplyr::mutate(Proportion = total_preferred_coder / total) %>%
      dplyr::filter(Proportion >= min_proportion)
  }

  # Plot
  if (plot_proportion) {
    if (!"Proportion" %in% names(df_all_summary)) {
      total <- sum(df_all_summary$total_preferred_coder, na.rm = TRUE)
      df_all_summary <- df_all_summary %>%
        dplyr::mutate(Proportion = total_preferred_coder / total)
    }

    p <- ggplot2::ggplot(df_all_summary,
                         ggplot2::aes(x = reorder(Code, Proportion), y = Proportion)) +
      ggplot2::geom_col(fill = "#330662") +
      ggplot2::coord_flip() +
      ggplot2::labs(title = "Code Frequencies (Proportion)",
                    x = "Code", y = "Proportion")
  } else {
    p <- ggplot2::ggplot(df_all_summary,
                         ggplot2::aes(x = reorder(Code, total_preferred_coder), y = total_preferred_coder)) +
      ggplot2::geom_col(fill = "#330662") +
      ggplot2::coord_flip() +
      ggplot2::labs(title = "Code Frequencies (Raw Count)",
                    x = "Code", y = "Count")
  }

  return(p)
}
