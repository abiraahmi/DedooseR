#' Plot saturation comparison across threshold sets
#'
#' This function plots the saturation of codes across multiple threshold sets.
#' Codes that do not meet the threshold in a given set are excluded (not plotted).
#'
#' @param compare_results A data frame with at least `code`, `count`, and one or
#'   more logical columns indicating whether the code meets each threshold set.
#' @param thresholds_list A named list of thresholds used to generate
#'   `compare_results`. The names will be used as facet labels.
#'
#' @return A ggplot object showing code saturation by threshold set.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#'
#' compare_results <- tibble::tibble(
#'   code = c("code_a", "code_b", "code_c"),
#'   count = c(10, 5, 2),
#'   Set1 = c(TRUE, TRUE, FALSE),
#'   Set2 = c(FALSE, TRUE, TRUE)
#' )
#'
#' thresholds_list <- list(Set1 = 3, Set2 = 5)
#'
#' plot_compare_saturation(compare_results, thresholds_list)
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
