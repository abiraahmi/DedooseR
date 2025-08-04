#' Plot Saturation Tracking Bar Chart
#'
#' @param long_codes A data frame output from create_saturation_tracking()
#' @return A ggplot object showing stacked bar chart of Priority and
#' Heterogeneity code counts
#' @examples
#' @importFrom ggplot2 ggplot aes geom_bar coord_flip labs scale_fill_manual theme theme_minimal element_text
#' long_codes <- create_saturation_tracking("path/to/your/data.xlsx",
#' c("coder1", "coder2"))
#' plot_saturation(long_codes)
#'
plot_saturation <- function(long_codes) {
  # Check input
  if (is.null(long_codes) || !is.data.frame(long_codes)) {
    stop("Please provide a valid data frame from create_saturation_tracking().")
  }

  # Step 1: Reshape the data
  long_codes_melted <- long_codes %>%
    dplyr::select(Code, Priority_Count, Heterogeneity_Count) %>%
    tidyr::pivot_longer(cols = c(Priority_Count, Heterogeneity_Count),
                 names_to = "Type",
                 values_to = "Count") %>%
    dplyr::filter(Count > 0)

  # Step 2: Create the bar chart
  p <- ggplot2::ggplot(long_codes_melted, aes(x = reorder(Code, Count), y = Count,
                                     fill = Type)) +
    ggplot2::geom_bar(stat = "identity", position = "stack") +
    ggplot2:: theme_minimal() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Priority and Heterogeneity Counts",
      x = "Code",
      y = "Count",
      fill = "Type"
    ) +
    ggplot2::cale_fill_manual(values = c("Priority_Count" = "#330662",
                                 "Heterogeneity_Count" = "#3CBBB1")) +
    ggplot2::theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  print(p)
  invisible(p)
}

