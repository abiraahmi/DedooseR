#' Set Saturation Thresholds and Plot Saturation Tracking Bar Chart
#'
#' This function filters the saturation tracking data based on user-defined
#' minimum counts for Priority and Heterogeneity codes, then plots a stacked bar chart
#' showing the filtered codes meeting these thresholds.
#'
#' @param long_codes A data frame output from \code{create_saturation_tracking()} containing
#'   columns \code{Code}, \code{Priority_Count}, and \code{Heterogeneity_Count}.
#' @param min_priority Integer. Minimum count threshold for Priority codes to include. Default is 1.
#' @param min_heterogeneity Integer. Minimum count threshold for Heterogeneity codes to include. Default is 1.
#'
#' @return A ggplot object showing a stacked bar chart of Priority and Heterogeneity code counts for codes meeting the thresholds.
#'
#' @examples
#' \dontrun{
#' long_codes <- create_saturation_tracking("path/to/data.xlsx", c("coder1", "coder2"))
#' set_saturation(long_codes, min_priority = 3, min_heterogeneity = 2)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_bar coord_flip labs scale_fill_manual theme theme_minimal element_text
#' @importFrom dplyr filter select
#' @importFrom tidyr pivot_longer
#'
#' @export
set_saturation <- function(long_codes, min_priority = 3, min_heterogeneity = 3, plot = TRUE) {
  # Check input validity
  if (is.null(long_codes) || !is.data.frame(long_codes)) {
    stop("Please provide a valid data frame from create_saturation_tracking().")
  }

  # Filter codes meeting both minimum thresholds
  filtered_codes <- long_codes %>%
    dplyr::filter(Priority_Count >= min_priority & Heterogeneity_Count >= min_heterogeneity)

  if (nrow(filtered_codes) == 0) {
    warning("No codes meet the minimum count thresholds.")
    return(invisible(NULL))
  }

  if (plot) {
    # Reshape data for plotting (long format)
    filtered_melted <- filtered_codes %>%
      dplyr::select(Code, Priority_Count, Heterogeneity_Count) %>%
      tidyr::pivot_longer(cols = c(Priority_Count, Heterogeneity_Count),
                          names_to = "Type",
                          values_to = "Count") %>%
      dplyr::filter(Count > 0)

    # Generate plot
    p <- ggplot2::ggplot(filtered_melted, ggplot2::aes(x = reorder(Code, Count), y = Count, fill = Type)) +
      ggplot2::geom_bar(stat = "identity", position = "stack") +
      ggplot2::theme_minimal() +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = paste("Codes with Priority >=", min_priority, "and Heterogeneity >=", min_heterogeneity),
        x = "Code",
        y = "Count",
        fill = "Type"
      ) +
      ggplot2::scale_fill_manual(values = c("Priority_Count" = "#330662",
                                            "Heterogeneity_Count" = "#3CBBB1")) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

    print(p)
    invisible(p)

  } else {
    # Return filtered codes in wide format with counts as columns
    return(filtered_codes %>%
             dplyr::select(Code, Priority_Count, Heterogeneity_Count))
  }
}


