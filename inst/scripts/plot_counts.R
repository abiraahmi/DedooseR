plot_counts <- function(summary_data,
                        plot_proportion = FALSE,
                        min_count = NULL,
                        min_proportion = NULL,
                        exclude_codes = NULL) {
  # Ensure it's a tibble or data frame
  if (!tibble::is_tibble(summary_data)) {
    summary_data <- as.data.frame(summary_data)
  }

  # Sanity check: required columns
  if (!"Code" %in% colnames(summary_data) || !"total_preferred_coder" %in% colnames(summary_data)) {
    stop("`summary_data` must contain columns `Code` and `total_preferred_coder`.")
  }

  # Exclude certain codes
  if (!is.null(exclude_codes)) {
    summary_data <- summary_data %>% dplyr::filter(!Code %in% exclude_codes)
  }

  # Filter by count
  if (!is.null(min_count)) {
    summary_data <- summary_data %>% dplyr::filter(total_preferred_coder >= min_count)
  }

  # Filter by proportion
  if (!is.null(min_proportion)) {
    total <- sum(summary_data$total_preferred_coder, na.rm = TRUE)
    summary_data <- summary_data %>%
      dplyr::mutate(Proportion = total_preferred_coder / total) %>%
      dplyr::filter(Proportion >= min_proportion)
  }

  # Plot
  if (plot_proportion) {
    if (!"Proportion" %in% names(summary_data)) {
      total <- sum(summary_data$total_preferred_coder, na.rm = TRUE)
      summary_data <- summary_data %>%
        dplyr::mutate(Proportion = total_preferred_coder / total)
    }

    p <- ggplot2::ggplot(summary_data,
                         ggplot2::aes(x = reorder(Code, Proportion), y = Proportion)) +
      ggplot2::geom_col(fill = "#330662") +
      ggplot2::coord_flip() +
      ggplot2::labs(title = "Code Frequencies (Proportion)",
                    x = "Code", y = "Proportion")
  } else {
    p <- ggplot2::ggplot(summary_data,
                         ggplot2::aes(x = reorder(Code, total_preferred_coder), y = total_preferred_coder)) +
      ggplot2::geom_col(fill = "#330662") +
      ggplot2::coord_flip() +
      ggplot2::labs(title = "Code Frequencies (Raw Count)",
                    x = "Code", y = "Count")
  }

  return(p)
}
