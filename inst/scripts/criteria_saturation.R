
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



