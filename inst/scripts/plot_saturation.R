

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

  # Step 2: Calculate total counts per Code (sum Priority + Heterogeneity)
  total_counts <- long_codes_melted %>%
    dplyr::group_by(Code) %>%
    dplyr::summarise(Total = sum(Count), .groups = "drop")

  # Step 3: Join totals back and calculate proportion per Type
  long_codes_prop <- long_codes_melted %>%
    dplyr::left_join(total_counts, by = "Code") %>%
    dplyr::mutate(Proportion = Count / Total)

  # Step 4: Plot proportions instead of counts
  p <- ggplot(long_codes_prop, aes(x = reorder(Code, Proportion), y = Proportion, fill = Type)) +
    geom_bar(stat = "identity", position = "stack") +
    theme_minimal() +
    coord_flip() +
    labs(
      title = "Priority and Heterogeneity Proportions by Code",
      x = "Code",
      y = "Proportion",
      fill = "Type"
    ) +
    scale_fill_manual(values = c("Priority_Count" = "#330662",
                                 "Heterogeneity_Count" = "#3CBBB1")) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  print(p)
  invisible(p)
}



library(DedooseR)
library(tidyverse)
library(dplyr)
library(readxl)

raw_data <- read_xlsx("inst/raw_data/test_data_manipulated.xlsx")

# Summarize codes
excerpts <- read_xlsx("inst/raw_data/test_data_manipulated.xlsx")
preferred_coders <- c("s", "r", "l", "a")
summarize_codes(excerpts, preferred_coders, output_type = "datatable")


# Plot saturation by qual indicators
plot_saturation(excerpts)


