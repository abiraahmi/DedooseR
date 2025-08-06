library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)  # for color manipulation

plot_saturation <- function(df_all_summary, df_qual_summary,
                            qual_indicators = NULL,
                            min_counts = NULL,
                            stacked = TRUE,
                            as_proportion = FALSE # if TRUE, plots proportions
) {
  # Validate inputs
  if (missing(df_all_summary) || missing(df_qual_summary)) {
    stop("Please provide both df_all_summary and df_qual_summary data frames.")
  }
  if (is.null(qual_indicators) || length(qual_indicators) == 0) {
    stop("Please provide a character vector of quality indicator names.")
  }

  # Confirm all required columns exist in df_qual_summary
  count_cols <- qual_indicators
  missing_cols <- setdiff(count_cols, colnames(df_qual_summary))
  if (length(missing_cols) > 0) {
    stop("Missing quality indicator columns: ",
         paste(missing_cols, collapse = ", "))
  }

  # Join total_preferred_coder counts from df_all_summary to df_qual_summary by Code
  plot_df <- df_qual_summary %>%
    left_join(df_all_summary %>% select(Code, total_preferred_coder), by = "Code")

  # Pivot longer by quality indicators for plotting
  plot_long <- plot_df %>%
    select(Code, total_preferred_coder, all_of(count_cols)) %>%
    pivot_longer(cols = all_of(count_cols),
                 names_to = "QualityIndicator",
                 values_to = "Count")

  # Apply minimum count filtering if min_counts provided
  if (!is.null(min_counts)) {
    if (!all(names(min_counts) %in% qual_indicators)) {
      stop("All names of min_counts must be in qual_indicators.")
    }
    plot_long <- plot_long %>%
      filter(!(QualityIndicator %in% names(min_counts) &
                 Count < min_counts[QualityIndicator]))
  }

  # Keep only codes that have all quality indicators with Count > 0
  codes_with_all_qual_indicators <- plot_long %>%
    group_by(Code) %>%
    summarize(
      all_present = all(qual_indicators %in% QualityIndicator[Count > 0]),
      .groups = "drop"
    ) %>%
    filter(all_present) %>%
    pull(Code)

  plot_long <- plot_long %>%
    filter(Code %in% codes_with_all_qual_indicators)

  # If as_proportion TRUE, convert counts per code to proportions
  if (as_proportion) {
    plot_long <- plot_long %>%
      group_by(Code) %>%
      mutate(TotalCount = sum(Count),
             Proportion = ifelse(TotalCount > 0, Count / TotalCount, 0)) %>%
      ungroup()
  }

  # Order codes by total_preferred_coder descending
  remaining_codes <- plot_long %>%
    distinct(Code, total_preferred_coder) %>%
    arrange(desc(total_preferred_coder)) %>%
    pull(Code)
  plot_long$Code <- factor(plot_long$Code, levels = remaining_codes)

  # Generate discrete gradient palette from #330662
  generate_palette <- function(n) {
    base_col <- "#31e2d8"
    scales::gradient_n_pal(c(base_col, "#330662"))(seq(0, 1, length.out = n))
  }
  n_colors <- length(unique(plot_long$QualityIndicator))
  palette <- generate_palette(n_colors)

  # Plot
  p <- ggplot(plot_long, aes(x = Code, fill = QualityIndicator)) +
    labs(
      title = "Code Counts by Quality Indicators",
      x = "Code",
      fill = "Quality Indicator"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_flip() +
    scale_fill_manual(values = palette)

  if (as_proportion) {
    # Use Proportion for y and set y-axis label accordingly
    p <- p +
      geom_bar(aes(y = Proportion),
               stat = "identity",
               position = ifelse(stacked, "stack", "dodge")) +
      ylab("Proportion of Total Counts")
  } else {
    # Use Count for y
    p <- p +
      geom_bar(aes(y = Count), stat = "identity",
               position = ifelse(stacked, "stack", "dodge")) +
      ylab("Count")
  }

  return(p)
}
