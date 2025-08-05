#' Plot Code Saturation by Quality Indicators
#'
#' Creates a horizontal stacked or dodged bar plot visualizing counts or
#' proportions of quality indicator annotations per code.
#' Only codes that have *all* specified quality indicators present at least
#' once (count > 0) are shown.
#'
#' @param df_all_summary A data frame (tibble) summarizing codes, must contain at
#' least `Code` and `total_preferred_coder` columns.
#' @param df_qual_summary A data frame (tibble) containing quality indicator counts
#' per code. Must have columns matching `paste0(qual_indicators, "_Count")` and
#' a `Code` column.
#' @param qual_indicators A character vector of quality indicator names to plot
#' (e.g., `c("Priority excerpt", "Heterogeniety")`). These determine which
#' columns to use and filter on.
#' @param min_counts Optional named numeric vector specifying minimum counts for
#'  each quality indicator to include a code
#'  (e.g., `c("Priority excerpt" = 20, "Heterogeniety" = 30)`). Codes with
#'  counts below these thresholds for the respective quality indicators will be
#'  excluded.
#' @param stacked Logical; if `TRUE` (default), bars for quality indicators will
#'  be stacked; if `FALSE`, bars will be dodged (side-by-side).
#' @param as_proportion Logical; if `TRUE`, the y-axis will represent
#' proportions of counts per code rather than raw counts.
#'
#' @return A `ggplot` object displaying the counts or proportions of quality
#' indicator annotations by code.
#'
#' @details
#' - The function filters to only display codes that have counts greater than
#' zero for *all* specified quality indicators.
#' - The plot orders codes by descending total counts from
#' `total_preferred_coder`.
#' - Colors are generated with a discrete gradient palette for visual clarity.
#' - Input data frames should be outputs from `summarize_codes()` and
#' `quality_indicators()` functions or have equivalent structure.
#'
#' @examples
#' \dontrun{
#' summary_data <- summarize_codes(excerpts, preferred_coders,
#' output_type = "tibble")
#' quality_data <- quality_indicators(excerpts, preferred_coders,
#' qual_indicators = c("Priority excerpt", "Heterogeniety"))
#'
#' plot_saturation(
#'   summary_data,
#'   quality_data,
#'   qual_indicators = c("Priority excerpt", "Heterogeniety"),
#'   min_counts = c("Priority excerpt" = 3, "Heterogeniety" = 3),
#'   stacked = TRUE,
#'   as_proportion = FALSE
#' )
#' }
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @importFrom scales gradient_n_pal
#' @export

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
  count_cols <- paste0(qual_indicators, "_Count")
  missing_cols <- setdiff(count_cols, colnames(df_qual_summary))
  if (length(missing_cols) > 0) {
    stop("Missing quality indicator count columns: ",
         paste(missing_cols, collapse = ", "))
  }

  # Join total_preferred_coder counts from df_all_summary to df_qual_summary by
  # Code
  plot_df <- df_qual_summary %>%
    left_join(df_all_summary %>% select(Code, total_preferred_coder), by =
                "Code")

  # Pivot longer by quality indicators for plotting
  plot_long <- plot_df %>%
    select(Code, total_preferred_coder, all_of(count_cols)) %>%
    pivot_longer(cols = all_of(count_cols),
                 names_to = "QualityIndicator",
                 values_to = "Count") %>%
    mutate(
      QualityIndicator = sub("_Count$", "", QualityIndicator)
    )

  # Apply minimum count filtering if min_counts provided
  if (!is.null(min_counts)) {
    if (!all(names(min_counts) %in% qual_indicators)) {
      stop("All names of min_counts must be in qual_indicators.")
    }
    plot_long <- plot_long %>%
      filter(!(QualityIndicator %in% names(min_counts) &
                 Count < min_counts[QualityIndicator]))
  }

  # Keep only codes that have all quality indicators with Count > 0 ***
  codes_with_all_qual_indicators <- plot_long %>%
    group_by(Code) %>%
    summarize(
      all_present = all(qual_indicators %in% QualityIndicator[Count > 0])
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
