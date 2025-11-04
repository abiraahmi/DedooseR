compare_saturation <- function(code_summary,
                               thresholds_list,
                               output_type = c("tibble", "kable"),
                               plot = FALSE,
                               plot_metric = c("count", "prop", "both")) {
  output_type <- match.arg(output_type)
  plot_metric <- match.arg(plot_metric)

  # --- Validate inputs ---
  required_cols <- c("code", "count", "n_media_titles", "prop_media_titles")
  missing_cols <- setdiff(required_cols, names(code_summary))
  if (length(missing_cols) > 0) {
    stop("`code_summary` must contain the following columns: ",
         paste(missing_cols, collapse = ", "))
  }

  # --- Apply thresholds ---
  compare_df <- code_summary
  for (set_name in names(thresholds_list)) {
    rules <- thresholds_list[[set_name]]
    compare_df[[set_name]] <-
      compare_df$count >= rules$code_count &
      compare_df$prop_media_titles >= rules$prop_media_title
  }

  # --- Output table ---
  if (output_type == "kable") {
    comparison_output <- knitr::kable(compare_df,
                                      caption = "Saturation Comparison")
  } else {
    comparison_output <- compare_df
  }

  # --- Optional plotting ---
  if (plot) {
    set_cols <- names(thresholds_list)

    plot_data <- compare_df %>%
      dplyr::select(code, count, prop_media_titles, dplyr::all_of(set_cols)) %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(set_cols),
        names_to = "Set",
        values_to = "Meets"
      ) %>%
      dplyr::filter(Meets == TRUE)

    # ---- Choose metric ----
    if (plot_metric == "count") {
      p <- ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = reorder(code, count),
                     y = count,
                     fill = Set)
      ) +
        ggplot2::geom_col() +
        ggplot2::coord_flip() +
        ggplot2::facet_wrap(~ Set, ncol = 1, scales = "free_y") +
        ggplot2::labs(
          x = "Code",
          y = "Excerpt Frequency",
          title = "Code Saturation Comparison by Threshold Set (Counts)"
        ) +
        ggplot2::theme_minimal()

    } else if (plot_metric == "prop") {
      p <- ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = reorder(code, prop_media_titles),
                     y = prop_media_titles,
                     fill = Set)
      ) +
        ggplot2::geom_col() +
        ggplot2::coord_flip() +
        ggplot2::facet_wrap(~ Set, ncol = 1, scales = "free_y") +
        ggplot2::labs(
          x = "Code",
          y = "Proportion of Media Titles",
          title = "Code Saturation Comparison by Threshold Set (Proportions)"
        ) +
        ggplot2::theme_minimal()

    } else if (plot_metric == "both") {
      scale_factor <- max(plot_data$count, na.rm = TRUE) /
        max(plot_data$prop_media_titles, na.rm = TRUE)

      p <- ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = reorder(code, count),
                     y = count,
                     fill = Set)
      ) +
        ggplot2::geom_col() +
        ggplot2::coord_flip() +
        ggplot2::facet_wrap(~ Set, ncol = 1, scales = "free_y") +
        ggplot2::scale_y_continuous(
          name = "Excerpt Frequency",
          sec.axis = ggplot2::sec_axis(~ . / scale_factor,
                                       name = "Proportion of Media Titles")
        ) +
        ggplot2::labs(
          x = "Code",
          title = "Code Saturation Comparison by Threshold Set (Counts + Proportions)"
        ) +
        ggplot2::theme_minimal()
    }

    return(list(results = comparison_output, plot = p))
  }

  return(comparison_output)
}


# Test

# Load libraries
library(DedooseR)
library(tidyverse)
library(dplyr)
library(readxl)

# Clean data
filepath <- read_xlsx("inst/raw_data/test_data.xlsx")
preferred_coders <- c("a", "l", "i", "r", "s", "v", "c", "n", "k")
df <- clean_data(filepath,
                 preferred_coders,
                 rename_vars = list(memo_destigmatization = "...274"),
                 relabel_vars = list(title = "Memo: Destigmatization"))
data <- df$data
codebook <- df$codebook

# Recode themes
excerpts_recoded <- recode_themes(data,
                               recodes = list(
                                 c_belonging_connectedness = c(
                                   "c_sense_of_belonging", "c_sense_of_belonging_others", "c_sense_of_belonging_self",
                                   "c_sense_of_connectedness", "c_sense_of_connectedness_family",
                                   "c_sense_of_connectedness_peers", "c_sense_of_connectedness_school_community",
                                   "c_sense_of_connectedness_staff"
                                 ),
                                 c_suicide_comfort = c("c__suicide_comfort_directing_change", "c__suicide_comfort_general")
                               ),
                               relabel_vars = list(
                                 c_belonging_connectedness = "Sense of Belonging & Connectedness",
                                 c_suicide_comfort = "Suicide Comfort Conversing"
                               ))

data_recode <- excerpts_recoded$data_recode
codebook_recode <- excerpts_recoded$codebook_recode

# Code summary
code_summary <- create_code_summary(data_recode, output_type = "tibble",
                                    exclude = c("c_priority_excerpt"))

# Compare saturation
thresholds_list <- list(
  "Liberal" = list(code_count = 40, prop_media_title = 0.2),
  "Strict"  = list(code_count = 60, prop_media_title = 0.6)
)

# Compare against thresholds
saturation_comparison <- compare_saturation(code_summary, thresholds_list,
                                            plot = TRUE,
                                            plot_metric = "both")

# View table
saturation_comparison$results

# View plot
saturation_comparison$plot
