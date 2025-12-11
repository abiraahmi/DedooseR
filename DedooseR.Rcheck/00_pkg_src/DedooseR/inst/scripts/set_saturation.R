set_saturation <- function(code_counts,
                           total_media_titles = NULL,
                           table_min_count = 1,
                           table_min_prop = NULL,
                           output_type = c("tibble", "kable"),
                           plot = FALSE,
                           plot_min_count = NULL,
                           plot_min_prop = NULL,
                           plot_metric = c("prop", "count", "both"),
                           fill_color = "steelblue") {
  output_type <- match.arg(output_type)
  plot_metric <- match.arg(plot_metric)

  if (is.list(code_counts) && "table" %in% names(code_counts)) {
    code_counts <- code_counts$table
  }

  if (!is.data.frame(code_counts)) {
    stop("`code_counts` must be a tibble or data frame (from create_code_summary()).")
  }
  if (!all(c("code", "count", "n_media_titles") %in% names(code_counts))) {
    stop("`code_counts` must contain columns `code`, `count`, and `n_media_titles`.")
  }

  if (is.null(total_media_titles)) {
    total_media_titles <- max(code_counts$n_media_titles, na.rm = TRUE)
  }

  # --- Compute proportions and filter for table ---
  df <- code_counts %>%
    dplyr::filter(count >= table_min_count) %>%
    dplyr::mutate(
      prop_media_titles = round(n_media_titles / total_media_titles, 2)
    ) %>%
    dplyr::select("code", "count", "prop_media_titles")

  if (!is.null(table_min_prop)) {
    df <- df %>% dplyr::filter(prop_media_titles >= table_min_prop)
  }

  df <- df %>% dplyr::arrange(dplyr::desc(count))

  caption_text <- paste(
    "Code Counts with Transcript Proportions (table_min_count =", table_min_count,
    ", table_min_prop =",
    ifelse(is.null(table_min_prop), "none", table_min_prop),
    ")"
  )

  table_out <- if (output_type == "kable") {
    knitr::kable(df, caption = caption_text, digits = 2)
  } else {
    df
  }

  # --- Plot output ---
  if (plot) {
    if (is.null(plot_min_count)) plot_min_count <- table_min_count
    if (is.null(plot_min_prop)) plot_min_prop <- table_min_prop

    plot_df <- df %>% dplyr::filter(count >= plot_min_count)
    if (!is.null(plot_min_prop)) {
      plot_df <- plot_df %>% dplyr::filter(prop_media_titles >= plot_min_prop)
    }

    plot_df <- plot_df %>% dplyr::arrange(prop_media_titles)

    if (plot_metric == "prop") {
      p <- ggplot2::ggplot(plot_df, ggplot2::aes(
        x = reorder(code, prop_media_titles),
        y = prop_media_titles
      )) +
        ggplot2::geom_col(fill = fill_color) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          x = "Code",
          y = "Proportion of Media Titles",
          title = "Proportion of Media Titles"
        ) +
        ggplot2::theme_minimal()

    } else if (plot_metric == "count") {
      p <- ggplot2::ggplot(plot_df, ggplot2::aes(
        x = reorder(code, count),
        y = count
      )) +
        ggplot2::geom_col(fill = fill_color) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          x = "Code",
          y = "Excerpt Frequency",
          title = "Code Counts"
        ) +
        ggplot2::theme_minimal()

    } else if (plot_metric == "both") {
      scale_factor <- max(plot_df$count, na.rm = TRUE) / max(plot_df$prop_media_titles, na.rm = TRUE)

      p <- ggplot2::ggplot(plot_df, ggplot2::aes(
        x = reorder(code, count),
        y = count
      )) +
        ggplot2::geom_col(fill = fill_color) +
        ggplot2::coord_flip() +
        ggplot2::scale_y_continuous(
          name = "Excerpt Frequency",
          sec.axis = ggplot2::sec_axis(~ . / scale_factor,
                                       name = "Proportion of Media Titles")
        ) +
        ggplot2::labs(
          x = "Code",
          title = "Code Saturation"
        ) +
        ggplot2::theme_minimal()
    }

    return(list(table = df, plot = p))
  }

  return(table_out)
}


# test_script.R

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


# Create code summary
create_code_summary <- create_code_summary(data_recode,
                                           table_min_count = 40,
                                           plot = TRUE,
                                           output_type = "tibble")


# Plot both metrics on the same graph
out <- set_saturation(create_code_summary, table_min_count = 100, plot = TRUE, plot_metric = "count")
out$plot
