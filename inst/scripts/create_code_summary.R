create_code_summary <- function(
    excerpts,
    table_min_count = 1,
    table_min_prop = NULL,
    plot = FALSE,
    plot_min_count = NULL,
    plot_min_prop = NULL,
    output_type = c("tibble", "kable", "datatable"),
    exclude = NULL,
    plot_metric = c("count", "prop", "both"),
    fill_color = "steelblue"
) {
  output_type <- match.arg(output_type)
  plot_metric <- match.arg(plot_metric)

  # --- Validate inputs ---
  if (!is.data.frame(excerpts)) stop("`excerpts` must be a data frame.")
  if (!"media_title" %in% names(excerpts)) stop("`excerpts` must contain a `media_title` column.")

  # Identify logical columns (codes)
  code_columns <- colnames(excerpts)[vapply(excerpts, is.logical, logical(1))]

  if (!is.null(exclude)) {
    exclude <- intersect(exclude, code_columns)
    code_columns <- setdiff(code_columns, exclude)
  }
  if (length(code_columns) == 0) stop("No logical (code) columns found after exclusions.")

  # --- Create name → label lookup ---
  label_lookup <- purrr::map_chr(code_columns, function(x) {
    lbl <- attr(excerpts[[x]], "label")
    if (is.null(lbl) || lbl == "") x else lbl
  })
  names(label_lookup) <- code_columns

  # --- Summarize code frequencies ---
  total_counts <- excerpts %>%
    dplyr::select("media_title", dplyr::all_of(code_columns)) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(code_columns),
      names_to = "code",
      values_to = "applied"
    ) %>%
    dplyr::filter(applied == TRUE) %>%
    dplyr::group_by(code) %>%
    dplyr::summarise(
      count = dplyr::n(),
      n_media_titles = dplyr::n_distinct(media_title),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      prop_media_titles = round(n_media_titles / max(n_media_titles, na.rm = TRUE), 2),
      code_label = label_lookup[as.character(code)] |> unname()
    ) %>%
    dplyr::select("code" = "code_label", "count", "n_media_titles", "prop_media_titles")

  # --- Apply table filters ---
  if (!is.null(table_min_prop)) {
    max_val <- max(total_counts$count, na.rm = TRUE)
    total_counts <- dplyr::filter(total_counts, count >= table_min_prop * max_val)
  }
  total_counts <- dplyr::filter(total_counts, count >= table_min_count)

  # --- Output table ---
  caption_text <- paste("Total Code Counts (min_count =", table_min_count, ")")
  table_out <- switch(
    output_type,
    tibble = total_counts,
    kable = knitr::kable(total_counts, caption = caption_text),
    datatable = DT::datatable(
      total_counts,
      caption = caption_text,
      options = list(pageLength = 30, autoWidth = TRUE)
    )
  )

  # --- Plot section ---
  if (plot) {
    # Default plot mins to table mins if not provided
    if (is.null(plot_min_count)) plot_min_count <- table_min_count
    if (is.null(plot_min_prop)) plot_min_prop <- table_min_prop

    plot_df <- total_counts
    plot_df <- dplyr::filter(plot_df, count >= plot_min_count)

    if (!is.null(plot_min_prop)) {
      max_val <- max(plot_df$count, na.rm = TRUE)
      plot_df <- dplyr::filter(plot_df, count >= plot_min_prop * max_val)
    }

    plot_df <- dplyr::arrange(plot_df, dplyr::desc(count))

    # --- Plot by selected metric ---
    if (plot_metric == "count") {
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

    } else if (plot_metric == "prop") {
      p <- ggplot2::ggplot(plot_df, ggplot2::aes(
        x = reorder(code, prop_media_titles),
        y = prop_media_titles
      )) +
        ggplot2::geom_col(fill = fill_color) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          x = "Code",
          y = "Proportion of Media Titles",
          title = "Code Frequencies by Media Title Coverage"
        ) +
        ggplot2::theme_minimal()

    } else if (plot_metric == "both") {
      scale_factor <- max(plot_df$count, na.rm = TRUE) /
        max(plot_df$prop_media_titles, na.rm = TRUE)

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
          title = "Code Frequencies: Counts and Proportions"
        ) +
        ggplot2::theme_minimal()
    }

    return(list(table = total_counts, plot = p))
  }

  return(table_out)
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
clean_data <- clean_data(filepath,
                         preferred_coders,
                         rename_vars = list(memo_destigmatization = "...274"),
                         relabel_vars = list(title = "Memo: Destigmatization"))
data <- clean_data$data
codebook <- clean_data$codebook

# Merge codes
merge_codes <- merge_codes(data,
                           merges = list(
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

data_merged <- merge_codes$data
codebook_merged <- merge_codes$codebook

# Create code summary
create_code_summary <- create_code_summary(data_merged,
                                    table_min_count = 40,
                                    table_min_prop = 0.25,
                                    plot = TRUE,
                                    plot_metric = "prop")
