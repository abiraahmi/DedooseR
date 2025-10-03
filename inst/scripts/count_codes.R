count_codes <- function(excerpts,
                        min_count = 1,
                        output_type = c("tibble", "kable", "datatable"),
                        exclude = NULL) {
  output_type <- match.arg(output_type)

  if (!is.data.frame(excerpts)) {
    stop("`excerpts` must be a data frame.")
  }

  if (!"media_title" %in% names(excerpts)) {
    stop("`excerpts` must contain a `media_title` column.")
  }

  # Identify code columns — only logical columns assumed to be codes
  code_columns <- colnames(excerpts)[vapply(excerpts, is.logical, logical(1))]

  # Drop excluded codes if specified
  if (!is.null(exclude)) {
    code_columns <- setdiff(code_columns, exclude)
  }

  # Summarize total counts for each code
  total_counts <- excerpts %>%
    dplyr::select(media_title, all_of(code_columns)) %>%
    tidyr::pivot_longer(
      cols = all_of(code_columns),
      names_to = "code",
      values_to = "applied"
    ) %>%
    dplyr::filter(applied == TRUE) %>%
    dplyr::group_by(code) %>%
    dplyr::summarise(
      count = dplyr::n(),  # total # of excerpts with this code
      n_media_titles = dplyr::n_distinct(media_title),  # unique media_title coverage
      .groups = "drop"
    ) %>%
    dplyr::filter(count >= min_count) %>%
    dplyr::arrange(desc(count))

  # Return as requested format
  if (output_type == "kable") {
    return(knitr::kable(total_counts,
                        caption = paste("Total Code Counts (min_count =", min_count, ")")))
  } else if (output_type == "datatable") {
    return(DT::datatable(total_counts,
                         caption = paste("Total Code Counts (min_count =", min_count, ")"),
                         options = list(pageLength = 30, autoWidth = TRUE)))
  } else {
    return(total_counts)
  }
}
