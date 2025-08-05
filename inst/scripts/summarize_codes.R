summarize_codes <- function(excerpts, preferred_coders,
                            output_type = c("tibble", "kable", "datatable")) {
  output_type <- match.arg(output_type)

  if (!is.data.frame(excerpts)) {
    stop("`excerpts` must be a data frame.")
  }

  if (missing(preferred_coders) || is.null(preferred_coders)) {
    stop("Please provide a vector of preferred_coders in order of preference.")
  }

  # Identify code columns — only logical columns assumed to be codes
  code_columns <- colnames(excerpts)[vapply(excerpts, is.logical, logical(1))]

  # Filter to preferred coder per Media Title
  excerpts_clean <- excerpts %>%
    dplyr::mutate(coder_rank = match(`Excerpt Creator`, preferred_coders)) %>%
    dplyr::filter(!is.na(coder_rank)) %>%
    dplyr::group_by(`Media Title`) %>%
    dplyr::filter(coder_rank == min(coder_rank)) %>%
    dplyr::ungroup()


  # Count codes from preferred coders only (filtered excerpts)
  total_preferred_coder <- excerpts_clean %>%
    dplyr::select(all_of(code_columns)) %>%
    tidyr::pivot_longer(cols = everything(), names_to = "Code", values_to = "Applied") %>%
    dplyr::filter(Applied == TRUE) %>%
    dplyr::count(Code, name = "total_preferred_coder")

  # Count per coder from filtered excerpts only
  coder_code_counts <- excerpts_clean %>%
    dplyr::select(`Excerpt Creator`, all_of(code_columns)) %>%
    tidyr::pivot_longer(cols = all_of(code_columns), names_to = "Code", values_to = "Applied") %>%
    dplyr::filter(Applied == TRUE) %>%
    dplyr::count(Code, `Excerpt Creator`, name = "Count") %>%
    tidyr::pivot_wider(names_from = `Excerpt Creator`, values_from = Count, values_fill = 0) %>%
    dplyr::arrange(desc(Code))

  # Join totals to the coder_code_counts
  combined <- coder_code_counts %>%
    dplyr::left_join(total_preferred_coder, by = "Code") %>%
    dplyr::arrange(desc(total_preferred_coder))

  # Return as requested format
  if (output_type == "kable") {
    return(knitr::kable(combined, caption = "Code Counts by Coder with Totals"))
  } else if (output_type == "datatable") {
    return(DT::datatable(combined,
                         caption = "Code Counts by Coder with Totals",
                         options = list(pageLength = 30, autoWidth = TRUE)))
  } else {
    return(combined)
  }
}
