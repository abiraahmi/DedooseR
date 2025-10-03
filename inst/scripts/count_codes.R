count_codes <- function(excerpts, preferred_coders,
                        output_type = c("tibble", "kable", "datatable"),
                        include_zero = FALSE) {
  output_type <- match.arg(output_type)

  if (!is.data.frame(excerpts)) {
    stop("`excerpts` must be a data frame.")
  }

  if (missing(preferred_coders) || is.null(preferred_coders)) {
    stop("Please provide a vector of preferred_coders in order of preference.")
  }

  # Identify code columns — all vars starting with "c_"
  code_columns <- grep("^c_", colnames(excerpts), value = TRUE)

  # Keep ALL excerpts from the preferred coder for each transcript
  excerpts_clean <- excerpts %>%
    dplyr::mutate(coder_rank = match(excerpt_creator, preferred_coders)) %>%
    dplyr::filter(!is.na(coder_rank)) %>%
    dplyr::filter(coder_rank == min(coder_rank, na.rm = TRUE), .by = media_title)

  # ----- Code counts -----
  total_counts <- excerpts_clean %>%
    dplyr::select(all_of(code_columns)) %>%
    tidyr::pivot_longer(cols = everything(), names_to = "Code", values_to = "Applied") %>%
    dplyr::group_by(Code) %>%
    dplyr::summarise(total_preferred_coder = sum(Applied, na.rm = TRUE), .groups = "drop")

  # ----- Transcript counts -----
  total_transcripts <- dplyr::n_distinct(excerpts_clean$media_title)

  transcript_counts <- excerpts_clean %>%
    dplyr::select(media_title, all_of(code_columns)) %>%
    tidyr::pivot_longer(cols = -media_title, names_to = "Code", values_to = "Applied") %>%
    dplyr::group_by(media_title, Code) %>%
    dplyr::summarise(any_applied = any(Applied == 1, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(any_applied) %>%
    dplyr::count(Code, name = "transcript_count") %>%
    dplyr::mutate(transcript_proportion = round(transcript_count / total_transcripts, 2))

  # ----- Merge -----
  combined_final <- total_counts %>%
    dplyr::left_join(transcript_counts, by = "Code") %>%
    dplyr::mutate(
      transcript_count = dplyr::coalesce(transcript_count, 0L),
      transcript_proportion = dplyr::coalesce(transcript_proportion, 0)
    )

  # Optionally filter out zeros
  if (!include_zero) {
    combined_final <- combined_final %>%
      dplyr::filter(total_preferred_coder > 0)
  }

  combined_final <- combined_final %>%
    dplyr::arrange(desc(total_preferred_coder))

  # ----- Output -----
  if (output_type == "kable") {
    return(knitr::kable(combined_final, caption = "Code Counts (Preferred Coders Only)"))
  } else if (output_type == "datatable") {
    return(DT::datatable(
      combined_final,
      caption = "Code Counts (Preferred Coders Only)",
      options = list(pageLength = 30, autoWidth = TRUE)
    ))
  } else {
    return(combined_final)
  }
}
