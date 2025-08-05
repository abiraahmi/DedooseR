summarize_codes <- function(excerpts, preferred_coders = NULL,
                                        output_type = c("tibble", "kable", "datatable")) {
  output_type <- match.arg(output_type)

  if (!is.data.frame(excerpts)) {
    stop("`excerpts` must be a data frame.")
  }

  if (is.null(preferred_coders)) {
    stop("Please provide a vector of preferred_coders in order of preference.")
  }

  # Drop unneeded columns and keep preferred coders
  excerpts_clean <- excerpts %>%
    select(-c(`Excerpt Range`, `Excerpt Date`, `Resource Creator`, `Resource Date`),
           -ends_with("Range"), -ends_with("Weight")) %>%
    mutate(coder_rank = match(`Excerpt Creator`, preferred_coders)) %>%
    filter(!is.na(coder_rank)) %>%
    group_by(`Media Title`) %>%
    filter(coder_rank == min(coder_rank)) %>%
    ungroup()

  # Identify relevant code columns
  code_columns <- grep("^Code: ", colnames(excerpts_clean), value = TRUE)
  code_columns <- setdiff(code_columns, c(
    "Code: Priority excerpt Applied",
    "Code: Heterogeniety Applied"
  ))

  # Count how many times each coder applied each code
  coder_code_counts <- excerpts_clean %>%
    select(`Excerpt Creator`, all_of(code_columns)) %>%
    pivot_longer(cols = all_of(code_columns),
                 names_to = "Code", values_to = "Applied") %>%
    filter(Applied == "True") %>%
    mutate(
      Code = str_replace(Code, "^Code: ", ""),
      Code = str_replace(Code, " Applied$", "")
    ) %>%
    count(Code, `Excerpt Creator`, name = "Count") %>%
    pivot_wider(names_from = `Excerpt Creator`, values_from = Count, values_fill = 0) %>%
    mutate(Total = rowSums(across(-Code))) %>%
    arrange(desc(Total))

  # Return as table
  if (output_type == "kable") {
    return(knitr::kable(coder_code_counts, caption = "Code Counts by Coder"))
  } else if (output_type == "datatable") {
    return(DT::datatable(coder_code_counts,
                         caption = "Code Counts by Coder",
                         options = list(pageLength = 30, autoWidth = TRUE)))
  } else {
    return(coder_code_counts)
  }
}


