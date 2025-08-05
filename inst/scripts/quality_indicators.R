# Load packages
library(tidyverse)
library(readxl)
library(writexl)
library(knitr)

quality_indicators <- function(excerpts = NULL,
                               preferred_coders = NULL,
                               qual_indicators = NULL) {

  # Validate inputs
  if (is.null(excerpts) || !is.data.frame(excerpts)) {
    stop("Please provide a data frame `excerpts`.")
  }
  if (is.null(preferred_coders)) {
    stop("Please provide a vector of `preferred_coders` in order of preference.")
  }
  if (is.null(qual_indicators) || length(qual_indicators) == 0) {
    stop("Please specify at least one quality indicator code name in `qual_indicators`.")
  }

  # Construct full column names for quality indicators
  qual_cols <- paste0("Code: ", qual_indicators, " Applied")

  # Check if all quality indicator columns exist
  missing_cols <- setdiff(qual_cols, colnames(excerpts))
  if (length(missing_cols) > 0) {
    stop("These quality indicator columns are missing: ", paste(missing_cols, collapse = ", "))
  }

  # Clean excerpts - keep preferred coder per Media Title
  excerpts_clean <- excerpts %>%
    select(-c(`Excerpt Range`, `Excerpt Date`, `Resource Creator`, `Resource Date`),
           -ends_with("Range"), -ends_with("Weight")) %>%
    mutate(coder_rank = match(`Excerpt Creator`, preferred_coders)) %>%
    filter(!is.na(coder_rank)) %>%
    group_by(`Media Title`) %>%
    filter(coder_rank == min(coder_rank)) %>%
    ungroup() %>%
    select(-coder_rank, -`Excerpt Creator`)

  # Identify excerpts where each quality indicator applied
  qual_excerpts_list <- lapply(qual_cols, function(col) {
    excerpts %>%
      filter(.data[[col]] == "True") %>%
      pull(`Excerpt Copy`)
  })
  names(qual_excerpts_list) <- qual_indicators

  # Identify all code columns except the quality indicators
  code_columns <- grep("^Code: ", colnames(excerpts), value = TRUE)
  code_columns <- setdiff(code_columns, qual_cols)

  # Convert to long format and add indicator flags
  long_codes <- excerpts %>%
    select(`Excerpt Copy`, all_of(code_columns)) %>%
    pivot_longer(cols = all_of(code_columns), names_to = "Code", values_to = "Applied") %>%
    filter(Applied == "True") %>%
    mutate(
      Code = str_replace(Code, "^Code: ?", ""),
      Code = str_replace(Code, " Applied$", ""),
      Code = str_replace(Code, "\\\\", "\\\\\\\\")
    )

  # Add columns for each quality indicator flag
  for (qual in qual_indicators) {
    long_codes[[paste0(qual, "_Applied")]] <- ifelse(long_codes$`Excerpt Copy` %in% qual_excerpts_list[[qual]], 1, 0)
  }

  # Summarize counts per code for each quality indicator
  summary_cols <- paste0(qual_indicators, "_Applied")
  summarise_expr <- purrr::set_names(
    lapply(summary_cols, function(col) rlang::expr(sum(!!rlang::sym(col)))),
    paste0(qual_indicators, "_Count")
  )

  long_codes %>%
    group_by(Code) %>%
    summarise(!!!summarise_expr, .groups = "drop") %>%
    arrange(across(starts_with(qual_indicators[1]), desc), across(starts_with(qual_indicators[2]), desc))
}



