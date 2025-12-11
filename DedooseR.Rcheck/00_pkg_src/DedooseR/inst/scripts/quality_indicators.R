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

  # Check if all quality indicator columns exist (clean names, no prefix/suffix)
  missing_cols <- setdiff(qual_indicators, colnames(excerpts))
  if (length(missing_cols) > 0) {
    stop("These quality indicator columns are missing: ", paste(missing_cols, collapse = ", "))
  }

  # Filter excerpts to preferred coders per Media Title (if not already done)
  # Optional: skip if already done in clean_data()
  excerpts_clean <- excerpts %>%
    dplyr::mutate(coder_rank = base::match(`Excerpt Creator`, preferred_coders)) %>%
    dplyr::filter(!is.na(coder_rank)) %>%
    dplyr::group_by(`Media Title`) %>%
    dplyr::filter(coder_rank == min(coder_rank)) %>%
    dplyr::ungroup()

  # Identify excerpts where each quality indicator applied (logical TRUE)
  qual_excerpts_list <- lapply(qual_indicators, function(col) {
    excerpts_clean %>%
      dplyr::filter(.data[[col]] == TRUE) %>%
      dplyr::pull(`Excerpt Copy`)
  })
  names(qual_excerpts_list) <- qual_indicators

  # Identify all code columns except the quality indicators
  code_columns <- setdiff(
    colnames(excerpts_clean)[vapply(excerpts_clean, is.logical, logical(1))],
    qual_indicators
  )

  # Convert to long format and add indicator flags
  long_codes <- excerpts_clean %>%
    dplyr::select(`Excerpt Copy`, dplyr::all_of(code_columns)) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(code_columns), names_to = "Code", values_to = "Applied") %>%
    dplyr::filter(Applied == TRUE)

  # Add columns for each quality indicator flag (1 if excerpt copy is in that qual indicator's excerpts)
  for (qual in qual_indicators) {
    long_codes[[paste0(qual, "_Applied")]] <- ifelse(long_codes$`Excerpt Copy` %in% qual_excerpts_list[[qual]], 1, 0)
  }

  # Summarize counts per code for each quality indicator
  summary_cols <- paste0(qual_indicators, "_Applied")
  summarise_expr <- purrr::set_names(
    lapply(summary_cols, function(col) rlang::expr(sum(!!rlang::sym(col)))),
    paste0(qual_indicators, "_Count")
  )

  arrange_exprs <- purrr::map(paste0(qual_indicators, "_Count"), function(col) {
    rlang::expr(dplyr::desc(!!rlang::sym(col)))
  })

  long_codes %>%
    group_by(Code) %>%
    summarise(!!!summarise_expr, .groups = "drop") %>%
    arrange(!!!arrange_exprs)
}
