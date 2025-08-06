#' Summarize Quality Indicator Counts by Code from Excerpts Data
#'
#' This function analyzes a data frame of coded excerpts to summarize counts of specified quality indicators
#' by code. It filters excerpts to preferred coders per media title (if not already filtered),
#' identifies excerpts where each quality indicator applies (logical TRUE),
#' and returns a summary table counting how many excerpts for each code have each quality indicator applied.
#'
#' @param excerpts A data frame containing excerpt data with logical code columns.
#'   Code columns should use clean variable names without prefixes or suffixes (e.g., `"Priority excerpt"`).
#' @param preferred_coders A character vector specifying coders in order of preference.
#'   Used to filter excerpts to only those from the highest-ranked coder per media title.
#' @param qual_indicators A character vector of quality indicator code names to summarize.
#'   These should match the clean column names present in `excerpts`.
#'
#' @return A tibble grouped by `Code` containing counts of excerpts where each quality indicator was applied.
#'   Columns are named as `"<quality_indicator>_Count"`.
#'   The output is arranged in descending order by these counts.
#'
#' @details
#' - The function expects the `excerpts` data frame to have logical columns representing codes.
#' - It filters to preferred coders per media title to avoid duplicates.
#' - It computes counts of excerpts for each code where each quality indicator was applied.
#' - Input excerpts should be pre-cleaned with consistent code naming (e.g., using `clean_data()`).
#'
#' @examples
#' \dontrun{
#' # Assume 'df' is your cleaned excerpts data frame
#' preferred_coders <- c("Coder1", "Coder2")
#' qual_indicators <- c("Priority excerpt", "Heterogeniety")
#' quality_indicators(df, preferred_coders, qual_indicators)
#' }
#'
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @importFrom rlang sym expr !!
#' @importFrom purrr map set_names
#'
#' @export

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

  # Check if all quality indicator columns exist
  missing_cols <- setdiff(qual_indicators, colnames(excerpts))
  if (length(missing_cols) > 0) {
    stop("These quality indicator columns are missing: ", paste(missing_cols, collapse = ", "))
  }

  # Filter excerpts to preferred coders per Media Title
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
    applied_col <- paste0(qual, "_Applied")
    long_codes[[applied_col]] <- ifelse(long_codes$`Excerpt Copy` %in% qual_excerpts_list[[qual]], 1, 0)
  }

  # Summarize counts per code using original qual_indicators as column names
  summary_exprs <- purrr::set_names(
    lapply(qual_indicators, function(qual) {
      applied_col <- paste0(qual, "_Applied")
      rlang::expr(sum(!!rlang::sym(applied_col)))
    }),
    qual_indicators
  )

  arrange_exprs <- purrr::map(qual_indicators, function(col) {
    rlang::expr(dplyr::desc(!!rlang::sym(col)))
  })

  long_codes %>%
    dplyr::group_by(Code) %>%
    dplyr::summarise(!!!summary_exprs, .groups = "drop") %>%
    dplyr::arrange(!!!arrange_exprs)
}
