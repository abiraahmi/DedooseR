#' Summarize Quality Indicators Applied to Excerpts by Preferred Coders
#'
#' This function analyzes coding excerpts data, filters to keep only excerpts
#' from preferred coders (by their order of preference), and summarizes how
#' frequently specified quality indicator codes are applied to excerpts.
#'
#' @param excerpts A data frame containing excerpt coding data, including columns
#'   named like `"Code: <Indicator> Applied"` (e.g., `"Code: Priority excerpt Applied"`).
#' @param preferred_coders A character vector specifying coder names in order of
#'   preference. For each media title, only excerpts from the highest-ranked coder are kept.
#' @param qual_indicators A character vector of quality indicator code names (without
#'   `"Code: "` prefix or `"Applied"` suffix). For example: `c("Priority excerpt", "Heterogeniety")`.
#'
#' @return A tibble summarizing counts of excerpts coded with each code, along with counts
#'   for each specified quality indicator applied, grouped by code.
#'
#' @details
#' The function:
#' - Validates the presence of quality indicator columns in the data.
#' - Filters excerpts to only include those from the preferred coder per media title.
#' - Converts wide code columns to long format.
#' - Flags excerpts where each quality indicator is applied.
#' - Summarizes and returns counts of how often each code appears along with counts
#'   of quality indicators applied.
#'
#' @examples
#' \dontrun{
#' excerpts <- readxl::read_xlsx("path/to/excerpts.xlsx")
#' preferred_coders <- c("s", "r", "l", "a")
#' quality_indicators(
#'   excerpts = excerpts,
#'   preferred_coders = preferred_coders,
#'   qual_indicators = c("Heterogeniety", "Priority excerpt", "Knowledge/Awareness")
#' )
#' }
#'
#' @importFrom dplyr select mutate filter group_by ungroup arrange summarise across
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_replace
#' @importFrom purrr set_names
#' @importFrom rlang expr sym
#'
#' @export
#' @importFrom magrittr %>%

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
  missing_cols <- base::setdiff(qual_cols, colnames(excerpts))
  if (length(missing_cols) > 0) {
    stop("These quality indicator columns are missing: ", paste(missing_cols, collapse = ", "))
  }

  # Clean excerpts - keep preferred coder per Media Title
  excerpts_clean <- excerpts %>%
    dplyr::select(-c(`Excerpt Range`, `Excerpt Date`, `Resource Creator`, `Resource Date`),
                  -dplyr::ends_with("Range"), -dplyr::ends_with("Weight")) %>%
    dplyr::mutate(coder_rank = base::match(`Excerpt Creator`, preferred_coders)) %>%
    dplyr::filter(!base::is.na(coder_rank)) %>%
    dplyr::group_by(`Media Title`) %>%
    dplyr::filter(coder_rank == base::min(coder_rank)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-coder_rank, -`Excerpt Creator`)

  # Identify excerpts where each quality indicator applied
  qual_excerpts_list <- base::lapply(qual_cols, function(col) {
    excerpts %>%
      dplyr::filter(.data[[col]] == "True") %>%
      dplyr::pull(`Excerpt Copy`)
  })
  base::names(qual_excerpts_list) <- qual_indicators

  # Identify all code columns except the quality indicators
  code_columns <- base::grep("^Code: ", colnames(excerpts), value = TRUE)
  code_columns <- base::setdiff(code_columns, qual_cols)

  # Convert to long format and add indicator flags
  long_codes <- excerpts %>%
    dplyr::select(`Excerpt Copy`, dplyr::all_of(code_columns)) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(code_columns), names_to = "Code", values_to = "Applied") %>%
    dplyr::filter(Applied == "True") %>%
    dplyr::mutate(
      Code = stringr::str_replace(Code, "^Code: ?", ""),
      Code = stringr::str_replace(Code, " Applied$", ""),
      Code = stringr::str_replace(Code, "\\\\", "\\\\\\\\")
    )

  # Add columns for each quality indicator flag
  for (qual in qual_indicators) {
    long_codes[[paste0(qual, "_Applied")]] <- ifelse(long_codes$`Excerpt Copy` %in% qual_excerpts_list[[qual]], 1, 0)
  }

  # Summarize counts per code for each quality indicator
  summary_cols <- paste0(qual_indicators, "_Applied")
  summarise_expr <- purrr::set_names(
    base::lapply(summary_cols, function(col) rlang::expr(sum(!!rlang::sym(col)))),
    paste0(qual_indicators, "_Count")
  )

  long_codes %>%
    dplyr::group_by(Code) %>%
    dplyr::summarise(!!!summarise_expr, .groups = "drop") %>%
    dplyr::arrange(
      dplyr::across(dplyr::starts_with(qual_indicators[1]), dplyr::desc),
      dplyr::across(dplyr::starts_with(qual_indicators[2]), dplyr::desc)
    )
}
