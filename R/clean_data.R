#' Clean and Prepare Excerpts Data from Excel File
#'
#' This function reads an Excel file containing excerpt data, cleans and processes it by:
#' - Reading all columns as text to avoid type guessing issues,
#' - Dropping columns whose names end with "Range" or "Weight",
#' - Converting code columns (those starting with "Code: ") from text to logical,
#'   interpreting "true" (case insensitive) as TRUE, otherwise FALSE,
#' - Renaming code columns by removing the prefix "Code: " and suffix " Applied",
#' - Filtering the data to keep only one preferred coder per `Media Title` based on the provided order.
#'
#' @param filepath A character string giving the path to the Excel file to read.
#' @param preferred_coders A character vector listing coders in order of preference.
#'   The function keeps excerpts only from the highest-ranked coder per `Media Title`.
#'
#' @return A cleaned tibble/data frame containing filtered excerpts with logical code columns
#'   and only preferred coders per media title.
#'
#' @details
#' The function expects columns starting with "Code: " to contain textual "true"/"false" values,
#' which are converted to logical TRUE/FALSE. Columns ending with "Range" or "Weight" are removed.
#' Excerpts are filtered so that for each `Media Title`, only the coder highest in the
#' `preferred_coders` vector is retained.
#'
#' @importFrom readxl read_xlsx
#' @importFrom dplyr select all_of rename_with starts_with ends_with mutate filter group_by ungroup
#' @importFrom stringr str_replace
#'
#' @examples
#' \dontrun{
#' preferred <- c("Coder1", "Coder2", "Coder3")
#' cleaned_data <- clean_data("path/to/excerpts.xlsx", preferred)
#' }
#'
#' @export
clean_data <- function(filepath, preferred_coders) {
  if (!base::file.exists(filepath)) {
    stop("The specified file does not exist.")
  }
  if (missing(preferred_coders) || is.null(preferred_coders)) {
    stop("Please provide a vector of preferred_coders in order of preference.")
  }

  # Read all columns as text to avoid type guessing warnings
  excerpts <- readxl::read_xlsx(filepath, col_types = "text")

  # Drop columns ending with Range or Weight
  drop_cols <- base::grep("(Range|Weight)$", base::colnames(excerpts), value = TRUE)
  excerpts <- dplyr::select(excerpts, -dplyr::all_of(drop_cols))

  # Identify code columns (starting with "Code: ")
  code_cols <- base::grep("^Code: ", base::colnames(excerpts), value = TRUE)

  # Convert code columns from text to logical: TRUE if "true" (case insensitive), else FALSE
  for (col in code_cols) {
    vals <- excerpts[[col]]
    excerpts[[col]] <- tolower(vals) == "true"
  }

  # Rename code columns: remove prefix "Code: " and suffix " Applied"
  excerpts <- dplyr::rename_with(excerpts,
                                 ~ stringr::str_replace(., "^Code: ", ""),
                                 dplyr::starts_with("Code:"))
  excerpts <- dplyr::rename_with(excerpts,
                                 ~ stringr::str_replace(., " Applied$", ""),
                                 dplyr::ends_with("Applied"))

  # Filter to preferred coder per Media Title
  excerpts <- excerpts %>%
    dplyr::mutate(coder_rank = base::match(`Excerpt Creator`, preferred_coders)) %>%
    dplyr::filter(!is.na(coder_rank)) %>%
    dplyr::group_by(`Media Title`) %>%
    dplyr::filter(coder_rank == min(coder_rank)) %>%
    dplyr::ungroup()

  return(excerpts)
}
