#' Create Saturation Tracking Data
#'
#' This function processes qualitative data exported from Dedoose
#' to create saturation tracking by analyzing Priority and Heterogeneity
#' code applications across different coders.
#'
#' @param data_path A string. Path to the Excel file containing excerpt data.
#' @param preferred_coders A character vector. Ordered list of preferred coders
#'        (based on reliability scores), used to select excerpts from the
#'        highest-ranked coder per transcript.
#' @return A data frame with counts of Priority and Heterogeneity codes per theme.
#' @export
#'
#' @importFrom readxl read_xlsx
#' @importFrom dplyr select filter mutate group_by ungroup summarise arrange pull ends_with all_of desc
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_replace
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' # Use your actual data file path and coders vector here
#' result <- create_saturation_tracking(
#'   data_path = "path/to/your/data.xlsx",
#'   preferred_coders = c("coder1", "coder2", "coder3")
#' )
#'
#' # Example of input to criteria_saturation() function
#' long_codes <- data.frame(
#'   Code = c("Theme A", "Theme B"),
#'   Priority_Count = c(3, 2),
#'   Heterogeneity_Count = c(3, 4)
#' )
#' criteria_saturation(long_codes, min_priority = 3, min_heterogeneity = 3, plot = FALSE)
#' }

create_saturation_tracking <- function(data_path = NULL,
                                     preferred_coders = NULL) {

  # Check if required parameters are provided
  if (is.null(data_path)) {
    stop("Please provide a data_path to your Excel file")
  }
  if (is.null(preferred_coders)) {
    stop("Please provide a vector of preferred_coders in order of preference")
  }

  # Load data
  excerpts <- readxl::read_xlsx(data_path, col_types = "text")

  excerpts_clean <- excerpts %>%
    # Drop columns you don't need
    dplyr::select(-c(`Excerpt Range`, `Excerpt Date`, `Resource Creator`,
              `Resource Date`), -dplyr::ends_with("Range"), -dplyr::ends_with("Weight")) %>%
    # Filter so that for each transcript, we only keep one coder, in order
    # of preferences ranked above (based on reliability scores)
    dplyr::mutate(coder_rank = match(`Excerpt Creator`, preferred_coders)) %>%
    dplyr::filter(!is.na(coder_rank)) %>%
    dplyr::group_by(`Media Title`) %>%
    dplyr::filter(coder_rank == min(coder_rank)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-coder_rank, -`Excerpt Creator`)


  # Step 1: Identify excerpts with Priority or Heterogeneity applied
  priority_excerpts <- excerpts %>%
    dplyr::filter(`Code: Priority excerpt Applied` == "True") %>%
    dplyr::pull(`Excerpt Copy`)

  hetero_excerpts <- excerpts %>%
    dplyr::filter(`Code: Heterogeniety Applied` == "True") %>%
    dplyr::pull(`Excerpt Copy`)

  # Step 2: Identify all code columns (excluding excerpt and subcodes)
  code_columns <- grep("^Code: ", colnames(excerpts), value = TRUE)
  code_columns <- setdiff(code_columns, c("Code: Priority excerpt Applied", "Code: Heterogeniety Applied"))

  # Step 3: Convert to long format and count occurrences separately
  long_codes <- excerpts %>%
    dplyr::select(`Excerpt Copy`, dplyr::all_of(code_columns)) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(code_columns), names_to = "Code", values_to = "Applied") %>%
    dplyr::filter(Applied == "True") %>%
    dplyr::mutate(
      Priority_Applied = ifelse(`Excerpt Copy` %in% priority_excerpts, 1, 0),
      Heterogeneity_Applied = ifelse(`Excerpt Copy` %in% hetero_excerpts, 1, 0)
    ) %>%
    dplyr::group_by(Code) %>%
    dplyr::summarise(
      Priority_Count = sum(Priority_Applied),
      Heterogeneity_Count = sum(Heterogeneity_Applied),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(Priority_Count), dplyr::desc(Heterogeneity_Count)) %>%
    dplyr::mutate(Code = stringr::str_replace(Code, "^Code:", "")) %>% # Remove "Code:" only from the start
    dplyr::mutate(Code = stringr::str_replace(Code, " Applied$", "")) %>% # Remove "Applied" from the end if it exists
    dplyr::mutate(Code = stringr::str_replace(Code, "\\\\", "\\\\\\\\")) # Escape backslashes properly

  return(long_codes)
}

