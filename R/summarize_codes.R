#' Summarize Code Counts by Coder and from Preferred Coders
#'
#' This function summarizes how many times each code was applied by each coder,
#' and presents the data in a wide format: one row per code, one column per
#' coder,
#' along with two total count columns:
#' - Total count from all coders (regardless of preference)
#' - Total count from preferred coders only (one coder per media title, selected
#'  in order of preference)
#'
#' @param excerpts A data frame of coded excerpts, typically read from an Excel
#' file.
#'        Must include a column `Excerpt Creator` indicating who coded each
#'        excerpt,
#'        a column `Media Title` identifying the transcript,
#'        and columns prefixed with `Code:` for each code (binary indicators).
#' @param preferred_coders A character vector of coder names in preferred order,
#'        used to resolve duplicates when multiple coders coded the same
#'        transcript.
#' @param output_type A string indicating the output format: one of `"tibble"`,
#'        `"kable"`, or `"datatable"`. Defaults to `"tibble"`.
#'
#' @return A table summarizing the number of times each code was applied by
#' each coder,
#'         including total counts from all coders and from preferred coders only.
#'         The format depends on `output_type`.
#' @export
#' @importFrom DT datatable
#' @importFrom knitr kable
#'
#' @examples
#' \dontrun{
#' excerpts <- readxl::read_xlsx("data/my_coded_excerpts.xlsx")
#' preferred_coders <- c("s", "r", "l", "a")
#' summarize_codes(excerpts, preferred_coders, output_type = "datatable")
#' }

summarize_codes <- function(excerpts, preferred_coders = NULL,
                            output_type = c("tibble", "kable", "datatable")) {
  output_type <- match.arg(output_type)

  if (!is.data.frame(excerpts)) {
    stop("`excerpts` must be a data frame.")
  }

  if (is.null(preferred_coders)) {
    stop("Please provide a vector of preferred_coders in order of preference.")
  }

  # Identify relevant code columns from the full data
  code_columns <- grep("^Code: ", colnames(excerpts), value = TRUE)
  # Remove columns that end with "Range" or "Weight"
  code_columns <- code_columns[!grepl("(Range|Weight)$", code_columns)]

  # Count total codes from all coders (full data)
  total_all_coders <- excerpts %>%
    dplyr::select(all_of(code_columns)) %>%
    tidyr::pivot_longer(cols = everything(),
                        names_to = "Code", values_to = "Applied") %>%
    dplyr::filter(Applied == "True") %>%
    dplyr::mutate(Code = stringr::str_replace(Code, "^Code: ", ""),
                  Code = stringr::str_replace(Code, " Applied$", "")) %>%
    dplyr::count(Code, name = "total_all_coders")

  # Filter excerpts to keep only preferred coder per Media Title
  excerpts_clean <- excerpts %>%
    dplyr::select(-c(`Excerpt Range`, `Excerpt Date`, `Resource Creator`, `Resource Date`),
                  -dplyr::ends_with("Range"), -dplyr::ends_with("Weight")) %>%
    dplyr::mutate(coder_rank = match(`Excerpt Creator`, preferred_coders)) %>%
    dplyr::filter(!is.na(coder_rank)) %>%
    dplyr::group_by(`Media Title`) %>%
    dplyr::filter(coder_rank == min(coder_rank)) %>%
    dplyr::ungroup()

  # Count codes from preferred coders only
  total_preferred_coder <- excerpts_clean %>%
    dplyr::select(all_of(code_columns)) %>%
    tidyr::pivot_longer(cols = everything(),
                        names_to = "Code", values_to = "Applied") %>%
    dplyr::filter(Applied == "True") %>%
    dplyr::mutate(Code = stringr::str_replace(Code, "^Code: ", ""),
                  Code = stringr::str_replace(Code, " Applied$", "")) %>%
    dplyr::count(Code, name = "total_preferred_coder")

  # Count per coder from preferred excerpts only (for columns per coder)
  coder_code_counts <- excerpts_clean %>%
    dplyr::select(`Excerpt Creator`, all_of(code_columns)) %>%
    tidyr::pivot_longer(cols = all_of(code_columns),
                        names_to = "Code", values_to = "Applied") %>%
    dplyr::filter(Applied == "True") %>%
    dplyr::mutate(
      Code = stringr::str_replace(Code, "^Code: ", ""),
      Code = stringr::str_replace(Code, " Applied$", "")
    ) %>%
    dplyr::count(Code, `Excerpt Creator`, name = "Count") %>%
    tidyr::pivot_wider(names_from = `Excerpt Creator`, values_from = Count, values_fill = 0) %>%
    dplyr::arrange(desc(Code))  # we'll arrange after joining totals

  # Join totals to the coder_code_counts
  combined <- coder_code_counts %>%
    dplyr::left_join(total_all_coders, by = "Code") %>%
    dplyr::left_join(total_preferred_coder, by = "Code") %>%
    dplyr::arrange(desc(total_preferred_coder), desc(total_all_coders))

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
