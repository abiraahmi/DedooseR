#' Summarize Code Counts by Coder
#'
#' This function summarizes how many times each code was applied by each coder
#' and presents the data in a wide format: one row per code, one column per coder,
#' and a total count column at the end.
#'
#' @param excerpts A data frame of coded excerpts, typically read from an Excel file.
#'        Must include a column `Excerpt Creator` indicating who coded each excerpt,
#'        a column `Media Title` identifying the transcript, and columns prefixed
#'        with `Code:` for each code.
#' @param preferred_coders A character vector of coder names in preferred order,
#'        used to resolve duplicates when multiple coders coded the same transcript.
#' @param output_type A string indicating the output format: one of `"tibble"`,
#'        `"kable"`, or `"datatable"`. Defaults to `"tibble"`.
#'
#' @return A table summarizing the number of times each code was applied by each coder,
#'         with a total count column. The format depends on `output_type`.
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

  # Drop unneeded columns and keep preferred coders
  excerpts_clean <- excerpts %>%
    dplyr::select(-c(`Excerpt Range`, `Excerpt Date`, `Resource Creator`, `Resource Date`),
           -ends_with("Range"), -ends_with("Weight")) %>%
    dplyr::mutate(coder_rank = match(`Excerpt Creator`, preferred_coders)) %>%
    dplyr::filter(!is.na(coder_rank)) %>%
    dplyr::group_by(`Media Title`) %>%
    dplyr::filter(coder_rank == min(coder_rank)) %>%
    dplyr::ungroup()

  # Identify relevant code columns
  code_columns <- grep("^Code: ", colnames(excerpts_clean), value = TRUE)

  # Count how many times each coder applied each code
  coder_code_counts <- excerpts_clean %>%
    dplyr::select(`Excerpt Creator`, all_of(code_columns)) %>%
    tidyr::pivot_longer(cols = all_of(code_columns),
                 names_to = "Code", values_to = "Applied") %>%
    dplyr::filter(Applied == "True") %>%
    dplyr::mutate(
      Code = str_replace(Code, "^Code: ", ""),
      Code = str_replace(Code, " Applied$", "")
    ) %>%
    dplyr::count(Code, `Excerpt Creator`, name = "Count") %>%
    tidyr::pivot_wider(names_from = `Excerpt Creator`, values_from = Count, values_fill = 0) %>%
    dplyr::mutate(Total = rowSums(across(-Code))) %>%
    dplyr::arrange(desc(Total))

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

