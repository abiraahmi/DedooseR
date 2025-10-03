#' Count code applications across excerpts
#'
#' This function summarizes the frequency of applied codes (logical columns)
#' across a dataset of excerpts. It returns the total number of excerpts where
#' each code appears, as well as the number of unique `media_title`s in which
#' the code is applied. Results can be returned as a tibble, a formatted table
#' (`knitr::kable`), or an interactive table (`DT::datatable`).
#'
#' @param excerpts A data frame containing at least a `media_title` column and
#'   one or more logical columns (assumed to represent codes). Each row should
#'   represent a coded excerpt.
#' @param min_count An integer specifying the minimum number of occurrences
#'   required for a code to be included in the output. Defaults to `1`.
#' @param output_type Character string specifying the output format. Must be one of:
#'   \itemize{
#'     \item `"tibble"`: returns a tibble (default).
#'     \item `"kable"`: returns a formatted table via \code{knitr::kable()}.
#'     \item `"datatable"`: returns an interactive table via \code{DT::datatable()}.
#'   }
#'
#' @return A summary of code counts in the format specified by `output_type`.
#'   By default, a tibble with the following columns:
#'   \describe{
#'     \item{code}{The name of the code (column name).}
#'     \item{count}{Total number of excerpts where the code was applied.}
#'     \item{n_media_titles}{Number of unique `media_title`s containing at least one excerpt with the code.}
#'   }
#'
#' @details
#' The function automatically identifies code columns as those that are logical
#' (`TRUE`/`FALSE`) within the provided `excerpts` data frame. It reshapes the
#' data to long format, filters for `TRUE` values, and aggregates counts at the
#' code level.
#'
#' @examples
#' # Example dataset
#' df <- data.frame(
#'   media_title = c("Doc1", "Doc1", "Doc2", "Doc2", "Doc3"),
#'   c_theme1 = c(TRUE, FALSE, TRUE, FALSE, TRUE),
#'   c_theme2 = c(FALSE, TRUE, TRUE, TRUE, FALSE),
#'   c_theme3 = c(FALSE, FALSE, FALSE, TRUE, TRUE)
#' )
#'
#' # Get code counts as a tibble
#' count_codes(df)
#'
#' # Apply a minimum count filter (codes must appear in >= 2 excerpts)
#' count_codes(df, min_count = 2)
#'
#' # Return results as a kable
#' if (requireNamespace("knitr", quietly = TRUE)) {
#'   count_codes(df, output_type = "kable")
#' }
#'
#' # Return results as an interactive datatable
#' if (requireNamespace("DT", quietly = TRUE)) {
#'   count_codes(df, output_type = "datatable")
#' }
#'
#' @importFrom dplyr select filter group_by summarise n n_distinct arrange
#' @importFrom tidyr pivot_longer
#' @importFrom knitr kable
#' @importFrom DT datatable
#' @export
count_codes <- function(excerpts,
                        min_count = 1,
                        output_type = c("tibble", "kable", "datatable")) {
  output_type <- match.arg(output_type)

  if (!is.data.frame(excerpts)) {
    stop("`excerpts` must be a data frame.")
  }

  if (!"media_title" %in% names(excerpts)) {
    stop("`excerpts` must contain a `media_title` column.")
  }

  # Identify code columns — only logical columns assumed to be codes
  code_columns <- colnames(excerpts)[vapply(excerpts, is.logical, logical(1))]

  # Summarize total counts for each code
  total_counts <- excerpts %>%
    dplyr::select(media_title, all_of(code_columns)) %>%
    tidyr::pivot_longer(cols = all_of(code_columns),
                        names_to = "code",
                        values_to = "applied") %>%
    dplyr::filter(applied == TRUE) %>%
    dplyr::group_by(code) %>%
    dplyr::summarise(
      count = dplyr::n(),  # total # of excerpts with this code
      n_media_titles = dplyr::n_distinct(media_title),  # unique media_title coverage
      .groups = "drop"
    ) %>%
    dplyr::filter(count >= min_count) %>%
    dplyr::arrange(desc(count))

  # Return as requested format
  if (output_type == "kable") {
    return(knitr::kable(total_counts,
                        caption = paste("Total Code Counts (min_count =", min_count, ")")))
  } else if (output_type == "datatable") {
    return(DT::datatable(total_counts,
                         caption = paste("Total Code Counts (min_count =", min_count, ")"),
                         options = list(pageLength = 30, autoWidth = TRUE)))
  } else {
    return(total_counts)
  }
}
