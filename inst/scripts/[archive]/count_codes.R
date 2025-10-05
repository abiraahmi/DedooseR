#' Count the Frequency of Qualitative Codes
#'
#' @description
#' Summarizes the frequency of each logical (TRUE/FALSE) code variable in a dataset of qualitative excerpts.
#' Each code column is assumed to represent whether a theme or code was applied to an excerpt (e.g., TRUE = applied).
#' The function returns total counts of excerpts per code and the number of unique media sources (based on
#' the `media_title` column). Optionally, codes can be excluded, and the results can be returned as a tibble,
#' a formatted `knitr::kable` table, or an interactive `DT::datatable`.
#'
#' Variable labels (stored in the `"label"` attribute, e.g. via `haven::labelled`) are displayed
#' in place of variable names when available.
#'
#' @param excerpts A data frame containing at least one logical column representing codes and a column named `media_title`.
#' Each logical column should indicate whether a given code applies to that excerpt.
#' @param min_count Numeric. The minimum number of excerpts required for a code to be included in the summary.
#' Defaults to `1`.
#' @param output_type Character. Specifies the output format.
#' One of `"tibble"`, `"kable"`, or `"datatable"`. Defaults to `"tibble"`.
#' @param exclude Character vector of code variable names to exclude from the count. Defaults to `NULL`.
#'
#' @return
#' Depending on the `output_type`:
#' * `"tibble"` – a tibble with columns:
#'   - `code`: variable label (or name if no label available)
#'   - `count`: number of excerpts where the code was applied
#'   - `n_media_titles`: number of unique media titles where the code appeared
#' * `"kable"` – a formatted table created using [knitr::kable()].
#' * `"datatable"` – an interactive table created using [DT::datatable()].
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#' library(purrr)
#'
#' # Example dataset
#' df <- data.frame(
#'   media_title = c("A", "B", "C", "D"),
#'   code1 = c(TRUE, FALSE, TRUE, TRUE),
#'   code2 = c(FALSE, TRUE, TRUE, FALSE),
#'   code3 = c(FALSE, TRUE, FALSE, FALSE)
#' )
#'
#' # Add variable labels
#' attr(df$code1, "label") <- "Emotional Support"
#' attr(df$code2, "label") <- "Academic Support"
#' attr(df$code3, "label") <- "Family Relationships"
#'
#' # Default: return as tibble
#' count_codes(df)
#'
#' # Return as kable
#' count_codes(df, output_type = "kable")
#'
#' # Return as datatable, excluding one code
#' count_codes(df, output_type = "datatable", exclude = "code3")
#'
#' @export
count_codes <- function(
    excerpts,
    min_count = 1,
    output_type = c("tibble", "kable", "datatable"),
    exclude = NULL
) {
  # Validate arguments ----
  output_type <- match.arg(output_type)

  if (!is.data.frame(excerpts)) stop("`excerpts` must be a data frame.")
  if (!"media_title" %in% names(excerpts)) stop("`excerpts` must contain a `media_title` column.")
  if (!is.numeric(min_count) || min_count < 1) stop("`min_count` must be a positive number.")

  # Identify logical columns (assumed to be codes)
  code_columns <- colnames(excerpts)[vapply(excerpts, is.logical, logical(1))]

  # Drop excluded codes if specified
  if (!is.null(exclude)) {
    exclude <- intersect(exclude, code_columns)
    code_columns <- setdiff(code_columns, exclude)
  }

  if (length(code_columns) == 0) stop("No logical (code) columns found after exclusions.")

  # Create name → label lookup
  label_lookup <- purrr::map_chr(code_columns, function(x) {
    lbl <- attr(excerpts[[x]], "label")
    if (is.null(lbl) || lbl == "") x else lbl
  })
  names(label_lookup) <- code_columns

  # Summarize total counts for each code
  total_counts <- excerpts %>%
    dplyr::select(media_title, all_of(code_columns)) %>%
    tidyr::pivot_longer(
      cols = all_of(code_columns),
      names_to = "code",
      values_to = "applied"
    ) %>%
    dplyr::filter(applied == TRUE) %>%
    dplyr::group_by(code) %>%
    dplyr::summarise(
      count = dplyr::n(),
      n_media_titles = dplyr::n_distinct(media_title),
      .groups = "drop"
    ) %>%
    dplyr::filter(count >= min_count) %>%
    dplyr::arrange(desc(count)) %>%
    dplyr::mutate(
      code_label = label_lookup[as.character(code)] |> unname()
    ) %>%
    dplyr::select(code = code_label, count, n_media_titles)

  # Output formatting
  caption_text <- paste("Total Code Counts (min_count =", min_count, ")")

  if (output_type == "kable") {
    knitr::kable(total_counts, caption = caption_text)
  } else if (output_type == "datatable") {
    DT::datatable(
      total_counts,
      caption = caption_text,
      options = list(pageLength = 30, autoWidth = TRUE)
    )
  } else {
    total_counts
  }
}
