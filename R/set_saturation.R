#' Compute Code Saturation Metrics
#'
#' @description
#' Calculates the proportion of media sources (`media_title`s) associated with each code,
#' as a measure of saturation. This function builds on the output of
#' [create_code_summary()], and can accept either a tibble of code counts or
#' the full list output (when `plot = TRUE`).
#'
#' @param code_counts A tibble or list output from [create_code_summary()].
#'   If a list is provided (e.g., when `plot = TRUE`), the `$table` element will be used.
#' @param total_media_titles Optional numeric. The total number of media sources
#'   across which codes were applied. If `NULL`, the function uses the maximum
#'   `n_media_titles` value in the input.
#' @param min_count Minimum excerpt count required for a code to be included.
#'   Defaults to `1`.
#' @param min_prop_media_titles Minimum proportion (0–1) of media titles in which a code
#'   appears for inclusion. Defaults to `NULL`.
#' @param output_type Output format: `"tibble"` or `"kable"`. Defaults to `"tibble"`.
#'
#' @return A tibble (or `knitr::kable`) summarizing each code’s excerpt count and
#' the proportion of media titles in which it appears.
#'
#' @examples
#' library(dplyr)
#'
#' df <- data.frame(
#'   media_title = c("A", "B", "C", "D"),
#'   code1 = c(TRUE, FALSE, TRUE, TRUE),
#'   code2 = c(FALSE, TRUE, TRUE, FALSE)
#' )
#' attr(df$code1, "label") <- "Emotional Support"
#' attr(df$code2, "label") <- "Academic Support"
#'
#' code_summary <- create_code_summary(df)
#' set_saturation(code_summary)
#'
#' @importFrom dplyr filter mutate select arrange desc
#' @importFrom knitr kable
#' @export
set_saturation <- function(code_counts,
                           total_media_titles = NULL,
                           min_count = 1,
                           min_prop_media_titles = NULL,
                           output_type = c("tibble", "kable")) {
  output_type <- match.arg(output_type)

  # Handle both tibble or list input from create_code_summary()
  if (is.list(code_counts) && "table" %in% names(code_counts)) {
    code_counts <- code_counts$table
  }

  # Input validation
  if (!is.data.frame(code_counts)) {
    stop("`code_counts` must be a tibble or data frame (from create_code_summary()).")
  }
  if (!all(c("code", "count", "n_media_titles") %in% names(code_counts))) {
    stop("`code_counts` must contain columns `code`, `count`, and `n_media_titles`.")
  }

  # Determine denominator for proportions
  if (is.null(total_media_titles)) {
    total_media_titles <- max(code_counts$n_media_titles, na.rm = TRUE)
  }

  # Filter by count first
  df <- code_counts %>%
    dplyr::filter(.data$count >= min_count) %>%
    dplyr::mutate(
      prop_media_titles = round(.data$n_media_titles / total_media_titles, 2)
    ) %>%
    dplyr::select(.data$code, .data$count, .data$prop_media_titles)

  # Filter by proportion if requested
  if (!is.null(min_prop_media_titles)) {
    df <- df %>%
      dplyr::filter(.data$prop_media_titles >= min_prop_media_titles)
  }

  # Arrange
  df <- df %>%
    dplyr::arrange(dplyr::desc(.data$count))

  # Return in requested format
  caption_text <- paste(
    "Code Counts with Transcript Proportions (min_count =", min_count,
    ", min_prop_media_titles =",
    ifelse(is.null(min_prop_media_titles), "none", min_prop_media_titles),
    ")"
  )

  if (output_type == "kable") {
    knitr::kable(df, caption = caption_text, digits = 2)
  } else {
    df
  }
}
