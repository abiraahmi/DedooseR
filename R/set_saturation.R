#' Set saturation thresholds for codes
#'
#' This function processes the output of [count_codes()] and allows the user to:
#' (1) apply a minimum excerpt count threshold for codes, and
#' (2) compute transcript coverage as a proportion of all media titles
#' (rounded to two decimals). The output contains only `code`, `count`,
#' and the coverage proportion.
#'
#' @param code_counts A tibble or data frame output from [count_codes()]. Must
#'   contain at least the columns `code`, `count`, and `n_media_titles`.
#' @param total_media_titles Optional integer giving the total number of unique
#'   media titles in the dataset. If `NULL` (default), this is inferred as the
#'   maximum observed `n_media_titles`.
#' @param min_count An integer specifying the minimum excerpt frequency required
#'   for a code to be included. Defaults to `1`.
#' @param min_prop_media_titles A numeric value between 0 and 1 specifying the
#'   minimum transcript coverage proportion required for a code to be included.
#'   Defaults to `NULL` (no filter).
#' @param output_type Output format. One of:
#'   \itemize{
#'     \item `"tibble"` (default): returns a tibble.
#'     \item `"kable"`: returns a formatted table with \code{knitr::kable()}.
#'   }
#'
#' @return A tibble (or kable table) with three columns:
#'   \describe{
#'     \item{code}{The code name.}
#'     \item{count}{Total number of excerpts where the code was applied.}
#'     \item{prop}{Proportion of unique media titles containing the code,
#'       rounded to two decimals.}
#'   }
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
#' code_counts <- count_codes(df)
#'
#' # Default saturation (keep all codes)
#' set_saturation(code_counts)
#'
#' # Require codes to appear in at least 2 excerpts
#' set_saturation(code_counts, min_count = 2)
#'
#' # Require codes to appear in at least 50% of media titles
#' set_saturation(code_counts, min_prop_media_titles = 0.5)
#'
#' # Return formatted as kable
#' if (requireNamespace("knitr", quietly = TRUE)) {
#'   set_saturation(code_counts, min_count = 2, output_type = "kable")
#' }
#'
#' @importFrom dplyr filter mutate arrange select
#' @importFrom knitr kable
#' @export
set_saturation <- function(code_counts,
                           total_media_titles = NULL,
                           min_count = 1,
                           min_prop_media_titles = NULL,
                           output_type = c("tibble", "kable")) {
  output_type <- match.arg(output_type)

  # Check input
  if (!is.data.frame(code_counts)) {
    stop("`code_counts` must be a tibble or data frame (from count_codes()).")
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
    dplyr::filter(count >= min_count) %>%
    dplyr::mutate(
      prop_media_titles = round(n_media_titles / total_media_titles, 2)
    ) %>%
    dplyr::select(code, count, prop_media_titles)   # keep only these columns

  # Filter by proportion if requested
  if (!is.null(min_prop_media_titles)) {
    df <- df %>%
      dplyr::filter(prop_media_titles >= min_prop_media_titles)
  }

  # Arrange
  df <- df %>%
    dplyr::arrange(dplyr::desc(count))

  # Return in requested format
  if (output_type == "kable") {
    return(knitr::kable(
      df,
      caption = paste(
        "Code Counts with Transcript Proportions (min_count =",
        min_count,
        ", min_prop_media_titles =",
        ifelse(is.null(min_prop_media_titles), "none", min_prop_media_titles),
        ")"
      ),
      digits = 2
    ))
  } else {
    return(df)
  }
}
