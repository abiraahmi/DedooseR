#' Compare Code Saturation Across Threshold Sets
#'
#' This function evaluates whether each code meets one or more user-defined
#' saturation thresholds. Each threshold set is defined by a minimum code count
#' and a minimum proportion of distinct media titles in which the code appears.
#'
#' @param code_counts A data frame produced by \code{count_codes()}, containing
#'   columns \code{code}, \code{count}, and \code{n_media_titles}.
#' @param excerpts The full excerpts data frame used to generate \code{code_counts}.
#'   Must contain a column named \code{media_title}.
#' @param thresholds_list A named list of threshold sets. Each element should
#'   itself be a list with two components: \code{code_count} (minimum frequency
#'   across excerpts) and \code{prop_media_title} (minimum proportion of distinct
#'   media titles in which the code appears).
#' @param output_type A character string specifying the output format. Options are:
#'   \code{"tibble"} (default) to return a tibble, or \code{"kable"} to return a
#'   formatted table via \code{knitr::kable}.
#'
#' @return A tibble or kable-formatted table with one row per code. Includes the
#'   following columns:
#'   \itemize{
#'     \item \code{code} — the code label.
#'     \item \code{count} — number of excerpts with this code.
#'     \item \code{n_media_titles} — number of distinct media titles with this code.
#'     \item \code{prop_media_title} — proportion of total media titles with this code.
#'     \item One column for each threshold set, indicating \code{TRUE}/\code{FALSE}
#'           if the code meets the set’s criteria.
#'   }
#'
#' @examples
#' \dontrun{
#' # Example thresholds
#' thresholds_list <- list(
#'   Set1 = list(code_count = 10, prop_media_title = 0.2),
#'   Set2 = list(code_count = 10, prop_media_title = 0.4)
#' )
#'
#' # Compute code counts
#' code_counts <- count_codes(excerpts)
#'
#' # Compare saturation against thresholds
#' compare_saturation(code_counts, excerpts, thresholds_list)
#'
#' # Return as a formatted kable table
#' compare_saturation(code_counts, excerpts, thresholds_list, output_type = "kable")
#' }
#'
#' @export
compare_saturation <- function(code_counts, excerpts, thresholds_list,
                               output_type = c("tibble", "kable")) {
  output_type <- match.arg(output_type)

  # total distinct media titles
  total_media_titles <- dplyr::n_distinct(excerpts$media_title)

  # add proportion column
  code_counts <- code_counts %>%
    dplyr::mutate(prop_media_title = n_media_titles / total_media_titles)

  # loop over thresholds
  for (set_name in names(thresholds_list)) {
    rules <- thresholds_list[[set_name]]
    code_counts[[set_name]] <-
      code_counts$count >= rules$code_count &
      code_counts$prop_media_title >= rules$prop_media_title
  }

  # output
  if (output_type == "kable") {
    return(knitr::kable(code_counts,
                        caption = "Saturation Comparison"))
  } else {
    return(code_counts)
  }
}
