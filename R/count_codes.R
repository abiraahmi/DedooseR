#' Count codes by preferred coder and transcript
#'
#' Summarizes binary \code{c_*} code columns after restricting each transcript
#' to a single preferred coder. Returns per-code totals across excerpts and,
#' per code, the number and proportion of transcripts in which the code appears
#' at least once.
#'
#' @param excerpts A data frame containing at least \code{media_title},
#'   \code{excerpt_creator}, and one or more numeric/binary columns whose names
#'   start with \code{"c_"} (0/1 recommended).
#' @param preferred_coders Character vector of coder names, ordered from most-
#'   to least-preferred. Used to select which coder's excerpts are retained per
#'   transcript (\code{media_title}).
#' @param output_type One of \code{"tibble"}, \code{"kable"}, or
#'   \code{"datatable"}; controls the return type. Default is \code{"tibble"}.
#' @param include_zero Logical; if \code{TRUE}, keep codes with zero total
#'   applications. Default \code{FALSE}.
#'
#' @details
#' For each transcript (\code{media_title}), the function keeps \emph{all}
#' excerpts from the most-preferred coder present in \code{preferred_coders}
#' and drops rows by other coders. It then:
#' \enumerate{
#' \item Computes \code{total_preferred_coder}: the sum of 1s for each code
#'   across the retained excerpts.
#' \item Computes \code{transcript_count}: the number of distinct transcripts
#'   in which a code appears at least once, and \code{transcript_proportion}:
#'   that count divided by the number of transcripts retained after filtering.
#' }
#'
#' Uses dplyr's \code{.by} argument (requires dplyr >= 1.1.0). If you need
#' compatibility with older dplyr versions, replace that step with an explicit
#' \code{group_by(media_title)} + \code{filter()}.
#'
#' @return
#' If \code{output_type = "tibble"}, a tibble with columns:
#' \itemize{
#' \item \code{Code}: the code variable name (a \code{c_*} column).
#' \item \code{total_preferred_coder}: total number of applications (sum of 1s).
#' \item \code{transcript_count}: number of transcripts with at least one 1.
#' \item \code{transcript_proportion}: proportion of transcripts with at least one 1
#'   (0–1, rounded to 2 decimals).
#' }
#' If \code{"kable"}, a \code{knitr_kable} object; if \code{"datatable"}, a
#' \code{DT::datatable} widget.
#'
#' @examples
#' set.seed(1)
#' toy <- data.frame(
#'   media_title     = rep(paste0("T", 1:3), each = 4),
#'   excerpt_creator = rep(c("Ann","Bob","Ann","Cara"), 3),
#'   c_help          = rbinom(12, 1, .4),
#'   c_harm          = rbinom(12, 1, .2),
#'   stringsAsFactors = FALSE
#' )
#' # Prefer Ann over Bob over Cara
#' count_codes(toy, preferred_coders = c("Ann","Bob","Cara"))
#'
#' # Keep zero-count codes
#' count_codes(toy, c("Ann","Bob","Cara"), include_zero = TRUE)
#'
#' # Return a knitr::kable table
#' count_codes(toy, c("Ann","Bob","Cara"), output_type = "kable")
#'
#' @export
count_codes <- function(excerpts, preferred_coders,
                        output_type = c("tibble", "kable", "datatable"),
                        include_zero = FALSE) {
  output_type <- match.arg(output_type)

  if (!is.data.frame(excerpts)) {
    stop("`excerpts` must be a data frame.")
  }

  if (missing(preferred_coders) || is.null(preferred_coders)) {
    stop("Please provide a vector of preferred_coders in order of preference.")
  }

  # Identify code columns — all vars starting with "c_"
  code_columns <- grep("^c_", colnames(excerpts), value = TRUE)

  # Keep ALL excerpts from the preferred coder for each transcript
  excerpts_clean <- excerpts %>%
    dplyr::mutate(coder_rank = match(excerpt_creator, preferred_coders)) %>%
    dplyr::filter(!is.na(coder_rank)) %>%
    dplyr::filter(coder_rank == min(coder_rank, na.rm = TRUE), .by = media_title)

  # ----- Code counts -----
  total_counts <- excerpts_clean %>%
    dplyr::select(all_of(code_columns)) %>%
    tidyr::pivot_longer(cols = everything(), names_to = "Code", values_to = "Applied") %>%
    dplyr::group_by(Code) %>%
    dplyr::summarise(total_preferred_coder = sum(Applied, na.rm = TRUE), .groups = "drop")

  # ----- Transcript counts -----
  total_transcripts <- dplyr::n_distinct(excerpts_clean$media_title)

  transcript_counts <- excerpts_clean %>%
    dplyr::select(media_title, all_of(code_columns)) %>%
    tidyr::pivot_longer(cols = -media_title, names_to = "Code", values_to = "Applied") %>%
    dplyr::group_by(media_title, Code) %>%
    dplyr::summarise(any_applied = any(Applied == 1, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(any_applied) %>%
    dplyr::count(Code, name = "transcript_count") %>%
    dplyr::mutate(transcript_proportion = round(transcript_count / total_transcripts, 2))

  # ----- Merge -----
  combined_final <- total_counts %>%
    dplyr::left_join(transcript_counts, by = "Code") %>%
    dplyr::mutate(
      transcript_count = dplyr::coalesce(transcript_count, 0L),
      transcript_proportion = dplyr::coalesce(transcript_proportion, 0)
    )

  # Optionally filter out zeros
  if (!include_zero) {
    combined_final <- combined_final %>%
      dplyr::filter(total_preferred_coder > 0)
  }

  combined_final <- combined_final %>%
    dplyr::arrange(desc(total_preferred_coder))

  # ----- Output -----
  if (output_type == "kable") {
    return(knitr::kable(combined_final, caption = "Code Counts (Preferred Coders Only)"))
  } else if (output_type == "datatable") {
    return(DT::datatable(
      combined_final,
      caption = "Code Counts (Preferred Coders Only)",
      options = list(pageLength = 30, autoWidth = TRUE)
    ))
  } else {
    return(combined_final)
  }
}
