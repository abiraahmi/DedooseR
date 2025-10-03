#' Clean and prepare excerpt-level coding data
#'
#' This function cleans a dataset of coded excerpts exported from Dedoose (or a
#' similar qualitative coding platform). It standardizes variable names,
#' converts code columns to logicals, filters to the preferred coder per
#' transcript, and assigns variable labels. A codebook is also generated and
#' returned along with the cleaned dataset.
#'
#' @param excerpts A data frame containing raw excerpt-level coding data.
#'   Must include at least the columns `media_title` and `excerpt_creator`.
#' @param preferred_coders A character vector of coder IDs in order of
#'   preference (e.g., \code{c("a", "l", "i", "r")}).
#'
#' @details
#' The cleaning steps performed are:
#' \enumerate{
#'   \item Standardize all column names to lowercase and replace spaces with underscores.
#'   \item Rename \code{excerpt_copy} to \code{excerpt} if present.
#'   \item Drop columns ending in "range" or "weight".
#'   \item Identify code columns (names starting with "code" and ending with "applied").
#'   \item Convert code columns from text to logical (\code{TRUE} if "true", otherwise \code{FALSE}).
#'   \item Clean and rename code column names by stripping prefixes/suffixes and
#'         adding a \code{c_} prefix.
#'   \item Filter to the preferred coder for each \code{media_title}, based on the
#'         order of \code{preferred_coders}.
#'   \item Apply human-readable variable labels for common metadata fields and
#'         all code columns.
#'   \item Generate a codebook (variable, label, and type).
#' }
#'
#' @return A list with two elements:
#' \describe{
#'   \item{\code{data}}{The cleaned data frame of excerpts with standardized columns,
#'   filtered to the preferred coder, and with labeled variables.}
#'   \item{\code{codebook}}{A data frame containing variable names, labels, and types.}
#' }
#'
#' @examples
#' \dontrun{
#' # Example usage
#' raw_excerpts <- readxl::read_xlsx("inst/raw_data/test_data.xlsx")
#' preferred <- c("a", "l", "i", "r", "s")
#' result <- clean_data(raw_excerpts, preferred)
#'
#' # Extract cleaned data
#' cleaned <- result$data
#'
#' # View the generated codebook
#' head(result$codebook)
#' }
#'
#' @importFrom dplyr rename select all_of rename_with mutate filter group_by ungroup
#' @importFrom labelled var_label
#' @export
clean_data <- function(excerpts, preferred_coders) {
  if (missing(preferred_coders) || is.null(preferred_coders)) {
    stop("Please provide a vector of preferred_coders in order of preference.")
  }

  # --- (1) Standardize column names ---
  names(excerpts) <- tolower(names(excerpts))
  names(excerpts) <- gsub(" ", "_", names(excerpts))

  # --- (2) Rename "excerpt_copy" → "excerpt" if present ---
  if ("excerpt_copy" %in% names(excerpts)) {
    excerpts <- dplyr::rename(excerpts, excerpt = excerpt_copy)
  }

  # --- (3) Drop columns ending with "range" or "weight" ---
  drop_cols <- grep("(range|weight)$", names(excerpts), value = TRUE, ignore.case = TRUE)
  excerpts <- dplyr::select(excerpts, -dplyr::all_of(drop_cols))

  # --- (4) Identify code columns ---
  code_cols <- grep("^code.*applied$", names(excerpts), value = TRUE, ignore.case = TRUE)

  # --- (5) Convert code columns to logical ---
  for (col in code_cols) {
    excerpts[[col]] <- tolower(trimws(excerpts[[col]])) == "true"
  }

  # --- (6) Clean and rename code columns ---
  clean_names <- code_cols
  clean_names <- gsub("^code[:_ ]*", "", clean_names, ignore.case = TRUE)
  clean_names <- gsub("_*applied$", "", clean_names, ignore.case = TRUE)
  clean_names <- gsub("[^a-z0-9]+", "_", clean_names)
  clean_names <- paste0("c_", clean_names)

  excerpts <- dplyr::rename_with(excerpts,
                                 .cols = all_of(code_cols),
                                 .fn = ~ clean_names)

  # --- (7) Save final code column names ---
  final_code_cols <- clean_names

  # --- (8) Filter to preferred coder per media_title ---
  excerpts <- excerpts %>%
    dplyr::mutate(coder_rank = match(excerpt_creator, preferred_coders)) %>%
    dplyr::filter(!is.na(coder_rank)) %>%
    dplyr::group_by(media_title) %>%
    dplyr::filter(coder_rank == min(coder_rank)) %>%
    dplyr::ungroup()

  # --- (9) Add variable labels ---
  if ("media_title" %in% names(excerpts))
    labelled::var_label(excerpts$media_title) <- "transcript/media title"
  if ("excerpt_creator" %in% names(excerpts))
    labelled::var_label(excerpts$excerpt_creator) <- "coder who created the excerpt"
  if ("excerpt_date" %in% names(excerpts))
    labelled::var_label(excerpts$excerpt_date) <- "date excerpt was created"
  if ("excerpt" %in% names(excerpts))
    labelled::var_label(excerpts$excerpt) <- "full text of excerpt"
  if ("codes_applied_combined" %in% names(excerpts))
    labelled::var_label(excerpts$codes_applied_combined) <- "all codes applied to excerpt"
  if ("resource_date" %in% names(excerpts))
    labelled::var_label(excerpts$resource_date) <- "date media/transcript was added to Dedoose"
  if ("coder_rank" %in% names(excerpts))
    labelled::var_label(excerpts$coder_rank) <- "rank of coder, according to listed coder preference"

  # Auto-label code columns
  for (col in final_code_cols) {
    labelled::var_label(excerpts[[col]]) <- col
  }

  # --- (10) Create codebook ---
  codebook <- data.frame(
    variable = names(excerpts),
    label = sapply(names(excerpts), function(col) {
      lbl <- labelled::var_label(excerpts[[col]])
      if (is.null(lbl) || lbl == "") col else lbl
    }),
    type = sapply(excerpts, function(x) class(x)[1]),
    stringsAsFactors = FALSE
  )

  # Return both cleaned data + codebook
  return(list(
    data = excerpts,
    codebook = codebook
  ))
}
