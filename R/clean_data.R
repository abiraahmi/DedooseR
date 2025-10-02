#' Clean and prepare Dedoose excerpt data
#'
#' This function takes excerpt data exported from Dedoose (either as a data frame
#' or as a file path to an Excel file) and prepares it for analysis by:
#' \itemize{
#'   \item Standardizing variable names to lowercase
#'   \item Renaming `"excerpt copy"` to `"excerpt"`
#'   \item Dropping columns ending with `"range"` or `"weight"`
#'   \item Converting code columns (prefixed with `"code: "`) to logical variables
#'   \item Renaming code columns to remove prefixes/suffixes
#'   \item Filtering excerpts to retain only the coder with the highest preference
#'   \item Adding variable labels for interpretability
#' }
#'
#' In addition to returning a cleaned data frame, the function creates a
#' `codebook` object in the global environment that contains metadata on all
#' variables in the dataset, including variable names, labels, and types.
#'
#' @param excerpts A data frame of excerpts or a file path (character string) to
#'   an Excel file exported from Dedoose.
#' @param preferred_coders A character vector giving the coders in order of
#'   preference. Excerpts will be filtered so that, for each media title,
#'   only excerpts from the highest-ranked coder are retained.
#'
#' @return A cleaned \code{data.frame} containing excerpts and associated
#'   variables.
#'
#' @details
#' The function also creates a \code{codebook} object in the global environment
#' with three columns:
#' \itemize{
#'   \item \code{variable}: the variable name
#'   \item \code{label}: the variable label (if assigned, otherwise the name)
#'   \item \code{type}: the storage type/class of the variable
#' }
#'
#' @examples
#' \dontrun{
#' # Example usage
#' excerpts <- readxl::read_xlsx("inst/raw_data/test_data_manipulated.xlsx")
#' preferred_coders <- c("s", "r", "l", "a")
#' cleaned <- clean_data(excerpts, preferred_coders)
#'
#' # View codebook
#' head(codebook)
#' }
#'
#' @export
clean_data <- function(excerpts, preferred_coders) {
  if (missing(preferred_coders) || is.null(preferred_coders)) {
    stop("Please provide a vector of preferred_coders in order of preference.")
  }

  # If a filepath was passed, read it in
  if (is.character(excerpts) && length(excerpts) == 1) {
    if (!file.exists(excerpts)) {
      stop("The specified file does not exist.")
    }
    excerpts <- readxl::read_xlsx(excerpts, col_types = "text")
  }

  # Make variable names lower case
  names(excerpts) <- tolower(names(excerpts))

  # Rename "excerpt copy" to "excerpt" for consistency
  if ("excerpt copy" %in% names(excerpts)) {
    excerpts <- dplyr::rename(excerpts, excerpt = `excerpt copy`)
  }

  # Drop columns ending with range or weight
  drop_cols <- grep("(range|weight)$", colnames(excerpts), value = TRUE)
  if (length(drop_cols) > 0) {
    excerpts <- dplyr::select(excerpts, -dplyr::all_of(drop_cols))
  }

  # Identify code columns (starting with "code: ")
  code_cols <- grep("^code: ", colnames(excerpts), value = TRUE)

  # Convert code columns from text to logical: TRUE if "true" (case insensitive)
  for (col in code_cols) {
    excerpts[[col]] <- stringr::str_to_lower(excerpts[[col]]) == "true"
  }

  # Rename code columns: remove prefix "code: " and suffix " applied"
  excerpts <- dplyr::rename_with(
    excerpts,
    ~ stringr::str_replace(., "^code: ", ""),
    .cols = dplyr::starts_with("code: ")
  )
  excerpts <- dplyr::rename_with(
    excerpts,
    ~ stringr::str_replace(., regex(" applied$", ignore_case = TRUE), ""),
    .cols = dplyr::ends_with("applied")
  )

  # Save the final code column names (after renaming)
  renamed_code_cols <- stringr::str_replace(code_cols, "^code: ", "")
  renamed_code_cols <- stringr::str_replace(renamed_code_cols, regex(" applied$", ignore_case = TRUE), "")
  final_code_cols <- intersect(colnames(excerpts), renamed_code_cols)

  # Filter to preferred coder per media title
  excerpts <- excerpts %>%
    dplyr::mutate(coder_rank = match(`excerpt creator`, preferred_coders)) %>%
    dplyr::filter(!is.na(coder_rank)) %>%
    dplyr::group_by(`media title`) %>%
    dplyr::slice_min(coder_rank, with_ties = FALSE) %>%
    dplyr::ungroup()

  # Add variable labels
  if ("media title" %in% names(excerpts))
    labelled::var_label(excerpts$`media title`) <- "transcript/media title"
  if ("excerpt creator" %in% names(excerpts))
    labelled::var_label(excerpts$`excerpt creator`) <- "coder who created the excerpt"
  if ("excerpt date" %in% names(excerpts))
    labelled::var_label(excerpts$`excerpt date`) <- "date excerpt was created"
  if ("excerpt" %in% names(excerpts))
    labelled::var_label(excerpts$excerpt) <- "full text of excerpt"
  if ("codes applied combined" %in% names(excerpts))
    labelled::var_label(excerpts$`codes applied combined`) <- "all codes applied to excerpt"
  if ("resource date" %in% names(excerpts))
    labelled::var_label(excerpts$`resource date`) <- "date media/transcript was added to Dedoose"
  if ("coder rank" %in% names(excerpts))
    labelled::var_label(excerpts$`coder rank`) <- "rank of coder, according to listed coder preference"

  # Auto-label all code columns with their variable names
  for (col in final_code_cols) {
    labelled::var_label(excerpts[[col]]) <- col
  }

  # Save codebook in global environment
  codebook <- data.frame(
    variable = names(excerpts),
    label = sapply(names(excerpts), function(col) {
      lbl <- labelled::var_label(excerpts[[col]])
      if (is.null(lbl) || lbl == "") col else lbl
    }),
    type = sapply(excerpts, function(x) class(x)[1]), # take first class if multiple
    stringsAsFactors = FALSE
  )
  assign("codebook", codebook, envir = .GlobalEnv)
  # --------------------------------------------------------

  # Force return as base data.frame
  excerpts <- as.data.frame(excerpts, stringsAsFactors = FALSE)

  return(excerpts)
}
