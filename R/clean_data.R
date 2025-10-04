#' Clean and standardize coded qualitative excerpts
#'
#' @description
#' Cleans, standardizes, and labels a dataset of coded qualitative excerpts
#' (e.g., exported from Dedoose or a similar platform). The function performs
#' the following key operations:
#'
#' 1. Standardizes column names to lowercase and replaces spaces with underscores.
#' 2. Renames `excerpt_copy` to `excerpt` if present.
#' 3. Removes columns ending with "range" or "weight".
#' 4. Converts all code columns (those starting with "code" and ending with "applied")
#'    into logical (`TRUE`/`FALSE`) variables.
#' 5. Cleans and renames code variables using a `c_` prefix.
#' 6. Filters the dataset to retain excerpts from a preferred coder (based on rank order).
#' 7. Adds descriptive variable labels using the `labelled` package.
#' 8. Optionally renames or relabels variables.
#' 9. Removes columns that are entirely `NA`.
#' 10. Generates a codebook summarizing variable names, labels, and types.
#'
#' @param excerpts A data frame containing coded excerpts (e.g., exported from Dedoose).
#' @param preferred_coders A character vector listing coders in order of preference (e.g.,
#'   `c("coder1", "coder2")`). Used to select the preferred version of each excerpt.
#' @param rename_vars Optional named list or vector of variables to rename, in the format
#'   `c(oldname = "newname")`.
#' @param relabel_vars Optional named list of new variable labels, in the format
#'   `list(varname = "new label")`.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{data}{The cleaned and filtered excerpt dataset.}
#'   \item{codebook}{A data frame listing variable names, labels, and data types.}
#' }
#'
#' @details
#' The function standardizes and cleans a coded excerpt dataset while preserving
#' coder priority and metadata. It returns both the cleaned dataset and its
#' associated codebook for easy reference or export.
#'
#' @examples
#' # Example data frame
#' excerpts <- data.frame(
#'   media_title = c("Transcript 1", "Transcript 1", "Transcript 2"),
#'   excerpt_creator = c("alice", "bob", "alice"),
#'   excerpt_copy = c("It was a good day.", "It was a good day.", "We learned a lot."),
#'   code_emotion_applied = c("TRUE", "FALSE", "TRUE"),
#'   code_learning_applied = c("FALSE", "TRUE", "TRUE"),
#'   weight = c(1, 2, 3)
#' )
#'
#' # Preferred coders (in order)
#' preferred_coders <- c("alice", "bob")
#'
#' # Run cleaning function
#' result <- clean_data(
#'   excerpts = excerpts,
#'   preferred_coders = preferred_coders,
#'   relabel_vars = list(c_emotion = "Emotion expressed", c_learning = "Learning mentioned")
#' )
#'
#' # Extract cleaned data and codebook
#' cleaned_excerpts <- result$data
#' codebook <- result$codebook
#'
#' # View results
#' head(cleaned_excerpts)
#' head(codebook)
#'
#' @importFrom dplyr rename select all_of rename_with mutate filter group_by ungroup any_of where
#' @importFrom labelled var_label
#' @export
clean_data <- function(excerpts, preferred_coders,
                       rename_vars = NULL,
                       relabel_vars = NULL) {
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

  excerpts <- dplyr::rename_with(
    excerpts,
    .cols = dplyr::all_of(code_cols),
    .fn = ~ clean_names
  )

  # --- (7) Save final code column names ---
  final_code_cols <- clean_names

  # --- (8) Filter to preferred coder per media_title ---
  excerpts <- excerpts %>%
    dplyr::mutate(coder_rank = match(excerpt_creator, preferred_coders)) %>%
    dplyr::filter(!is.na(coder_rank)) %>%
    dplyr::group_by(media_title) %>%
    dplyr::filter(coder_rank == min(coder_rank)) %>%
    dplyr::ungroup()

  # --- (9) Add default variable labels ---
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

  # --- (10) Optional variable renaming ---
  if (!is.null(rename_vars)) {
    excerpts <- dplyr::rename(excerpts, !!!rename_vars)
  }

  # --- (11) Optional variable relabeling ---
  if (!is.null(relabel_vars)) {
    for (col in names(relabel_vars)) {
      if (col %in% names(excerpts)) {
        labelled::var_label(excerpts[[col]]) <- relabel_vars[[col]]
      }
    }
  }

  # --- (12) Drop columns that are entirely NA ---
  excerpts <- dplyr::select(excerpts, where(~ !all(is.na(.))))

  # --- (13) Create codebook ---
  codebook <- data.frame(
    variable = names(excerpts),
    label = sapply(names(excerpts), function(col) {
      lbl <- labelled::var_label(excerpts[[col]])
      if (is.null(lbl) || lbl == "") col else lbl
    }),
    type = sapply(excerpts, function(x) class(x)[1]),
    stringsAsFactors = FALSE
  )

  # Return both outputs
  return(list(
    data = excerpts,
    codebook = codebook
  ))
}
