#' Clean and harmonize coded excerpts, keep preferred coder, and attach a codebook
#'
#' Reads (optionally) an Excel file of excerpts, normalizes column names,
#' converts Dedoose-style code columns (names starting with \code{code} and
#' ending with \code{applied}) to binary \code{c_*} variables, and keeps only
#' the most-preferred coder's excerpts per transcript. Also applies
#' human-readable variable labels and attaches a simple codebook as an attribute.
#'
#' @param excerpts A data frame of excerpts \emph{or} a single-length character
#'   file path to an \code{.xlsx} file. If a path is provided, it is read via
#'   \code{readxl::read_xlsx(col_types = "text")} and then processed.
#' @param preferred_coders Character vector of coder names ordered from most- to
#'   least-preferred. Used to select the coder to retain within each transcript
#'   (\code{media_title}).
#'
#' @details
#' \strong{Name normalization}
#' \itemize{
#' \item All column names are converted to lowercase and spaces are replaced with
#'   underscores.
#' \item If present, \code{excerpt_copy} is renamed to \code{excerpt}.
#' \item Columns with names ending in \code{"range"} or \code{"weight"} are dropped.
#' }
#'
#' \strong{Code columns -> binary \code{c_*}}
#' \itemize{
#' \item Columns whose names match \code{^code.*applied$} are interpreted as
#'   Dedoose code flags. Their contents are coerced using
#'   \code{tolower(trimws(x)) == "true"} and then cast to numeric
#'   (TRUE -> 1, FALSE -> 0; \code{NA} remains \code{NA}).
#' \item Those columns are then renamed: remove the leading \code{code} token and the
#'   trailing \code{applied}, sanitize punctuation/spaces to underscores, and prefix with
#'   \code{"c_"} (e.g., \code{code "Help" applied} -> \code{c_help}).
#' \item As a safeguard, \emph{all} columns starting with \code{c_} are coerced to
#'   numeric 0/1 via \code{as.numeric(as.logical(x))}.
#' }
#'
#' \strong{Preferred coder filtering}
#' \itemize{
#' \item Within each \code{media_title}, the function computes the coder's rank
#'   via \code{match(excerpt_creator, preferred_coders)} and retains all rows
#'   from the single most-preferred coder present (ties are broken by first match
#'   in \code{preferred_coders}; other coders' rows are removed).
#' }
#'
#' \strong{Labels and codebook}
#' \itemize{
#' \item Applies descriptive labels to common columns (e.g., \code{media_title},
#'   \code{excerpt_creator}, \code{excerpt_date}, \code{excerpt}, etc.) using
#'   \code{labelled::var_label()}.
#' \item Auto-labels every \code{c_*} variable with its own name.
#' \item Builds a simple codebook with variables, labels, and types and
#'   attaches it as \code{attr(x, "codebook")}.
#' }
#'
#' @return A base \code{data.frame} with:
#' \itemize{
#' \item normalized column names,
#' \item binary \code{c_*} columns (0/1; may contain \code{NA}),
#' \item only the preferred coder's excerpts per \code{media_title},
#' \item attached variable labels (via \code{labelled}).
#' }
#' Additionally, a \code{data.frame} codebook is attached as
#' \code{attr(result, "codebook")} with columns \code{variable}, \code{label},
#' and \code{type}.
#'
#' @section Requirements:
#' The input must contain at least \code{media_title} and \code{excerpt_creator}.
#' If you want automatic code detection, code variables should follow the pattern
#' \code{^code.*applied$} prior to renaming.
#'
#' @examples
#' \dontrun{
#' # From a data frame
#' df <- data.frame(
#'   media_title = c("T1","T1","T2"),
#'   excerpt_creator = c("Ann","Bob","Ann"),
#'   `code "Help" applied` = c("TRUE","false", NA),
#'   code_harm_applied = c("true","true","false"),
#'   stringsAsFactors = FALSE
#' )
#' clean <- clean_data(df, preferred_coders = c("Ann","Bob"))
#' attr(clean, "codebook")  # access the attached codebook
#'
#' # From a file path (all columns initially read as text)
#' # clean <- clean_data("path/to/excerpts.xlsx", preferred_coders = c("Ann","Bob","Cara"))
#' }
#'
#' @importFrom readxl read_xlsx
#' @importFrom dplyr mutate filter group_by slice_min ungroup select rename rename_with
#' @importFrom stringr str_replace
#' @importFrom labelled var_label
#' @importFrom rlang .data sym
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

  # Make variable names lower case and replace spaces with underscores
  names(excerpts) <- gsub(" ", "_", tolower(names(excerpts)))

  # Rename "excerpt_copy" to "excerpt" for consistency (use tidy-eval to avoid NSE NOTE)
  if ("excerpt_copy" %in% names(excerpts)) {
    excerpts <- dplyr::rename(excerpts, excerpt = !!rlang::sym("excerpt_copy"))
  }

  # Drop columns ending with range or weight
  drop_cols <- grep("(range|weight)$", colnames(excerpts), value = TRUE)
  if (length(drop_cols) > 0) {
    excerpts <- dplyr::select(excerpts, -dplyr::all_of(drop_cols))
  }

  # Identify code columns (starting with "code" and ending with "applied")
  code_cols <- grep("^code.*applied$", colnames(excerpts), value = TRUE)

  # Convert code columns from text -> logical -> numeric (0/1)
  for (col in code_cols) {
    excerpts[[col]] <- tolower(trimws(excerpts[[col]])) == "true"
    excerpts[[col]] <- as.numeric(excerpts[[col]])  # TRUE->1, FALSE->0
  }

  # Rename code columns: clean up names and add "c_" prefix
  excerpts <- dplyr::rename_with(
    excerpts,
    ~ {
      new <- stringr::str_replace(., "^code[:_ ]*", "")        # remove "code" + punctuation
      new <- stringr::str_replace(new, "[:_ ]*applied$", "")   # remove trailing "applied"
      new <- gsub('[\"\']', "", new)                           # remove quotes
      new <- gsub("[[:punct:] ]+", "_", new)                   # replace punctuation/spaces -> "_"
      new <- gsub("_+", "_", new)                              # collapse multiple underscores
      new <- trimws(new, whitespace = "_")                     # strip leading/trailing "_"
      paste0("c_", new)                                        # add "c_" prefix
    },
    .cols = code_cols
  )

  # Save the final code column names
  final_code_cols <- grep("^c_", colnames(excerpts), value = TRUE)

  # Ensure all final code columns are numeric (double-safety)
  excerpts[final_code_cols] <- lapply(
    excerpts[final_code_cols],
    function(x) as.numeric(as.logical(x))
  )

  # Filter to preferred coder per media_title (use .data pronoun to avoid NSE NOTES)
  excerpts <- excerpts %>%
    dplyr::mutate(coder_rank = match(.data$excerpt_creator, preferred_coders)) %>%
    dplyr::filter(!is.na(.data$coder_rank)) %>%
    dplyr::group_by(.data$media_title) %>%
    dplyr::slice_min(.data$coder_rank, with_ties = FALSE) %>%
    dplyr::ungroup()

  # Add variable labels
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

  # Auto-label all code columns with their variable names
  for (col in final_code_cols) {
    labelled::var_label(excerpts[[col]]) <- col
  }

  # Build codebook and ATTACH as an attribute (no global assignment)
  codebook <- data.frame(
    variable = names(excerpts),
    label = vapply(names(excerpts), function(col) {
      lbl <- labelled::var_label(excerpts[[col]])
      if (is.null(lbl) || lbl == "") col else lbl
    }, FUN.VALUE = character(1)),
    type = vapply(excerpts, function(x) class(x)[1], FUN.VALUE = character(1)),
    stringsAsFactors = FALSE
  )
  attr(excerpts, "codebook") <- codebook

  # Force return as base data.frame (attributes preserved)
  excerpts <- as.data.frame(excerpts, stringsAsFactors = FALSE)
  return(excerpts)
}
