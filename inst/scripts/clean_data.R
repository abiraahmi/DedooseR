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

  # Rename "excerpt_copy" to "excerpt" for consistency
  if ("excerpt_copy" %in% names(excerpts)) {
    excerpts <- dplyr::rename(excerpts, excerpt = excerpt_copy)
  }

  # Drop columns ending with range or weight
  drop_cols <- grep("(range|weight)$", colnames(excerpts), value = TRUE)
  if (length(drop_cols) > 0) {
    excerpts <- dplyr::select(excerpts, -dplyr::all_of(drop_cols))
  }

  # Identify code columns (starting with "code" and ending with "applied")
  code_cols <- grep("^code.*applied$", colnames(excerpts), value = TRUE)

  # Convert code columns from text → logical → numeric (0/1)
  for (col in code_cols) {
    excerpts[[col]] <- tolower(trimws(excerpts[[col]])) == "true"
    excerpts[[col]] <- as.numeric(excerpts[[col]])  # TRUE→1, FALSE→0
  }

  # Rename code columns: clean up names and add "c_" prefix
  excerpts <- dplyr::rename_with(
    excerpts,
    ~ {
      new <- stringr::str_replace(., "^code[:_ ]*", "")        # remove "code" + punctuation
      new <- stringr::str_replace(new, "[:_ ]*applied$", "")   # remove trailing "applied"
      new <- gsub('[\"\']', "", new)                           # remove quotes
      new <- gsub("[[:punct:] ]+", "_", new)                   # replace punctuation/spaces → "_"
      new <- gsub("_+", "_", new)                              # collapse multiple underscores
      new <- trimws(new, whitespace = "_")                     # strip leading/trailing "_"
      paste0("c_", new)                                        # add "c_" prefix
    },
    .cols = code_cols
  )

  # Save the final code column names
  final_code_cols <- grep("^c_", colnames(excerpts), value = TRUE)

  # Ensure all final code columns are numeric (double-safety)
  excerpts[final_code_cols] <- lapply(excerpts[final_code_cols], function(x) as.numeric(as.logical(x)))

  # Filter to preferred coder per media_title
  excerpts <- excerpts %>%
    dplyr::mutate(coder_rank = match(excerpt_creator, preferred_coders)) %>%
    dplyr::filter(!is.na(coder_rank)) %>%
    dplyr::group_by(media_title) %>%
    dplyr::slice_min(coder_rank, with_ties = FALSE) %>%
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
