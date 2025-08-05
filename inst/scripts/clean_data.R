clean_data <- function(filepath, preferred_coders) {
  if (!base::file.exists(filepath)) {
    stop("The specified file does not exist.")
  }
  if (missing(preferred_coders) || is.null(preferred_coders)) {
    stop("Please provide a vector of preferred_coders in order of preference.")
  }

  # Read all columns as text to avoid type guessing warnings
  excerpts <- readxl::read_xlsx(filepath, col_types = "text")

  # Drop columns ending with Range or Weight
  drop_cols <- base::grep("(Range|Weight)$", base::colnames(excerpts), value = TRUE)
  excerpts <- dplyr::select(excerpts, -dplyr::all_of(drop_cols))

  # Identify code columns (starting with "Code: ")
  code_cols <- base::grep("^Code: ", base::colnames(excerpts), value = TRUE)

  # Convert code columns from text to logical: TRUE if "true" (case insensitive), else FALSE
  for (col in code_cols) {
    vals <- excerpts[[col]]
    excerpts[[col]] <- tolower(vals) == "true"
  }

  # Rename code columns: remove prefix "Code: " and suffix " Applied"
  excerpts <- dplyr::rename_with(excerpts,
                                 ~ stringr::str_replace(., "^Code: ", ""),
                                 dplyr::starts_with("Code:"))
  excerpts <- dplyr::rename_with(excerpts,
                                 ~ stringr::str_replace(., " Applied$", ""),
                                 dplyr::ends_with("Applied"))

  # Filter to preferred coder per Media Title
  excerpts <- excerpts %>%
    dplyr::mutate(coder_rank = base::match(`Excerpt Creator`, preferred_coders)) %>%
    dplyr::filter(!is.na(coder_rank)) %>%
    dplyr::group_by(`Media Title`) %>%
    dplyr::filter(coder_rank == min(coder_rank)) %>%
    dplyr::ungroup()

  return(excerpts)
}


