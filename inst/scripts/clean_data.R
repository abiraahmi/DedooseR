clean_data <- function(excerpts,
                       preferred_coders,
                       rename_vars = NULL,
                       relabel_vars = NULL,
                       output_path = NULL,
                       output_type = c("none", "xlsx", "dta")) {

  output_type <- match.arg(output_type)

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
                                 .cols = dplyr::all_of(code_cols),
                                 .fn = ~ clean_names)

  # --- (7) Save mapping of original to cleaned names ---
  name_map <- data.frame(
    clean = clean_names,
    original = code_cols,
    stringsAsFactors = FALSE
  )

  final_code_cols <- clean_names

  # --- (8) Filter to preferred coder per media_title ---
  excerpts <- excerpts %>%
    dplyr::mutate(coder_rank = match(excerpt_creator, preferred_coders)) %>%
    dplyr::filter(!is.na(coder_rank)) %>%
    dplyr::group_by(media_title) %>%
    dplyr::filter(coder_rank == min(coder_rank)) %>%
    dplyr::ungroup()

  # --- (9) Add default variable labels ---
  label_safe <- function(var, label) {
    if (var %in% names(excerpts))
      labelled::var_label(excerpts[[var]]) <<- label
  }

  label_safe("media_title", "transcript/media title")
  label_safe("excerpt_creator", "coder who created the excerpt")
  label_safe("excerpt_date", "date excerpt was created")
  label_safe("excerpt", "full text of excerpt")
  label_safe("codes_applied_combined", "all codes applied to excerpt")
  label_safe("resource_date", "date media/transcript was added to Dedoose")
  label_safe("coder_rank", "rank of coder, according to listed coder preference")

  # --- (10) Auto-label code columns ---
  for (i in seq_along(final_code_cols)) {
    col_clean <- name_map$clean[i]
    original <- name_map$original[i]
    label_pretty <- gsub("^code[:_ ]*", "", original, ignore.case = TRUE)
    label_pretty <- gsub("_*applied$", "", label_pretty, ignore.case = TRUE)
    label_pretty <- gsub("_", " ", label_pretty)
    label_pretty <- trimws(label_pretty)
    labelled::var_label(excerpts[[col_clean]]) <- label_pretty
  }

  # --- (11) Optional renaming ---
  if (!is.null(rename_vars)) {
    excerpts <- dplyr::rename(excerpts, !!!rename_vars)
  }

  # --- (12) Optional relabeling ---
  if (!is.null(relabel_vars)) {
    for (col in names(relabel_vars)) {
      if (col %in% names(excerpts)) {
        labelled::var_label(excerpts[[col]]) <- relabel_vars[[col]]
      }
    }
  }

  # --- (13) Drop columns that are entirely NA ---
  excerpts <- dplyr::select(excerpts, where(~ !all(is.na(.))))

  # --- (14) Create codebook ---
  codebook <- data.frame(
    variable = names(excerpts),
    label = sapply(names(excerpts), function(col) {
      lbl <- labelled::var_label(excerpts[[col]])
      if (is.null(lbl) || lbl == "") col else lbl
    }),
    type = sapply(excerpts, function(x) class(x)[1]),
    stringsAsFactors = FALSE
  )

  # --- (15) Save output if requested ---
  if (!is.null(output_path) && output_type != "none") {
    if (output_type == "xlsx") {
      openxlsx::write.xlsx(excerpts, output_path)
    } else if (output_type == "dta") {
      # Preserve logicals as TRUE/FALSE (not coerced to numeric)
      logical_cols <- sapply(excerpts, is.logical)
      for (col in names(excerpts)[logical_cols]) {
        attr(excerpts[[col]], "label") <- labelled::var_label(excerpts[[col]])
        class(excerpts[[col]]) <- "logical"
      }
      haven::write_dta(excerpts, output_path, version = 15)
    }
  }

  return(list(
    data = excerpts,
    codebook = codebook
  ))
}

# test_script.R

# Load libraries
library(DedooseR)
library(tidyverse)
library(dplyr)
library(readxl)
library(stringr)

# Clean data
filepath <- read_xlsx("inst/raw_data/test_data.xlsx")
preferred_coders <- c("a", "l", "i", "r", "s", "v", "c", "n", "k")
clean_data <- clean_data(filepath,
           preferred_coders,
           rename_vars = list(memo_destigmatization = "...274"),
           relabel_vars = list(title = "Memo: Destigmatization"))
excerpts <- clean_data$data
codebook <- clean_data$codebook
