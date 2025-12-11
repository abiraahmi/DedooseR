create_cooccur_matrix <- function(excerpts,
                                  min_bold = 10,
                                  scale = c("count", "proportion"),
                                  output = c("kable", "tibble", "data.frame")) {
  scale <- match.arg(scale)
  output <- match.arg(output)

  if (!"media_title" %in% names(excerpts)) {
    stop("`excerpts` must contain a `media_title` column.")
  }

  # identify code columns
  code_columns <- grep("^c_", names(excerpts), value = TRUE)
  if (length(code_columns) == 0) {
    stop("No code columns found (columns must start with 'c_').")
  }

  # collapse to transcript-level presence
  code_by_transcript <- excerpts %>%
    dplyr::group_by(media_title) %>%
    dplyr::summarise(across(all_of(code_columns),
                            ~ as.integer(any(. == 1))),
                     .groups = "drop")

  # build co-occurrence matrix
  code_matrix <- as.matrix(code_by_transcript[,-1])
  coccur_matrix <- t(code_matrix) %*% code_matrix

  # drop rows/cols with all 0s
  keep <- which(rowSums(coccur_matrix) > 0 | colSums(coccur_matrix) > 0)
  coccur_matrix <- coccur_matrix[keep, keep, drop = FALSE]

  # scale
  if (scale == "proportion") {
    marginals <- diag(coccur_matrix) # how often each code appears
    coccur_matrix <- sweep(coccur_matrix, 2, marginals, "/")
    coccur_matrix <- round(coccur_matrix, 3)
  }

  # convert to df
  coccur_df <- as.data.frame(coccur_matrix)
  rownames(coccur_df) <- rownames(coccur_matrix)

  if (output == "tibble") {
    return(tibble::as_tibble(coccur_df, rownames = "code"))
  }

  if (output == "data.frame") {
    return(coccur_df)
  }

  # otherwise format for kable
  if (scale == "count") {
    coccur_df_fmt <- coccur_df %>%
      dplyr::mutate(across(
        everything(),
        ~ ifelse(. >= min_bold,
                 kableExtra::cell_spec(., bold = TRUE),
                 as.character(.))
      ))
  } else {
    coccur_df_fmt <- coccur_df %>%
      dplyr::mutate(across(
        everything(),
        ~ ifelse(. >= min_bold,
                 kableExtra::cell_spec(sprintf("%.3f", .), bold = TRUE),
                 sprintf("%.3f", .))
      ))
  }
  rownames(coccur_df_fmt) <- rownames(coccur_df)

  knitr::kable(coccur_df_fmt,
               format = "html",
               escape = FALSE,
               caption = paste(
                 "Code Co-occurrence Matrix (Within Transcript)",
                 ifelse(scale == "count", "Counts", "Proportions")
               ),
               align = "c") %>%
    kableExtra::kable_styling(full_width = FALSE,
                              bootstrap_options = c("striped", "hover", "condensed"))
}

# Testing

# Load libraries
library(DedooseR)
library(tidyverse)
library(dplyr)
library(readxl)

# Clean data
excerpts <- read_xlsx("inst/raw_data/test_data.xlsx")
preferred_coders <- c("a", "l", "i", "r", "s", "v", "c", "n", "k")
excerpts <- clean_data(excerpts, preferred_coders)

# Create matrix
cooccur_matrix <- create_cooccur_matrix(excerpts, min_bold = 0.4, scale = "count", output = "data.frame")

