merge_codes <- function(data, merges) {
  # data: a data.frame or tibble
  # merges: a named list, where names are new vars, values are character vectors of old vars

  for (new_var in names(merges)) {
    from_vars <- merges[[new_var]]

    # check inputs
    if (!all(from_vars %in% names(data))) {
      stop(paste("Some variables for", new_var, "not found in dataset"))
    }

    # create the new variable: TRUE if any of the from_vars are TRUE
    data[[new_var]] <- apply(data[from_vars], 1, function(x) any(x == TRUE, na.rm = TRUE))

    # drop the old vars
    data <- data[, !names(data) %in% from_vars, drop = FALSE]
  }

  return(data)
}

# Test
# Load libraries
library(DedooseR)
library(tidyverse)
library(dplyr)
library(readxl)

# Clean data
filepath <- read_xlsx("inst/raw_data/test_data.xlsx")
preferred_coders <- c("a", "l", "i", "r", "s", "v", "c", "n", "k")
clean_data <- clean_data(filepath, preferred_coders)
excerpts <- clean_data$data
codebook <- clean_data$codebook

# Merge codes
excerpts <- merge_codes(excerpts, list(
  c_belonging_connectedness = c(
    "c_sense_of_belonging", "c_sense_of_belonging_others", "c_sense_of_belonging_self",
    "c_sense_of_connectedness", "c_sense_of_connectedness_family",
    "c_sense_of_connectedness_peers", "c_sense_of_connectedness_school_community",
    "c_sense_of_connectedness_staff"
  ),
  c_suicide_comfort = c("c__suicide_comfort_directing_change", "c__suicide_comfort_general")
))

