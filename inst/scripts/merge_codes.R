recode <- function(data, recodes, relabel_vars = NULL) {
  # data: a data.frame or tibble
  # recodes: a named list, where names are new vars, values are character vectors of old vars

  all_from_vars <- c()

  for (new_var in names(recodes)) {
    from_vars <- recodes[[new_var]]

    # Check that all source variables exist
    if (!all(from_vars %in% names(data))) {
      stop(paste("Some variables for", new_var, "not found in dataset"))
    }

    # Create the new variable (TRUE if any source is TRUE)
    data[[new_var]] <- apply(data[from_vars], 1, function(x) any(x == TRUE, na.rm = TRUE))

    # Collect old vars for dropping (but NOT the new_var itself)
    all_from_vars <- c(all_from_vars, setdiff(from_vars, new_var))

    # Assign variable label if provided
    if (!is.null(relabel_vars) && new_var %in% names(relabel_vars)) {
      labelled::var_label(data[[new_var]]) <- relabel_vars[[new_var]]
    } else {
      # Default label: use the new variable name
      labelled::var_label(data[[new_var]]) <- new_var
    }
  }

  # Drop all old vars at once
  data_recode <- data[, !names(data) %in% all_from_vars, drop = FALSE]

  # Create codebook for merged data
  codebook_recode <- data.frame(
    variable = names(data_recode),
    label = sapply(names(data_recode), function(col) {
      lbl <- labelled::var_label(data_recode[[col]])
      if (is.null(lbl) || lbl == "") col else lbl
    }),
    type = sapply(data_recode, function(x) class(x)[1]),
    stringsAsFactors = FALSE
  )

  # Return both outputs
  return(list(
    data_recode = data_recode,
    codebook_recode = codebook_recode
  ))
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
data <- clean_data$data
codebook <- clean_data$codebook

# Merge codes
excerpts_recoded <- recode(data,
                        recodes = list(
  c_belonging_connectedness = c(
    "c_sense_of_belonging", "c_sense_of_belonging_others", "c_sense_of_belonging_self",
    "c_sense_of_connectedness", "c_sense_of_connectedness_family",
    "c_sense_of_connectedness_peers", "c_sense_of_connectedness_school_community",
    "c_sense_of_connectedness_staff"
  ),
  c_suicide_comfort = c("c__suicide_comfort_directing_change", "c__suicide_comfort_general")
),
relabel_vars = list(
  c_belonging_connectedness = "Sense of Belonging & Connectedness",
  c_suicide_comfort = "Suicide Comfort Conversing"
))

data_recode <- excerpts_recoded$data_recode
codebook_recode <- excerpts_recoded$codebook_recode
