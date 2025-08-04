# Script to test saturation function for package
# Abi
# July 31, 2025

# Load packages
library(tidyverse)
library(readxl)
library(writexl)
library(knitr)


#' Create Saturation Tracking Data
#'
#' @param data_path Path to the Excel file containing excerpt data
#' @param preferred_coders Vector of preferred coders in order of preference (based on reliability scores)
#' @return A data frame with code counts for Priority and Heterogeneity
#' @examples
#' # Example usage:
#' # result <- create_saturation_tracking(
#' #   data_path = "path/to/your/data.xlsx",
#' #   preferred_coders = c("coder1", "coder2", "coder3")
#' # )
create_saturation_tracking <- function(data_path = NULL,
                                     preferred_coders = NULL) {

  # Check if required parameters are provided
  if (is.null(data_path)) {
    stop("Please provide a data_path to your Excel file")
  }
  if (is.null(preferred_coders)) {
    stop("Please provide a vector of preferred_coders in order of preference")
  }

  # Load data
  excerpts <- read_xlsx(data_path, col_types = "text")

  excerpts_clean <- excerpts %>%
    # Drop columns you don't need
    select(-c(`Excerpt Range`, `Excerpt Date`, `Resource Creator`,
              `Resource Date`), -ends_with("Range"), -ends_with("Weight")) %>%
    # Filter so that for each transcript, we only keep one coder, in order
    # of preferences ranked above (based on reliability scores)
    mutate(coder_rank = match(`Excerpt Creator`, preferred_coders)) %>%
    filter(!is.na(coder_rank)) %>%
    group_by(`Media Title`) %>%
    filter(coder_rank == min(coder_rank)) %>%
    ungroup() %>%
    select(-coder_rank, -`Excerpt Creator`)


  # Step 1: Identify excerpts with Priority or Heterogeneity applied
  priority_excerpts <- excerpts %>%
    filter(`Code: Priority excerpt Applied` == "True") %>%
    pull(`Excerpt Copy`)

  hetero_excerpts <- excerpts %>%
    filter(`Code: Heterogeniety Applied` == "True") %>%
    pull(`Excerpt Copy`)

  # Step 2: Identify all code columns (excluding excerpt and subcodes)
  code_columns <- grep("^Code: ", colnames(excerpts), value = TRUE)
  code_columns <- setdiff(code_columns, c("Code: Priority excerpt Applied", "Code: Heterogeniety Applied"))

  # Step 3: Convert to long format and count occurrences separately
  long_codes <- excerpts %>%
    select(`Excerpt Copy`, all_of(code_columns)) %>%
    pivot_longer(cols = all_of(code_columns), names_to = "Code", values_to = "Applied") %>%
    filter(Applied == "True") %>%
    mutate(
      Priority_Applied = ifelse(`Excerpt Copy` %in% priority_excerpts, 1, 0),
      Heterogeneity_Applied = ifelse(`Excerpt Copy` %in% hetero_excerpts, 1, 0)
    ) %>%
    group_by(Code) %>%
    summarise(
      Priority_Count = sum(Priority_Applied),
      Heterogeneity_Count = sum(Heterogeneity_Applied),
      .groups = "drop"
    ) %>%
    arrange(desc(Priority_Count), desc(Heterogeneity_Count)) %>%
    mutate(Code = str_replace(Code, "^Code:", "")) %>% # Remove "Code:" only from the start
    mutate(Code = str_replace(Code, " Applied$", "")) %>% # Remove "Applied" from the end if it exists
    mutate(Code = str_replace(Code, "\\\\", "\\\\\\\\")) # Escape backslashes properly

  return(long_codes)

}



