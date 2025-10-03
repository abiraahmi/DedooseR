#' Merge multiple Boolean code variables into new collapsed codes
#'
#' This function takes a dataset with Boolean (logical) code variables and
#' collapses groups of them into new variables. For each new code, the function
#' assigns `TRUE` if any of the specified source variables are `TRUE` (ignoring `NA`),
#' and then removes the original source variables from the dataset.
#'
#' @param data A `data.frame` or tibble containing the code variables.
#' @param merges A named list, where each element name is the name of a new
#'   variable to create, and the value is a character vector of variable names
#'   to merge together.
#'
#' @return A `data.frame` with the new collapsed variables added and the original
#'   source variables removed.
#'
#' @examples
#' library(dplyr)
#'
#' # Example dataset
#' excerpts <- tibble(
#'   Destigmatization = c(TRUE, FALSE, FALSE),
#'   `Shift in MH/S norms` = c(FALSE, TRUE, FALSE),
#'   c_sense_of_belonging = c(FALSE, FALSE, TRUE),
#'   c_sense_of_connectedness = c(TRUE, FALSE, FALSE),
#'   c__suicide_comfort_directing_change = c(TRUE, FALSE, TRUE),
#'   c__suicide_comfort_general = c(FALSE, TRUE, FALSE)
#' )
#'
#' # Collapse multiple groups of codes into new variables
#' merged <- merge_codes(excerpts, list(
#'   Destigmatization = c("Destigmatization", "Shift in MH/S norms"),
#'   Belonging_and_Connectedness = c("c_sense_of_belonging", "c_sense_of_connectedness"),
#'   c__suicide_comfort = c("c__suicide_comfort_directing_change", "c__suicide_comfort_general")
#' ))
#'
#' merged
#'
#'
#' @export
merge_codes <- function(data, merges) {
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
