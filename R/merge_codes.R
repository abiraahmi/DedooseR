#' Merge multiple binary code variables into new composite variables
#'
#' This function merges sets of binary (TRUE/FALSE or 0/1) variables into new composite
#' variables. Each new variable is created as \code{TRUE} if any of its corresponding
#' source variables are \code{TRUE}. The original source variables are dropped after merging.
#'
#' @param data A \code{data.frame} or \code{tibble} containing the variables to merge.
#' @param merges A named list where each name corresponds to the name of a new variable
#'   to create, and each value is a character vector of existing variable names to merge.
#'   For example: \code{list(themeA = c("code1", "code2"), themeB = c("code3", "code4"))}.
#'
#' @return A \code{data.frame} with the new merged variables added and the original
#' source variables removed.
#'
#' @details
#' The function checks that all variables specified in \code{merges} exist in the dataset.
#' For each merge group, it creates a new variable that is \code{TRUE} if any of the
#' source variables are \code{TRUE}, ignoring \code{NA} values. After merging,
#' the source variables are dropped from the dataset.
#'
#' @examples
#' df <- data.frame(a = c(TRUE, FALSE, TRUE),
#'                  b = c(FALSE, FALSE, TRUE),
#'                  c = c(TRUE, TRUE, FALSE))
#'
#' merges <- list(new1 = c("a", "b"), new2 = c("c"))
#' merge_codes(df, merges)
#'
#' @export
merge_codes <- function(data, merges) {
  all_from_vars <- c()

  for (new_var in names(merges)) {
    from_vars <- merges[[new_var]]

    # check that all source variables exist
    if (!all(from_vars %in% names(data))) {
      stop(paste("Some variables for", new_var, "not found in dataset"))
    }

    # create the new variable (TRUE if any source is TRUE)
    data[[new_var]] <- apply(data[from_vars], 1, function(x) any(x == TRUE, na.rm = TRUE))

    # collect old vars for dropping (but NOT the new_var itself)
    all_from_vars <- c(all_from_vars, setdiff(from_vars, new_var))
  }

  # drop all old vars at once
  data <- data[, !names(data) %in% all_from_vars, drop = FALSE]

  return(data)
}
