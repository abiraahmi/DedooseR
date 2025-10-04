#' Merge logical code variables and optionally relabel them
#'
#' @description
#' Merges multiple logical (TRUE/FALSE) code variables into new composite variables.
#' For each new variable, the function computes a logical OR across the specified
#' source variables — that is, the new variable will be `TRUE` if *any* of the
#' source variables are `TRUE`.
#' Optionally, users can assign descriptive labels to the newly created variables.
#' The function also generates a codebook summarizing the resulting dataset.
#'
#' @param data A data frame or tibble containing logical code variables (e.g.,
#'   from `clean_data()` output).
#' @param merges A named list, where each name is a new variable to create and
#'   each value is a character vector of existing variable names to merge.
#'   For example:
#'   `list(c_help = c("c_support", "c_assist"), c_stress = c("c_anxiety", "c_pressure"))`
#' @param relabel_vars Optional named list of variable labels for the new merged
#'   variables, in the format
#'   `list(new_var = "New variable label")`.
#'   If not provided, variable names are used as default labels.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{data_merged}{A data frame containing the updated dataset with merged
#'   logical code variables.}
#'   \item{codebook_merged}{A data frame summarizing variable names, variable
#'   labels (if available), and data types.}
#' }
#'
#' @details
#' The function first checks that all specified source variables exist in the dataset.
#' It then creates new logical variables defined by the merges, assigns user-specified
#' or default labels, removes the original source variables (unless one overlaps with a
#' new variable name), and builds a codebook summarizing the merged dataset.
#'
#' @examples
#' # Example dataset
#' df <- data.frame(
#'   c_support = c(TRUE, FALSE, TRUE),
#'   c_assist = c(FALSE, TRUE, TRUE),
#'   c_anxiety = c(TRUE, FALSE, FALSE),
#'   c_pressure = c(FALSE, TRUE, FALSE)
#' )
#'
#' # Define merges
#' merges <- list(
#'   c_help = c("c_support", "c_assist"),
#'   c_stress = c("c_anxiety", "c_pressure")
#' )
#'
#' # Run merge_codes() with new labels
#' result <- merge_codes(
#'   data = df,
#'   merges = merges,
#'   relabel_vars = list(
#'     c_help = "Mentions of helping or supporting others",
#'     c_stress = "Mentions of stress or pressure"
#'   )
#' )
#'
#' # Extract merged data and codebook
#' data_merged <- result$data_merged
#' codebook_merged <- result$codebook_merged
#'
#' # View merged dataset
#' head(data_merged)
#'
#' # View codebook
#' head(codebook_merged)
#'
#' @importFrom labelled var_label
#' @export
merge_codes <- function(data, merges, relabel_vars = NULL) {
  # data: a data.frame or tibble
  # merges: a named list, where names are new vars, values are character vectors of old vars

  all_from_vars <- c()

  for (new_var in names(merges)) {
    from_vars <- merges[[new_var]]

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
  data_merged <- data[, !names(data) %in% all_from_vars, drop = FALSE]

  # Create codebook for merged data
  codebook_merged <- data.frame(
    variable = names(data_merged),
    label = sapply(names(data_merged), function(col) {
      lbl <- labelled::var_label(data_merged[[col]])
      if (is.null(lbl) || lbl == "") col else lbl
    }),
    type = sapply(data_merged, function(x) class(x)[1]),
    stringsAsFactors = FALSE
  )

  # Return both outputs
  return(list(
    data_merged = data_merged,
    codebook_merged = codebook_merged
  ))
}
