#' Compare Code Saturation Across Multiple Threshold Sets
#'
#' Creates stacked (or side-by-side) bar charts visualizing code counts by
#' quality indicators
#' for multiple sets of user-defined thresholds. Each set of thresholds
#' corresponds to a unique
#' plot, which are combined into a multi-panel figure.
#'
#' @param df_all_summary A data frame containing overall code summary
#' information, including at least
#'   the `Code` column and `total_preferred_coder` counts.
#' @param df_qual_summary A data frame containing quality indicator counts for
#' each code. Columns
#'   should include `Code` and columns with names ending in `_Count`
#'   representing counts per quality indicator.
#' @param thresholds_list A named list of named lists. Each inner named list
#' specifies minimum count
#'   thresholds for each quality indicator. For example:
#'   ```
#'   list(
#'     Set1 = list(Heterogeniety = 3, `Priority excerpt` = 2),
#'     Set2 = list(Heterogeniety = 1, `Priority excerpt` = 3)
#'   )
#'   ```
#' @param stacked Logical; if TRUE, bars are stacked. If FALSE, bars are dodged
#' side-by-side.
#' @param as_proportion Logical; if TRUE, plots proportions relative to the
#' total counts per code,
#'   otherwise plots raw counts.
#' @param ncol Number of columns to arrange the multiple plots into.
#'
#' @return A patchwork object containing combined ggplot2 bar charts, one for
#' each threshold set.
#'
#' @examples
#' \dontrun{
#' thresholds <- list(
#'   Set1 = list(Heterogeniety = 3, `Priority excerpt` = 2),
#'   Set2 = list(Heterogeniety = 1, `Priority excerpt` = 3)
#' )
#' plot_saturation_comp(df_all_summary, df_qual_summary, thresholds,
#' stacked = TRUE, as_proportion = FALSE)
#' }
#'
#' @export

plot_saturation_comp <- function(df_all_summary,
                                 df_qual_summary,
                                 thresholds_list,
                                 stacked = TRUE,
                                 as_proportion = FALSE,
                                 ncol = 2) {

  # Check thresholds_list is named list of named lists
  if (!is.list(thresholds_list) || is.null(names(thresholds_list))) {
    stop("thresholds_list must be a named list of named lists with thresholds per quality indicator.")
  }

  # Extract quality indicators from all threshold sets (union of names)
  qual_indicators <- unique(unlist(lapply(thresholds_list, names)))

  # Confirm df_qual_summary has columns for these indicators (either ind or ind_Count)
  qual_cols_found <- sapply(qual_indicators, function(ind) {
    any(c(ind, paste0(ind, "_Count")) %in% colnames(df_qual_summary))
  })
  if (!all(qual_cols_found)) {
    missing_cols <- qual_indicators[!qual_cols_found]
    stop("df_qual_summary is missing columns for these quality indicators: ",
         paste(missing_cols, collapse = ", "))
  }

  plots <- lapply(names(thresholds_list), function(set_name) {
    thresholds <- thresholds_list[[set_name]]

    # Make sure all indicators in this threshold set are included in qual_indicators
    missing_thresholds <- setdiff(names(thresholds), qual_indicators)
    if (length(missing_thresholds) > 0) {
      stop(sprintf("Thresholds missing for indicators in set '%s': %s",
                   set_name,
                   paste(missing_thresholds, collapse = ", ")))
    }

    thresh_label <- paste(
      paste0(names(thresholds), " = ", thresholds),
      collapse = ", "
    )

    # Call plot_saturation with the indicator names (no _Count suffix)
    p <- plot_saturation(
      df_all_summary = df_all_summary,
      df_qual_summary = df_qual_summary,
      qual_indicators = names(thresholds),  # just indicator names here
      min_counts = thresholds,
      stacked = stacked,
      as_proportion = as_proportion
    ) +
      ggtitle(set_name) +
      labs(caption = paste("Thresholds:", thresh_label)) +
      theme(plot.caption = element_text(hjust = 0, face = "italic", size = 9))

    return(p)
  })

  combined <- patchwork::wrap_plots(plots, ncol = ncol)
  return(combined)
}
