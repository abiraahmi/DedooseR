plot_saturation_comp <- function(df_all_summary,
                                 df_qual_summary,
                                 thresholds_list,
                                 stacked = TRUE,
                                 as_proportion = FALSE,
                                 ncol = 2) {

  # Identify quality indicators from *_Count columns in df_qual_summary
  indicator_cols <- grep("_Count$", colnames(df_qual_summary), value = TRUE)
  qual_indicators <- sub("_Count$", "", indicator_cols)

  if (!is.list(thresholds_list) || is.null(names(thresholds_list))) {
    stop("thresholds_list must be a named list of named lists with thresholds
         per quality indicator.")
  }

  plots <- lapply(names(thresholds_list), function(set_name) {
    thresholds <- thresholds_list[[set_name]]

    missing_thresholds <- setdiff(qual_indicators, names(thresholds))
    if (length(missing_thresholds) > 0) {
      stop(
        sprintf("Thresholds missing for indicators in set '%s': %s",
                set_name,
                paste(missing_thresholds, collapse = ", "))
      )
    }

    # Create a neat string of thresholds, e.g. "Heterogeniety=3,
    # Priority excerpt=2"
    thresh_label <- paste(
      paste0(names(thresholds), " = ", thresholds),
      collapse = ", "
    )

    p <- plot_saturation(
      df_all_summary = df_all_summary,
      df_qual_summary = df_qual_summary,
      qual_indicators = qual_indicators,
      min_counts = thresholds,
      stacked = stacked,
      as_proportion = as_proportion
    ) +
      ggtitle(paste0(set_name)) +
      labs(caption = paste("Thresholds:", thresh_label)) +
      theme(plot.caption = element_text(hjust = 0, face = "italic", size = 9))

    return(p)
  })

  combined <- patchwork::wrap_plots(plots, ncol = ncol)
  return(combined)
}
