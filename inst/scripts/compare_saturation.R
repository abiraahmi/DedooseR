compare_saturation <- function(code_counts, excerpts, thresholds_list,
                               output_type = c("tibble", "kable")) {
  output_type <- match.arg(output_type)

  # total distinct media titles
  total_media_titles <- dplyr::n_distinct(excerpts$media_title)

  # add proportion column
  code_counts <- code_counts %>%
    dplyr::mutate(prop_media_title = n_media_titles / total_media_titles)

  # loop over thresholds
  for (set_name in names(thresholds_list)) {
    rules <- thresholds_list[[set_name]]
    code_counts[[set_name]] <-
      code_counts$count >= rules$code_count &
      code_counts$prop_media_title >= rules$prop_media_title
  }

  # output
  if (output_type == "kable") {
    return(knitr::kable(code_counts,
                        caption = "Saturation Comparison"))
  } else {
    return(code_counts)
  }
}


thresholds_list <- list(
  Set1 = list(code_count = 10, prop_media_title = 0.2),
  Set2 = list(code_count = 10, prop_media_title = 0.4)
)

# Apply thresholds
comp_saturation <- compare_saturation(code_counts, excerpts, thresholds_list)

total_media_titles <- max(code_counts$n_media_titles, na.rm = TRUE)
is.numeric(total_media_titles)
total_media_titles

code_counts %>%
  dplyr::mutate(
    test = n_media_titles / total_media_titles
  ) %>%
  head()
