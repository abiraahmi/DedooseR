#' Create and Visualize a Code Frequency Summary
#'
#' @description
#' Summarizes and optionally visualizes the frequency of each logical (TRUE/FALSE)
#' code variable in a qualitative dataset. Each logical column is assumed to represent
#' whether a code was applied to an excerpt (e.g., TRUE = applied).
#'
#' The function produces a summary table showing how often each code appears across
#' all excerpts and across unique `media_title` sources. Optionally, a bar plot can
#' be generated to visualize code frequencies.
#'
#' Users can filter which codes are displayed in the table or plot based on absolute
#' count thresholds (`*_min_count`) or relative proportions of the most frequent code
#' (`*_min_prop`).
#'
#' @param excerpts A data frame containing at least one logical column representing codes
#'   and a column named `media_title`.
#' @param table_min_count Minimum count threshold for including codes in the summary table.
#'   Defaults to `1`.
#' @param table_min_prop Minimum proportion of the maximum count (0–1) for including codes
#'   in the summary table. Defaults to `NULL`.
#' @param plot Logical; if `TRUE`, also generates a bar plot of code frequencies.
#' @param plot_min_count Minimum count threshold for including codes in the plot. Defaults to `NULL`.
#' @param plot_min_prop Minimum proportion of the maximum count (0–1) for including codes in the plot.
#'   Defaults to `NULL`.
#' @param output_type Output format for the summary table: `"tibble"`, `"kable"`, or `"datatable"`.
#'   Defaults to `"tibble"`.
#' @param exclude Character vector of code variable names to exclude from both table and plot.
#' @param plot_metric Which metric to plot: `"count"` or `"n_media_titles"`. Defaults to `"count"`.
#' @param fill_color Bar fill color for the plot. Defaults to `"steelblue"`.
#'
#' @return
#' - If `plot = FALSE`: returns a tibble, kable, or datatable (depending on `output_type`).
#' - If `plot = TRUE`: returns a list with two elements:
#'   * `$table` – the summary table (tibble)
#'   * `$plot` – a `ggplot2` object showing code frequencies
#'
#' @details
#' Variable labels (stored as the `"label"` attribute, e.g. via `haven::labelled`) are displayed
#' in place of variable names when available. If no label is found, the variable name is used.
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#' library(purrr)
#'
#' df <- data.frame(
#'   media_title = c("A", "B", "C", "D"),
#'   code1 = c(TRUE, FALSE, TRUE, TRUE),
#'   code2 = c(FALSE, TRUE, TRUE, FALSE),
#'   code3 = c(FALSE, TRUE, FALSE, FALSE)
#' )
#' attr(df$code1, "label") <- "Emotional Support"
#' attr(df$code2, "label") <- "Academic Support"
#' attr(df$code3, "label") <- "Family Relationships"
#'
#' # Default (table only)
#' create_code_summary(df)
#'
#' # Include plot
#' res <- create_code_summary(df, plot = TRUE)
#' res$plot
#'
#' # Exclude codes and adjust thresholds
#' create_code_summary(df, exclude = "code3", table_min_count = 2, plot = TRUE)
#'
#' @importFrom dplyr select filter group_by summarise n n_distinct arrange mutate all_of
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map_chr
#' @importFrom knitr kable
#' @importFrom DT datatable
#' @importFrom ggplot2 ggplot aes geom_col coord_flip labs theme_minimal
#' @importFrom stats reorder
#' @export
create_code_summary <- function(
    excerpts,
    table_min_count = 1,
    table_min_prop = NULL,
    plot = FALSE,
    plot_min_count = NULL,
    plot_min_prop = NULL,
    output_type = c("tibble", "kable", "datatable"),
    exclude = NULL,
    plot_metric = c("count", "n_media_titles"),
    fill_color = "steelblue"
) {
  output_type <- match.arg(output_type)
  plot_metric <- match.arg(plot_metric)

  # Validate inputs
  if (!is.data.frame(excerpts))
    stop("`excerpts` must be a data frame.")
  if (!"media_title" %in% names(excerpts))
    stop("`excerpts` must contain a `media_title` column.")

  # Identify logical columns (codes)
  code_columns <- colnames(excerpts)[vapply(excerpts, is.logical, logical(1))]

  if (!is.null(exclude)) {
    exclude <- intersect(exclude, code_columns)
    code_columns <- setdiff(code_columns, exclude)
  }
  if (length(code_columns) == 0)
    stop("No logical (code) columns found after exclusions.")

  # Create name → label lookup
  label_lookup <- purrr::map_chr(code_columns, function(x) {
    lbl <- attr(excerpts[[x]], "label")
    if (is.null(lbl) || lbl == "") x else lbl
  })
  names(label_lookup) <- code_columns

  # Summarize code frequencies
  total_counts <- excerpts %>%
    dplyr::select(media_title, dplyr::all_of(code_columns)) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(code_columns),
      names_to = "code",
      values_to = "applied"
    ) %>%
    dplyr::filter(applied == TRUE) %>%
    dplyr::group_by(code) %>%
    dplyr::summarise(
      count = dplyr::n(),
      n_media_titles = dplyr::n_distinct(media_title),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(count)) %>%
    dplyr::mutate(code_label = unname(label_lookup[as.character(code)])) %>%
    dplyr::select(code = code_label, count, n_media_titles)

  # Apply table filters
  if (!is.null(table_min_prop)) {
    max_val <- max(total_counts$count, na.rm = TRUE)
    total_counts <- dplyr::filter(total_counts, count >= table_min_prop * max_val)
  }
  total_counts <- dplyr::filter(total_counts, count >= table_min_count)

  # Output table
  caption_text <- paste("Total Code Counts (min_count =", table_min_count, ")")
  table_out <- switch(
    output_type,
    tibble = total_counts,
    kable = knitr::kable(total_counts, caption = caption_text),
    datatable = DT::datatable(
      total_counts,
      caption = caption_text,
      options = list(pageLength = 30, autoWidth = TRUE)
    )
  )

  # Generate plot if requested
  if (plot) {
    plot_df <- total_counts

    # Apply plot thresholds
    if (!is.null(plot_min_count)) {
      plot_df <- dplyr::filter(plot_df, .data[[plot_metric]] >= plot_min_count)
    }
    if (!is.null(plot_min_prop)) {
      max_val <- max(plot_df[[plot_metric]], na.rm = TRUE)
      plot_df <- dplyr::filter(plot_df, .data[[plot_metric]] >= plot_min_prop * max_val)
    }

    # Order and plot
    plot_df <- dplyr::arrange(plot_df, .data[[plot_metric]])
    p <- ggplot2::ggplot(plot_df, ggplot2::aes(
      x = reorder(code, .data[[plot_metric]]),
      y = .data[[plot_metric]]
    )) +
      ggplot2::geom_col(fill = fill_color) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        x = "Code",
        y = ifelse(plot_metric == "count", "Excerpt Frequency", "Media Title Coverage"),
        title = paste("Code Frequencies by", ifelse(plot_metric == "count", "Excerpts", "Media Titles"))
      ) +
      ggplot2::theme_minimal()

    return(list(table = total_counts, plot = p))
  }

  return(table_out)
}
