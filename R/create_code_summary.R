#' Create a Summary Table and Optional Plot of Coded Excerpts
#'
#' Summarizes the frequency and coverage of logical (code) variables across a set of
#' media excerpts, using variable labels (from `haven`) as display names when available.
#' Optionally produces a bar plot showing counts, proportions, or both.
#'
#' @param excerpts A data frame containing a column named `media_title` and one or more
#'   logical (TRUE/FALSE) columns indicating the presence of each code. Each code variable
#'   may have a `label` attribute (e.g., from `haven::read_sav()`) used for display instead
#'   of the variable name.
#' @param table_min_count Minimum number of occurrences required for a code to appear
#'   in the summary table. Defaults to `1`.
#' @param table_min_prop Optional numeric between 0 and 1 indicating the minimum proportion
#'   (relative to the most frequent code) required for inclusion in the table.
#' @param plot Logical; whether to generate a ggplot2 bar chart summarizing code frequencies.
#'   Defaults to `FALSE`.
#' @param plot_min_count Minimum count threshold for including codes in the plot.
#'   If `NULL`, defaults to the same as `table_min_count`.
#' @param plot_min_prop Optional numeric between 0 and 1 indicating the minimum proportion
#'   (relative to the maximum count) required for inclusion in the plot. If `NULL`, defaults
#'   to the same as `table_min_prop`.
#' @param output_type Character string specifying the output format for the summary table.
#'   One of `"tibble"` (default), `"kable"`, or `"datatable"`.
#' @param exclude Optional character vector of code variable names to exclude from the summary.
#' @param plot_metric Character string specifying what the plot should display:
#'   `"count"` (absolute frequencies), `"prop"` (proportion of media titles covered),
#'   or `"both"` (dual-axis plot showing both). Defaults to `"count"`.
#' @param fill_color Character string specifying the fill color for the bars in the plot.
#'   Defaults to `"steelblue"`.
#'
#' @details
#' Logical variables in `excerpts` are interpreted as binary indicators of whether each
#' code applies to a given media item. The function counts the number of excerpts per code
#' and computes the number and proportion of unique `media_title` values represented.
#' Variable labels (if present via `attr(x, "label")`) are used for display instead of
#' raw variable names.
#'
#' The resulting table can be returned as a tibble, formatted with `knitr::kable()`,
#' or displayed as an interactive `DT::datatable()`. If `plot = TRUE`, both the table
#' and a ggplot object are returned invisibly as a list.
#'
#' @return
#' - If `plot = FALSE`: the formatted table in the format specified by `output_type`
#'   (tibble, kable, or datatable).
#' - If `plot = TRUE`: an invisible list with two elements:
#'   \describe{
#'     \item{table}{The formatted summary table.}
#'     \item{plot}{A `ggplot2` object showing code frequencies.}
#'   }
#'
#' @examples
#' \dontrun{
#' library(haven)
#' library(dplyr)
#'
#' # Example data frame with haven-style labels
#' df <- tibble::tibble(
#'   media_title = c("A", "B", "C", "D"),
#'   code_01 = c(TRUE, FALSE, TRUE, TRUE),
#'   code_02 = c(FALSE, TRUE, TRUE, FALSE)
#' )
#' attr(df$code_01, "label") <- "Empathy"
#' attr(df$code_02, "label") <- "Resilience"
#'
#' create_code_summary(df, plot = TRUE, output_type = "kable")
#' }
#'
#' @importFrom dplyr select all_of filter group_by summarise mutate n n_distinct arrange desc recode
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map_chr
#' @importFrom ggplot2 ggplot aes geom_col coord_flip labs theme_minimal scale_y_continuous sec_axis
#' @importFrom knitr kable
#' @importFrom DT datatable
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
    plot_metric = c("count", "prop", "both"),
    fill_color = "steelblue"
) {
  output_type <- match.arg(output_type)
  plot_metric <- match.arg(plot_metric)

  # --- Validate inputs ---
  if (!is.data.frame(excerpts)) stop("`excerpts` must be a data frame.")
  if (!"media_title" %in% names(excerpts)) stop("`excerpts` must contain a `media_title` column.")

  # Identify logical columns (codes)
  code_columns <- colnames(excerpts)[vapply(excerpts, is.logical, logical(1))]

  if (!is.null(exclude)) {
    exclude <- intersect(exclude, code_columns)
    code_columns <- setdiff(code_columns, exclude)
  }
  if (length(code_columns) == 0) stop("No logical (code) columns found after exclusions.")

  # --- Create name → label lookup (via haven labels) ---
  label_lookup <- purrr::map_chr(code_columns, function(x) {
    lbl <- attr(excerpts[[x]], "label")
    if (is.null(lbl) || is.na(lbl) || lbl == "") x else lbl
  })
  names(label_lookup) <- code_columns

  # --- Summarize code frequencies ---
  total_counts <- excerpts %>%
    dplyr::select("media_title", dplyr::all_of(code_columns)) %>%
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
    dplyr::mutate(
      prop_media_titles = round(n_media_titles / max(n_media_titles, na.rm = TRUE), 2),
      code_label = dplyr::recode(code, !!!label_lookup)
    ) %>%
    dplyr::select(
      code_label,
      count,
      n_media_titles,
      prop_media_titles
    ) %>%
    dplyr::rename(code = code_label)

  # --- Apply table filters ---
  if (!is.null(table_min_prop)) {
    max_val <- max(total_counts$count, na.rm = TRUE)
    total_counts <- dplyr::filter(total_counts, count >= table_min_prop * max_val)
  }
  total_counts <- dplyr::filter(total_counts, count >= table_min_count)

  # --- Output table ---
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

  # --- Plot section ---
  if (plot) {
    if (is.null(plot_min_count)) plot_min_count <- table_min_count
    if (is.null(plot_min_prop)) plot_min_prop <- table_min_prop

    plot_df <- dplyr::filter(total_counts, count >= plot_min_count)
    if (!is.null(plot_min_prop)) {
      max_val <- max(plot_df$count, na.rm = TRUE)
      plot_df <- dplyr::filter(plot_df, count >= plot_min_prop * max_val)
    }
    plot_df <- dplyr::arrange(plot_df, dplyr::desc(count))

    if (plot_metric == "count") {
      p <- ggplot2::ggplot(plot_df, ggplot2::aes(
        x = reorder(code, count),
        y = count
      )) +
        ggplot2::geom_col(fill = fill_color) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          x = "Code (Label)",
          y = "Excerpt Frequency",
          title = "Code Counts"
        ) +
        ggplot2::theme_minimal()

    } else if (plot_metric == "prop") {
      p <- ggplot2::ggplot(plot_df, ggplot2::aes(
        x = reorder(code, prop_media_titles),
        y = prop_media_titles
      )) +
        ggplot2::geom_col(fill = fill_color) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          x = "Code (Label)",
          y = "Proportion of Media Titles",
          title = "Code Frequencies by Media Title Coverage"
        ) +
        ggplot2::theme_minimal()

    } else if (plot_metric == "both") {
      scale_factor <- max(plot_df$count, na.rm = TRUE) /
        max(plot_df$prop_media_titles, na.rm = TRUE)

      p <- ggplot2::ggplot(plot_df, ggplot2::aes(
        x = reorder(code, count),
        y = count
      )) +
        ggplot2::geom_col(fill = fill_color) +
        ggplot2::coord_flip() +
        ggplot2::scale_y_continuous(
          name = "Excerpt Frequency",
          sec.axis = ggplot2::sec_axis(~ . / scale_factor,
                                       name = "Proportion of Media Titles")
        ) +
        ggplot2::labs(
          x = "Code (Label)",
          title = "Code Frequencies: Counts and Proportions"
        ) +
        ggplot2::theme_minimal()
    }

    # --- Return formatted table + plot ---
    print(table_out)  # ensure the chosen table format is displayed
    return(invisible(list(table = table_out, plot = p)))
  }

  # --- Return correct table format (no plot) ---
  print(table_out)
  return(invisible(table_out))
}
