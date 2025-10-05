#' Create a Code Frequency Summary and Optional Plot
#'
#' @description
#' Summarizes the frequency and coverage of logical code variables in a dataset of excerpts.
#' The function produces a summary table (as a tibble, `knitr::kable`, or `DT::datatable`)
#' and, optionally, a bar plot showing code frequencies or proportions.
#'
#' @param excerpts A data frame containing at least a `media_title` column and one or more logical columns
#'   representing coded excerpts (e.g., `c_help`, `c_stigma`).
#' @param table_min_count Minimum number of excerpts required for a code to be included in the summary table.
#'   Defaults to `1`.
#' @param table_min_prop Optional numeric threshold (0–1) specifying the minimum proportion of the
#'   most frequent code required for inclusion in the summary table.
#' @param plot Logical. If `TRUE`, generates a bar plot of code frequencies.
#'   Defaults to `FALSE`.
#' @param plot_min_count Minimum number of excerpts required for a code to be included in the plot.
#'   If `NULL`, defaults to `table_min_count`.
#' @param plot_min_prop Optional numeric threshold (0–1) specifying the minimum proportion
#'   of the maximum frequency required for inclusion in the plot.
#'   If `NULL`, defaults to `table_min_prop`.
#' @param output_type Type of output for the summary table: one of `"tibble"`, `"kable"`, or `"datatable"`.
#'   Defaults to `"tibble"`.
#' @param exclude Optional character vector of code variable names to exclude from the summary.
#' @param plot_metric Character string specifying which metric to plot:
#'   * `"count"` – excerpt frequencies,
#'   * `"prop"` – proportions of unique media titles, or
#'   * `"both"` – dual-axis plot with counts (bottom) and proportions (top).
#'   Defaults to `"count"`.
#' @param fill_color Character string specifying the fill color for bars in the plot.
#'   Defaults to `"steelblue"`.
#'
#' @return
#' If `plot = FALSE`, returns a summary table as a tibble, `knitr::kable`, or `DT::datatable`
#' depending on `output_type`.
#' If `plot = TRUE`, returns a list with two elements:
#' \describe{
#'   \item{`table`}{A tibble of summarized code counts and proportions.}
#'   \item{`plot`}{A `ggplot2` object showing the frequency and/or proportion of codes.}
#' }
#'
#' @details
#' Logical columns in `excerpts` are treated as binary code indicators.
#' Variable labels (if defined via `attr(x, "label")`) are used as code names in the output.
#'
#' @examples
#' # Example dataset
#' df <- data.frame(
#'   media_title = rep(paste0("Transcript_", 1:5), each = 3),
#'   c_help = sample(c(TRUE, FALSE), 15, replace = TRUE),
#'   c_stigma = sample(c(TRUE, FALSE), 15, replace = TRUE),
#'   c_hope = sample(c(TRUE, FALSE), 15, replace = TRUE)
#' )
#'
#' # Add variable labels
#' attr(df$c_help, "label") <- "Help-Seeking"
#' attr(df$c_stigma, "label") <- "Stigma"
#' attr(df$c_hope, "label") <- "Hope"
#'
#' # Summarize codes (tibble output)
#' create_code_summary(df, table_min_count = 2)
#'
#' # Display as formatted table
#' create_code_summary(df, table_min_count = 2, output_type = "kable")
#'
#' # Plot excerpt frequencies
#' create_code_summary(df, table_min_count = 1, plot = TRUE, plot_metric = "count")
#'
#' # Plot proportions of media titles
#' create_code_summary(df, table_min_count = 1, plot = TRUE, plot_metric = "prop")
#'
#' # Plot both frequency and proportion on dual axes
#' create_code_summary(df, table_min_count = 1, plot = TRUE, plot_metric = "both")
#'
#' @importFrom dplyr select filter group_by summarise n n_distinct arrange mutate all_of
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map_chr
#' @importFrom knitr kable
#' @importFrom DT datatable
#' @importFrom ggplot2 ggplot aes geom_col coord_flip labs theme_minimal scale_y_continuous sec_axis
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

  # --- Create name → label lookup ---
  label_lookup <- purrr::map_chr(code_columns, function(x) {
    lbl <- attr(excerpts[[x]], "label")
    if (is.null(lbl) || lbl == "") x else lbl
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
      code_label = label_lookup[as.character(code)] |> unname()
    ) %>%
    dplyr::select("code" = "code_label", "count", "n_media_titles", "prop_media_titles")

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
    # Default plot mins to table mins if not provided
    if (is.null(plot_min_count)) plot_min_count <- table_min_count
    if (is.null(plot_min_prop)) plot_min_prop <- table_min_prop

    plot_df <- total_counts
    plot_df <- dplyr::filter(plot_df, count >= plot_min_count)

    if (!is.null(plot_min_prop)) {
      max_val <- max(plot_df$count, na.rm = TRUE)
      plot_df <- dplyr::filter(plot_df, count >= plot_min_prop * max_val)
    }

    plot_df <- dplyr::arrange(plot_df, dplyr::desc(count))

    # --- Plot by selected metric ---
    if (plot_metric == "count") {
      p <- ggplot2::ggplot(plot_df, ggplot2::aes(
        x = reorder(code, count),
        y = count
      )) +
        ggplot2::geom_col(fill = fill_color) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          x = "Code",
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
          x = "Code",
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
          x = "Code",
          title = "Code Frequencies: Counts and Proportions"
        ) +
        ggplot2::theme_minimal()
    }

    return(list(table = total_counts, plot = p))
  }

  return(table_out)
}
