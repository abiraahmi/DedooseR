#' Create a Code Co-occurrence Matrix and Network Plot
#'
#' @description
#' Builds a co-occurrence matrix showing how often qualitative codes appear together
#' within the same unit (e.g., transcript, document, or media title).
#' The function can take either a coded dataset (`excerpts`) or an existing
#' co-occurrence matrix, returning both a formatted matrix and (optionally)
#' a network visualization.
#'
#' @details
#' The function identifies columns beginning with `"c_"` as code variables.
#' It computes co-occurrences by summing pairwise intersections of codes across
#' all unique `media_title` units. The diagonal represents the marginal frequencies
#' (the number of transcripts where each code appears).
#'
#' When `scale = "prop"`, the matrix entries are normalized by the marginal frequency
#' of each column, yielding conditional probabilities (e.g., P(B | A)).
#'
#' The resulting matrix can be output as a tibble, a simple data frame,
#' or a formatted HTML table via `knitr::kable`. If `plot = TRUE`, the function also
#' returns a network visualization of code co-occurrences using `ggraph` and `igraph`.
#'
#' @param excerpts Optional data frame containing coded excerpts, with a column
#'   named `media_title` and code columns prefixed with `"c_"`.
#' @param coccur_matrix Optional existing co-occurrence matrix to use instead of computing
#'   one from `excerpts`. Must be a numeric square matrix or coercible to one.
#' @param min_bold Minimum value for bold highlighting in HTML table output (if `output = "kable"`).
#'   Default is `10`.
#' @param scale Whether to display raw counts (`"count"`) or conditional proportions (`"prop"`).
#'   Default is `"count"`.
#' @param output The format of the co-occurrence matrix output.
#'   One of `"kable"`, `"tibble"`, or `"data.frame"`. Default is `"kable"`.
#' @param plot Logical; whether to produce a network visualization. Default is `TRUE`.
#' @param edge_min Minimum edge weight for displaying connections in the plot.
#'   When `scale = "count"`, this is a frequency threshold; when `scale = "prop"`,
#'   it must be between 0 and 1. Default is `10`.
#' @param plot_threshold Minimum node frequency required to display a code in the network plot.
#'   When `scale = "prop"`, must be between 0 and 1. Default is `0`.
#' @param layout Graph layout for network visualization (passed to `ggraph::ggraph`).
#'   Common options include `"circle"`, `"fr"`, or `"kk"`. Default is `"circle"`.
#' @param edge_color_low,edge_color_high Color gradient for edge weights in the plot.
#'   Default is `"lightgray"` to `"purple"`.
#' @param node_color Color for node points in the network plot. Default is `"lightblue"`.
#' @param use_labels Logical; if `TRUE`, replaces code variable names with descriptive labels
#'   from a provided `codebook`. Default is `FALSE`.
#' @param codebook Optional data frame with columns:
#'   - `variable`: the code variable name (e.g., `"c_family"`)
#'   - `label`: the descriptive name for the code (e.g., `"Family Connectedness"`).
#'   Required when `use_labels = TRUE`.
#'
#' @return
#' A named list with two elements:
#' \describe{
#'   \item{matrix}{A tibble, data frame, or formatted HTML table of the co-occurrence matrix.}
#'   \item{plot}{A `ggplot` object visualizing the co-occurrence network (if `plot = TRUE`).}
#' }
#'
#' @examples
#' # Example 1: Basic co-occurrence matrix from excerpts
#' df <- data.frame(
#'   media_title = c("Doc1", "Doc2", "Doc3"),
#'   c_hope = c(1, 0, 1),
#'   c_family = c(1, 1, 0),
#'   c_school = c(0, 1, 1)
#' )
#'
#' result <- cooccur(
#'   excerpts = df,
#'   scale = "count",
#'   output = "tibble",
#'   plot = TRUE
#' )
#'
#' result$matrix  # Co-occurrence matrix
#' result$plot    # Network plot
#'
#' # Example 2: Use descriptive labels from a codebook
#' codebook <- data.frame(
#'   variable = c("c_hope", "c_family", "c_school"),
#'   label = c("Hope & Optimism", "Family Connectedness", "School Belonging")
#' )
#'
#' labeled_result <- cooccur(
#'   excerpts = df,
#'   use_labels = TRUE,
#'   codebook = codebook,
#'   scale = "prop",
#'   output = "kable",
#'   plot = TRUE,
#'   edge_min = 0.3
#' )
#'
#' labeled_result$matrix
#' labeled_result$plot
#'
#' # Example 3: Provide an existing co-occurrence matrix
#' m <- matrix(c(3, 2, 1,
#'               2, 3, 1,
#'               1, 1, 2),
#'             nrow = 3, dimnames = list(c("A", "B", "C"), c("A", "B", "C")))
#'
#' cooccur(coccur_matrix = m, scale = "count", plot = FALSE)
#'
#' @importFrom dplyr group_by summarise across mutate everything
#' @importFrom tibble as_tibble
#' @importFrom kableExtra cell_spec kable_styling
#' @importFrom knitr kable
#' @importFrom igraph graph_from_adjacency_matrix delete_edges delete_vertices E V degree
#' @importFrom ggraph ggraph geom_edge_link geom_node_point geom_node_text scale_edge_width
#'   scale_edge_color_gradient
#' @importFrom ggplot2 theme_void
#' @export
cooccur <- function(excerpts = NULL,
                    coccur_matrix = NULL,
                    min_bold = 10,
                    scale = c("count", "prop"),
                    output = c("kable", "tibble", "data.frame"),
                    plot = TRUE,
                    edge_min = 10,
                    plot_threshold = 0,
                    layout = "circle",
                    edge_color_low = "lightgray",
                    edge_color_high = "purple",
                    node_color = "lightblue",
                    use_labels = FALSE,
                    codebook = NULL # dataframe with columns: variable, label
) {
  # --- Argument validation ---
  scale <- match.arg(scale)
  output <- match.arg(output)

  # --- Case 1: Build co-occurrence matrix from excerpts ---
  if (!is.null(excerpts)) {
    if (!"media_title" %in% names(excerpts)) {
      stop("`excerpts` must contain a `media_title` column.")
    }

    code_columns <- grep("^c_", names(excerpts), value = TRUE)
    if (length(code_columns) == 0) {
      stop("No code columns found (columns must start with 'c_').")
    }

    # Collapse to transcript-level presence
    code_by_transcript <- excerpts %>%
      dplyr::group_by(media_title) %>%
      dplyr::summarise(across(all_of(code_columns),
                              ~ as.integer(any(. == 1))),
                       .groups = "drop")

    # Build co-occurrence matrix
    code_matrix <- as.matrix(code_by_transcript[,-1])
    coccur_matrix <- t(code_matrix) %*% code_matrix

    # Drop all-zero rows/cols
    keep <- which(rowSums(coccur_matrix) > 0 | colSums(coccur_matrix) > 0)
    coccur_matrix <- coccur_matrix[keep, keep, drop = FALSE]

    # Scale if needed
    if (scale == "prop") {
      marginals <- diag(coccur_matrix)
      coccur_matrix <- sweep(coccur_matrix, 2, marginals, "/")
      coccur_matrix <- round(coccur_matrix, 3)
    }
  }
  # --- Case 2: Use existing co-occurrence matrix ---
  else if (!is.null(coccur_matrix)) {
    coccur_matrix <- as.matrix(coccur_matrix)
  } else {
    stop("You must provide either `excerpts` or `coccur_matrix`.")
  }

  # --- Apply code labels if requested ---
  if (use_labels) {
    if (is.null(codebook)) {
      stop("You must provide a `codebook` dataframe when `use_labels = TRUE`.")
    }
    if (!all(c("variable", "label") %in% names(codebook))) {
      stop("`codebook` must have columns named `variable` and `label`.")
    }

    # Create lookup vector
    label_lookup <- setNames(codebook$label, codebook$variable)

    # Replace row and column names with labels if available
    matched_rows <- rownames(coccur_matrix) %in% names(label_lookup)
    matched_cols <- colnames(coccur_matrix) %in% names(label_lookup)

    rownames(coccur_matrix)[matched_rows] <- label_lookup[rownames(coccur_matrix)[matched_rows]]
    colnames(coccur_matrix)[matched_cols] <- label_lookup[colnames(coccur_matrix)[matched_cols]]
  }

  # --- Convert to data frame ---
  coccur_df <- as.data.frame(coccur_matrix)
  rownames(coccur_df) <- rownames(coccur_matrix)

  # --- Format output ---
  if (output == "tibble") {
    matrix_out <- tibble::as_tibble(coccur_df, rownames = "code")
  } else if (output == "data.frame") {
    matrix_out <- coccur_df
  } else if (output == "kable") {
    if (scale == "count") {
      coccur_df_fmt <- coccur_df %>%
        dplyr::mutate(across(
          everything(),
          ~ ifelse(. >= min_bold,
                   kableExtra::cell_spec(., bold = TRUE),
                   as.character(.))
        ))
    } else {
      coccur_df_fmt <- coccur_df %>%
        dplyr::mutate(across(
          everything(),
          ~ ifelse(. >= min_bold,
                   kableExtra::cell_spec(sprintf("%.3f", .), bold = TRUE),
                   sprintf("%.3f", .))
        ))
    }
    rownames(coccur_df_fmt) <- rownames(coccur_df)
    matrix_out <- knitr::kable(coccur_df_fmt,
                               format = "html",
                               escape = FALSE,
                               caption = paste(
                                 "Code Co-occurrence Matrix (Within Transcript)",
                                 ifelse(scale == "count", "Counts", "Proportions")
                               ),
                               align = "c") %>%
      kableExtra::kable_styling(full_width = FALSE,
                                bootstrap_options = c("striped", "hover", "condensed"))
  }

  # --- Optional plot ---
  plot_out <- NULL
  if (plot) {
    g <- igraph::graph_from_adjacency_matrix(coccur_matrix,
                                             mode = "undirected",
                                             weighted = TRUE,
                                             diag = FALSE)

    # --- Filter edges ---
    if (scale == "count") {
      g <- igraph::delete_edges(g, igraph::E(g)[weight < edge_min])
    } else {
      if (edge_min < 0 || edge_min > 1) stop("When scale = 'prop', `edge_min` must be between 0 and 1.")
      g <- igraph::delete_edges(g, igraph::E(g)[weight < edge_min])
    }

    # --- Filter nodes ---
    freq <- diag(coccur_matrix)
    igraph::V(g)$freq <- freq[igraph::V(g)$name]
    if (scale == "count") {
      g <- igraph::delete_vertices(g, igraph::V(g)[freq < plot_threshold])
    } else {
      if (plot_threshold < 0 || plot_threshold > 1)
        stop("When scale = 'prop', `plot_threshold` must be between 0 and 1.")
      g <- igraph::delete_vertices(g, igraph::V(g)[freq < plot_threshold])
    }

    g <- igraph::delete_vertices(g, which(igraph::degree(g) == 0))

    label_var <- igraph::V(g)$name
    plot_out <- ggraph::ggraph(g, layout = layout) +
      ggraph::geom_edge_link(aes(width = weight, color = weight), alpha = 0.6) +
      ggraph::scale_edge_width(range = c(0.2, 2), guide = "none") +
      ggraph::scale_edge_color_gradient(low = edge_color_low,
                                        high = edge_color_high,
                                        guide = "none") +
      ggraph::geom_node_point(aes(size = freq), color = node_color) +
      ggraph::geom_node_text(aes(label = label_var),
                             repel = TRUE,
                             max.overlaps = Inf) +
      ggplot2::theme_void()
  }

  # --- Return ---
  output_list <- list(matrix = matrix_out, plot = plot_out)
  return(output_list)
}
