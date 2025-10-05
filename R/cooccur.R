#' Generate a Code Co-Occurrence Matrix and Network Plot
#'
#' @description
#' This function computes a co-occurrence matrix of qualitative codes based on
#' transcript-level data (e.g., each transcript coded with multiple binary `c_` variables),
#' filters the matrix based on a minimum co-occurrence threshold, and optionally visualizes
#' the network of code co-occurrences using `ggraph`.
#'
#' @details
#' The function expects an input data frame (`excerpts`) that includes a column called
#' `media_title` identifying transcript or participant names, and one or more binary
#' code columns whose names start with `"c_"`. It aggregates codes at the transcript level
#' and computes a symmetric co-occurrence matrix using the cross-product of code columns.
#'
#' Rows and columns with all zeros are dropped, and you can control how many co-occurrences
#' are required to be included in the matrix and plot using `matrix_threshold`.
#'
#' If variable labels are attached (e.g., via `haven::labelled()`), these will automatically
#' be used for the node labels in the plot.
#'
#' @param excerpts A data frame containing a `media_title` column and code columns (prefixed with `"c_"`).
#' @param coccur_matrix Optional. A pre-computed co-occurrence matrix (alternative to providing `excerpts`).
#' @param min_bold Integer. Minimum co-occurrence count for bolding cells in the `kable` output. Default = 10.
#' @param output Character. Output format for the matrix: `"kable"`, `"tibble"`, or `"data.frame"`. Default = `"kable"`.
#' @param plot Logical. Whether to produce a `ggraph` network plot. Default = `TRUE`.
#' @param matrix_threshold Integer. Minimum co-occurrence count required to retain a code pair. Default = 5.
#' @param plot_threshold Integer. Minimum code frequency (diagonal value) required for inclusion in the plot. Default = 0.
#' @param layout Character. Layout for `ggraph`. Options include `"circle"`, `"fr"`, `"kk"`, etc. Default = `"circle"`.
#' @param edge_color_low Character. Color for weakest edges. Default = `"lightgray"`.
#' @param edge_color_high Character. Color for strongest edges. Default = `"purple"`.
#' @param node_color Character. Color for nodes in the network. Default = `"lightblue"`.
#'
#' @return
#' A named list with two elements:
#' \itemize{
#'   \item `matrix` - A formatted co-occurrence matrix (as a `kable`, `tibble`, or `data.frame`).
#'   \item `plot` - A `ggraph` network plot object (if `plot = TRUE`), or `NULL` otherwise.
#' }
#'
#' @examples
#' \dontrun{
#' # Example data frame
#' library(dplyr)
#' excerpts <- data.frame(
#'   media_title = rep(c("A", "B", "C", "D"), each = 2),
#'   c_selfesteem = c(1,0,1,1,0,1,1,0),
#'   c_helpseeking = c(0,1,1,0,1,1,0,0),
#'   c_belonging = c(1,1,0,1,0,1,1,1)
#' )
#'
#' # Compute and plot
#' co <- cooccur(
#'   excerpts = excerpts,
#'   matrix_threshold = 2,
#'   plot_threshold = 1,
#'   output = "kable"
#' )
#'
#' # View formatted matrix
#' co$matrix
#'
#' # Show network plot
#' co$plot
#' }
#'
#' @importFrom dplyr group_by summarise across mutate
#' @importFrom tibble as_tibble
#' @importFrom knitr kable
#' @importFrom kableExtra cell_spec kable_styling
#' @importFrom igraph graph_from_adjacency_matrix delete_vertices vertex_attr V E degree
#' @importFrom ggraph ggraph geom_edge_link scale_edge_width scale_edge_color_gradient geom_node_point geom_node_text
#' @importFrom ggplot2 theme_void
#' @importFrom haven labelled
#'
#' @export
cooccur <- function(excerpts = NULL,
                    coccur_matrix = NULL,
                    min_bold = 10,
                    output = c("kable", "tibble", "data.frame"),
                    plot = TRUE,
                    matrix_threshold = 5,
                    plot_threshold = 0,
                    layout = "circle",
                    edge_color_low = "lightgray",
                    edge_color_high = "purple",
                    node_color = "lightblue") {

  # --- Argument validation ---
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
  }
  # --- Case 2: Use existing co-occurrence matrix ---
  else if (!is.null(coccur_matrix)) {
    coccur_matrix <- as.matrix(coccur_matrix)
  } else {
    stop("You must provide either `excerpts` or `coccur_matrix`.")
  }

  # --- Apply matrix_threshold ---
  coccur_matrix[coccur_matrix < matrix_threshold] <- 0

  # Drop all-zero rows/cols
  keep <- which(rowSums(coccur_matrix) > 0 | colSums(coccur_matrix) > 0)
  coccur_matrix <- coccur_matrix[keep, keep, drop = FALSE]

  # --- Convert to data frame ---
  coccur_df <- as.data.frame(coccur_matrix)
  rownames(coccur_df) <- rownames(coccur_matrix)

  # --- Format output ---
  if (output == "tibble") {
    matrix_out <- tibble::as_tibble(coccur_df, rownames = "code")

  } else if (output == "data.frame") {
    matrix_out <- coccur_df

  } else if (output == "kable") {
    coccur_df_fmt <- coccur_df %>%
      dplyr::mutate(across(
        everything(),
        ~ ifelse(. >= min_bold,
                 kableExtra::cell_spec(., bold = TRUE),
                 as.character(.))
      ))
    rownames(coccur_df_fmt) <- rownames(coccur_df)

    matrix_out <- knitr::kable(coccur_df_fmt,
                               format = "html",
                               escape = FALSE,
                               caption = paste(
                                 "Code Co-occurrence Matrix (>= ",
                                 matrix_threshold,
                                 " counts)",
                                 sep = ""
                               ),
                               align = "c") %>%
      kableExtra::kable_styling(full_width = FALSE,
                                bootstrap_options = c("striped", "hover", "condensed"))
  }

  # --- Optional plot ---
  plot_out <- NULL
  if (plot) {
    # Build igraph
    g <- igraph::graph_from_adjacency_matrix(coccur_matrix,
                                             mode = "undirected",
                                             weighted = TRUE,
                                             diag = FALSE)

    # --- Node frequencies (diagonal = total appearances) ---
    freq <- diag(coccur_matrix)
    igraph::V(g)$freq <- freq[igraph::V(g)$name]

    # --- Filter low-frequency nodes ---
    g <- igraph::delete_vertices(g, igraph::V(g)[freq < plot_threshold])

    # --- Drop isolated nodes ---
    g <- igraph::delete_vertices(g, which(igraph::degree(g) == 0))

    # --- Attach haven labels if available ---
    if (!is.null(excerpts)) {
      var_labels <- sapply(excerpts, function(x) attr(x, "label"))
      var_labels <- var_labels[!is.na(var_labels)]
      matching_labels <- var_labels[names(var_labels) %in% igraph::V(g)$name]

      if (length(matching_labels) > 0) {
        igraph::V(g)$label[match(names(matching_labels), igraph::V(g)$name)] <- matching_labels
        message("Using variable labels from dataset for node labels.")
      } else {
        message("No variable labels found; using variable names instead.")
      }
    }

    # --- Node label selection ---
    label_var <- if ("label" %in% names(igraph::vertex_attr(g))) igraph::V(g)$label else igraph::V(g)$name

    # --- Plot ---
    plot_out <- ggraph::ggraph(g, layout = layout) +
      ggraph::geom_edge_link(aes(width = weight, color = weight), alpha = 0.6) +
      ggraph::scale_edge_width(range = c(0.2, 2), guide = "none") +
      ggraph::scale_edge_color_gradient(low = edge_color_low, high = edge_color_high, guide = "none") +
      ggraph::geom_node_point(aes(size = freq), color = node_color) +
      ggraph::geom_node_text(aes(label = label_var), repel = TRUE, max.overlaps = Inf) +
      ggplot2::theme_void()
  }

  # --- Return both objects ---
  output_list <- list(matrix = matrix_out, plot = plot_out)
  return(output_list)
}
