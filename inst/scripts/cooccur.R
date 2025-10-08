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


# Testing

# Load libraries
library(DedooseR)
library(tidyverse)
library(dplyr)
library(readxl)

# Clean data
filepath <- read_xlsx("inst/raw_data/test_data.xlsx")
preferred_coders <- c("a", "l", "i", "r", "s", "v", "c", "n", "k")
df <- clean_data(filepath,
                 preferred_coders,
                 rename_vars = list(memo_destigmatization = "...274"),
                 relabel_vars = list(title = "Memo: Destigmatization"))
data <- df$data
codebook <- df$codebook

# Merge codes
excerpts_merged <- merge_codes(data,
                               merges = list(
                                 c_belonging_connectedness = c(
                                   "c_sense_of_belonging", "c_sense_of_belonging_others", "c_sense_of_belonging_self",
                                   "c_sense_of_connectedness", "c_sense_of_connectedness_family",
                                   "c_sense_of_connectedness_peers", "c_sense_of_connectedness_school_community",
                                   "c_sense_of_connectedness_staff"
                                 ),
                                 c_suicide_comfort = c("c__suicide_comfort_directing_change", "c__suicide_comfort_general")
                               ),
                               relabel_vars = list(
                                 c_belonging_connectedness = "Sense of Belonging & Connectedness",
                                 c_suicide_comfort = "Suicide Comfort Conversing"
                               ))

data_merged <- excerpts_merged$data
codebook_merged <- excerpts_merged$codebook

# Return kable and plot network
cooccur_results <- cooccur(data_merged, output = "tibble", plot = TRUE,
                           use_labels = TRUE, codebook = codebook_merged)

# View results
cooccur_results$matrix  # tibble or table of co-occurrence values
cooccur_results$plot    # ggplot2/ggraph object

