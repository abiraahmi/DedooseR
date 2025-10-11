cooccur <- function(
  excerpts = NULL,
  min_bold = 10,
  scale = c("count", "prop"),
  output = c("kable", "tibble", "data.frame"),
  plot = TRUE,
  edge_min = 10,
  layout = "circle",
  edge_color_low = "lightgray",
  edge_color_high = "purple",
  node_color = "lightblue",
  use_labels = FALSE,
  codebook = NULL
) {
  if (is.null(excerpts)) {
    stop("You must provide `excerpts`.")
  }
  scale <- match.arg(scale)
  output <- match.arg(output)

  if (!"media_title" %in% names(excerpts)) {
    stop("`excerpts` must contain a `media_title` column.")
  }

  code_columns <- grep("^c_", names(excerpts), value = TRUE)
  if (length(code_columns) == 0) stop("No code columns found (columns must start with 'c_').")

  code_by_transcript <- excerpts %>%
    dplyr::group_by(media_title) %>%
    dplyr::summarise(across(all_of(code_columns), ~ as.integer(any(. == 1))), .groups = "drop")

  code_matrix <- as.matrix(code_by_transcript[, -1])
  coccur_counts <- t(code_matrix) %*% code_matrix
  keep <- which(rowSums(coccur_counts) > 0 | colSums(coccur_counts) > 0)
  coccur_counts <- coccur_counts[keep, keep, drop = FALSE]

  if (scale == "prop") {
    marginals <- diag(coccur_counts)
    matrix_values <- sweep(coccur_counts, 2, marginals, "/")
    matrix_values <- round(matrix_values, 3)
    matrix_values[!is.finite(matrix_values)] <- 0
  } else {
    matrix_values <- coccur_counts
  }
  plot_matrix <- coccur_counts

  if (use_labels) {
    if (is.null(codebook)) stop("You must provide a `codebook` dataframe when `use_labels = TRUE`.")
    if (!all(c("variable", "label") %in% names(codebook))) {
      stop("`codebook` must have columns named `variable` and `label`.")
    }
    label_lookup <- setNames(codebook$label, codebook$variable)
    relabel_matrix <- function(mat) {
      matched_rows <- rownames(mat) %in% names(label_lookup)
      matched_cols <- colnames(mat) %in% names(label_lookup)
      rownames(mat)[matched_rows] <- label_lookup[rownames(mat)[matched_rows]]
      colnames(mat)[matched_cols] <- label_lookup[colnames(mat)[matched_cols]]
      mat
    }
    matrix_values <- relabel_matrix(matrix_values)
    plot_matrix <- relabel_matrix(plot_matrix)
  }

  coccur_df <- as.data.frame(matrix_values)
  rownames(coccur_df) <- rownames(matrix_values)

  if (output == "tibble") {
    matrix_out <- tibble::as_tibble(coccur_df, rownames = "code")
  } else if (output == "data.frame") {
    matrix_out <- coccur_df
  } else {
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
    matrix_out <- knitr::kable(
      coccur_df_fmt,
      format = "html",
      escape = FALSE,
      caption = paste(
        "Code Co-occurrence Matrix (Within Transcript)",
        ifelse(scale == "count", "Counts", "Proportions")
      ),
      align = "c"
    ) %>%
      kableExtra::kable_styling(full_width = FALSE,
                                bootstrap_options = c("striped", "hover", "condensed"))
  }

  plot_out <- NULL
  if (plot) {
    g <- igraph::graph_from_adjacency_matrix(plot_matrix,
                                             mode = "undirected",
                                             weighted = TRUE,
                                             diag = FALSE)
    g <- igraph::delete_edges(g, igraph::E(g)[weight < edge_min])
    freq <- diag(plot_matrix)
    igraph::V(g)$freq <- freq[igraph::V(g)$name]
    g <- igraph::delete_vertices(g, which(igraph::degree(g) == 0))

    plot_out <- ggraph::ggraph(g, layout = layout) +
      ggraph::geom_edge_link(aes(width = weight, color = weight), alpha = 0.6) +
      ggraph::scale_edge_width(range = c(0.2, 2), guide = "none") +
      ggraph::scale_edge_color_gradient(low = edge_color_low,
                                        high = edge_color_high,
                                        guide = "none") +
      ggraph::geom_node_point(aes(size = freq), color = node_color) +
      ggraph::geom_node_text(aes(label = igraph::V(g)$name),
                             repel = TRUE,
                             max.overlaps = Inf) +
      ggplot2::theme_void()
  }
  list(matrix = matrix_out, plot = plot_out)
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
cooccur_results <- cooccur(data_merged, scale = "count", 
                          output = "kable", plot = TRUE,
                           use_labels = TRUE, codebook = codebook_merged)

# View results
cooccur_results_table <- cooccur_results$matrix  # tibble or table of co-occurrence values
cooccur_results$plot    # ggplot2/ggraph object
