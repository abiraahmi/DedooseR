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
                                 "Code Co-occurrence Matrix (≥ ",
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
        message("✅ Using variable labels from dataset for node labels.")
      } else {
        message("ℹ No variable labels found; using variable names instead.")
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

# Plot
library(haven)

# Create matrix, set threshold and plot
cooccur_01 <- cooccur(data_merged,
              matrix_threshold = 15)
cooccur_01$matrix
cooccur_01$plot

