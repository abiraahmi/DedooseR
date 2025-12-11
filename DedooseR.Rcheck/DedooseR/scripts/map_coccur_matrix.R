map_cooccur_matrix <- function(coccur_matrix,
                               edge_min = 10,
                               layout = "circle",
                               edge_color_low = "lightgray",
                               edge_color_high = "purple",
                               node_color = "lightblue") {
  # check input
  if (!is.matrix(coccur_matrix) && !is.data.frame(coccur_matrix)) {
    stop("`coccur_matrix` must be a matrix or data.frame.")
  }

  coccur_matrix <- as.matrix(coccur_matrix)

  # Build igraph object
  g <- igraph::graph_from_adjacency_matrix(coccur_matrix,
                                           mode = "undirected",
                                           weighted = TRUE,
                                           diag = FALSE)

  # Filter weak edges
  g <- igraph::delete_edges(g, igraph::E(g)[weight < edge_min])

  # Drop isolated nodes (no edges left after filtering)
  g <- igraph::delete_vertices(g, which(igraph::degree(g) == 0))

  # Add node frequencies (number of transcripts each code appears in = diagonal)
  freq <- diag(coccur_matrix)
  igraph::V(g)$freq <- freq[igraph::V(g)$name]

  # Plot with ggraph
  p <- ggraph::ggraph(g, layout = layout) +
    ggraph::geom_edge_link(aes(width = weight, color = weight), alpha = 0.6) +
    ggraph::scale_edge_width(range = c(0.2, 2), guide = "none") +
    ggraph::scale_edge_color_gradient(low = edge_color_low,
                                      high = edge_color_high,
                                      guide = "none") +
    ggraph::geom_node_point(aes(size = freq), color = node_color) +
    ggraph::geom_node_text(aes(label = name), repel = TRUE) +
    ggplot2::theme_void()

  return(p)
}

# Test
# Load libraries
library(DedooseR)
library(tidyverse)
library(dplyr)
library(readxl)

# Clean data
excerpts <- read_xlsx("inst/raw_data/test_data.xlsx")
preferred_coders <- c("a", "l", "i", "r", "s", "v", "c", "n", "k")
excerpts <- clean_data(excerpts, preferred_coders)

# Create matrix
cooccur_matrix <- create_cooccur_matrix(excerpts, min_bold = 20, scale = "count", output = "data.frame")

create_cooccur_matrix(excerpts, output = "data.frame") %>%
  map_cooccur_matrix(edge_min = 15)

map_cooccur_matrix(cooccur_matrix, edge_min = 15)
