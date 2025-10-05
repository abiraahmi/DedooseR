#' Plot a Code Co-occurrence Network
#'
#' This function takes a code co-occurrence matrix (e.g., from
#' \code{create_cooccur_matrix()}) and produces a network visualization using
#' \pkg{igraph} and \pkg{ggraph}. Edges represent co-occurrence counts
#' (within transcripts or documents), and node sizes reflect the number of
#' transcripts in which each code appears.
#'
#' @param coccur_matrix A square co-occurrence matrix (numeric matrix or
#'   data frame) where rows and columns represent codes, and diagonal entries
#'   indicate the number of transcripts in which each code appears.
#' @param edge_min Minimum edge weight (co-occurrence count) to retain in the
#'   network. Edges with lower weights are removed. Default is 10.
#' @param layout Layout algorithm for node placement. Passed to
#'   \code{ggraph::ggraph()}. Common options are \code{"circle"},
#'   \code{"fr"} (Fruchterman–Reingold), or \code{"kk"} (Kamada–Kawai).
#'   Default is \code{"circle"}.
#' @param edge_color_low Color for weakest edges. Default is \code{"lightgray"}.
#' @param edge_color_high Color for strongest edges. Default is \code{"purple"}.
#' @param node_color Color for nodes. Default is \code{"lightblue"}.
#'
#' @return A \pkg{ggplot2} object containing the network visualization.
#'   Can be further customized with standard ggplot2 functions.
#'
#' @examples
#' \dontrun{
#' # Suppose 'excerpts' is your dataset of coded transcripts
#' coccur_df <- create_cooccur_matrix(excerpts, output = "data.frame")
#'
#' # Plot network with minimum edge weight of 10
#' map_cooccur_matrix(coccur_df, edge_min = 10)
#'
#' # Use a force-directed layout instead of circle
#' map_cooccur_matrix(coccur_df, edge_min = 5, layout = "fr")
#' }
#'
#' @importFrom igraph graph_from_adjacency_matrix delete_edges delete_vertices degree V E
#' @importFrom ggraph ggraph geom_edge_link scale_edge_width scale_edge_color_gradient geom_node_point geom_node_text
#' @importFrom ggplot2 aes theme_void
#' @export
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
