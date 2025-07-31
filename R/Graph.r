#' @export
graph <- function(data,
                  directed = FALSE) {

  graph <- list(data = data,
                graph = NULL,
                directed = directed,
                layout = NULL)

  class(graph) <- "graph"

  graph
}

#' @export
build <- function(graph, ...) UseMethod("build")
#' @export
build.graph <- function(graph, vertices, edges, edge_attr = NULL) {
  edges <- get_edges(graph$data, vertices, edges, edge_attr)
  graph$graph <- igraph::graph_from_data_frame(edges, directed = graph$directed)
  graph$layout <- get_coords(graph, layout_method = "drl", options = list(simmer.attraction = 0))
  graph
}

#' @export
get_coords <- function(graph, ...) UseMethod("get_coords")
#' @export
get_coords.graph <- function(graph, layout_method = "auto", ...) {
  get_coords.igraph(graph$graph, layout_method = layout_method, ...)
}
#' @export
get_coords.igraph <- function(graph, layout_method = "auto", ...) {
  coords <- switch(
    layout_method,
    "fr" = igraph::layout_with_fr(graph, ...),
    "kk" = igraph::layout_with_kk(graph, ...),
    "drl" = igraph::layout_with_drl(graph, ...),
    igraph::layout_nicely(graph, ...)
  )
  layout_df <- as.data.frame(coords)
  colnames(layout_df) <- c("x", "y")
  layout_df
}

#' @export
get_density <- function(graph, ...) UseMethod("get_density")
#' @export
get_density.graph <- function(graph) {
  igraph::edge_density(graph$graph)
}

#' @export
get_transitivity <- function(graph, ...) UseMethod("get_transitivity")
#' @export
get_transitivity.graph <- function(graph, type = "global") {
  igraph::transitivity(graph$graph, type = type)
}

#' @export
get_centrality <- function(graph, ...) UseMethod("get_centrality")
#' @export
get_centrality.graph <- function(graph, method = "degree") {
  switch(method,
         "degree" = igraph::degree(graph$graph),
         "betweenness" = igraph::betweenness(graph$graph),
         "harmonic" = igraph::harmonic_centrality(graph$graph),
         NULL)
}

#' @export
get_diameter <- function(graph, ...) UseMethod("get_diameter")
#' @export
get_diameter.graph <- function(graph) {
  igraph::diameter(graph$graph, directed = graph$directed, weights = NULL)
}

#' @export
get_average_shortest_path <- function(graph, ...) UseMethod("get_average_shortest_path")
#' @export
get_average_shortest_path.graph <- function(graph) {
  igraph::mean_distance(graph$graph, directed = igraph::is_directed(graph$graph), unconnected = TRUE)
}

#' @export
get_reachability <- function(graph, ...) UseMethod("get_reachability")
#' @export
get_reachability.graph <- function(graph) {
  dists <- igraph::distances(graph$graph)
  n <- igraph::vcount(graph$graph)
  reachable_pairs <- sum(is.finite(dists)) - n
  total_pairs <- n * (n - 1)
  reachable_pairs / total_pairs
}

#' @export
get_cutpoints <- function(graph, ...) UseMethod("get_cutpoints")
#' @export
get_cutpoints.graph <- function(graph) {
  igraph::articulation_points(graph$graph)
}

#' @export
get_most_central_vertices <- function(graph, ...) UseMethod("get_most_central_vertices")
#' @export
get_most_central_vertices.graph <- function(graph, method = "degree", n = 10) {
  centrality <- get_centrality(graph, method = method)
  format_names(igraph::V(graph$graph)$name[order(-centrality)[1:n]])
}

#' @export
set_communities <- function(graph, ...) UseMethod("set_communities")
#' @export
set_communities.graph <- function(graph, vertex_attr = NULL) {
  if (is.null(vertex_attr)) {
    comm <- igraph::cluster_louvain(graph$graph)
    igraph::V(graph$graph)$community <- as.factor(comm$membership)
  } else {
    if (!vertex_attr %in% igraph::vertex_attr_names(graph$graph)) {
      cli::cli_abort("Vertex attribute {.val {vertex_attr}} not found in graph.")
    }
    igraph::V(graph$graph)$community <- as.factor(igraph::vertex_attr(graph$graph, vertex_attr))
  }
  graph
}

#' @export
get_communities <- function(graph, ...) UseMethod("get_communities")
#' @export
get_communities.graph <- function(graph) {
  get_communities.igraph(graph$graph)
}
#' @export
get_communities.igraph <- function(graph) {
  if (!"community" %in% igraph::vertex_attr_names(graph)) {
    cli::cli_abort(c(
      "x" = "Communities not set",
      "i" = "Call {.fn set_communities} first"
    ))
  }
  igraph::V(graph)$community
}

#' @export
get_most_central_per_community <- function(graph, ...) UseMethod("get_most_central_per_community")
#' @export
get_most_central_per_community.graph <- function(graph, method = "degree") {
  comm <- get_communities(graph)
  centrality <- get_centrality(graph, method = method)
  split_indices <- split(seq_along(comm), comm)
  sapply(split_indices, function(indices) {
    most_central <- indices[which.max(centrality[indices])]
    igraph::V(graph$graph)$name[most_central]
  })
}

#' @export
save_centrality_data <- function(graph, ...) UseMethod("save_centrality_data")
#' @export
save_centrality_data.graph <- function(graph, output_file = "output/centrality_data.csv") {
  output_data <- data.frame(
    degree = get_centrality(graph, method = "degree"),
    harmonic = get_centrality(graph, method = "harmonic"),
    betweenness = get_centrality(graph, method = "betweenness")
  )
  rownames(output_data) <- format_names(igraph::V(graph$graph)$name)
  output_data <- output_data[statnet.common::order(-output_data$degree), ]
  utils::write.csv(output_data, output_file, row.names = TRUE)
}
