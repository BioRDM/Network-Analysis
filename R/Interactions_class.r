library(rlang)
library(dplyr)
library(tidyverse)
library(igraph)
library(intergraph)
library(statnet)

#' @export
Interactions <- function(file_path, csv_delimiter = ";", csv_column_name = "Author") {
  if (grepl(".csv", file_path)) {
    data <- utils::read.csv(file_path, stringsAsFactor = FALSE)
    graph <- make_graph_from_csv(file_path, delimiter = csv_delimiter, column_name = csv_column_name)
  } else if (grepl(".net", file_path)) {
    data <- NA
    graph <- load_graph(file_path)
  } else {
    stop("File type not supported.")
  }
  network <- intergraph::asNetwork(graph)

  interactions <- list(file_path = file_path, graph = graph, network = network, data = data)

  # Assign the class name
  class(interactions) <- "Interactions"

  return(interactions)
}

#' @export
load_graph <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("File not found.")
  } else {
    graph <- igraph::read_graph(file_path, format = "pajek")
  }
  return(graph)
}

#' @export
make_graph_from_csv <- function(file_path, delimiter = ";", column_name = "Author") {
  if (!file.exists(file_path)) {
    stop("File not found.")
  } else {
    data <- utils::read.csv(file_path, stringsAsFactor = FALSE)
  }

  col_sym <- rlang::sym(column_name)

  # Create edges: pairwise combinations of co-authors
  edges <- data %>%
    dplyr::mutate(Authors = stringr::str_split(!!col_sym, delimiter)) %>%
    dplyr::filter(purrr::map_int(Authors, length) > 1) %>%  # Remove papers with less than 2 authors (no interaction can be inferred from them)
    dplyr::mutate(pairs = purrr::map(strsplit(!!col_sym, delimiter), ~utils::combn(.x, 2, simplify = FALSE))) %>%  # Generate all pairs of authors
    tidyr::unnest(pairs) %>%
    tidyr::unnest_wider(pairs, names_sep = "_") %>%
    dplyr::mutate(pairs_1 = trimws(pairs_1), pairs_2 = trimws(pairs_2)) %>%  # Remove leading/trailing whitespaces
    dplyr::rowwise() %>%
    dplyr::mutate(Author1 = min(pairs_1, pairs_2), Author2 = max(pairs_1, pairs_2)) %>%  # Order author pairs alphabetically
    dplyr::ungroup() %>%
    dplyr::count(Author1, Author2, name = "weight")  # Count the frequency of each pair

  graph <- igraph::graph_from_data_frame(edges, directed = FALSE)
  return(graph)
}

#' @export
get_network_description.Interactions <- function(interactions) {
  interactions$directed <- igraph::is_directed(interactions$graph)
  interactions$weighted <- igraph::is_weighted(interactions$graph)
  return(interactions)
}

get_network_description <- function(interactions) {
  UseMethod("get_network_description", interactions)
}

#' @export
get_cohesion.Interactions <- function(interactions) {
  interactions$dyadcount <- network::network.dyadcount(interactions$network) # How many dyads? (n*n-1)
  interactions$edgecount <- network::network.edgecount(interactions$network) # How many edges?
  interactions$size <- network::network.size(interactions$network) # How large is the network?
  return(interactions)
}

get_cohesion <- function(interactions) {
  UseMethod("get_cohesion", interactions)
}

#' @export
get_density.Interactions <- function(interactions) {
  return(sna::gden(interactions$network))
}

get_density <- function(interactions) {
  UseMethod("get_density", interactions)
}

#' @export
get_transitivity.Interactions <- function(interactions) {
  return(sna::gtrans(interactions$network))
}

get_transitivity <- function(interactions) {
  UseMethod("get_transitivity", interactions)
}

#' @export
get_centrality.Interactions <- function(interactions) {
  return(igraph::degree(interactions$graph))
}

get_centrality <- function(interactions) {
  UseMethod("get_centrality", interactions)
}

#' @export
get_betweenness.Interactions <- function(interactions) {
  return(igraph::betweenness(interactions$graph))
}

get_betweenness <- function(interactions) {
  UseMethod("get_betweenness", interactions)
}

#' @export
get_closeness.Interactions <- function(interactions) {
  return(igraph::closeness(interactions$graph))
}

get_closeness <- function(interactions) {
  UseMethod("get_closeness", interactions)
}

#' @export
get_diameter.Interactions <- function(interactions) {
  return(igraph::diameter(interactions$graph, directed = FALSE, weights = NA))
}

get_diameter <- function(interactions) {
  UseMethod("get_diameter", interactions)
}

#' @export
plot_graph.Interactions <- function(interactions, output_file = "graph.png") {
  grDevices::png(filename = output_file, width = 2500, height = 2500, res = 400)
  comm <- igraph::cluster_louvain(interactions$graph)
  colors <- grDevices::rainbow(length(unique(comm$membership)), alpha = 0.4)

  centrality <- get_centrality(interactions)
  vertex_size <- 1 + (centrality / max(centrality)) * 15

  graphics::par(mfrow = c(1, 1), mar = c(1, 1, 1, 1))
  graphics::plot(
    interactions$graph,
    vertex.label = NA,
    vertex.size = vertex_size,
    vertex.color = colors[comm$membership],
    mark.groups = split(1:vcount(interactions$graph), comm$membership),
    mark.col = colors,
    mark.border = NA,
    edge.width = 0.8,
    edge.color = "gray",
    layout = layout.fruchterman.reingold
  )
  dev.off()
}

plot_graph <- function(interactions, output_file) {
  UseMethod("plot_graph", interactions)
}
