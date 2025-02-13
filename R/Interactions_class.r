library(rlang)
library(dplyr)
library(tidyverse)
library(igraph)
library(intergraph)
library(statnet)

#' @export
Interactions <- function(file_path, csv_delimiter = ";", csv_column_name = "Author", directed = FALSE) {
  if (grepl(".csv", file_path)) {
    data <- utils::read.csv(file_path, stringsAsFactor = FALSE)
    graph <- make_graph_from_csv(file_path, delimiter = csv_delimiter, column_name = csv_column_name, directed = directed)
  } else if (grepl(".net", file_path)) {
    data <- NA
    graph <- load_graph(file_path)
  } else {
    stop("File type not supported.")
  }
  network <- intergraph::asNetwork(graph)

  interactions <- list(file_path = file_path, graph = graph, network = network, data = data, directed = directed)

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
make_graph_from_csv <- function(file_path, delimiter = ";", column_name = "Author", directed = FALSE) {
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

  graph <- igraph::graph_from_data_frame(edges, directed = directed)
  return(graph)
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
  return(igraph::diameter(interactions$graph, directed = interactions$directed, weights = NULL))
}

get_diameter <- function(interactions) {
  UseMethod("get_diameter", interactions)
}

#' @export
get_communities.Interactions <- function(interactions) {
  return(igraph::cluster_louvain(interactions$graph))
}

get_communities <- function(interactions) {
  UseMethod("get_communities", interactions)
}

#' @export
get_most_central_per_community.Interactions <- function(interactions) {
  comm <- get_communities(interactions)
  centrality <- get_centrality(interactions)
  most_central_authors <- sapply(unique(comm$membership), function(group) {
    group_vertices <- which(comm$membership == group)
    group_centrality <- centrality[group_vertices]
    most_central_vertex <- group_vertices[which.max(group_centrality)]
    igraph::V(interactions$graph)$name[most_central_vertex]
  })
  return(most_central_authors)
}

get_most_central_per_community <- function(interactions) {
  UseMethod("get_most_central_per_community", interactions)
}

#' @export
plot_graph.Interactions <- function(interactions, output_file = "output/graph.png") {
  grDevices::png(filename = output_file, width = 2500, height = 1800, res = 360)

  comm <- get_communities(interactions)
  colors <- grDevices::rainbow(length(unique(comm$membership)), alpha = 0.4)

  centrality <- get_centrality(interactions)
  vertex_size <- 1 + (centrality / max(centrality)) * 15

  graphics::par(mfrow = c(1, 1), mar = c(1, 1, 1, 10))
  graphics::plot(
    interactions$graph,
    vertex.label = NA,
    vertex.size = vertex_size,
    vertex.color = colors[comm$membership],
    mark.groups = split(1:igraph::vcount(interactions$graph), comm$membership),
    mark.col = colors,
    mark.border = NA,
    edge.width = 0.8,
    edge.color = "gray",
    layout = igraph::layout_with_fr(interactions$graph)
  )

  most_central_authors <- get_most_central_per_community(interactions)
  add_graph_legend(leg_x = 1.3, leg_y = 0, leg_items = most_central_authors, leg_colors = colors, leg_title = "Most central author")

  dev.off()
}

plot_graph <- function(interactions, output_file) {
  UseMethod("plot_graph", interactions)
}

#' @export
add_graph_legend <- function(leg_x, leg_y, leg_items, leg_colors, leg_title) {
  par(xpd = NA)
  leg_spread <- 0.12 * length(leg_items) / 2
  leg_y <- seq(leg_y - leg_spread, leg_y + leg_spread, length.out = length(leg_items))
  text(x = leg_x,
       y = max(leg_y) + 0.12,
       labels = leg_title,
       adj = c(0, 0.5),
       cex = 1,
       font = 2)
  points(x = rep(leg_x, length(leg_items)),
         y = leg_y,
         col = "black",
         bg = leg_colors,
         pch = 21,
         cex = 2)
  text(x = rep(leg_x + 0.1, length(leg_items)),
       y = leg_y,
       col = "black",
       labels = leg_items,
       adj = c(0, 0.5),
       cex = 0.8)
}