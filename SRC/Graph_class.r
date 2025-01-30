library(igraph)
library(intergraph)
library(statnet)

Graph <- function(file_path) {
  data <- load_graph(file_path)
  network <- asNetwork(data)
  graph <- list(file_path = file_path, data = data, network = network)

  # Assign the class name
  class(graph) <- "Graph"

  return(graph)
}

load_graph <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("File not found.")
  } else {
    data <- read_graph(file_path, format = "pajek")
  }
  return(data)
}

get_network_description.Graph <- function(graph) {
  graph$directed <- is_directed(graph$data)
  graph$weighted <- is_weighted(graph$data)
  return(graph)
}

get_network_description <- function(graph) {
  UseMethod("get_network_description", graph)
}

get_cohesion.Graph <- function(graph) {
  graph$dyadcount <- network.dyadcount(graph$network) # How many dyads? (n*n-1)
  graph$edgecount <- network.edgecount(graph$network) # How many edges?
  graph$size <- network.size(graph$network) # How large is the network?
  return(graph)
}

get_cohesion <- function(graph) {
  UseMethod("get_cohesion", graph)
}

get_density.Graph <- function(graph) {
  return(gden(graph$network))
}

get_density <- function(graph) {
  UseMethod("get_density", graph)
}

get_transitivity.Graph <- function(graph) {
  return(gtrans(graph$network))
}

get_transitivity <- function(graph) {
  UseMethod("get_transitivity", graph)
}

get_centrality.Graph <- function(graph) {
  return(degree(graph$data))
}

get_centrality <- function(graph) {
  UseMethod("get_centrality", graph)
}

get_betweenness.Graph <- function(graph) {
  return(betweenness(graph$data))
}

get_betweenness <- function(graph) {
  UseMethod("get_betweenness", graph)
}

get_closeness.Graph <- function(graph) {
  return(closeness(graph$data))
}

get_closeness <- function(graph) {
  UseMethod("get_closeness", graph)
}

get_diameter.Graph <- function(graph) {
  return(diameter(graph$data, directed = FALSE, weights = NA))
}

get_diameter <- function(graph) {
  UseMethod("get_diameter", graph)
}
