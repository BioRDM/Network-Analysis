library(igraph)
library(intergraph)
library(statnet)

#' @export
Interactions <- function(file_path,
                         author_delimiter = ";",
                         csv_column_name = "Author",
                         max_authors_per_paper = 50,
                         min_papers_per_author = 1,
                         directed = FALSE) {

  if (grepl(".csv", file_path)) {
    data <- utils::read.csv(file_path, stringsAsFactor = FALSE)
    if (!csv_column_name %in% colnames(data)) {
      stop(paste0("Column name ", csv_column_name, " not found in the dataset."))
    }
    result <- filter_papers_by_authors(data,
                                       column_name = csv_column_name,
                                       delimiter = author_delimiter,
                                       max_authors = max_authors_per_paper)
    data <- result[[1]]
    papers_removed <- result[[2]]
    n_papers <- nrow(data)
    graph <- make_graph_from_df(data,
                                delimiter = author_delimiter,
                                column_name = csv_column_name,
                                max_authors = max_authors_per_paper,
                                directed = directed)
    result <- filter_small_authors(graph, min_occurrences = min_papers_per_author)
    graph <- result[[1]]
    authors_removed <- result[[2]]
  } else if (grepl(".net", file_path)) {
    data <- NA
    n_papers <- NA
    papers_removed <- NA
    graph <- load_graph(file_path)
  } else {
    stop("File type not supported.")
  }
  network <- intergraph::asNetwork(graph)

  interactions <- list(file_path = file_path,
                       graph = graph,
                       network = network,
                       data = data,
                       n_papers = n_papers,
                       max_authors = max_authors_per_paper,
                       min_papers = min_papers_per_author,
                       papers_removed = papers_removed,
                       authors_removed = authors_removed,
                       directed = directed)

  # Assign the class name
  class(interactions) <- "Interactions"

  return(interactions)
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
