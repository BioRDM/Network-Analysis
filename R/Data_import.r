library(rlang)
library(dplyr)
library(tidyverse)

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
make_graph_from_df <- function(data, delimiter = ";", column_name = "Author", max_authors = 50, directed = FALSE) {
  col_sym <- rlang::sym(column_name)

  # Create edges: pairwise combinations of co-authors
  edges <- data %>%
    dplyr::mutate(item_list = stringr::str_split(!!col_sym, delimiter)) %>%
    dplyr::filter(purrr::map_int(item_list, length) > 1) %>%  # Remove papers with less than 2 authors
    dplyr::mutate(pairs = purrr::map(stringr::str_split(!!col_sym, delimiter), ~utils::combn(.x, 2, simplify = FALSE))) %>%  # Generate all pairs of items
    tidyr::unnest(pairs) %>%
    tidyr::unnest_wider(pairs, names_sep = "_") %>%
    dplyr::mutate(pairs_1 = trimws(pairs_1), pairs_2 = trimws(pairs_2)) %>%  # Remove leading/trailing whitespaces
    dplyr::rowwise() %>%
    dplyr::mutate(item1 = min(pairs_1, pairs_2), item2 = max(pairs_1, pairs_2)) %>%  # Order item pairs alphabetically
    dplyr::ungroup() %>%
    dplyr::count(item1, item2, name = "weight")  # Count the frequency of each pair

  graph <- igraph::graph_from_data_frame(edges, directed = directed)
  return(graph)
}

#' @export
filter_papers_by_authors <- function(data, column_name = "Author", delimiter = ";", max_authors) {
  col_sym <- rlang::sym(column_name)

  filtered_data <- data %>%
    dplyr::mutate(item_list = stringr::str_split(!!col_sym, delimiter)) %>%
    dplyr::mutate(Num_items = purrr::map_int(item_list, length))

  too_few <- (filtered_data %>%
                filter(Num_items <= 1) %>%
                dplyr::summarize(count = n()) %>%
                dplyr::pull(count))
  too_many <- (filtered_data %>%
                 filter(Num_items > max_authors) %>%
                 dplyr::summarize(count = n()) %>%
                 dplyr::pull(count))

  if (too_few > 0) {
    print(paste0(too_few, " papers were removed from the dataset due to having only one author."))
  }
  if (too_many > 0) {
    print(paste0(too_many, " papers were removed from the dataset due to having more than ", max_authors, " authors."))
  }
  filtered_data <- filtered_data %>% dplyr::filter(Num_items <= max_authors & Num_items > 1)
  print(paste0(nrow(filtered_data), " papers were included in the network analysis."))
  return(list(filtered_data, too_many, too_few))
}

#' @export
filter_small_authors <- function(graph, min_occurrences) {
  # Get the edge list from the graph
  edge_list <- igraph::as_data_frame(graph, what = "edges")

  # Count the occurrences of each author
  author_counts <- edge_list %>%
    tidyr::pivot_longer(cols = starts_with("from") | starts_with("to"),
                        names_to = "type", values_to = "author") %>%
    dplyr::count(author, name = "count")

  # Filter authors that appear more than min_occurrences times
  frequent_authors <- author_counts %>%
    dplyr::filter(count >= min_occurrences) %>%
    dplyr::pull(author)

  # Remove vertices from the graph that are not in the frequent_authors list
  vertices_to_remove <- setdiff(igraph::V(graph)$name, frequent_authors)
  filtered_graph <- igraph::delete_vertices(graph, vertices_to_remove)

  if (length(vertices_to_remove) > 0) {
    print(paste0("Removed ", length(vertices_to_remove), " authors that appeared less than ", min_occurrences, " times."))
  }
  print(paste0("Constructed network with ", igraph::vcount(filtered_graph), " authors."))

  return(list(filtered_graph, length(vertices_to_remove)))
}
