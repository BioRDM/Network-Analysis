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

  too_few <- filtered_data %>% filter(Num_items <= 1) %>% nrow()
  too_many <- filtered_data %>% filter(Num_items > max_authors) %>% nrow()

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
