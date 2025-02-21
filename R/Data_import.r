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

  data <- (data %>%
             dplyr::mutate(item_list = stringr::str_split(!!col_sym, delimiter)) %>%
             dplyr::filter(purrr::map_int(item_list, length) > 1))  # Remove papers with less than 2 authors

  if (data %>% nrow() < 1) {
    stop("No papers with more than one author were found in the dataset. Check that the author delimiter is correctly set.")
  }

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
