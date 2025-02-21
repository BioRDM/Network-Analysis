library(rlang)
library(dplyr)
library(tidyverse)

#' @export
filter_papers_by_authors <- function(data, column_name = "Author", delimiter = ";", max_authors) {
  col_sym <- rlang::sym(column_name)

  filtered_data <- data %>%
    dplyr::mutate(item_list = stringr::str_split(!!col_sym, delimiter)) %>%
    dplyr::mutate(Num_items = purrr::map_int(item_list, length))

  too_few <- filtered_data %>% dplyr::filter(Num_items <= 1) %>% nrow()
  too_many <- filtered_data %>% dplyr::filter(Num_items > max_authors) %>% nrow()

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
