#' @export
import_csv_data <- function(file_path) {
  check_file_format(file_path)
  data <- utils::read.csv(file_path, stringsAsFactor = FALSE)
  return(data)
}

#' @export
make_graph_from_df <- function(data, delimiter = ";", column_name = "Author", directed = FALSE) {
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
  print(paste0("Constructed network with ", graph %>% igraph::vcount(), " authors"))
  return(graph)
}

#' @export
tidy_authors <- function(data, author_column = "Author", source_column = "Source") {
  if ("Source" %in% colnames(data)) {
    author_col <- rlang::sym(author_column)
    source_col <- rlang::sym(source_column)

    data <- data %>%
      dplyr::mutate(!!author_col := dplyr::case_when(
        toupper(!!source_col) == "PUBMED" ~ stringr::str_replace_all(!!author_col, ",", ";"),
        TRUE ~ !!author_col
      )) %>%
      dplyr::mutate(!!author_col := stringi::stri_trans_general(!!author_col, "Latin-ASCII")) %>%  # Convert special characters
      dplyr::mutate(!!author_col := stringr::str_replace_all(!!author_col, "[^A-Za-z;]", "")) %>%  # Remove non-letter characters except semicolons
      dplyr::mutate(!!author_col := stringr::str_replace_all(!!author_col, "\\s+", ""))  # Remove all white spaces
  } else {
    print("No source column found. Skipping author tidying.")
  }
  return(data)
}

#' @export
read_config <- function(config) {
  # Define default values for the configuration options
  input_file <- tools::file_path_sans_ext(basename(config$file_path))
  default_config <- list(
    input_file = input_file,
    author_column_name = "Author",
    author_delimiter = ";",
    year_column_name = "Year",
    max_authors_per_paper = 50,
    min_papers_per_author = 2,
    directed = FALSE,
    from_year = NULL,
    to_year = NULL,
    output_path = "output",
    figures_path = "figures"
  )

  # Merge the provided config with the default config
  config <- modifyList(default_config, config)

  return(config)
}