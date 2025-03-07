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
    print("No papers with more than one author were found in the dataset. Skipping report.")
    return(NULL)
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
tidy_authors <- function(data, author_column = "Author", source_column = "Source", delimiter = ";") {
  author_col <- rlang::sym(author_column)
  if ("Source" %in% colnames(data)) {
    source_col <- rlang::sym(source_column)

    data <- data %>%
      dplyr::mutate(!!author_col := dplyr::case_when(
        toupper(!!source_col) == "PUBMED" ~ stringr::str_replace_all(!!author_col, ",", ";"),
        TRUE ~ !!author_col
      ))
  }

  data <- data %>%
    dplyr::mutate(!!author_col := stringi::stri_trans_general(!!author_col, "Latin-ASCII")) %>%  # Convert special characters
    dplyr::mutate(!!author_col := stringr::str_replace_all(!!author_col, paste0("[^A-Za-z", delimiter, "]"), "")) %>%  # Remove non-letter characters except delimiters
    dplyr::mutate(!!author_col := stringr::str_replace_all(!!author_col, "\\s+", "")) %>% # Remove all white spaces
    dplyr::filter(!is.na(!!author_col))

  return(data)
}

#' @export
read_config <- function(config) {
  # Replace spaces with dots in column names to match R column names
  config$author_column_name <- gsub(" ", ".", config$author_column_name)
  config$year_column_name <- gsub(" ", ".", config$year_column_name)
  # Define default values for the configuration options
  default_config <- list(
    input_name = tools::file_path_sans_ext(basename(config$file_path)),
    output_path = "output",
    author_column_name = "Author",
    author_delimiter = ";",
    year_column_name = "Year",
    max_authors_per_paper = 50,
    min_papers_per_author = 2,
    directed = FALSE,
    from_year = NULL,
    to_year = NULL,
    split_per_year = NULL
  )

  # Merge the provided config with the default config
  config <- modifyList(default_config, config, keep.null = TRUE)

  return(config)
}