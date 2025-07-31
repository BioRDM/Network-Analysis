#' @export
network <- function(data,
                    vertex_column = NULL,
                    vertex_delimiter = NULL,
                    edge_id = NULL,
                    year_column = NULL) {
  obj <- list(
    raw = data,
    filtered = data,
    vertex_column = vertex_column,
    vertex_delimiter = vertex_delimiter,
    edge_id = edge_id,
    year_column = year_column,
    n_too_many_vertices = 0,
    n_infrequent_vertices_removed = 0
  )

  class(obj) <- "network"
  obj
}


#' @export
unnest_vertex_column <- function(...) UseMethod("unnest_vertex_column")
#' @export
unnest_vertex_column.network <- function(network) {
  network$raw <- unnest_vertex_column.data.frame(
    network$raw,
    column = network$vertex_column,
    delimiter = network$vertex_delimiter
  )
  network$filtered <- network$raw
  network
}
#' @export
unnest_vertex_column.data.frame <- function(data, column, delimiter = NULL) {
  check_column(data, column)
  check_delimiter(delimiter)
  if (!is.null(delimiter)) {
    data <- data |>
      tidyr::separate_longer_delim(!!rlang::sym(column), delim = delimiter)
  }
  data
}


#' @export
tidy_names <- function(...) UseMethod("tidy_names")
#' @export
tidy_names.network <- function(network, column) {
  check_column(network$filtered, column)
  tidy_names.data.frame(network$filtered, column = column)
}

#' @importFrom rlang .data
#' @importFrom data.table :=
#' @export
tidy_names.data.frame <- function(data, column) {
  if (suppressWarnings(!all(is.na(as.numeric(data[[column]]))))) {
    return(data)
  }
  data |>
    dplyr::mutate(!!rlang::sym(column) := stringi::stri_trans_general(!!rlang::sym(column), "Latin-ASCII")) |> # Convert special characters to ASCII
    dplyr::mutate(!!rlang::sym(column) := stringr::str_replace_all(!!rlang::sym(column), "[^A-Za-z]", "")) |> # Remove all non-alphabetic characters
    dplyr::mutate(!!rlang::sym(column) := stringr::str_replace_all(!!rlang::sym(column), "\\s+", "")) |> # Remove all white spaces
    dplyr::filter(!is.na(!!rlang::sym(column)))
}


#' @export
get_edges_per_vertex <- function(...) UseMethod("get_edges_per_vertex")
#' @export
get_edges_per_vertex.network <- function(network, raw = FALSE) {
  if (raw) {
    get_edges_per_vertex.data.frame(
      network$raw,
      vertex_column = network$vertex_column
    )
  } else {
    get_edges_per_vertex.data.frame(
      network$filtered,
      vertex_column = network$vertex_column
    )
  }
}
#' @export
get_edges_per_vertex.data.frame <- function(data, vertex_column) {
  check_column(data, vertex_column)
  data |>
    dplyr::group_by(.data[[vertex_column]]) |>
    dplyr::summarise(Edges = dplyr::n(), .groups = "drop")
}


#' @export
save_edges_per_vertex <- function(...) UseMethod("save_edges_per_vertex")
#' @export
save_edges_per_vertex.network <- function(network, output_file = "output/edges_per_vertex.csv") {
  edges_per_vertex <- dplyr::full_join(
    get_edges_per_vertex(network, raw = TRUE),
    get_edges_per_vertex(network, raw = FALSE),
    by = network$vertex_column,
    suffix = c("_prefilter", "_postfilter")
  ) |>
    dplyr::mutate(
      Edges_prefilter = tidyr::replace_na(.data$Edges_prefilter, 0),
      Edges_postfilter = tidyr::replace_na(.data$Edges_postfilter, 0),
      Edges_removed = .data$Edges_prefilter - .data$Edges_postfilter
    ) |>
    dplyr::arrange(dplyr::desc(.data$Edges_removed))

  utils::write.csv(edges_per_vertex, output_file, row.names = FALSE)
}
