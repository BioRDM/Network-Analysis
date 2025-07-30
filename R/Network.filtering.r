#' @export
filter_by_year <- function(...) UseMethod("filter_by_year")
#' @export
filter_by_year.network <- function(network, from_year, to_year) {
  check_column(network$raw, network$year_column)
  check_year_range(from_year, to_year)
  network$filtered <- filter_by_year.data.frame(
    network$filtered, network$year_column, from_year, to_year
  )
  network$from_year <- from_year
  network$to_year <- to_year
  network
}
#' @export
filter_by_year.data.frame <- function(data, year_column, from_year, to_year) {
  from_year <- as.Date(from_year, format = "%Y")
  to_year <- as.Date(to_year, format = "%Y")
  data[[year_column]] <- parse_year(data[[year_column]])
  col <- rlang::sym(year_column)
  if (length(from_year) > 0 && length(to_year) > 0) {
    dplyr::filter(data, !!col >= from_year & !!col <= to_year)
  } else if (length(from_year) > 0) {
    dplyr::filter(data, !!col >= from_year)
  } else if (length(to_year) > 0) {
    dplyr::filter(data, !!col <= to_year)
  } else {
    data
  }
}


#' @export
filter_by_vertex_occurences <- function(...) UseMethod("filter_by_vertex_occurences")

#' @importFrom rlang .data
#' @export
filter_by_vertex_occurences.network <- function(network, max_vertices = Inf) {
  check_column(network$filtered, network$vertex_column)
  check_column(network$filtered, network$edge_id)
  group_col <- network$edge_id

  col_sym <- rlang::sym(group_col)

  vertex_counts <- network$filtered |>
    dplyr::group_by(!!col_sym) |>
    dplyr::summarise(vertex_count = dplyr::n(), .groups = "drop")

  filtered_data <- network$filtered |>
    dplyr::left_join(vertex_counts, by = group_col)

  n_too_few <- filtered_data |> dplyr::filter(.data$vertex_count <= 1) |> dplyr::pull(!!col_sym) |> unique() |> length()
  n_too_many <- filtered_data |> dplyr::filter(.data$vertex_count > max_vertices) |> dplyr::pull(!!col_sym) |> unique() |> length()

  filtered_data <- filtered_data |>
    dplyr::filter(.data$vertex_count <= max_vertices & .data$vertex_count > 1) |>
    dplyr::select(-"vertex_count")

  network$filtered <- filtered_data
  network$n_too_many_vertices <- n_too_many
  network$n_too_few_vertices <- n_too_few

  network
}

#' @importFrom rlang .data
#' @export
filter_by_vertex_occurences.data.frame <- function(data, vertex_column, edge_id, max_vertices = Inf) {
  col_sym <- rlang::sym(edge_id)

  vertex_counts <- data |>
    dplyr::group_by(!!col_sym) |>
    dplyr::summarise(vertex_count = dplyr::n(), .groups = "drop")

  filtered_data <- data |>
    dplyr::left_join(vertex_counts, by = edge_id) |>
    dplyr::filter(.data$vertex_count <= max_vertices & .data$vertex_count > 1) |>
    dplyr::select(-"vertex_count")

  filtered_data
}

#' @export
filter_infrequent_vertices <- function(...) UseMethod("filter_infrequent_vertices")

#' @importFrom rlang .data
#' @export
filter_infrequent_vertices.network <- function(network, min_occurrences = 5) {
  check_vertex_column(network$filtered, network$vertex_column)
  col_sym <- rlang::sym(network$vertex_column)

  vertex_counts <- network$filtered |>
    dplyr::count(!!col_sym, name = "count")

  frequent_vertices <- vertex_counts |>
    dplyr::filter(.data$count >= min_occurrences) |>
    dplyr::pull(!!col_sym)

  n_removed_vertices <- length(setdiff(vertex_counts[[network$vertex_column]], frequent_vertices))

  filtered_data <- network$filtered |>
    dplyr::filter((!!col_sym) %in% frequent_vertices)

  network$filtered <- filtered_data
  network$n_infrequent_vertices_removed <- n_removed_vertices

  network
}

#' @importFrom rlang .data
#' @export
filter_infrequent_vertices.data.frame <- function(data, vertex_column, min_occurrences = 5) {
  col_sym <- rlang::sym(vertex_column)

  vertex_counts <- data |>
    dplyr::count(!!col_sym, name = "count")

  frequent_vertices <- vertex_counts |>
    dplyr::filter(.data$count >= min_occurrences) |>
    dplyr::pull(!!col_sym)

  filtered_data <- data |>
    dplyr::filter((!!col_sym) %in% frequent_vertices)

  filtered_data
}
