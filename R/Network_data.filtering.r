#' @export
filter_by_year <- function(data, ...) UseMethod("filter_by_year")
#' @export
filter_by_year.network_data <- function(data, from_year, to_year) {
  check_column(data$filtered, data$year_column)
  check_year_range(from_year, to_year)
  data$filtered <- filter_by_year.data.frame(
    data$filtered, data$year_column, from_year, to_year
  )
  data
}
#' @export
filter_by_year.data.frame <- function(data, year_column, from_year, to_year) {
  from_year <- as.Date(from_year, format = "%Y")
  to_year <- as.Date(to_year, format = "%Y")
  data$filtered[[year_column]] <- parse_year(data[[year_column]])
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
filter_by_vertex_count <- function(data, ...) UseMethod("filter_by_vertex_count")

#' @importFrom rlang .data
#' @export
filter_by_vertex_count.network_data <- function(data, max_vertices = Inf) {
  check_column(data$filtered, data$vertex_column)
  group_col <- setdiff(names(data$filtered), data$vertex_column)

  vertex_counts <- data$filtered |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_col))) |>
    dplyr::summarise(vertex_count = dplyr::n(), .groups = "drop")

  filtered_data <- data$filtered |>
    dplyr::left_join(vertex_counts, by = group_col)

  n_too_few <- filtered_data |> dplyr::filter(.data$vertex_count <= 1) |> dplyr::pull(!!group_col) |> unique() |> length()
  n_too_many <- filtered_data |> dplyr::filter(.data$vertex_count > max_vertices) |> dplyr::pull(!!group_col) |> unique() |> length()

  filtered_data <- filtered_data |>
    dplyr::filter(.data$vertex_count <= max_vertices & .data$vertex_count > 1) |>
    dplyr::select(-"vertex_count")

  data$filtered <- filtered_data
  data$n_too_many_vertices <- n_too_many
  data$n_too_few_vertices <- n_too_few

  data
}

#' @importFrom rlang .data
#' @export
filter_by_vertex_count.data.frame <- function(data, vertex_column, max_vertices = Inf) {
  group_col <- setdiff(names(data), vertex_column)

  vertex_counts <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_col))) |>
    dplyr::summarise(vertex_count = dplyr::n(), .groups = "drop")

  filtered_data <- data |>
    dplyr::left_join(vertex_counts, by = group_col) |>
    dplyr::filter(.data$vertex_count <= max_vertices & .data$vertex_count > 1) |>
    dplyr::select(-"vertex_count")

  filtered_data
}

#' @export
filter_infrequent_vertices <- function(data, ...) UseMethod("filter_infrequent_vertices")

#' @importFrom rlang .data
#' @export
filter_infrequent_vertices.network_data <- function(data, min_occurrences = 5) {
  check_vertex_column(data$filtered, data$vertex_column)
  col_sym <- rlang::sym(data$vertex_column)

  vertex_counts <- data$filtered |>
    dplyr::count(!!col_sym, name = "count")

  frequent_vertices <- vertex_counts |>
    dplyr::filter(.data$count >= min_occurrences) |>
    dplyr::pull(!!col_sym)

  n_removed_vertices <- length(setdiff(vertex_counts[[data$vertex_column]], frequent_vertices))

  filtered_data <- data$filtered |>
    dplyr::filter((!!col_sym) %in% frequent_vertices)

  data$filtered <- filtered_data
  data$n_infrequent_vertices_removed <- n_removed_vertices

  data
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
