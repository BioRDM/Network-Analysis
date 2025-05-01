#' @export
filter_by_year <- function(data, year_column_name, from_year, to_year) {
  check_year_column(data, year_column_name)
  check_year_filter(from_year, to_year)

  data <- year_parser(data, year_column_name)
  from_year <- as.Date(from_year, format = "%Y")
  to_year <- as.Date(to_year, format = "%Y")

  col <- rlang::sym(year_column_name)
  if (length(from_year) > 0 && length(to_year) > 0) {
    data |> dplyr::filter(!!col >= from_year & !!col <= to_year)
  } else if (length(from_year) > 0) {
    data |> dplyr::filter(!!col >= from_year)
  } else if (length(to_year) > 0) {
    data |> dplyr::filter(!!col <= to_year)
  } else {
    data
  }
}

#' @export
#' @importFrom rlang .data
filter_papers_by_authors <- function(data, column_name = "Author", delimiter = ";", max_authors) {
  col_sym <- rlang::sym(column_name)

  max_authors <- ifelse(is.null(max_authors), 9999, max_authors)

  filtered_data <- data |>
    dplyr::mutate(item_list = stringr::str_split(!!col_sym, delimiter)) |>
    dplyr::mutate(Num_items = purrr::map_int(.data$item_list, length))

  too_few <- filtered_data |> dplyr::filter(.data$Num_items <= 1) |> nrow()
  too_many <- filtered_data |> dplyr::filter(.data$Num_items > max_authors) |> nrow()

  if (too_few > 0) {
    print(paste0(too_few, " papers were removed from the dataset due to having only one author."))
  }
  if (too_many > 0) {
    print(paste0(too_many, " papers were removed from the dataset due to having more than ", max_authors, " authors."))
  }
  filtered_data <- filtered_data |>
    dplyr::filter(.data$Num_items <= max_authors & .data$Num_items > 1) |>
    dplyr::select(-"item_list", -"Num_items")
  print(paste0(nrow(filtered_data), " papers were included in the network analysis."))

  list(filtered_data, too_many, too_few)
}

#' @export
#' @importFrom rlang .data
#' @importFrom data.table :=
filter_infrequent_authors <- function(data, column_name = "Author", delimiter = ";", min_occurrences = 5) {
  col_sym <- rlang::sym(column_name)

  min_occurrences <- ifelse(is.null(min_occurrences), 0, min_occurrences)

  author_list <- data |>
    dplyr::mutate(item_list = stringr::str_split(!!col_sym, delimiter)) |>
    tidyr::unnest("item_list") |>
    dplyr::rename(author = "item_list")

  all_authors <- unique(author_list$author)

  frequent_authors <- author_list |>
    dplyr::count(.data$author, name = "count") |>
    dplyr::filter(.data$count >= min_occurrences) |>
    dplyr::pull(.data$author)

  removed_authors <- length(setdiff(all_authors, frequent_authors))

  filtered_data <- data |>
    dplyr::mutate(item_list = stringr::str_split(!!col_sym, delimiter)) |>
    dplyr::mutate(item_list = purrr::map(.data$item_list, ~ .x[.x %in% frequent_authors])) |>
    dplyr::mutate(!!col_sym := purrr::map_chr(.data$item_list, ~ paste(.x, collapse = delimiter))) |>
    dplyr::filter(!!col_sym != "") |>
    dplyr::mutate(item_list = stringr::str_split(!!col_sym, delimiter)) |>
    dplyr::filter(purrr::map_int(.data$item_list, length) >= 2) |>
    dplyr::select(-"item_list")

  if (removed_authors > 0) {
    print(paste0("Removed ", removed_authors, " authors that appeared less than ", min_occurrences, " times."))
  }

  list(filtered_data, removed_authors)
}
