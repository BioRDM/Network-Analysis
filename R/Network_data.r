#' @export
network_data <- function(data) {
  obj <- list(
    raw = data,
    filtered = data,
    vertex_column = NULL,
    vertex_delimiter = NULL,
    year_column = NULL
  )

  class(obj) <- "network_data"
  obj
}

#' @export
unnest_column <- function(data, ...) UseMethod("unnest_column")
#' @export
unnest_column.network_data <- function(data) {
  unnest_column.data.frame(
    data$filtered,
    column = data$vertex_column,
    delimiter = data$vertex_delimiter
  )
}
#' @export
unnest_column.data.frame <- function(data, column, delimiter) {
  check_column(data, column)
  check_delimiter(delimiter)
  data |>
    tidyr::separate_longer_delim(!!rlang::sym(column), delim = delimiter)
}

#' @export
tidy_names <- function(data, ...) UseMethod("tidy_names")
#' @export
tidy_names.network_data <- function(data, column, delimiter) {
  check_column(data$filtered, column)
  check_delimiter(delimiter)
  tidy_names.data.frame(data$filtered, column = column, delimiter = delimiter)
}

#' @importFrom rlang .data
#' @importFrom data.table :=
#' @export
tidy_names.data.frame <- function(data, column, delimiter) {
  if (suppressWarnings(!all(is.na(as.numeric(data[[column]]))))) {
    return(data)
  }

  data |>
    dplyr::mutate(!!rlang::sym(column) := stringi::stri_trans_general(!!rlang::sym(column), "Latin-ASCII")) |>
    dplyr::mutate(!!rlang::sym(column) := stringr::str_replace_all(!!rlang::sym(column), paste0("[^A-Za-z", delimiter, "]"), "")) |>
    dplyr::mutate(!!rlang::sym(column) := stringr::str_replace_all(!!rlang::sym(column), "\\s+", "")) |> # Remove all white spaces
    dplyr::filter(!is.na(!!rlang::sym(column)))
}
