#' @export
get_years_from_to <- function(data, config) {
  year_col <- config$year_column_name

  check_split_per_year(config$split_per_year)
  check_year_column(data, year_col)

  data <- year_parser(data, year_col)

  year_from <- max(min(data[[year_col]]), config$from_year)
  year_to <- min(max(data[[year_col]]), config$to_year)
  if (!is.null(config$split_per_year)) {
    years_from <- seq(year_from, year_to, by = config$split_per_year)
    years_to <- c(years_from[-1] - 1, year_to)
  } else {
    years_from <- year_from
    years_to <- year_to
  }
  list(years_from = years_from, years_to = years_to)
}

#' @export
#' @importFrom data.table :=
year_parser <- function(data, year_column_name) {
  date_formats <- c("Y", "y", "ymd", "mdy", "dmy", "Ymd", "mdY", "dmY")
  year_col <- rlang::sym(year_column_name)
  data |>
    dplyr::mutate(!!year_col := lubridate::parse_date_time(!!year_col,
                                                           orders = date_formats,
                                                           quiet = TRUE)) |>
    dplyr::mutate(!!year_col := lubridate::year(!!year_col)) |>
    dplyr::mutate(!!year_col := as.numeric(!!year_col)) |>
    dplyr::filter(!is.na(!!year_col))
}