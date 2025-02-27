library(rlang)
library(dplyr)
library(lubridate)

#' @export
get_years_from_to <- function(data, config) {
  year_col <- config$year_column_name

  # Check config and data
  check_split_per_year(config$split_per_year)
  check_year_column(data, year_col)

  # Parse year column in case there are different formats
  data <- year_parser(data, year_col)

  # Define start and end years to process the data
  year_from <- max(min(data[[year_col]]), config$from_year)
  year_to <- min(max(data[[year_col]]), config$to_year)
  if (!is.null(config$split_per_year)) {
    years_from <- seq(year_from, year_to, by = config$split_per_year)
    years_to <- c(years_from[-1] - 1, max(data[[year_col]]))
  } else {
    years_from <- year_from
    years_to <- year_to
  }
  return(list(years_from = years_from, years_to = years_to))
}

#' @export
year_parser <- function(data, year_column_name) {
  date_formats <- c("Y", "y", "ymd", "mdy", "dmy", "Ymd", "mdY", "dmY")
  year_col <- sym(year_column_name)
  data <- data %>%
    dplyr::mutate(!!year_col := lubridate::parse_date_time(!!year_col,
                                                           orders = date_formats,
                                                           quiet = TRUE)) %>%
    dplyr::mutate(!!year_col := year(!!year_col)) %>%
    dplyr::mutate(!!year_col := as.numeric(!!year_col))
  return(data)
}