get_years_from_to <- function(data, config) {
  check_split_per_year(config$split_per_year)
  year_from <- max(min(data[[config$year_column_name]]), config$from_year)
  year_to <- min(max(data[[config$year_column_name]]), config$to_year)
  if (!is.null(config$split_per_year)) {
    years_from <- seq(year_from, year_to, by = config$split_per_year)
    years_to <- c(years_from[-1] - 1, max(data[[config$year_column_name]]))
  } else {
    years_from <- year_from
    years_to <- year_to
  }
  return(list(years_from = years_from, years_to = years_to))
}
