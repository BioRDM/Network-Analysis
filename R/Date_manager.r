get_years_from_to <- function(data, config) {
  check_split_per_year(config$split_per_year)
  if (!is.null(config$split_per_year)) {
    years_from <- seq(min(data[[config$year_column_name]]),
                      max(data[[config$year_column_name]]),
                      by = config$split_per_year)
    years_to <- c(years_from[-1] - 1, max(data[[config$year_column_name]]))
  } else {
    years_from <- config$from_year
    years_to <- config$to_year
  }
  return(list(years_from = years_from, years_to = years_to))
}
