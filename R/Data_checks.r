check_split_per_year <- function(split_per_year) {
  if (!is.numeric(split_per_year) && !is.null(split_per_year)) {
    cli::cli_abort(c(
      "x" = "{.arg split_per_year} must be an integer value or NULL.",
      "i" = "You provided a {.cls {class(split_per_year)}}."
    ))
  }
}

check_column <- function(data, column) {
  if (!column %in% colnames(data)) {
    cli::cli_abort(c(
      "x" = "{.arg column} {.val {column}} not found in the input table.",
      "i" = "Please check the vertex column name in your table.",
      "i" = "Ensure the column name matches case and pluralisation."
    ))
  }
}

check_delimiter <- function(delimiter) {
  if (is.null(delimiter) || !is.character(delimiter) || nchar(delimiter) == 0) {
    cli::cli_abort(c(
      "x" = "{.arg delimiter} must be a non-empty string.",
      "i" = "Current delimiter: {.var {delimiter}}."
    ))
  }
}

check_year_range <- function(from_year, to_year) {
  if (!is.null(from_year) && !is.numeric(from_year)) {
    cli::cli_abort(c(
      "x" = "{.arg from_year} must be a numeric value or NULL.",
      "i" = "You provided a {.cls {class(from_year)}}."
    ))
  }
  if (!is.null(to_year) && !is.numeric(to_year)) {
    cli::cli_abort(c(
      "x" = "{.arg to_year} must be a numeric value or NULL.",
      "i" = "You provided a {.cls {class(to_year)}}."
    ))
  }
  if (!is.null(from_year) && !is.null(to_year) && from_year > to_year) {
    cli::cli_abort(c(
      "x" = "{.arg from_year} must be less than or equal to {.arg to_year}.",
      "i" = "You provided {.arg from_year} = {.val {from_year}} and {.arg to_year} = {.val {to_year}}."
    ))
  }
}
