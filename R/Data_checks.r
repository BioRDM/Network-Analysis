check_file_format <- function(file_path) {
  if (!grepl(".csv$", file_path)) {
    cli::cli_abort(c(
      "x" = ".{tools::file_ext(file_path)} format is not supported.",
      "i" = "Please upload a table in CSV format."
    ))
  }
}

check_split_per_year <- function(split_per_year) {
  if (!is.numeric(split_per_year) && !is.null(split_per_year)) {
    cli::cli_abort(c(
      "x" = "{.arg split_per_year} must be an integer value or NULL.",
      "i" = "You provided a {.cls {class(split_per_year)}}."
    ))
  }
}

check_author_column <- function(data, author_column_name) {
  if (!author_column_name %in% colnames(data)) {
    cli::cli_abort(c(
      "x" = "{.arg author_column_name} {.val {author_column_name}} not found in the input table.",
      "i" = "Please check the author column name in your table.",
      "i" = "Ensure the column name matches case and pluralisation."
    ))
  }
}

check_year_column <- function(data, year_column_name) {
  if (!year_column_name %in% colnames(data)) {
    cli::cli_abort(c(
      "x" = "{.arg year_column_name} {.val {year_column_name}} not found in the input table.",
      "i" = "Please check the year column name in your table.",
      "i" = "Ensure the column name matches case and pluralisation."
    ))
  }
}

check_year_filter <- function(from_year, to_year) {
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
