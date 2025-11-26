#' @export
check_split_per_year <- function(split_per_year) {
  if (!is.numeric(split_per_year) && !is.null(split_per_year)) {
    cli::cli_abort(c(
      "x" = "{.arg split_per_year} must be an integer value or NULL.",
      "i" = "You provided a {.cls {class(split_per_year)}}."
    ))
  }
}

#' @export
check_column <- function(data, column, call = rlang::caller_env()) {
  if (length(column) == 0 || is.null(column)) {
    cli::cli_abort(c(
      "x" = "A required column name is NULL.",
      "i" = "Please provide a valid column name."
    ), call = call)
  }
  if (!column %in% colnames(data)) {
    cli::cli_abort(c(
      "x" = "Column {.val {column}} not found in the input table.",
      "i" = "Please check the column name in your table.",
      "i" = "Ensure the column name matches case and pluralisation."
    ), call = call)
  }
}

#' @export
check_delimiter <- function(delimiter, call = rlang::caller_env()) {
  if (!is.null(delimiter) && (!is.character(delimiter) || nchar(delimiter) == 0)) {
    cli::cli_abort(c(
      "x" = "{.arg delimiter} must be a non-empty string.",
      "i" = "Current delimiter: {.var {delimiter}}."
    ), call = call)
  }
}

#' @export
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

#' @export
check_palette_length <- function(custom_palette, custom_order) {
  if (length(custom_palette) != length(custom_order)) {
    cli::cli_abort(c(
      "x" = "Palette length must match the number of levels for the variable.",
      "!" = "If you provided both node_palette and node_order, they must be of the same length.",
      "!" = "If you provided only node_palette, ensure its length matches the number of unique values in the variable.",
      "i" = "Node palette has {.val {length(custom_palette)}} values and there are {.val {length(custom_order)}} levels to the variable."
    ))
  }
}
