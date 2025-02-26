check_file_format <- function(file_path) {
  if (!grepl(".csv$", file_path)) {
    stop("Only CSV files are supported.")
  }
}

check_split_per_year <- function(split_per_year) {
  if (!is.numeric(split_per_year) && !is.null(split_per_year)) {
    stop("split_per_year must be an integer value or NULL.")
  }
}

check_author_column <- function(data, author_column_name) {
  if (!author_column_name %in% colnames(data)) {
    stop(paste0("Column name ", author_column_name, " not found in the dataset."))
  }
}

check_year_column <- function(data, year_column_name) {
  if (!year_column_name %in% colnames(data)) {
    stop(paste0("Column name ", year_column_name, " not found in the dataset."))
  }
}

check_year_filter <- function(from_year, to_year) {
  if (!is.null(from_year) && !is.numeric(from_year)) {
    stop("from_year must be a numeric value or NULL.")
  }
  if (!is.null(to_year) && !is.numeric(to_year)) {
    stop("to_year must be a numeric value or NULL.")
  }
  if (!is.null(from_year) && !is.null(to_year) && from_year > to_year) {
    stop("from_year must be less than or equal to to_year.")
  }
}