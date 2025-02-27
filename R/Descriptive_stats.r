#' @export
get_author_stats <- function(data, author_column_name = "Author", delimiter = ";") {
  # Extract the author column
  author_column <- data[[author_column_name]]

  # Count the number of authors per line
  author_counts <- sapply(author_column, function(x) length(strsplit(x, delimiter)[[1]]))

  # Calculate statistics
  avg_authors <- mean(author_counts)
  median_authors <- median(author_counts)
  min_authors <- min(author_counts)
  max_authors <- max(author_counts)

  all_authors <- unique(unlist(strsplit(paste(author_column, collapse = delimiter), delimiter)))
  unique_author_count <- length(all_authors)

  # Return the statistics as a list
  return(list(
    sum = unique_author_count,
    average = avg_authors,
    median = median_authors,
    min = min_authors,
    max = max_authors
  ))
}