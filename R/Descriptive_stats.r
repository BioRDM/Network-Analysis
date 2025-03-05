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

#' @export
get_summary_stats <- function(interactions) {
  # Calculate summary statistics
  author_stats <- get_author_stats(interactions$data, author_column_name = interactions$author_column_name, delimiter = interactions$author_delimiter)
  centrality <- get_centrality(interactions)
  diameter <- get_diameter(interactions)

  # Create a summary table
  summary_table <- data.frame(
    "Start_year" = interactions$from_year,
    "End_year" = interactions$to_year,
    "Total_Papers" = interactions$n_papers,
    "Total_Authors" = author_stats$sum,
    "Average_Authors_per_Paper" = author_stats$average,
    "Median_Authors_per_Paper" = author_stats$median,
    "Min_Authors_per_Paper" = author_stats$min,
    "Max_Authors_per_Paper" = author_stats$max,
    "Density" = get_density(interactions),
    "Transitivity" = get_transitivity(interactions),
    "Mean_degree_centrality" = round(mean(centrality$degree), digits = 3),
    "Mean_betweenness_centrality" = round(mean(centrality$betweenness), digits = 3),
    "Mean_harmonic_centrality" = round(mean(centrality$harmonic), digits = 3),
    "Mean_shortest_path" = diameter$average_shortest_path,
    "Number_of_cutpoints" = sum(get_cutpoints(interactions))
  )
  return(summary_table)
}
