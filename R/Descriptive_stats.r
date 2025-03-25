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

  return(list(
    sum = unique_author_count,
    average = avg_authors,
    median = median_authors,
    min = min_authors,
    max = max_authors
  ))
}

#' @export
get_papers_per_author <- function(data, author_column_name = "Author", delimiter = ";") {
  # Extract the author column
  author_column <- data[[author_column_name]]

  # Split the author column by the delimiter
  authors <- strsplit(author_column, delimiter)

  # Flatten the list of authors
  all_authors <- unlist(authors)

  # Remove empty strings
  all_authors <- all_authors[all_authors != ""]

  # Count the number of occurences of each author in the list
  author_df <- as.data.frame(table(all_authors), stringsAsFactors = FALSE)
  colnames(author_df) <- c("Author", "Papers")

  return(author_df)
}

#' @export
get_summary_stats <- function(interactions) {
  # Calculate summary statistics
  author_stats <- get_author_stats(interactions$data, author_column_name = interactions$author_column_name, delimiter = interactions$author_delimiter)
  centrality <- get_centrality(interactions)
  diameter <- get_diameter(interactions)
  cutpoints <- get_cutpoints(interactions)

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
    "Number_of_cutpoints" = length(cutpoints),
    "Cutpoint_authors" = paste(cutpoints, collapse = "; ")
  )
  return(summary_table)
}

save_papers_per_author <- function(prefilter_papers_per_author, postfilter_papers_per_author, output_file = "output/centrality_data.csv") {
  # Merge prefilter and postfilter dataframes
  merged_papers_per_author <- dplyr::full_join(
    prefilter_papers_per_author %>% dplyr::rename(Papers_prefilter = Papers),
    postfilter_papers_per_author %>% dplyr::rename(Papers_postfilter = Papers),
    by = "Author"
  ) %>%
    dplyr::mutate(
      Papers_prefilter = tidyr::replace_na(Papers_prefilter, 0),
      Papers_postfilter = tidyr::replace_na(Papers_postfilter, 0)
    ) %>%
    dplyr::mutate(Papers_removed = Papers_prefilter - Papers_postfilter) %>%
    dplyr::arrange(dplyr::desc(Papers_removed))

  # Save the merged dataframe as a CSV
  write.csv(merged_papers_per_author, output_file, row.names = FALSE)
}