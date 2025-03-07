#' @export
assemble_report <- function(config) {
  summary_var <<- list() # Global variable to store summary variables
  config <- read_config(config)

  # Create output folders
  paths <- Paths(config)

  # Import the data
  data <- import_csv_data(paths$input_file)

  # Replace spaces with dots in column names to match R column names
  config$author_column_name <- gsub(" ", ".", config$author_column_name)
  config$year_column_name <- gsub(" ", ".", config$year_column_name)

  # Check that the author column exists
  check_author_column(data, config$author_column_name)

  # Tidy the author column
  data <- tidy_authors(data, author_column = config$author_column_name, delimiter = config$author_delimiter)  # Tidy the author names

  # Calculate the year intervals for the reports
  years <- get_years_from_to(data, config)
  years_from <- years$years_from
  years_to <- years$years_to

  # Initialize the summary table
  summary_stats <- data.frame()

  # Create the report(s)
  for (i in seq_along(years_from)) {
    report_var <<- list() # Clear the report variables at each iteration
    report_var$config <- config
    from_year <- years_from[i]
    to_year <- years_to[i]
    date_range <- paste0(from_year, "-", to_year)
    print(paste0("Creating report for the period ", from_year, " to ", to_year))

    # Retrieve authors statistics before filtering
    report_var$prefilter_author_stats <- get_author_stats(data, author_column_name = config$author_column_name, delimiter = config$author_delimiter)

    interactions <- Interactions(data = data,
                                 author_delimiter = config$author_delimiter,
                                 author_column_name = config$author_column_name,
                                 year_column_name = config$year_column_name,
                                 max_authors_per_paper = config$max_authors_per_paper,
                                 min_papers_per_author = config$min_papers_per_author,
                                 directed = config$directed,
                                 from_year = from_year,
                                 to_year = to_year)
    if (is.null(interactions)) {
      next
    }

    # Retrieve authors statistics after filtering
    report_var$postfilter_author_stats <- get_author_stats(interactions$data, author_column_name = config$author_column_name, delimiter = config$author_delimiter)

    # Generate network metrics
    interactions <- generate_network_metrics(interactions)

    # Add all interactions variables to report_var
    report_var$interactions <- interactions

    # Generate report figures
    report_var$figures <- generate_figures(interactions, paths, date_range)

    # Save centrality data as csv
    save_centrality_data(interactions, paste0(paths$centrality_data, date_range, ".csv"))

    # Generate the summary statistics and save as csv
    summary_stats <- rbind(summary_stats, get_summary_stats(interactions))

    # Generate the pdf report
    print("Exporting PDF...")
    rmarkdown::render(paste0(paths$templates, "/Report_template.Rmd"), output_file = paste0(paths$dataset, "/Report_", date_range, ".pdf"), output_format = "pdf_document", quiet = TRUE)
    print("PDF exported successfully!")
  }

  # Write the summary stats csv table
  write.table(summary_stats, paths$summary_table, sep = ",", row.names = FALSE, col.names = TRUE)

  summary_var$summary_stats <- format_summary_stats(summary_stats)
  print("Exporting Summary Table...")
  rmarkdown::render(paste0(paths$templates, "/Summary_template.Rmd"), output_file = paste0(paths$dataset, "/Summary.pdf"), output_format = "pdf_document", quiet = TRUE)
  print("Summary Table exported successfully!")

}
