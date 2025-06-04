#' @export
assemble_report <- function(config, metadata) {
  summary_var <<- list() # Global variable to store summary variables
  config <- read_config(config)
  metadata <- read_metadata(metadata)

  # Create output folders
  paths <- Paths(config)

  # Save the config as a json file
  json_config <- jsonlite::toJSON(config, pretty = TRUE, auto_unbox = TRUE, null = "null")
  write(json_config, file = paste0(paths$dataset, "/config.json"))

  # Import the data
  data <- import_csv_data(paths$input_file)

  # Check that the author column exists
  check_author_column(data, config$author_column_name)

  # Tidy the author column
  data <- tidy_authors(data, author_column = config$author_column_name, delimiter = config$author_delimiter)  # Tidy the author names

  # Calculate the year intervals for the reports
  years <- get_years_from_to(data, config)

  # Initialize the summary table
  summary_stats <- data.frame()

  # Create the report(s)
  for (i in seq_along(years$years_from)) {
    report_var <<- list() # Global variable to store report variables
    report_var$config <- config
    report_var$metadata <- metadata

    # Filter for the current year range
    from_year <- years$years_from[i]
    to_year <- years$years_to[i]
    date_range <- paste0(from_year, "-", to_year)
    report_var$date_range <- date_range
    current_data <- filter_by_year(data, config$year_column_name, from_year, to_year)
    print(paste0("Creating report for the period ", from_year, " to ", to_year))

    # Save raw data before filtering
    utils::write.csv(current_data, paste0(paths$data, "/Raw_data_", date_range, ".csv"), row.names = FALSE)

    # Retrieve the number of papers per author before filtering
    prefilter_papers_per_author <- get_papers_per_author(
      current_data,
      author_column_name = config$author_column_name,
      delimiter = config$author_delimiter
    )

    # Retrieve authors (per paper) before filtering
    report_var$prefilter_author_stats <- get_author_stats(
      current_data,
      author_column_name = config$author_column_name,
      delimiter = config$author_delimiter
    )

    interactions <- Interactions(data = current_data,
                                 author_delimiter = config$author_delimiter,
                                 author_column_name = config$author_column_name,
                                 year_column_name = config$year_column_name,
                                 max_authors_per_paper = config$max_authors_per_paper,
                                 min_papers_per_author = config$min_papers_per_author,
                                 directed = config$directed,
                                 from_year = from_year,
                                 to_year = to_year)

    if (is.null(interactions)) {
      next  # Skip the report if graph building failed (because there were not enough authors)
    }

    # Save raw data after filtering
    utils::write.csv(interactions$data, paste0(paths$data, "/Filtered_data_", date_range, ".csv"), row.names = FALSE)

    postfilter_papers_per_author <- get_papers_per_author(
      interactions$data,
      author_column_name = config$author_column_name,
      delimiter = config$author_delimiter
    )
    report_var$postfilter_author_stats <- get_author_stats(
      interactions$data,
      author_column_name = config$author_column_name,
      delimiter = config$author_delimiter
    )

    interactions <- generate_network_metrics(interactions)
    report_var$interactions <- interactions

    report_var$figures <- generate_figures(interactions, paths, date_range)

    # Save centrality data as csv
    save_centrality_data(interactions, paste0(paths$centrality_data, date_range, ".csv"))

    # Save number of papers per author before and after filtering as csv
    save_papers_per_author(
      prefilter_papers_per_author,
      postfilter_papers_per_author,
      output_file =  paste0(paths$papers_per_author, date_range, ".csv")
    )

    # Add current report summary stats to summary stats dataframe
    summary_stats <- rbind(summary_stats, get_summary_stats(interactions))

    # Generate the pdf report
    print("Exporting PDF...")
    rmarkdown::render(
      paste0(paths$templates, "/Report_template.Rmd"),
      output_file = paste0(paths$dataset, "/Report_", date_range, ".pdf"),
      output_format = "pdf_document",
      quiet = TRUE
    )
    print("PDF exported successfully!")
  }

  # Write the summary stats csv table
  utils::write.table(summary_stats, paths$summary_table, sep = ",", row.names = FALSE, col.names = TRUE)

  summary_var$summary_stats <- format_summary_stats(summary_stats)

  print("Exporting Summary Table...")
  rmarkdown::render(
    paste0(paths$templates, "/Summary_template.Rmd"),
    output_file = paste0(paths$dataset, "/Summary.pdf"),
    output_format = "pdf_document",
    quiet = TRUE
  )
  print("Summary Table exported successfully!")

}
