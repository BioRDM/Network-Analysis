assemble_report <- function(config) {

  # Import the data
  data <- import_csv_data(config$file_path)
  # Replace spaces with dots in column names to match R column names
  config$author_column_name <- gsub(" ", ".", config$author_column_name)
  config$year_column_name <- gsub(" ", ".", config$year_column_name)
  check_author_column(data, config$author_column_name)

  # Create output folders
  paths <- create_output_paths(config)

  # Calculate the year intervals for the reports
  years <- get_years_from_to(data, config)
  years_from <- years$years_from
  years_to <- years$years_to

  # Create the report(s)
  for (i in seq_along(years_from)) {
    from_year <- years_from[i]
    to_year <- years_to[i]
    date_range <- paste0(from_year, "-", to_year)
    print(paste0("Creating report for the period ", from_year, " to ", to_year))

    interactions <- Interactions(data = data,
                                 author_delimiter = config$author_delimiter,
                                 author_column_name = config$author_column_name,
                                 year_column_name = config$year_column_name,
                                 max_authors_per_paper = config$max_authors_per_paper,
                                 min_papers_per_author = config$min_papers_per_author,
                                 directed = config$directed,
                                 from_year = from_year,
                                 to_year = to_year)

    # Initiate report
    report <- Report()

    # Add graph Figure
    report <- add_figure(
      report,
      plot = plot_graph(interactions, output_file = paste0(paths$figures, "/graph_", date_range, ".png")),
      fig_caption = "Visualisation of the Author network"
    )

    # Add network type paragraph
    report <- add(report, network_type(interactions))

    # Add cohesion metrics
    report <- add(report, cohesion_metrics(interactions))

    # Add density and transitivity info
    report <- add(report, density_transitivity(interactions))

    # Add centrality metrics
    report <- add(report, centrality_metrics(interactions))

    # Add top authors Figure
    report <- add_figure(
      report,
      plot = plot_top_authors(interactions, n = 15, output_file = paste0(paths$figures, "/top_authors_", date_range, ".png")),
      fig_caption = "Direct connections between the 15 most central authors"
    )

    # Save report as markdown
    save_md(report, paste0(paths$output, "/Report_", date_range, ".md"))

    # Convert the report to pdf
    export_pdf(report, "Report.md", output_file = paste0(paths$output, "/Report_", date_range, ".pdf"))
  }

}