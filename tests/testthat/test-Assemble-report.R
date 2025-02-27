library(mockery)

test_that("assemble_report works correctly", {
  folder <- tempdir()
  # Create a mock config object
  config <- list(
    file_path = "path/to/mock_data.csv",
    author_column_name = "Author Name",
    year_column_name = "Year",
    author_delimiter = ";",
    max_authors_per_paper = 5,
    min_papers_per_author = 1,
    directed = FALSE,
    output_path = "path/to/output",
    split_per_year = 2
  )

  # Mock functions
  mock_import_csv_data <- function(file_path) {
    data.frame(
      `Author Name` = c("Author1", "Author2", "Author3"),
      Year = c(2000, 2002, 2004)
    )
  }

  mock_check_author_column <- function(data, author_column_name) {
    TRUE
  }

  mock_create_output_paths <- function(config) {
    list(
      figures = "path/to/figures",
      output = "path/to/output"
    )
  }

  mock_get_years_from_to <- function(data, config) {
    list(
      years_from = seq(2000, 2004, by = 2),
      years_to = c(2001, 2003, 2004)
    )
  }

  mock_Interactions <- function(data, author_delimiter, author_column_name, year_column_name, max_authors_per_paper, min_papers_per_author, directed, from_year, to_year) {
    list()
  }

  mock_Report <- function() {
    list()
  }

  mock_add_figure <- function(report, plot, fig_caption) {
    report
  }

  mock_add <- function(report, content) {
    report
  }

  mock_plot_graph <- function(interactions, output_file) {
    NULL
  }

  mock_network_type <- function(interactions) {
    "Network Type"
  }

  mock_cohesion_metrics <- function(interactions) {
    "Cohesion Metrics"
  }

  mock_density_transitivity <- function(interactions) {
    "Density and Transitivity"
  }

  mock_centrality_metrics <- function(interactions) {
    "Centrality Metrics"
  }

  mock_reachability_metrics <- function(interactions) {
    "Reachability Metrics"
  }

  mock_plot_top_authors <- function(interactions, n, output_file) {
    NULL
  }

  mock_save_md <- function(report, file_path) {
    md_path <- paste0(folder, "/Report_2000-2001.md")
    writeLines("Mock MD Content", con = md_path)
  }

  mock_export_pdf <- function(report, input_file, output_file) {
    pdf_path <- paste0(folder, "/Report_2000-2001.pdf")
    writeLines("Mock PDF Content", con = pdf_path)
  }

  mock_save_centrality_data <- function(interactions, output_path) {
    csv_path <- paste0(folder, "/centrality_data_2000-2001.csv")
    writeLines("Mock CSV Content", con = csv_path)
  }

  mock_get_author_stats <- function(data, author_column_name = "Author", delimiter = ";") {
    list(
      sum = 3,
      average = 2,
      median = 2,
      min = 1,
      max = 3
    )
  }

  # Use with_mock to override the functions
  testthat::with_mocked_bindings(
    import_csv_data = mock_import_csv_data,
    check_author_column = mock_check_author_column,
    create_output_paths = mock_create_output_paths,
    get_years_from_to = mock_get_years_from_to,
    Interactions = mock_Interactions,
    Report = mock_Report,
    add_figure = mock_add_figure,
    add = mock_add,
    plot_graph = mock_plot_graph,
    network_type = mock_network_type,
    cohesion_metrics = mock_cohesion_metrics,
    density_transitivity = mock_density_transitivity,
    centrality_metrics = mock_centrality_metrics,
    reachability_metrics = mock_reachability_metrics,
    plot_top_authors = mock_plot_top_authors,
    save_md = mock_save_md,
    export_pdf = mock_export_pdf,
    save_centrality_data = mock_save_centrality_data,
    get_author_stats = mock_get_author_stats,
    {
      # Run the assemble_report function with the mock config
      assemble_report(config)

      # Add assertions to check the expected outcomes
      expect_true(file.exists(paste0(folder, "/Report_2000-2001.md")))
      expect_true(file.exists(paste0(folder, "/Report_2000-2001.pdf")))
    }
  )
})