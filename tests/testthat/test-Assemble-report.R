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
    output_path = folder,
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

  mock_Paths <- function(config) {
    list(
      input_file = config$file_path,
      output = config$output_path,
      figures = paste0(config$output_path, "/figures"),
      summary_table = paste0(config$output_path, "/Summary_statistics.csv")
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

  mock_plot_graph <- function(interactions, output_file) {
    NULL
  }

  mock_plot_top_authors <- function(interactions, n, output_file) {
    NULL
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

  mock_get_summary_stats <- function(interactions) {
    data.frame(
      Statistic = c("A", "B"),
      Value = c(1, 1)
    )
  }

  mock_generate_network_metrics <- function(interactions) {
    interactions$metrics <- list(
      density = 0.5,
      transitivity = 0.3,
      centrality = list(degree = c(1, 2, 3)),
      diameter = 4,
      unreachable_percentage = 10,
      cutpoints = c("Author1", "Author2")
    )
    return(interactions)
  }

  mock_generate_figures <- function(interactions, paths, date_range) {
    list(
      fig1 = paste0(paths$figures, "/graph_", date_range, ".png"),
      fig2 = paste0(paths$figures, "/top_authors_", date_range, ".png"),
      fig3 = paste0(paths$figures, "/cutpoints_", date_range, ".png"),
      fig4 = paste0(paths$figures, "/graph_betweenness_", date_range, ".png"),
      fig5 = paste0(paths$figures, "/graph_harmonic_", date_range, ".png"),
      fig6 = paste0(paths$figures, "/graph_no_centrality_", date_range, ".png")
    )
  }

  mock_format_summary_stats <- function(summary_stats) {
    data.frame(
      `Dates` = c("2000-2001", "2002-2003"),
      `Papers` = c(10, 15),
      `Authors` = c(5, 7),
      `Density (%)` = c(50, 60),
      `Transitivity (%)` = c(30, 40),
      `Mean Shortest Path` = c(2.5, 2.8),
      `Cutpoints` = c(1, 2)
    )
  }

  mockery::stub(write.table, "write.table", function(summary_stats, file, sep, row.names, col.names, append) {
    writeLines("Mock Summary Stats Content", con = file)
  })

  mock_rmarkdown_render <- function(input, output_file, output_format, quiet) {
    test_output_file <- paste0(folder, "/test_output_file.pdf")
    writeLines("Mock Render Content", con = test_output_file)
  }

  mockery::stub(assemble_report, "rmarkdown::render", mock_rmarkdown_render)

  # Use with_mock to override the functions
  testthat::with_mocked_bindings(
    import_csv_data = mock_import_csv_data,
    check_author_column = mock_check_author_column,
    Paths = mock_Paths,
    get_years_from_to = mock_get_years_from_to,
    Interactions = mock_Interactions,
    plot_graph = mock_plot_graph,
    plot_top_authors = mock_plot_top_authors,
    save_centrality_data = mock_save_centrality_data,
    get_author_stats = mock_get_author_stats,
    get_summary_stats = mock_get_summary_stats,
    generate_network_metrics = mock_generate_network_metrics,
    generate_figures = mock_generate_figures,
    format_summary_stats = mock_format_summary_stats,
    {
      # Run the assemble_report function with the mock config
      assemble_report(config)

      # Add assertions to check the expected outcomes
      expect_true(file.exists(paste0(folder, "/test_output_file.pdf")))
      expect_true(file.exists(paste0(folder, "/centrality_data_2000-2001.csv")))
    }
  )
})
