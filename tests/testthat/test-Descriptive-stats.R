test_that("get_author_stats calculates correct statistics", {
  data <- data.frame(Author = c("Author1;Author2", "Author1", "Author1;Author2;Author3"))
  result <- get_author_stats(data)

  expect_equal(result$sum, 3)
  expect_equal(result$average, 2)
  expect_equal(result$median, 2)
  expect_equal(result$min, 1)
  expect_equal(result$max, 3)
})

test_that("get_summary_stats works correctly", {
  # Mock data and interactions
  mock_data <- data.frame(
    `Author` = c("Author1;Author2", "Author3", "Author4;Author5;Author6"),
    Year = c(2000, 2002, 2004)
  )

  mock_interactions <- list(
    data = mock_data,
    author_column_name = "Author",
    author_delimiter = ";",
    from_year = 2000,
    to_year = 2004,
    n_papers = 3
  )

  # Mock functions
  mock_get_author_stats <- function(data, author_column_name = "Author", delimiter = ";") {
    list(
      sum = 6,
      average = 2,
      median = 2,
      min = 1,
      max = 3
    )
  }

  mock_get_centrality <- function(interactions) {
    list(
      degree = c(1, 2, 3),
      betweenness = c(0.1, 0.2, 0.3),
      harmonic = c(0.5, 0.6, 0.7)
    )
  }

  mock_get_diameter <- function(interactions) {
    list(
      average_shortest_path = 2.5
    )
  }

  mock_get_density <- function(interactions) {
    0.5
  }

  mock_get_transitivity <- function(interactions) {
    0.3
  }

  mock_get_cutpoints <- function(interactions) {
    c("Author1", "Author2")
  }

  # Use with_mock to override the functions
  with_mocked_bindings(
    get_author_stats = mock_get_author_stats,
    get_centrality = mock_get_centrality,
    get_diameter = mock_get_diameter,
    get_density = mock_get_density,
    get_transitivity = mock_get_transitivity,
    get_cutpoints = mock_get_cutpoints,
    {
      # Run the get_summary_stats function with the mock interactions
      summary_stats <- get_summary_stats(mock_interactions)

      # Add assertions to check the expected outcomes
      expect_equal(summary_stats$Start_year, 2000)
      expect_equal(summary_stats$End_year, 2004)
      expect_equal(summary_stats$Total_Papers, 3)
      expect_equal(summary_stats$Total_Authors, 6)
      expect_equal(summary_stats$Average_Authors_per_Paper, 2)
      expect_equal(summary_stats$Median_Authors_per_Paper, 2)
      expect_equal(summary_stats$Min_Authors_per_Paper, 1)
      expect_equal(summary_stats$Max_Authors_per_Paper, 3)
      expect_equal(summary_stats$Density, 0.5)
      expect_equal(summary_stats$Transitivity, 0.3)
      expect_equal(summary_stats$Mean_degree_centrality, 2)
      expect_equal(summary_stats$Mean_betweenness_centrality, 0.2)
      expect_equal(summary_stats$Mean_harmonic_centrality, 0.6)
      expect_equal(summary_stats$Mean_shortest_path, 2.5)
      expect_equal(summary_stats$Number_of_cutpoints, 2)
    }
  )
})
