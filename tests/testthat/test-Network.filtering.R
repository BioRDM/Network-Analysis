sample_data <- data.frame(
  Author = c("Author1", "Author2", "Author1", "Author3", "Author2", "Author4"),
  Year = c(2000, 2000, 2006, 2006, 2010, 2010),
  Project_ID = c("P1", "P1", "P2", "P2", "P3", "P3"),
  Success = c("Successful", "Successful", "Unsuccessful", "Unsuccessful", "Successful", "Successful"),
  stringsAsFactors = FALSE
)

test_that("filter_by_year works correctly", {
  network <- network(
    data = sample_data,
    vertex_column = "Author",
    vertex_delimiter = NULL,
    edge_id = "Project_ID",
    year_column = "Year"
  )

  network <- filter_by_year(network, from_year = 2000, to_year = 2006)

  expect_equal(nrow(network$filtered), 4)
  expect_true(all(network$filtered$Year %in% c(2000, 2006)))
})

test_that("filter_by_year returns an empty data frame when no data matches the criteria", {
  network <- network(
    data = sample_data,
    vertex_column = "Author",
    vertex_delimiter = NULL,
    edge_id = "Project_ID",
    year_column = "Year"
  )

  network <- filter_by_year(network, from_year = 2015, to_year = 2020)

  expect_equal(nrow(network$filtered), 0)
})

test_that("filter_by_year handles NULL values", {
  network <- network(
    data = sample_data,
    vertex_column = "Author",
    vertex_delimiter = NULL,
    edge_id = "Project_ID",
    year_column = "Year"
  )

  network_filtered <- filter_by_year(network, from_year = NULL, to_year = NULL)
  expect_equal(nrow(network_filtered$filtered), nrow(sample_data))

  network_filtered <- filter_by_year(network, from_year = 2010, to_year = NULL)
  expect_equal(nrow(network_filtered$filtered), 2)

  network_filtered <- filter_by_year(network, from_year = NULL, to_year = 2006)
  expect_equal(nrow(network_filtered$filtered), 4)
})

test_that("filter_by_vertex_occurrences works correctly", {
  sample_data <- data.frame(
    Author = c("Author1", "Author2", "Author1", "Author3", "Author2", "Author4"),
    Project_ID = c("P1", "P1", "P2", "P1", "P3", "P1"),
    stringsAsFactors = FALSE
  )

  network <- network(
    data = sample_data,
    vertex_column = "Author",
    vertex_delimiter = NULL,
    edge_id = "Project_ID",
    year_column = NULL
  )

  filtered_network <- filter_by_vertex_occurrences(network, max_vertices = 2)

  expect_equal(nrow(filtered_network$filtered), 2)
  expect_equal(filtered_network$filtered$Author, c("Author1", "Author2"))
})

test_that("filter_single_vertices works correctly", {
  sample_data <- data.frame(
    Author = c("Author1", "Author2", "Author1", "Author3", "Author2", "Author4"),
    Project_ID = c("P1", "P1", "P2", "P1", "P3", "P1"),
    stringsAsFactors = FALSE
  )

  network <- network(
    data = sample_data,
    vertex_column = "Author",
    vertex_delimiter = NULL,
    edge_id = "Project_ID",
    year_column = NULL
  )

  filtered_network <- filter_single_vertices(network)

  expect_equal(nrow(filtered_network$filtered), 4)
  expect_equal(filtered_network$filtered$Author, c("Author1", "Author2", "Author3", "Author4"))
})

test_that("filter_infrquent_vertices works correctly", {
  sample_data <- data.frame(
    Author = c("Author1", "Author2", "Author1", "Author3", "Author2", "Author4"),
    Project_ID = c("P1", "P1", "P2", "P1", "P3", "P1"),
    stringsAsFactors = FALSE
  )

  network <- network(
    data = sample_data,
    vertex_column = "Author",
    vertex_delimiter = NULL,
    edge_id = "Project_ID",
    year_column = NULL
  )

  filtered_network <- filter_infrequent_vertices(network, min_occurrences = 2)

  expect_equal(nrow(filtered_network$filtered), 4)
  expect_equal(filtered_network$filtered$Author, c("Author1", "Author2", "Author1", "Author2"))
})
