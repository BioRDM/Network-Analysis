sample_data <- data.frame(
  Author = c("Author1", "Author2", "Author1", "Author3", "Author2", "Author4"),
  Year = c(2000, 2000, 2006, 2006, 2010, 2010),
  Project_ID = c("P1", "P1", "P2", "P2", "P3", "P3"),
  Success = c("Successful", "Successful", "Unsuccessful", "Unsuccessful", "Successful", "Successful"),
  stringsAsFactors = FALSE
)

test_that("network gets created", {
  network <- network(
    data = sample_data,
    vertex_column = "Author",
    vertex_delimiter = NULL,
    edge_id = "Project_ID",
    year_column = "Year"
  )

  expect_s3_class(network, "network")
  expect_equal(network$vertex_column, "Author")
  expect_equal(network$edge_id, "Project_ID")
  expect_equal(network$year_column, "Year")
  expect_equal(nrow(network$raw), 6)
  expect_equal(nrow(network$filtered), 6)
})

test_that("unnest_vertex_column works", {
  nested_data <- data.frame(
    Author = c("Author1; Author2", "Author1; Author3", "Author2; Author4"),
    Year = c(2000, 2006, 2010),
    Project_ID = c("P1", "P2", "P3"),
    stringsAsFactors = FALSE
  )

  network <- network(
    data = nested_data,
    vertex_column = "Author",
    vertex_delimiter = "; ",
    edge_id = "Project_ID",
    year_column = "Year"
  )

  unnest_network <- unnest_vertex_column(network)
  expect_s3_class(unnest_network, "network")
  expect_equal(nrow(unnest_network$raw), 6)
  expect_equal(nrow(unnest_network$filtered), 6)
})

test_that("tidy_names works", {
  untidy_data <- data.frame(
    Author = c("Bérénice", "AuthorTwo", "Bérénice   ", "Gael31", "    AuthorTwo", "AuthorFour"),
    Year = c(2000, 2000, 2006, 2006, 2010, 2010),
    Project_ID = c("P1", "P1", "P2", "P2", "P3", "P3"),
    Success = c("Successful", "Successful", "Unsuccessful", "Unsuccessful", "Successful", "Successful"),
    stringsAsFactors = FALSE
  )

  network <- network(
    data = untidy_data,
    vertex_column = "Author",
    vertex_delimiter = NULL,
    edge_id = "Project_ID",
    year_column = "Year"
  )

  tidy_network <- tidy_names(network, column = "Author")
  expect_s3_class(tidy_network, "network")
  expect_true("Author" %in% colnames(tidy_network$filtered))
  expect_equal(tidy_network$filtered$Author, c("Berenice", "AuthorTwo", "Berenice", "Gael", "AuthorTwo", "AuthorFour"))
})

test_that("get_edges_per_vertex works", {
  network <- network(
    data = sample_data,
    vertex_column = "Author",
    vertex_delimiter = NULL,
    edge_id = "Project_ID",
    year_column = "Year"
  )

  edges_per_vertex <- get_edges_per_vertex(network)
  expect_s3_class(edges_per_vertex, "data.frame")
  expect_equal(nrow(edges_per_vertex), 4) # Each author should have at least one edge
  expect_true("Edges" %in% colnames(edges_per_vertex))
})

test_that("save_edges_per_vertex works", {
  network <- network(
    data = sample_data,
    vertex_column = "Author",
    vertex_delimiter = NULL,
    edge_id = "Project_ID",
    year_column = "Year"
  )

  temp_file <- tempfile(fileext = ".csv")

  save_edges_per_vertex(network, temp_file)

  expect_true(file.exists(temp_file))
  loaded_data <- read.csv(temp_file)
  expect_equal(nrow(loaded_data), nrow(get_edges_per_vertex(network)))

  unlink(temp_file) # Clean up
})
