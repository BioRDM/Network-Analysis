sample_data <- data.frame(
  Author = c(
    "Author1;Author2;Author3",
    "Author2;Author3;Author4",
    "Author1;Author4",
    "Author5"
  ),
  stringsAsFactors = FALSE
)

test_that("filter_by_year works correctly", {
  data <- data.frame(
    Author = c("Author1", "Author2", "Author3"),
    Year = c(2000, 2002, 2004)
  )

  # Test filtering with both from_year and to_year
  filtered_data <- filter_by_year(data, "Year", 2001, 2003)
  expect_equal(nrow(filtered_data), 1)
  expect_equal(filtered_data$Year, 2002)

  # Test filtering with only from_year
  filtered_data <- filter_by_year(data, "Year", 2001, NULL)
  expect_equal(nrow(filtered_data), 2)
  expect_equal(filtered_data$Year, c(2002, 2004))

  # Test filtering with only to_year
  filtered_data <- filter_by_year(data, "Year", NULL, 2001)
  expect_equal(nrow(filtered_data), 1)
  expect_equal(filtered_data$Year, 2000)

  # Test filtering with neither from_year nor to_year
  filtered_data <- filter_by_year(data, "Year", NULL, NULL)
  expect_equal(nrow(filtered_data), 3)
  expect_equal(filtered_data$Year, c(2000, 2002, 2004))
})

test_that("papers are removed", {
  res <- filter_papers_by_authors(sample_data, column_name = "Author", delimiter = ";", max_authors = 2)
  expect_equal(nrow(res[[1]]), 1)
  expect_equal(res[[2]], 2)
  expect_equal(res[[3]], 1)
})

test_that("filter_small_authors removes authors with fewer than min_occurrences", {
  # Create the initial graph
  graph <- make_graph_from_df(sample_data,
                              delimiter = ";",
                              column_name = "Author",
                              max_authors = 50,
                              directed = FALSE)

  # Filter authors that appear fewer than 2 times
  filtered_graph <- filter_small_authors(graph, min_occurrences = 2)[[1]]

  # Check that the graph is an igraph object
  expect_true(igraph::is_igraph(filtered_graph))

  # Check that specific vertices exist
  expect_true("Author1" %in% igraph::V(filtered_graph)$name)
  expect_true("Author2" %in% igraph::V(filtered_graph)$name)
  expect_true("Author3" %in% igraph::V(filtered_graph)$name)
  expect_true("Author4" %in% igraph::V(filtered_graph)$name)

  # Check that specific vertices do not exist
  expect_false("Author5" %in% igraph::V(filtered_graph)$name)
})
