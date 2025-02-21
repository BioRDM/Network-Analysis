sample_data <- data.frame(
  Author = c(
    "Author1;Author2;Author3",
    "Author2;Author3;Author4",
    "Author1;Author4",
    "Author5"
  ),
  stringsAsFactors = FALSE
)

test_that("papers are removed", {
  res <- filter_papers_by_authors(sample_data, column_name = "Author", delimiter = ";", max_authors = 2)
  expect_equal(nrow(res[[1]]), 1)
  expect_equal(res[[2]], 2)
  expect_equal(res[[3]], 1)
})

test_that("make_graph_from_df creates a graph with correct edges", {
  graph <- make_graph_from_df(sample_data, delimiter = ";", column_name = "Author", max_authors = 50, directed = FALSE)

  # Check that the graph is an igraph object
  expect_true(igraph::is_igraph(graph))

  # Check the number of vertices
  expect_equal(igraph::vcount(graph), 4)

  # Check the number of edges
  expect_equal(igraph::ecount(graph), 6)

  # Check that specific edges exist
  expect_true(igraph::are_adjacent(graph, "Author1", "Author2"))
  expect_true(igraph::are_adjacent(graph, "Author1", "Author3"))
  expect_true(igraph::are_adjacent(graph, "Author2", "Author3"))
  expect_true(igraph::are_adjacent(graph, "Author2", "Author4"))
  expect_true(igraph::are_adjacent(graph, "Author3", "Author4"))
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
