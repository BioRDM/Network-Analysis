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
  sample_data <- filter_papers_by_authors(sample_data, column_name = "Author", delimiter = ";", max_authors = 50)[[1]]
  graph <- make_graph_from_df(sample_data, delimiter = ";", column_name = "Author", max_authors = 50, directed = FALSE)

  # Check that the graph is an igraph object
  expect_true(is_igraph(graph))

  # Check the number of vertices
  expect_equal(vcount(graph), 4)

  # Check the number of edges
  expect_equal(ecount(graph), 6)

  # Check that specific edges exist
  expect_true(igraph::are_adjacent(graph, "Author1", "Author2"))
  expect_true(igraph::are_adjacent(graph, "Author1", "Author3"))
  expect_true(igraph::are_adjacent(graph, "Author2", "Author3"))
  expect_true(igraph::are_adjacent(graph, "Author2", "Author4"))
  expect_true(igraph::are_adjacent(graph, "Author3", "Author4"))
})
