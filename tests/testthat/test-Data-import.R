sample_data <- data.frame(
  Author = c(
    "Author1;Author2;Author3",
    "Author2;Author3;Author4",
    "Author1;Author4",
    "Author5"
  ),
  stringsAsFactors = FALSE
)

test_that("import_csv_data works correctly", {
  # Test importing the CSV file
  data <- import_csv_data("../data/SynthSysFinal_Direct_v2.csv")
  expect_equal(nrow(data), 103)
  expect_equal(ncol(data), 87)
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
