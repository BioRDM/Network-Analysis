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
  graph <- make_graph_from_df(sample_data, delimiter = ";", column_name = "Author", directed = FALSE)

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

test_that("tidy_authors processes authors correctly", {
  sample_data <- data.frame(
    Author = c("AuthorA, AuthorB", "Author C. D.; Author K.", "Author E.; Author F.", "Author G.; AuthorH"),
    Source = c("PubMed", "PURE", "Scopus", "Other"),
    stringsAsFactors = FALSE
  )

  # Apply the tidy_authors function
  tidy_data <- tidy_authors(sample_data)

  # Check the transformations
  expect_equal(tidy_data$Author[1], "AuthorA;AuthorB")  # PubMed: replace "," with ";"
  expect_equal(tidy_data$Author[2], "AuthorCD;AuthorK")  # PURE: remove non-letter characters except semicolons and white spaces
  expect_equal(tidy_data$Author[3], "AuthorE;AuthorF")  # Scopus: remove non-letter characters except semicolons and white spaces
  expect_equal(tidy_data$Author[4], "AuthorG;AuthorH")  # Other: remove non-letter characters except semicolons and white spaces

  # Check special character conversion
  sample_data_special <- data.frame(
    Author = c("Élise, André", "Jürgen; Müller"),
    Source = c("PubMed", "PURE"),
    stringsAsFactors = FALSE
  )

  tidy_data_special <- tidy_authors(sample_data_special)
  expect_equal(tidy_data_special$Author[1], "Elise;Andre")  # Special characters converted and white spaces removed
  expect_equal(tidy_data_special$Author[2], "Jurgen;Muller")  # Special characters converted, non-letter characters removed except semicolons, and white spaces removed
})
