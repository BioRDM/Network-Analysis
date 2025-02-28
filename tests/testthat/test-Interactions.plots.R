folder <- tempdir()
sample_data <- data.frame(
  Author = c(
    "Author1;Author2;Author3",
    "Author2;Author3;Author4",
    "Author1;Author4",
    "Author5"
  ),
  stringsAsFactors = FALSE
)
interactions <- Interactions(data = sample_data)

test_that("graph plot is created", {
  test_file <- paste0(folder, "graph.png")
  plot_graph(interactions, output_file = test_file)
  expect_true(file.exists(test_file))
})

test_that("top authors plot is created", {
  test_file <- paste0(folder, "top_authors.png")
  plot_top_authors(interactions, n = 3, output_file = test_file)
  expect_true(file.exists(test_file))
})

test_that("cutpoints plot is created", {
  test_file <- paste0(folder, "cutpoints.png")
  plot_cutpoints(interactions, output_file = test_file)
  expect_true(file.exists(test_file))
})

test_that("graph coordinates are returned correctly", {
  coords <- get_graph_coords(interactions$graph)
  expect_true(is.matrix(coords))
  expect_equal(nrow(coords), igraph::vcount(interactions$graph))
  expect_equal(ncol(coords), 2)
})