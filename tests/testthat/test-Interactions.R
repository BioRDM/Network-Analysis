folder <- tempdir()
file_path <- test_path("../data/SynthSysFinal_Direct_v2.csv")
institution <- Interactions(file_path = file_path)

test_that("Interactions is created", {
  institution <- Interactions(file_path = file_path)
  expect_type(institution, "list")
  expect_false(igraph::is_directed(institution$graph))
})

test_that("Interactions can be directed", {
  institution <- Interactions(file_path = file_path, directed = TRUE)
  expect_true(igraph::is_directed(institution$graph))
})

test_that("graph is correct", {
  graph <- make_graph_from_csv(file_path)
  expect_equal(igraph::vcount(graph), 398)
  expect_equal(igraph::ecount(graph), 3359)
})

test_that("network cohesion works", {
  institution <- get_cohesion(institution)
  expect_equal(institution$dyadcount, 79003)
  expect_equal(institution$edgecount, 3359)
  expect_equal(institution$size, 398)
})

test_that("interaction density works", {
  expect_equal(round(get_density(institution), digits = 3), 0.043)
})

test_that("transitivity works", {
  expect_equal(round(get_transitivity(institution), digits = 3), 0.64)
})

test_that("centrality works", {
  expect_equal(round(mean(get_centrality(institution)), digits = 3), 16.879)
})

test_that("betweenness works", {
  expect_equal(round(mean(get_betweenness(institution)), digits = 3), 351.841)
})

test_that("closeness works", {
  expect_equal(round(mean(get_closeness(institution)), digits = 3), 0.013)
})

test_that("diameter works", {
  expect_equal(round(get_diameter(institution), digits = 3), 5)
})

test_that("plot is created", {
  test_file <- paste0(folder, "graph.png")
  plot_graph(institution, output_file = test_file)
  expect_true(file.exists(test_file))
})
