folder <- tempdir()
file_path <- test_path("../data/SynthSysFinal_Direct_v2.csv")
institution <- Interactions(file_path = file_path)

test_that("Interactions is created", {
  institution <- Interactions(file_path = file_path)
  expect_type(institution, "list")
  expect_false(igraph::is_directed(institution$graph))
})

test_that("Interactions can be directed", {
  test_interactions <- Interactions(file_path = file_path, directed = TRUE)
  expect_true(igraph::is_directed(test_interactions$graph))
})

test_that("graph is correct", {
  data <- institution$data
  graph <- make_graph_from_df(data)
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
  expect_equal(round(get_diameter(institution), digits = 3), 8)
})

test_that("communities work", {
  comm <- get_communities(institution)
  expect_equal(class(comm), "communities")
})

test_that("most_central_authors works", {
  author_list <- get_most_central_per_community(institution)
  expect_true(length(author_list) > 10)
  expect_true("Ghazal, P." %in% author_list)
})
