sample_data <- data.frame(
  Author = c("Author1", "Author2", "Author1", "Author3", "Author2", "Author4"),
  Year = c(2000, 2000, 2006, 2006, 2010, 2010),
  Project_ID = c("P1", "P1", "P2", "P2", "P3", "P3"),
  Success = c("Successful", "Successful", "Unsuccessful", "Unsuccessful", "Successful", "Successful"),
  stringsAsFactors = FALSE
)

test_that("graph is created", {
  graph <- graph(data = sample_data, directed = FALSE)
  expect_type(graph, "list")
  expect_true("graph" %in% class(graph))
})

test_that("build function works", {
  graph <- graph(data = sample_data, directed = FALSE)
  vertices <- "Author"
  edges <- "Project_ID"

  graph <- build(graph, vertices = vertices, edges = edges)
  expect_true(igraph::is_igraph(graph$graph))
  expect_equal(igraph::vcount(graph$graph), length(unique(sample_data$Author)))
})

test_that("graph can be built with edge attributes", {
  graph <- graph(data = sample_data, directed = FALSE)
  vertices <- "Author"
  edges <- "Project_ID"
  edge_attr <- c("Year", "Success")

  graph <- build(graph, vertices = vertices, edges = edges, edge_attr = edge_attr)
  expect_true(igraph::is_igraph(graph$graph))
  expect_equal(igraph::edge_attr_names(graph$graph), c("Year", "Success", "weight"))
})

test_that("get_coords returns a graph layout", {
  graph <- graph(data = sample_data, directed = FALSE)
  vertices <- "Author"
  edges <- "Project_ID"

  graph <- build(graph, vertices = vertices, edges = edges)
  coords <- get_coords(graph)
  expect_true(is.data.frame(coords))
  expect_equal(ncol(coords), 2)
  expect_equal(nrow(coords), igraph::vcount(graph$graph))
})

test_that("get_density returns a numeric value", {
  graph <- graph(data = sample_data, directed = FALSE)
  vertices <- "Author"
  edges <- "Project_ID"

  graph <- build(graph, vertices = vertices, edges = edges)
  density <- get_density(graph)
  expect_type(density, "double")
})

test_that("get_transitivity returns a numeric value", {
  graph <- graph(data = sample_data, directed = FALSE)
  vertices <- "Author"
  edges <- "Project_ID"

  graph <- build(graph, vertices = vertices, edges = edges)
  transitivity <- get_transitivity(graph)
  expect_type(transitivity, "double")
})

test_that("get_centrality returns a numeric vector", {
  graph <- graph(data = sample_data, directed = FALSE)
  vertices <- "Author"
  edges <- "Project_ID"

  graph <- build(graph, vertices = vertices, edges = edges)
  centrality <- get_centrality(graph)
  expect_type(centrality, "double")
  expect_equal(length(centrality), igraph::vcount(graph$graph))
})

test_that("get_diameter returns a numeric value", {
  graph <- graph(data = sample_data, directed = FALSE)
  vertices <- "Author"
  edges <- "Project_ID"

  graph <- build(graph, vertices = vertices, edges = edges)
  diameter <- get_diameter(graph)
  expect_type(diameter, "double")
})

test_that("get_average_shortest_path returns a numeric value", {
  graph <- graph(data = sample_data, directed = FALSE)
  vertices <- "Author"
  edges <- "Project_ID"

  graph <- build(graph, vertices = vertices, edges = edges)
  avg_shortest_path <- get_average_shortest_path(graph)
  expect_type(avg_shortest_path, "double")
})

test_that("get_reachability returns the correct value", {
  graph <- graph(data = sample_data, directed = FALSE)
  vertices <- "Author"
  edges <- "Project_ID"

  graph <- build(graph, vertices = vertices, edges = edges)
  reachability <- get_reachability(graph)
  expect_equal(reachability, 1)
})

test_that("get_cutpoints returns a named vector", {
  graph <- graph(data = sample_data, directed = FALSE)
  vertices <- "Author"
  edges <- "Project_ID"

  graph <- build(graph, vertices = vertices, edges = edges)
  cutpoints <- get_cutpoints(graph)
  expect_type(cutpoints, "integer")
  expect_type(names(cutpoints), "character")
})

test_that("get_most_central_vertices returns a character vector", {
  graph <- graph(data = sample_data, directed = FALSE)
  vertices <- "Author"
  edges <- "Project_ID"

  graph <- build(graph, vertices = vertices, edges = edges)
  most_central <- get_most_central_vertices(graph, method = "degree", n = 2)
  expect_type(most_central, "character")
  expect_equal(length(most_central), 2)
})

test_that("set_communities sets communities correctly", {
  graph <- graph(data = sample_data, directed = FALSE)
  vertices <- "Author"
  edges <- "Project_ID"

  graph <- build(graph, vertices = vertices, edges = edges)
  graph <- set_communities(graph)
  expect_true("community" %in% igraph::vertex_attr_names(graph$graph))
})

test_that("get_communities returns a character vector", {
  graph <- graph(data = sample_data, directed = FALSE)
  vertices <- "Author"
  edges <- "Project_ID"

  graph <- build(graph, vertices = vertices, edges = edges)
  graph <- set_communities(graph)
  communities <- get_communities(graph)
  expect_type(communities, "integer")
  expect_equal(length(communities), igraph::vcount(graph$graph))
})

test_that("get_most_central_per_community returns a character vector", {
  graph <- graph(data = sample_data, directed = FALSE)
  vertices <- "Author"
  edges <- "Project_ID"

  graph <- build(graph, vertices = vertices, edges = edges)
  graph <- set_communities(graph)
  most_central <- get_most_central_per_community(graph, method = "degree")
  expect_type(most_central, "character")
  expect_equal(length(most_central), length(unique(get_communities(graph))))
})

test_that("save_centrality_data saves data correctly", {
  graph <- graph(data = sample_data, directed = FALSE)
  vertices <- "Author"
  edges <- "Project_ID"

  graph <- build(graph, vertices = vertices, edges = edges)

  temp_file <- tempfile(fileext = ".csv")
  save_centrality_data(graph, temp_file)

  expect_true(file.exists(temp_file))
  expect_gt(file.size(temp_file), 0)

  unlink(temp_file)
})
