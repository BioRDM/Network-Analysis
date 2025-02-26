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

test_that("Interactions is created", {
  interactions <- Interactions(data = sample_data)
  expect_type(interactions, "list")
  expect_false(igraph::is_directed(interactions$graph))
})

test_that("Interactions can be directed", {
  test_interactions <- Interactions(data = sample_data, directed = TRUE)
  expect_true(igraph::is_directed(test_interactions$graph))
})

test_that("network cohesion works", {
  interactions <- get_cohesion(interactions)
  expect_equal(interactions$dyadcount, 6)
  expect_equal(interactions$edgecount, 6)
  expect_equal(interactions$size, 4)
})

test_that("interaction density works", {
  expect_equal(get_density(interactions), 1)
})

test_that("transitivity works", {
  expect_equal(get_transitivity(interactions), 1)
})

test_that("centrality works", {
  expect_equal(mean(get_centrality(interactions)), 3)
})

test_that("betweenness works", {
  expect_equal(round(mean(get_betweenness(interactions)), digits = 3), 0.167)
})

test_that("closeness works", {
  expect_equal(round(mean(get_closeness(interactions)), digits = 3), 0.292)
})

test_that("diameter works", {
  expect_equal(get_diameter(interactions), 2)
})

test_that("communities work", {
  comm <- get_communities(interactions)
  expect_equal(class(comm), "communities")
})

test_that("most_central_authors works", {
  author_list <- get_most_central_per_community(interactions)
  expect_equal(author_list, "Author1")
})
