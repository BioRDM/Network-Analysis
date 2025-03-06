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
  centrality <- get_centrality(interactions)
  expect_equal(mean(centrality$degree), 3)
  expect_equal(round(mean(centrality$betweenness), digits = 3), 0.167)
  expect_equal(round(mean(centrality$harmonic), digits = 3), 2.750)
})

test_that("diameter works", {
  diameter <- get_diameter(interactions)
  expect_equal(diameter$diameter, 2)
  expect_equal(diameter$average_shortest_path, 0.875)
})

test_that("reachability works", {
  unreachable_fraction <- get_reachability(interactions)
  expect_equal(round(unreachable_fraction, digits = 2), -0.33)
})

test_that("communities work", {
  comm <- get_communities(interactions$graph)
  expect_equal(class(comm), "communities")
})

test_that("most_central_authors works", {
  author_list <- get_most_central_per_community(interactions)
  expect_equal(author_list, "Author1")
})

test_that("save_centrality_data saves the correct centrality data to a CSV file", {
  # Create a sample data frame
  data <- data.frame(
    Author = c("Author1;Author2", "Author2;Author3", "Author1;Author3"),
    Year = c(2020, 2021, 2022)
  )

  # Create an Interactions object
  interactions <- Interactions(data)

  # Define the output file path
  output_file <- tempfile(fileext = ".csv")

  # Call the save_centrality_data function
  save_centrality_data(interactions, output_file)

  # Read the output file
  output_data <- read.csv(output_file)

  # Check that the output data has the correct columns
  expect_true(all(c("degree", "harmonic", "betweenness") %in% colnames(output_data)))

  # Check that the output data is sorted by degree in descending order
  expect_true(all(diff(output_data$degree) <= 0))

  # Clean up the temporary file
  unlink(output_file)
})

test_that("get_cutpoints works correctly", {
  graph <- igraph::graph_from_edgelist(matrix(c(
    1, 2,
    2, 3,
    3, 4,
    4, 5,
    3, 6,
    6, 7,
    7, 8
  ), ncol = 2, byrow = TRUE))

  network <- intergraph::asNetwork(graph)
  network::network.vertex.names(network) <- as.character(1:network::network.size(network))

  print(network)
  print(network::network.vertex.names(network))

  mock_interactions <- list(
    network = network,
    graph = graph
  )
  class(mock_interactions) <- "Interactions"

  mock_format_names <- function(names) {
    return(names)
  }

  with_mocked_bindings(
    format_names = mock_format_names,
    {
      cutpoints <- get_cutpoints(mock_interactions)
      expect_equal(sort(cutpoints), sort(c("2", "3", "4", "6", "7")))
    }
  )
})
