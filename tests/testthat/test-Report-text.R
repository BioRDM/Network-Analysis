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

test_that("introduction is created", {
  output_string <- introduction()
  expect_type(output_string, "character")
  expect_true(length(output_string) > 0)
})

test_that("network type is created", {
  output_string <- network_type(institution)
  expect_type(output_string, "character")
  expect_true(length(output_string) > 0)
})

test_that("cohesion metrics is created", {
  output_string <- cohesion_metrics(institution)
  expect_type(output_string, "character")
  expect_true(length(output_string) > 0)
})

test_that("density transitivity is created", {
  output_string <- density_transitivity(institution)
  expect_type(output_string, "character")
  expect_true(length(output_string) > 0)
})

test_that("centrality metrics is created", {
  output_string <- centrality_metrics(institution)
  expect_type(output_string, "character")
  expect_true(length(output_string) > 0)
})

test_that("community detection is created", {
  output_string <- community_detection(institution)
  expect_type(output_string, "character")
  expect_true(length(output_string) > 0)
})
