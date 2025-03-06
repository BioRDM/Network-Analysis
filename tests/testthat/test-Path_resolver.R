config <- list(
  file_path = "data/input_file.csv",
  input_file = "input_file",
  output_path = tempdir()
)

test_that("get_output_path function works correctly", {
  stub(get_output_path, "getwd", function() "")
  output_path <- get_output_path(config)

  expect_equal(output_path, paste0("/", config$output_path, "/input_file"))
})

test_that("get_figures_path function works correctly", {
  stub(get_figures_path, "getwd", function() "")
  figures_path <- get_figures_path(config)

  expect_equal(figures_path, paste0("/", config$output_path, "/input_file/figures"))
})

test_that("get_summary_table_path function works correctly", {
  stub(get_summary_table_path, "getwd", function() "")
  summary_table_path <- get_summary_table_path(config)

  expect_equal(summary_table_path, paste0("/", config$output_path, "/input_file/data/Summary_statistics.csv"))
})

test_that("get_data_path function works correctly", {
  stub(get_data_path, "getwd", function() "")
  data_path <- get_data_path(config)

  expect_equal(data_path, paste0("/", config$output_path, "/input_file/data"))
})

test_that("get_centrality_data_path function works correctly", {
  stub(get_centrality_data_path, "getwd", function() "")
  centrality_data_path <- get_centrality_data_path(config)

  expect_equal(centrality_data_path, paste0("/", config$output_path, "/input_file/data/Centrality_data_"))
})
