config <- list(
  file_path = "data/input_file.csv",
  output_path = tempdir()
)

test_that("Paths function works correctly", {
  paths <- Paths(config)

  expect_equal(paths$input_file, config$file_path)
  expect_equal(paths$output, paste0(config$output_path, "/input_file"))
  expect_equal(paths$figures, paste0(config$output_path, "/input_file/figures"))
  expect_equal(paths$summary_table, paste0(config$output_path, "/input_file/Summary_statistics.csv"))
  expect_s3_class(paths, "Paths")
})

test_that("create_output_paths function works correctly", {
  paths <- create_output_paths(config)

  expect_true(dir.exists(paths$output))
  expect_true(dir.exists(paths$figures))
})

test_that("get_output_path function works correctly", {
  output_path <- get_output_path(config)

  expect_equal(output_path, paste0(config$output_path, "/input_file"))
})

test_that("get_figures_path function works correctly", {
  figures_path <- get_figures_path(config)

  expect_equal(figures_path, paste0(config$output_path, "/input_file/figures"))
})

test_that("get_summary_table_path function works correctly", {
  summary_table_path <- get_summary_table_path(config)

  expect_equal(summary_table_path, paste0(config$output_path, "/input_file/Summary_statistics.csv"))
})
