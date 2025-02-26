test_that("create_output_paths works correctly", {
  # Create a temporary directory for testing
  temp_dir <- tempdir()
  config <- list(
    output_path = temp_dir,
    file_path = "path/to/mock_data.csv"
  )

  # Run the function
  paths <- create_output_paths(config)

  # Check if the output paths are correct
  expected_output_path <- file.path(temp_dir, tools::file_path_sans_ext(basename(config$file_path)))
  expected_figures_path <- file.path(expected_output_path, "figures")

  expect_equal(paths$output, expected_output_path)
  expect_equal(paths$figures, expected_figures_path)

  # Check if the directories were created
  expect_true(dir.exists(expected_output_path))
  expect_true(dir.exists(expected_figures_path))

  # Clean up
  unlink(expected_output_path, recursive = TRUE)
})

test_that("create_output_paths handles existing directories", {
  # Create a temporary directory for testing
  temp_dir <- tempdir()
  config <- list(
    output_path = temp_dir,
    file_path = "path/to/mock_data.csv"
  )

  # Create the directories beforehand
  expected_output_path <- file.path(temp_dir, tools::file_path_sans_ext(basename(config$file_path)))
  expected_figures_path <- file.path(expected_output_path, "figures")
  dir.create(expected_output_path)
  dir.create(expected_figures_path)

  # Run the function
  paths <- create_output_paths(config)

  # Check if the output paths are correct
  expect_equal(paths$output, expected_output_path)
  expect_equal(paths$figures, expected_figures_path)

  # Check if the directories still exist
  expect_true(dir.exists(expected_output_path))
  expect_true(dir.exists(expected_figures_path))

  # Clean up
  unlink(expected_output_path, recursive = TRUE)
})