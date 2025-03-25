test_that("Paths function works correctly", {
  temp_file <- tempfile(fileext = ".csv")
  file.create(temp_file)
  config <- list(
    file_path = temp_file,
    output_path = file.path(tempdir(), "output", fsep = "/"),
    input_name = "input_file"
  )

  paths <- Paths(config)

  expect_equal(class(paths), "Paths")
  expect_equal(paths$input_file, tools::file_path_as_absolute(config$file_path))
  expect_equal(paths$output, tools::file_path_as_absolute(config$output_path))
  expect_equal(paths$dataset, paste0(normalizePath(config$output_path, winslash = "/"),
                                     "/", config$input_name))
  expect_equal(paths$figures, paste0(paths$dataset, "/figures"))
  expect_equal(paths$data, paste0(paths$dataset, "/data"))
  expect_equal(paths$summary_table, paste0(paths$data, "/Summary_statistics.csv"))
  expect_equal(paths$centrality_data, paste0(paths$data, "/Centrality_data_"))
  expect_equal(paths$templates, system.file("Templates", package = "NetworkAnalysis"))
})

test_that("create_output_paths function works correctly", {
  config <- list(
    output_path = file.path(tempdir(), "output", fsep = "/"),
    input_name = "input_file"
  )

  config <- create_output_paths(config)

  expect_true(dir.exists(config$output_path))
  expect_true(dir.exists(paste0(config$output_path, "/", config$input_name)))
  expect_true(dir.exists(paste0(config$output_path, "/", config$input_name, "/figures")))
  expect_true(dir.exists(paste0(config$output_path, "/", config$input_name, "/data")))
})

test_that("get_input_path function works correctly", {
  temp_file <- tempfile(fileext = ".csv")
  file.create(temp_file)
  config <- list(file_path = temp_file)
  expect_equal(get_input_path(config), tools::file_path_as_absolute(config$file_path))
})

test_that("get_output_path function works correctly", {
  config <- list(output_path = file.path(tempdir(), "output", fsep = "/"))
  expect_equal(get_output_path(config), tools::file_path_as_absolute(config$output_path))
})

test_that("get_dataset_path function works correctly", {
  config <- list(output_path = file.path(tempdir(), "output", fsep = "/"), input_name = "input_file")
  expect_equal(get_dataset_path(config), paste0(config$output_path, "/", config$input_name))
})

test_that("get_figures_path function works correctly", {
  config <- list(output_path = file.path(tempdir(), "output", fsep = "/"), input_name = "input_file")
  expect_equal(get_figures_path(config), paste0(get_dataset_path(config), "/figures"))
})

test_that("get_data_path function works correctly", {
  config <- list(output_path = file.path(tempdir(), "output", fsep = "/"), input_name = "input_file")
  expect_equal(get_data_path(config), paste0(get_dataset_path(config), "/data"))
})

test_that("get_centrality_data_path function works correctly", {
  config <- list(output_path = file.path(tempdir(), "output", fsep = "/"), input_name = "input_file")
  expect_equal(get_centrality_data_path(config), paste0(get_data_path(config), "/Centrality_data_"))
})

test_that("get_papers_per_author_path function works correctly", {
  config <- list(output_path = file.path(tempdir(), "output", fsep = "/"), input_name = "input_file")
  expect_equal(get_papers_per_author_path(config), paste0(get_data_path(config), "/Papers_per_author_"))
})

test_that("get_summary_table_path function works correctly", {
  config <- list(output_path = file.path(tempdir(), "output", fsep = "/"), input_name = "input_file")
  expect_equal(get_summary_table_path(config), paste0(get_data_path(config), "/Summary_statistics.csv"))
})

test_that("get_templates_path function works correctly", {
  config <- list()
  expect_equal(get_templates_path(config), system.file("Templates", package = "NetworkAnalysis"))
})
