temp_file <- tempfile(fileext = ".csv")
file.create(temp_file)
config <- list(
  data = list(
    file_path = temp_file,
    output_path = file.path(tempdir(), "output", fsep = "/"),
    output_suffix = "_test"
  ),
  .meta = list(
    input_name = "input_file"
  )
)

test_that("Paths function works correctly", {
  paths <- Paths(config)

  expect_equal(class(paths), "Paths")
  expect_equal(paths$input_file, tools::file_path_as_absolute(config$data$file_path))
  expect_equal(paths$output, tools::file_path_as_absolute(config$data$output_path))
  expect_equal(paths$dataset, paste0(normalizePath(config$data$output_path, winslash = "/"),
                                     "/",
                                     config$.meta$input_name,
                                     config$data$output_suffix))
  expect_equal(paths$data, paste0(paths$dataset, "/data"))
  expect_equal(paths$summary_table, paste0(paths$data, "/Summary_statistics.csv"))
  expect_equal(paths$centrality_data, paste0(paths$data, "/Centrality_data_"))
  expect_equal(paths$templates, system.file("Templates", package = "NetworkAnalysis"))
})

test_that("create_output_paths function works correctly", {
  config <- create_output_paths(config)

  expect_true(dir.exists(config$data$output_path))
  expect_true(dir.exists(paste0(config$data$output_path, "/", config$.meta$input_name)))
  expect_true(dir.exists(paste0(config$data$output_path, "/", config$.meta$input_name, "/data")))
})

test_that("get_input_path function works correctly", {
  expect_equal(get_input_path(config), tools::file_path_as_absolute(config$data$file_path))
})

test_that("get_output_path function works correctly", {
  expect_equal(get_output_path(config), tools::file_path_as_absolute(config$data$output_path))
})

test_that("get_dataset_path function works correctly", {
  expect_equal(get_dataset_path(config), paste0(config$data$output_path,
                                                "/",
                                                config$.meta$input_name,
                                                config$data$output_suffix))
})

test_that("get_data_path function works correctly", {
  expect_equal(get_data_path(config), paste0(get_dataset_path(config), "/data"))
})

test_that("get_centrality_data_path function works correctly", {
  expect_equal(get_centrality_data_path(config), paste0(get_data_path(config), "/Centrality_data_"))
})

test_that("get_papers_per_author_path function works correctly", {
  expect_equal(get_papers_per_author_path(config), paste0(get_data_path(config), "/Papers_per_author_"))
})

test_that("get_summary_table_path function works correctly", {
  expect_equal(get_summary_table_path(config), paste0(get_data_path(config), "/Summary_statistics.csv"))
})

test_that("get_templates_path function works correctly", {
  expect_equal(get_templates_path(config), system.file("Templates", package = "NetworkAnalysis"))
})
