#' @export
Paths <- function(config) {

  config <- create_output_paths(config)

  paths <- list(input_file = get_input_path(config),
                output = get_output_path(config),
                dataset = get_dataset_path(config),
                data = get_data_path(config),
                summary_table = get_summary_table_path(config),
                centrality_data = get_centrality_data_path(config),
                cutpoints = get_cutpoints_path(config),
                vertex_attributes = get_vertex_attributes_path(config),
                papers_per_author = get_papers_per_author_path(config),
                templates = get_templates_path(config))

  class(paths) <- "Paths"

  paths
}

create_output_paths <- function(config) {
  if (!dir.exists(config$data$output_path)) {
    dir.create(config$data$output_path)
  }
  config$data$output_path <- get_output_path(config)
  dataset_path <- get_dataset_path(config)
  if (!dir.exists(dataset_path)) {
    dir.create(dataset_path, recursive = TRUE)
  }
  data_path <- get_data_path(config)
  if (!dir.exists(data_path)) {
    dir.create(data_path, recursive = TRUE)
  }
  config
}

get_input_path <- function(config) {
  normalizePath(config$data$file_path, winslash = "/")
}

get_output_path <- function(config) {
  normalizePath(config$data$output_path, winslash = "/")
}

get_dataset_path <- function(config) {
  paste0(config$data$output_path,
         "/",
         config$.meta$input_name,
         config$data$output_suffix)
}

get_data_path <- function(config) {
  paste0(get_dataset_path(config), "/data")
}

get_centrality_data_path <- function(config) {
  paste0(get_data_path(config), "/Centrality_data_")
}

get_cutpoints_path <- function(config) {
  paste0(get_data_path(config), "/Cutpoints_")
}

get_vertex_attributes_path <- function(config) {
  paste0(get_data_path(config), "/Vertex_attributes_")
}

get_papers_per_author_path <- function(config) {
  paste0(get_data_path(config), "/Papers_per_author_")
}

get_summary_table_path <- function(config) {
  paste0(get_data_path(config), "/Summary_statistics.csv")
}

get_templates_path <- function(config) {
  system.file("Templates", package = "NetworkAnalysis")
}
