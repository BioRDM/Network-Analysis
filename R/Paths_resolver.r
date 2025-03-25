#' @export
Paths <- function(config) {

  config <- create_output_paths(config)

  paths <- list(input_file = get_input_path(config),
                output = get_output_path(config),
                dataset = get_dataset_path(config),
                figures = get_figures_path(config),
                data = get_data_path(config),
                summary_table = get_summary_table_path(config),
                centrality_data = get_centrality_data_path(config),
                papers_per_author = get_papers_per_author_path(config),
                templates = get_templates_path(config))

  # Assign the class name
  class(paths) <- "Paths"

  return(paths)
}

create_output_paths <- function(config) {
  if (!dir.exists(config$output_path)) {
    dir.create(config$output_path)
  }
  config$output_path <- get_output_path(config)
  dataset_path <- get_dataset_path(config)
  if (!dir.exists(dataset_path)) {
    dir.create(dataset_path, recursive = TRUE)
  }
  figures_path <- get_figures_path(config)
  if (!dir.exists(figures_path)) {
    dir.create(figures_path, recursive = TRUE)
  }
  data_path <- get_data_path(config)
  if (!dir.exists(data_path)) {
    dir.create(data_path, recursive = TRUE)
  }
  return(config)
}

get_input_path <- function(config) {
  return(normalizePath(config$file_path, winslash = "/"))
}

get_output_path <- function(config) {
  return(normalizePath(config$output_path, winslash = "/"))
}

get_dataset_path <- function(config) {
  return(paste0(config$output_path, "/", config$input_name))
}

get_figures_path <- function(config) {
  return(paste0(get_dataset_path(config), "/figures"))
}

get_data_path <- function(config) {
  return(paste0(get_dataset_path(config), "/data"))
}

get_centrality_data_path <- function(config) {
  return(paste0(get_data_path(config), "/Centrality_data_"))
}

get_papers_per_author_path <- function(config) {
  return(paste0(get_data_path(config), "/Papers_per_author_"))
}

get_summary_table_path <- function(config) {
  return(paste0(get_data_path(config), "/Summary_statistics.csv"))
}

get_templates_path <- function(config) {
  return(system.file("Templates", package = "NetworkAnalysis"))
}