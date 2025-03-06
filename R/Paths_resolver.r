#' @export
Paths <- function(config) {

  config$cur_dir <- getwd()
  create_output_paths(config)

  paths <- list(input_file = config$file_path,
                output = get_output_path(config),
                figures = get_figures_path(config),
                summary_table = get_summary_table_path(config),
                centrality_data = get_centrality_data_path(config),
                templates = get_templates_path(config))

  # Assign the class name
  class(paths) <- "Paths"

  return(paths)
}

create_output_paths <- function(config) {
  output_path <- get_output_path(config)
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }
  figures_path <- get_figures_path(config)
  if (!dir.exists(figures_path)) {
    dir.create(figures_path, recursive = TRUE)
  }
  data_path <- get_data_path(config)
  if (!dir.exists(data_path)) {
    dir.create(data_path, recursive = TRUE)
  }
  return(list(output = output_path, figures = figures_path, data = data_path))
}

get_output_path <- function(config) {
  return(paste0(config$cur_dir, "/", config$output_path, "/", config$input_file))
}

get_figures_path <- function(config) {
  return(paste0(get_output_path(config), "/figures"))
}

get_data_path <- function(config) {
  return(paste0(get_output_path(config), "/data"))
}

get_centrality_data_path <- function(config) {
  return(paste0(get_data_path(config), "/Centrality_data_"))
}

get_summary_table_path <- function(config) {
  return(paste0(get_data_path(config), "/Summary_statistics.csv"))
}

get_templates_path <- function(config) {
  return(paste0(system.file(package = "NetworkAnalysis"), "/R/Templates"))
}