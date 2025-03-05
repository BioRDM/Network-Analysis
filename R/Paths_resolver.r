#' @export
Paths <- function(config) {

  create_output_paths(config)

  paths <- list(input_file = config$file_path,
                output = get_output_path(config),
                figures = get_figures_path(config),
                summary_table = get_summary_table_path(config))

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
  return(list(output = output_path, figures = figures_path))
}

get_output_path <- function(config) {
  return(paste0(config$output_path, "/", tools::file_path_sans_ext(basename(config$file_path))))
}

get_figures_path <- function(config) {
  return(paste0(get_output_path(config), "/figures"))
}

get_summary_table_path <- function(config) {
  return(paste0(get_output_path(config), "/Summary_statistics.csv"))
}
